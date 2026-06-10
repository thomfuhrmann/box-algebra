use malachite::{
    Natural,
    base::num::arithmetic::traits::{SaturatingSub, SaturatingSubAssign},
};

use std::{
    cmp::Ordering::Equal,
    hash::{BuildHasher, Hash, Hasher},
    marker::PhantomData,
    ops::{Add, Mul},
};

use rapidhash::{HashMapExt, RapidHashMap, fast::RandomState};

/// Kind of boxes that can exist in an store
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum BoxKind {
    Box,
    Num,
    Polynum,
    Multinum,
    Pixel,
    Maxel,
}

/// Trait that describes the type of a box
pub trait BoxType: Sized + Clone {
    const KIND: BoxKind;

    /// Validates whether a raw uncommitted slice layout fits the structural constraints
    fn validate_layout(raw: RawBox<'_, Self>) -> bool;

    /// Defines whether the immediate children of this box are in a predefined order
    fn is_ordered() -> bool {
        false
    }
}

pub trait Num: BoxType {}
pub trait Polynum: Num {}
pub trait Multinum: Polynum {}

#[derive(Debug, Clone, Copy)]
pub struct AnyBox;
impl BoxType for AnyBox {
    const KIND: BoxKind = BoxKind::Box;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NumBox;
impl BoxType for NumBox {
    const KIND: BoxKind = BoxKind::Num;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        todo!()
    }
}
impl Num for NumBox {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PolynumBox;
impl BoxType for PolynumBox {
    const KIND: BoxKind = BoxKind::Polynum;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        todo!()
    }
}
impl Num for PolynumBox {}
impl Polynum for PolynumBox {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MultinumBox;
impl BoxType for MultinumBox {
    const KIND: BoxKind = BoxKind::Multinum;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        todo!()
    }
}
impl Num for MultinumBox {}
impl Polynum for MultinumBox {}
impl Multinum for MultinumBox {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PixelBox;
impl BoxType for PixelBox {
    const KIND: BoxKind = BoxKind::Pixel;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        todo!()
    }

    fn is_ordered() -> bool {
        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MaxelBox;
impl BoxType for MaxelBox {
    const KIND: BoxKind = BoxKind::Maxel;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        todo!()
    }
}

/// Trait for defining the output type of an addition between two boxes
pub trait BoxAdd<Rhs = Self> {
    type Output: BoxType;
}

impl<T: BoxType> BoxAdd for T {
    type Output = Self;
}

macro_rules! impl_box_add {
    ($lhs:ty, $rhs:ty => $out:ty) => {
        impl BoxAdd<$rhs> for $lhs {
            type Output = $out;
        }
        impl BoxAdd<$lhs> for $rhs {
            type Output = $out;
        }
    };
}

impl_box_add!(NumBox, PolynumBox => PolynumBox);
impl_box_add!(NumBox, MultinumBox => MultinumBox);
impl_box_add!(PolynumBox, MultinumBox => MultinumBox);

/// Trait for defining the output type of a multiplication between two boxes
pub trait BoxMul<Rhs = Self> {
    type Output: BoxType;
}

impl<T: BoxType> BoxMul for T {
    type Output = Self;
}

macro_rules! impl_box_mul {
    ($lhs:ty, $rhs:ty => $out:ty) => {
        impl BoxMul<$rhs> for $lhs {
            type Output = $out;
        }
        impl BoxMul<$lhs> for $rhs {
            type Output = $out;
        }
    };
}

impl_box_mul!(NumBox, PolynumBox => PolynumBox);
impl_box_mul!(NumBox, MultinumBox => MultinumBox);
impl_box_mul!(PolynumBox, MultinumBox => MultinumBox);

#[derive(Debug)]
pub struct BoxId<T: BoxType> {
    index: u32,
    _marker: std::marker::PhantomData<T>,
}

impl<T: BoxType> Clone for BoxId<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T: BoxType> Copy for BoxId<T> {}
impl<T: BoxType> PartialEq for BoxId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}
impl<T: BoxType> Eq for BoxId<T> {}

impl<T: BoxType> BoxId<T> {
    #[inline(always)]
    pub const fn new(index: u32) -> Self {
        Self {
            index,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub fn index(self) -> u32 {
        self.index
    }

    #[inline(always)]
    pub fn into_any(self) -> BoxId<AnyBox> {
        BoxId::new(self.index())
    }
}

/// Raw uncomitted box data
#[derive(Debug, Clone)]
pub struct RawBox<'a, T: BoxType> {
    pub colors: &'a [Color],
    pub multiplicities: &'a [Natural],
    pub lengths: &'a [u32],
    _marker: PhantomData<T>,
}

impl<'a, T: BoxType> RawBox<'a, T> {
    fn new(colors: &'a [Color], multiplicities: &'a [Natural], lengths: &'a [u32]) -> Self {
        Self {
            colors,
            multiplicities,
            lengths,
            _marker: PhantomData,
        }
    }

    pub fn kind(&self) -> BoxKind {
        T::KIND
    }

    pub fn cast<U: BoxType>(self) -> RawBox<'a, U> {
        RawBox::<U>::new(self.colors, self.multiplicities, self.lengths)
    }

    /// Hashes the box
    fn hash(&self, random_state: &RandomState) -> u64 {
        let mut hasher = random_state.build_hasher();
        T::KIND.hash(&mut hasher);
        self.colors.hash(&mut hasher);
        self.multiplicities.hash(&mut hasher);
        self.lengths.hash(&mut hasher);
        hasher.finish()
    }

    /// Hashes the box ignoring their outer color and multiplicity
    fn hash_content(&self, random_state: &RandomState) -> u64 {
        let mut hasher = random_state.build_hasher();
        T::KIND.hash(&mut hasher);
        self.colors[1..].hash(&mut hasher);
        self.multiplicities[1..].hash(&mut hasher);
        self.lengths.hash(&mut hasher);
        hasher.finish()
    }

    /// Compares box contents ignoring outer colors and multiplicities
    fn cmp_content(&self, other: &Self) -> bool {
        let left_len = self.lengths[0] as usize;
        let right_len = other.lengths[0] as usize;

        if left_len != right_len {
            return false;
        }

        let left_range = 0..left_len;
        let right_range = 0..right_len;

        let left_inner = 1..left_len;
        let right_inner = 1..right_len;

        self.colors[left_inner.clone()] == other.colors[right_inner.clone()]
            && self.multiplicities[left_inner] == other.multiplicities[right_inner]
            && self.lengths[left_range.clone()] == other.lengths[right_range.clone()]
    }

    /// Returns the color of the outer box
    pub fn color(&self) -> Color {
        self.colors[0]
    }

    /// Returns the multiplicity of the outer box
    pub fn multiplicity(&self) -> &Natural {
        &self.multiplicities[0]
    }

    /// Returns the length of the outer box
    pub fn length(&self) -> u32 {
        self.lengths[0]
    }
}

impl<'a, T: BoxType> PartialEq for RawBox<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.colors == other.colors
            && self.multiplicities == other.multiplicities
            && self.lengths == other.lengths
    }
}

impl<'a, T: BoxType> Eq for RawBox<'a, T> {}

impl<'a, T: BoxType> From<&'a RawBoxOwned<T>> for RawBox<'a, T> {
    fn from(raw_owned: &'a RawBoxOwned<T>) -> Self {
        RawBox::new(
            &raw_owned.colors,
            &raw_owned.multiplicities,
            &raw_owned.lengths,
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct RawBoxOwned<T: BoxType> {
    colors: Vec<Color>,
    multiplicities: Vec<Natural>,
    lengths: Vec<u32>,
    _marker: PhantomData<T>,
}

impl<T: BoxType> Default for RawBoxOwned<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: BoxType> RawBoxOwned<T> {
    /// Initializes an empty raw box
    pub fn new() -> Self {
        Self {
            colors: vec![],
            multiplicities: vec![],
            lengths: vec![],
            _marker: PhantomData,
        }
    }

    /// Returns a reference to the raw box data
    pub fn as_raw(&self) -> RawBox<'_, T> {
        RawBox::new(&self.colors, &self.multiplicities, &self.lengths)
    }

    /// Hashes the box
    fn hash(&self, random_state: &RandomState) -> u64 {
        self.as_raw().hash(random_state)
    }

    /// Sorts the immediate child boxes of this box
    fn sort_immediate_children(&mut self) {
        if self.lengths.is_empty() {
            return;
        }

        let box_len = self.lengths[0] as usize;
        if box_len <= 1 {
            return;
        }

        let start_idx = 1;
        let end_idx = box_len;

        // collect offset ranges of immediate children
        let mut child_ranges = Vec::new();
        let mut curr = start_idx;
        while curr < end_idx {
            let len = self.lengths[curr] as usize;
            child_ranges.push((curr, len));
            curr += len;
        }

        if child_ranges.len() <= 1 {
            return;
        }

        // sort ranges
        child_ranges.sort_by(|&(start_a, len_a), &(start_b, len_b)| {
            let range_a = start_a..(start_a + len_a);
            let range_b = start_b..(start_b + len_b);

            let len_cmp = self.lengths[range_a.clone()].cmp(&self.lengths[range_b.clone()]);
            if len_cmp != Equal {
                return len_cmp;
            }

            let col_cmp = self.colors[range_a.clone()].cmp(&self.colors[range_b.clone()]);
            if col_cmp != Equal {
                return col_cmp;
            }

            self.multiplicities[range_a].cmp(&self.multiplicities[range_b])
        });

        // load staging buffers
        let content_len = end_idx - start_idx;
        let mut sorted_colors = Vec::with_capacity(content_len);
        let mut sorted_lens = Vec::with_capacity(content_len);
        let mut sorted_mults = Vec::with_capacity(content_len);

        for &(start, len) in &child_ranges {
            let range = start..(start + len);
            sorted_colors.extend_from_slice(&self.colors[range.clone()]);
            sorted_lens.extend_from_slice(&self.lengths[range.clone()]);

            for idx in range {
                let item = std::mem::take(&mut self.multiplicities[idx]);
                sorted_mults.push(item);
            }
        }

        // load target buffers
        let target_range = start_idx..end_idx;
        self.colors[target_range.clone()].copy_from_slice(&sorted_colors);
        self.lengths[target_range.clone()].copy_from_slice(&sorted_lens);

        for (dest_idx, src_natural) in target_range.zip(sorted_mults) {
            self.multiplicities[dest_idx] = src_natural;
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BoxState<T: BoxType> {
    /// Uncommitted box, holding its own raw vectors
    Uncommitted(RawBoxOwned<T>),
    /// Box that has been committed to the permanent store
    Committed(BoxId<T>),
}

impl<T: BoxType> BoxState<T> {
    pub fn new_uncommitted() -> Self {
        BoxState::Uncommitted(RawBoxOwned::new())
    }

    pub fn new_committed(box_id: BoxId<T>) -> Self {
        BoxState::Committed(box_id)
    }

    pub fn as_raw<'a>(&'a self, store: &'a BoxStore) -> RawBox<'a, T> {
        match self {
            BoxState::Uncommitted(owned) => owned.as_raw(),
            BoxState::Committed(box_id) => store.get_raw(*box_id),
        }
    }

    pub fn commit(self, store: &mut BoxStore) -> BoxId<T> {
        match self {
            BoxState::Uncommitted(raw_box) => store.commit(raw_box),
            BoxState::Committed(box_id) => box_id,
        }
    }

    pub fn sort_and_commit(self, store: &mut BoxStore) -> BoxId<T> {
        match self {
            BoxState::Uncommitted(raw_box) => store.sort_and_commit(raw_box),
            BoxState::Committed(box_id) => box_id,
        }
    }
}

/// Side of a box in a binary operation (e.g. addition)
#[derive(Debug, Clone, Copy)]
enum Side {
    Left,
    Right,
}

/// Color of a box
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Color {
    Black,
    Red,
}

impl Add<Color> for Color {
    type Output = Color;

    fn add(self, rhs: Color) -> Self::Output {
        match (self, rhs) {
            (Color::Black, Color::Black) => Color::Black,
            (Color::Black, Color::Red) => Color::Red,
            (Color::Red, Color::Black) => Color::Red,
            (Color::Red, Color::Red) => Color::Black,
        }
    }
}

impl Mul<Color> for Color {
    type Output = Color;

    fn mul(self, rhs: Color) -> Self::Output {
        match (self, rhs) {
            (Color::Black, Color::Black) => Color::Black,
            (Color::Black, Color::Red) => Color::Red,
            (Color::Red, Color::Black) => Color::Red,
            (Color::Red, Color::Red) => Color::Black,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoxConstants {
    pub zero: BoxId<NumBox>,
    pub anti_zero: BoxId<NumBox>,
    pub one: BoxId<NumBox>,
    pub neg_one: BoxId<NumBox>,
    pub anti_one: BoxId<NumBox>,
    pub anti_neg_one: BoxId<NumBox>,
    pub alpha: BoxId<PolynumBox>,
    pub neg_alpha: BoxId<PolynumBox>,
    pub anti_alpha: BoxId<PolynumBox>,
    pub anti_neg_alpha: BoxId<PolynumBox>,
}

/// Global store for box computations
#[derive(Debug)]
pub struct BoxStore {
    /// Box colors
    pub colors: Vec<Color>,
    /// Multiplicities of boxes
    pub multiplicities: Vec<Natural>,
    /// Numbers of rows occupied by boxes
    pub lengths: Vec<u32>,
    /// Kinds of boxes
    pub kinds: Vec<BoxKind>,
    /// Pointers to starting indices of active boxes
    pub active_boxes: Vec<BoxId<AnyBox>>,
    /// Global box cache
    pub cache: RapidHashMap<u64, BoxId<AnyBox>>,
    /// Random state for hasher
    pub random_state: RandomState,
    /// Predefined constants
    pub constants: BoxConstants,
}

impl Default for BoxStore {
    fn default() -> Self {
        Self::new()
    }
}

const BOX_STORE_CAPACITY: usize = 32;
impl BoxStore {
    /// Initializes the store with elementary boxes
    pub fn new() -> Self {
        let random_state = RandomState::new();

        let active_boxes = Vec::with_capacity(BOX_STORE_CAPACITY);
        let mut cache = RapidHashMap::new();

        let mut kinds = Vec::with_capacity(BOX_STORE_CAPACITY);
        let mut colors = Vec::with_capacity(BOX_STORE_CAPACITY);
        let mut multiplicities = Vec::with_capacity(BOX_STORE_CAPACITY);
        let mut lengths = Vec::with_capacity(BOX_STORE_CAPACITY);

        let mut register_box = |kind: BoxKind, hash: u64, raw_box: RawBox<'_, AnyBox>| {
            let index = lengths.len() as u32;
            let id = BoxId::<AnyBox>::new(index);
            // Commit data
            cache.insert(hash, id);
            kinds.push(kind);

            colors.extend_from_slice(raw_box.colors);
            multiplicities.extend_from_slice(raw_box.multiplicities);
            lengths.extend_from_slice(raw_box.lengths);
            index
        };

        let mult = [Natural::from(1_u32)];
        let raw_zero = RawBox::<NumBox>::new(&[Color::Black], &mult, &[1]);
        let zero_id = BoxId::<NumBox>::new(register_box(
            raw_zero.kind(),
            raw_zero.hash(&random_state),
            raw_zero.cast(),
        ));

        let raw_anti_zero = RawBox::<NumBox>::new(&[Color::Red], &mult, &[1]);
        let anti_zero_id = BoxId::<NumBox>::new(register_box(
            raw_anti_zero.kind(),
            raw_anti_zero.hash(&random_state),
            raw_anti_zero.cast(),
        ));

        let mult = [Natural::from(1_u32), Natural::from(1_u32)];
        let raw_one = RawBox::<NumBox>::new(&[Color::Black, Color::Black], &mult, &[2, 1]);
        let one_id = BoxId::<NumBox>::new(register_box(
            raw_one.kind(),
            raw_one.hash(&random_state),
            raw_one.cast(),
        ));

        let raw_neg_one = RawBox::<NumBox>::new(&[Color::Black, Color::Red], &mult, &[2, 1]);
        let neg_one_id = BoxId::<NumBox>::new(register_box(
            raw_neg_one.kind(),
            raw_neg_one.hash(&random_state),
            raw_neg_one.cast(),
        ));

        let raw_anti_one = RawBox::<NumBox>::new(&[Color::Red, Color::Black], &mult, &[2, 1]);
        let anti_one_id = BoxId::<NumBox>::new(register_box(
            raw_anti_one.kind(),
            raw_anti_one.hash(&random_state),
            raw_anti_one.cast(),
        ));

        let raw_anti_neg_one = RawBox::<NumBox>::new(&[Color::Red, Color::Red], &mult, &[2, 1]);
        let anti_neg_one_id = BoxId::<NumBox>::new(register_box(
            raw_anti_neg_one.kind(),
            raw_anti_neg_one.hash(&random_state),
            raw_anti_neg_one.cast(),
        ));

        let mult = [
            Natural::from(1_u32),
            Natural::from(1_u32),
            Natural::from(1_u32),
        ];
        let raw_alpha = RawBox::<PolynumBox>::new(
            &[Color::Black, Color::Black, Color::Black],
            &mult,
            &[3, 2, 1],
        );
        let alpha_id = BoxId::<PolynumBox>::new(register_box(
            raw_alpha.kind(),
            raw_alpha.hash(&random_state),
            raw_alpha.cast(),
        ));

        let raw_neg_alpha =
            RawBox::<PolynumBox>::new(&[Color::Black, Color::Red, Color::Black], &mult, &[3, 2, 1]);
        let neg_alpha_id = BoxId::<PolynumBox>::new(register_box(
            raw_neg_alpha.kind(),
            raw_neg_alpha.hash(&random_state),
            raw_neg_alpha.cast(),
        ));

        let raw_anti_alpha =
            RawBox::<PolynumBox>::new(&[Color::Red, Color::Black, Color::Black], &mult, &[3, 2, 1]);
        let anti_alpha_id = BoxId::<PolynumBox>::new(register_box(
            raw_anti_alpha.kind(),
            raw_anti_alpha.hash(&random_state),
            raw_anti_alpha.cast(),
        ));

        let raw_anti_neg_alpha =
            RawBox::<PolynumBox>::new(&[Color::Red, Color::Red, Color::Black], &mult, &[3, 2, 1]);
        let anti_neg_alpha_id = BoxId::<PolynumBox>::new(register_box(
            raw_anti_neg_alpha.kind(),
            raw_anti_neg_alpha.hash(&random_state),
            raw_anti_neg_alpha.cast(),
        ));

        let constants = BoxConstants {
            zero: zero_id,
            anti_zero: anti_zero_id,
            one: one_id,
            anti_one: anti_one_id,
            alpha: alpha_id,
            neg_alpha: neg_alpha_id,
            anti_alpha: anti_alpha_id,
            anti_neg_alpha: anti_neg_alpha_id,
            neg_one: neg_one_id,
            anti_neg_one: anti_neg_one_id,
        };

        Self {
            kinds,
            colors,
            multiplicities,
            lengths,
            active_boxes,
            cache,
            random_state,
            constants,
        }
    }

    /// Returns the next available ID
    #[inline(always)]
    fn next_id<T: BoxType>(&self) -> BoxId<T> {
        BoxId::new(self.colors.len() as u32)
    }

    /// Getters for predefined constants
    pub fn zero(&self) -> BoxState<NumBox> {
        BoxState::Committed(self.constants.zero)
    }

    pub fn anti_zero(&self) -> BoxState<NumBox> {
        BoxState::Committed(self.constants.anti_zero)
    }

    pub fn one(&self) -> BoxState<NumBox> {
        BoxState::Committed(self.constants.one)
    }

    pub fn neg_one(&self) -> BoxState<NumBox> {
        BoxState::Committed(self.constants.neg_one)
    }

    pub fn anti_one(&self) -> BoxState<NumBox> {
        BoxState::Committed(self.constants.anti_one)
    }

    pub fn anti_neg_one(&self) -> BoxState<NumBox> {
        BoxState::Committed(self.constants.anti_neg_one)
    }

    pub fn alpha(&self) -> BoxState<PolynumBox> {
        BoxState::Committed(self.constants.alpha)
    }

    pub fn neg_alpha(&self) -> BoxState<PolynumBox> {
        BoxState::Committed(self.constants.neg_alpha)
    }

    pub fn anti_alpha(&self) -> BoxState<PolynumBox> {
        BoxState::Committed(self.constants.anti_alpha)
    }

    pub fn anti_neg_alpha(&self) -> BoxState<PolynumBox> {
        BoxState::Committed(self.constants.anti_neg_alpha)
    }

    /// Gets the raw data of a box given by its id
    pub fn get_raw<T: BoxType>(&self, box_id: BoxId<T>) -> RawBox<'_, T> {
        let idx = box_id.index() as usize;
        let len = self.lengths[idx] as usize;
        RawBox::new(
            &self.colors[idx..(idx + len)],
            &self.multiplicities[idx..(idx + len)],
            &self.lengths[idx..(idx + len)],
        )
    }

    /// Commits a raw, uncommitted box
    fn commit<T: BoxType>(&mut self, raw: RawBoxOwned<T>) -> BoxId<T> {
        let hash = raw.hash(&self.random_state);

        // cache hit
        if let Some(&box_id) = self.cache.get(&hash) {
            return BoxId::<T>::new(box_id.index());
        }

        // cache miss - commit to store
        let new_id = self.next_id();
        self.kinds.push(T::KIND);
        self.cache.insert(hash, new_id);
        self.active_boxes.push(new_id);

        self.colors.extend(raw.colors);
        self.multiplicities.extend(raw.multiplicities);
        self.lengths.extend(raw.lengths);

        BoxId::<T>::new(new_id.index())
    }

    /// Sorts the box before commiting it
    fn sort_and_commit<T: BoxType>(&mut self, mut raw: RawBoxOwned<T>) -> BoxId<T> {
        raw.sort_immediate_children();
        self.commit(raw)
    }

    /// Wraps an existing expression in a new box container
    pub fn wrap_in_box<T: BoxType, U: BoxType>(
        &mut self,
        source: &BoxState<T>,
        color: Color,
        multiplicity: Natural,
    ) -> BoxState<U> {
        let raw = source.as_raw(self);
        let source_len = raw.length() as usize;

        let mut result = RawBoxOwned::<U>::new();
        result.colors.push(color);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1 + source_len as u32);

        // copy source elements
        for i in 0..source_len {
            result.colors.push(raw.colors[i]);

            if i == 0 {
                result.multiplicities.push(multiplicity.clone());
            } else {
                result.multiplicities.push(raw.multiplicities[i].clone());
            }

            result.lengths.push(raw.lengths[i]);
        }

        BoxState::Uncommitted(result)
    }

    pub fn from_u32(&mut self, num: u32) -> BoxState<NumBox> {
        self.wrap_in_box(&self.zero(), Color::Black, Natural::from(num))
    }

    pub fn from_u64(&mut self, num: u64) -> BoxState<NumBox> {
        self.wrap_in_box(&self.zero(), Color::Black, Natural::from(num))
    }

    pub fn from_i32(&mut self, num: i32) -> BoxState<NumBox> {
        if num < 0 {
            self.wrap_in_box(
                &self.anti_zero(),
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        } else {
            self.wrap_in_box(
                &self.zero(),
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        }
    }

    pub fn from_i64(&mut self, num: i64) -> BoxState<NumBox> {
        if num < 0 {
            self.wrap_in_box(
                &self.anti_zero(),
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        } else {
            self.wrap_in_box(
                &self.zero(),
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        }
    }

    /// Returns the outer color of a box
    pub fn box_color<T: BoxType>(&self, box_id: BoxId<T>) -> Color {
        self.colors[box_id.index() as usize]
    }

    /// Returns the arity of a box
    pub fn box_multiplicity<T: BoxType>(&self, box_id: BoxId<T>) -> Natural {
        self.multiplicities[box_id.index() as usize].clone()
    }

    /// Returns the number of rows occupied by this box (including itself)
    pub fn box_len<T: BoxType>(&self, box_id: BoxId<T>) -> u32 {
        self.lengths[box_id.index() as usize]
    }

    /// Computes the arity of a box
    pub fn box_arity<T: BoxType>(&self, box_id: BoxId<T>) -> u32 {
        let len = self.box_len(box_id);
        if len == 0 {
            return 0;
        }

        let mut count = 0;
        let mut idx = box_id.index() + 1;
        let end = idx + len;

        while idx < end {
            count += 1;
            // idx += self.lengths[idx as usize];
            let child_len = self.lengths[idx as usize];

            // Advance the pointer past the entire child sub-tree
            idx += child_len;
        }
        count
    }

    /// Checks if two boxes have the same content structure ignoring their outer colors and multiplicities
    pub fn cmp_content<T: BoxType>(&self, left: BoxId<T>, right: BoxId<T>) -> bool {
        let raw_left = self.get_raw(left);
        let raw_right = self.get_raw(right);
        raw_left.cmp_content(&raw_right)
    }

    /// Hashes the content of a box ignoring its outer color and multiplicity
    pub fn hash_content<T: BoxType>(&self, box_id: BoxId<T>) -> u64 {
        let raw = self.get_raw(box_id);
        raw.hash_content(&self.random_state)
    }

    /// Internal helper that does not commit to the store
    fn add_raw<'a, 'b, L, R>(
        &self,
        lhs: RawBox<'a, L>,
        rhs: RawBox<'b, R>,
        result: &mut RawBoxOwned<L::Output>,
    ) where
        L: BoxType + BoxAdd<R>,
        R: BoxType,
    {
        let mut unique_children: RapidHashMap<u64, (Side, usize, Color, Natural)> =
            RapidHashMap::new();

        let mut process_child_boxes = |raw_box: RawBox<AnyBox>, side: Side| {
            let box_len = raw_box.length() as usize;
            let mut curr = 1;

            while curr < box_len {
                let curr_mul = raw_box.multiplicities[curr].clone();
                let curr_col = raw_box.colors[curr];
                let curr_len = raw_box.lengths[curr] as usize;
                let curr_raw = RawBox::<AnyBox>::new(
                    &raw_box.colors[curr..(curr + curr_len)],
                    &raw_box.multiplicities[curr..(curr + curr_len)],
                    &raw_box.lengths[curr..(curr + curr_len)],
                );
                let struct_hash = curr_raw.hash_content(&self.random_state);

                let mut found_match = false;
                if let Some((other_side, idx, other_col, other_mul)) =
                    unique_children.get_mut(&struct_hash)
                {
                    let other_raw = match *other_side {
                        Side::Left => RawBox::<AnyBox>::new(
                            &lhs.colors[*idx..(*idx + lhs.lengths[*idx] as usize)],
                            &lhs.multiplicities[*idx..(*idx + lhs.lengths[*idx] as usize)],
                            &lhs.lengths[*idx..(*idx + lhs.lengths[*idx] as usize)],
                        ),
                        Side::Right => RawBox::<AnyBox>::new(
                            &rhs.colors[*idx..(*idx + rhs.lengths[*idx] as usize)],
                            &rhs.multiplicities[*idx..(*idx + rhs.lengths[*idx] as usize)],
                            &rhs.lengths[*idx..(*idx + rhs.lengths[*idx] as usize)],
                        ),
                    };
                    if curr_raw.cmp_content(&other_raw) {
                        let curr_mul = curr_mul.clone();
                        if curr_col + *other_col == Color::Red {
                            if curr_mul < *other_mul {
                                other_mul.saturating_sub_assign(curr_mul);
                            } else {
                                *other_mul = curr_mul.saturating_sub(other_mul.clone());
                                *other_col = curr_col;
                            }
                        } else {
                            *other_mul += curr_mul;
                        }
                        found_match = true;
                    }
                }

                if !found_match {
                    unique_children.insert(struct_hash, (side, curr, curr_col, curr_mul.clone()));
                }

                curr += curr_len;
            }
        };

        let lhs_col = lhs.color();
        let rhs_col = rhs.color();

        process_child_boxes(lhs.clone().cast(), Side::Left);
        process_child_boxes(rhs.clone().cast(), Side::Right);

        result.colors.push(lhs_col + rhs_col);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(0);

        let mut written_len = 0;
        for (curr_side, curr_idx, curr_col, curr_mul) in unique_children.values() {
            if *curr_mul == 0 {
                continue;
            }

            let curr_raw = match *curr_side {
                Side::Left => RawBox::<AnyBox>::new(
                    &lhs.colors[*curr_idx..(*curr_idx + lhs.lengths[*curr_idx] as usize)],
                    &lhs.multiplicities[*curr_idx..(*curr_idx + lhs.lengths[*curr_idx] as usize)],
                    &lhs.lengths[*curr_idx..(*curr_idx + lhs.lengths[*curr_idx] as usize)],
                ),
                Side::Right => RawBox::<AnyBox>::new(
                    &rhs.colors[*curr_idx..(*curr_idx + rhs.lengths[*curr_idx] as usize)],
                    &rhs.multiplicities[*curr_idx..(*curr_idx + rhs.lengths[*curr_idx] as usize)],
                    &rhs.lengths[*curr_idx..(*curr_idx + rhs.lengths[*curr_idx] as usize)],
                ),
            };

            let len = curr_raw.length() as usize;
            for i in 0..len {
                if i == 0 {
                    result.colors.push(*curr_col);
                    result.multiplicities.push(curr_mul.clone());
                } else {
                    result.colors.push(curr_raw.colors[i]);
                    result
                        .multiplicities
                        .push(curr_raw.multiplicities[i].clone());
                }

                result.lengths.push(curr_raw.lengths[i]);
                written_len += 1;
            }
        }

        result.lengths[0] = (1 + written_len) as u32;
    }

    /// Adds two boxes
    pub fn add<L, R>(&mut self, lhs: &BoxState<L>, rhs: &BoxState<R>) -> BoxState<L::Output>
    where
        L: BoxType + BoxAdd<R>,
        R: BoxType,
    {
        let lhs_raw = lhs.as_raw(self);
        let rhs_raw = rhs.as_raw(self);
        let mut result = RawBoxOwned::<L::Output>::new();
        self.add_raw(lhs_raw, rhs_raw, &mut result);

        BoxState::Uncommitted(result)
    }

    /// Multiplication of boxes
    pub fn mul<L, R>(&mut self, lhs: BoxId<L>, rhs: BoxId<R>) -> BoxId<L::Output>
    where
        L: BoxType + BoxMul<R>,
        R: BoxType,
    {
        // extract left children
        let mut lhs_children = Vec::new();
        let lhs_start = lhs.index();
        let lhs_len = self.box_len(lhs);
        let mut curr = lhs_start + 1;
        let end = lhs_start + lhs_len;
        while curr < end {
            let child_id = BoxId::<AnyBox>::new(curr);
            lhs_children.push(child_id);
            curr += self.lengths[curr as usize];
        }

        // extract right children
        let mut rhs_children = Vec::new();
        let rhs_start = rhs.index();
        let rhs_len = self.box_len(rhs);
        let mut curr = rhs_start + 1;
        let end = rhs_start + rhs_len;
        while curr < end {
            let child_id = BoxId::<AnyBox>::new(curr);
            rhs_children.push(child_id);
            curr += self.lengths[curr as usize];
        }

        // let mut unique_children: RapidHashMap<u64, (BoxId, Color, Natural)> = RapidHashMap::new();
        let mut unique_children: RapidHashMap<u64, RawBoxOwned<AnyBox>> = RapidHashMap::new();

        for &left_child in &lhs_children {
            for &right_child in &rhs_children {
                let mut curr_owned = RawBoxOwned::new();
                let left_raw = self.get_raw(left_child);
                let right_raw = self.get_raw(right_child);
                self.add_raw(left_raw, right_raw, &mut curr_owned);
                curr_owned.sort_immediate_children();

                let curr_col = curr_owned.colors[0];
                let left_mul = self.box_multiplicity(left_child);
                let right_mul = self.box_multiplicity(right_child);
                let curr_mul = left_mul * right_mul;

                let curr_raw = curr_owned.as_raw();
                let struct_hash = curr_raw.hash_content(&self.random_state);

                let mut found_match = false;
                if let Some(other_owned) = unique_children.get_mut(&struct_hash) {
                    let other_raw = other_owned.as_raw();
                    if curr_raw.cmp_content(&other_raw) {
                        let other_col = &mut other_owned.colors[0];
                        let other_mul = &mut other_owned.multiplicities[0];
                        let curr_mul = curr_mul.clone();
                        if curr_col + *other_col == Color::Red {
                            if &curr_mul < other_mul {
                                other_mul.saturating_sub_assign(curr_mul);
                            } else {
                                *other_mul = curr_mul.saturating_sub(other_mul.clone());
                                *other_col = curr_col;
                            }
                        } else {
                            *other_mul += curr_mul;
                        }
                        found_match = true;
                    }
                }

                if !found_match {
                    curr_owned.multiplicities[0] = curr_mul.clone();
                    unique_children.insert(struct_hash, curr_owned);
                }
            }
        }

        let mut result = RawBoxOwned::<L::Output>::new();

        let lhs_col = self.box_color(lhs);
        let rhs_col = self.box_color(rhs);

        result.colors.push(lhs_col + rhs_col);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(0);

        let mut written_len = 0;
        for raw_box in unique_children.values() {
            let mul = raw_box.multiplicities[0].clone();
            if mul == 0 {
                continue;
            }

            let len = raw_box.lengths[0] as usize;
            let col = raw_box.colors[0];
            for i in 0..len {
                if i == 0 {
                    result.colors.push(col);
                    result.multiplicities.push(mul.clone());
                } else {
                    result.colors.push(raw_box.colors[i]);
                    result
                        .multiplicities
                        .push(raw_box.multiplicities[i].clone());
                }

                result.lengths.push(raw_box.lengths[i]);
                written_len += 1;
            }
        }

        result.lengths[0] = (1 + written_len) as u32;

        self.sort_and_commit(result)
    }

    // /// Instantiates a pixel from two boxes
    // pub fn pixel<T: BoxType>(&mut self, x: BoxId<T>, y: BoxId<T>) -> BoxId<PixelBox> {
    //     let x_raw = self.get_raw(x);
    //     let x_len = x_raw.length();

    //     let y_raw = self.get_raw(y);
    //     let y_len = y_raw.length();

    //     let mut colors = vec![Color::Black];
    //     let mut multiplicities = vec![Natural::from(1_u32)];
    //     let mut lengths = vec![1 + x_len + y_len];

    //     // copy source elements
    //     colors.extend_from_slice(x_raw.colors);
    //     multiplicities.extend_from_slice(x_raw.multiplicities);
    //     lengths.extend_from_slice(x_raw.lengths);

    //     colors.extend_from_slice(y_raw.colors);
    //     multiplicities.extend_from_slice(y_raw.multiplicities);
    //     lengths.extend_from_slice(y_raw.lengths);

    //     let raw = RawBox::new(&colors, &multiplicities, &lengths);
    //     self.commit(raw)
    // }

    // /// Returns the first component of a pixel
    // fn pixel_x(&self, pixel: BoxId<PixelBox>) -> BoxId<PixelBox> {
    //     let idx = pixel.index();
    //     BoxId::new(idx + 1)
    // }

    // /// Returns the second component of a pixel
    // fn pixel_y(&self, pixel: BoxId<PixelBox>) -> BoxId<PixelBox> {
    //     let idx = pixel.index();
    //     let idx_x = idx + 1;
    //     let len_x = self.lengths[idx_x as usize];
    //     BoxId::new(idx_x + len_x)
    // }

    // /// Multiplies two pixels
    // pub fn mul_pixel(
    //     &mut self,
    //     left: BoxId<PixelBox>,
    //     right: BoxId<PixelBox>,
    // ) -> Option<BoxId<PixelBox>> {
    //     let left_y = self.pixel_y(left);
    //     let right_x = self.pixel_x(right);

    //     let left_y_raw = self.get_raw(left_y);
    //     let right_x_raw = self.get_raw(right_x);

    //     if left_y_raw == right_x_raw {
    //         let left_x = self.pixel_x(left);
    //         let right_y = self.pixel_y(right);
    //         return Some(self.pixel(left_x, right_y));
    //     }

    //     None
    // }

    pub fn maxel(&mut self) -> BoxId<MaxelBox> {
        todo!()
    }
}

#[macro_export]
macro_rules! pixel_alt {
    ($store:expr, $x:expr, $y:expr) => {
        $store::pixel($x, $y)
    };
}

#[macro_export]
macro_rules! maxel_alt {
    ($store:ident, $([$x:expr, $y:expr]),* $(,)?) => {
        {
            let mut outer_box = $crate::MBox::new();
            $(
                let pix = $store::pixel(($x).into(), ($y).into());
                outer_box.insert_box(pix);
            )*
            outer_box
        }
    };
}

// impl BoxStore {
//     /// Inspects the serialized manifest to safely reconstruct a strongly-typed handle
//     pub fn get_typed_root<T: BoxType>(&self, index: u32) -> Option<BoxId<T>> {
//         let kind = self.kinds.get(index as usize)?;
//
//         if *kind == T::KIND {
//             Some(BoxId::<T>::new(index))
//         } else {
//             None
//         }
//     }
// }

// fn evaluate_expression(permanent_store: &mut BoxStore, expression: &Expression) -> BoxId<AnyBox> {
//     // 1. Allocate a fresh, isolated workspace for this evaluation run
//     let mut scratch_store = BoxStore::new(1);
//
//     // 2. Build a registry containing ONLY our volatile scratch spaces
//     let scratch_registry = ScratchRegistry::new(vec![&scratch_store]);
//
//     // 3. Execute the math. All intermediate nodes are written to the scratch store.
//     //    They generate lightweight, Copy-safe BoxIds instantly.
//     let lhs = scratch_store.add_constants(permanent_store.constants.identity);
//     let rhs = scratch_store.multiply_pixels(lhs, lhs);
//     let final_scratch_id = scratch_store.finalize_matrix(rhs);
//
//     // 4. Route the final result safely at the call site
//     if final_scratch_id.store_id == permanent_store.store_id {
//         // Edge case: The evaluation resulted in a structural no-op or direct global reference
//         final_scratch_id
//     } else {
//         // Standard path: Migrate the evaluated tree from the scratch zone to the permanent zone
//         permanent_store.commit_from_scratch(final_scratch_id, &scratch_registry)
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut store = BoxStore::new();
        let zero = store.zero();

        let two = store.add(&store.one(), &store.one());
        let expected = store.wrap_in_box::<_, NumBox>(&zero, Color::Black, Natural::from(2_u32));
        assert_eq!(two, expected);

        let minus_two = store.add(&store.neg_one(), &store.neg_one());
        let expected = store.wrap_in_box(&store.anti_zero(), Color::Black, Natural::from(2_u32));
        assert_eq!(minus_two, expected);

        let minus_one = store.wrap_in_box::<NumBox, NumBox>(
            &store.anti_zero(),
            Color::Black,
            Natural::from(1_u32),
        );
        let zero = store.add(&minus_one, &store.one());
        let zero_comm = zero.sort_and_commit(&mut store);
        assert_eq!(zero_comm, store.constants.zero);

        let two = store.from_u32(2);
        let three = store.from_u32(3);
        let five = store.add(&two, &three);
        let expected = store.from_u32(5);
        assert_eq!(five, expected);

        let minus_two = store.from_i32(-2);
        let three = store.from_u32(3);
        let one = store.add(&minus_two, &three);
        let expected = store.from_u32(1);
        assert_eq!(one, expected);

        let alpha = store.alpha();
        let two_alpha = store.add(&alpha, &alpha);
        let expected = store.wrap_in_box::<NumBox, PolynumBox>(
            &store.one(),
            Color::Black,
            Natural::from(2_u32),
        );
        assert_eq!(two_alpha, expected);

        let alpha_1 = store.wrap_in_box::<PolynumBox, MultinumBox>(
            &alpha,
            Color::Black,
            Natural::from(1_u32),
        );
        let two_alpha_1 = store.wrap_in_box::<PolynumBox, MultinumBox>(
            &alpha,
            Color::Black,
            Natural::from(2_u32),
        );
        let sum = store.add(&alpha_1, &two_alpha_1);
        let expected = store.wrap_in_box(&alpha, Color::Black, Natural::from(3_u32));
        assert_eq!(sum, expected);

        let one = store.one();
        let alpha = store.alpha();
        let sum_1 = store.add(&one, &alpha);
        let sum_2 = store.add(&alpha, &one);
        let s1_c = sum_1.sort_and_commit(&mut store);
        let s2_c = sum_2.sort_and_commit(&mut store);
        assert_eq!(s1_c, s2_c);
    }

    #[test]
    fn test_mul() {
        // let mut store = BoxStore::new();
        // let two = store.from_u32(2);
        // let three = store.from_u32(3);
        // let six = store.mul(two, three);
        // let expected = store.from_u32(6);
        // assert_eq!(six, expected);

        // let six_2 = store.mul(three, two);
        // assert_eq!(six, six_2);

        // let m_three = store.from_i32(-3);
        // let m_six = store.mul(two, m_three);
        // let expected = store.from_i32(-6);
        // assert_eq!(m_six, expected);

        // let alpha = store.alpha();
        // let alpha_2 = store.mul(alpha, alpha);
        // let two = store.from_u32(2);
        // let expected = store.wrap_in_box(two, Color::Black, Natural::from(1_u32));
        // assert_eq!(alpha_2, expected);

        // let one = store.one();
        // let alpha = store.alpha();
        // let p1 = store.add(BoxState::Committed(one), BoxState::Committed(alpha));
        // let p2 = store.add(
        //     BoxState::Committed(one),
        //     BoxState::Committed(store.neg_alpha()),
        // );
        // let prod = store.mul(BoxState::Committed(p1), BoxState::Committed(p2));
        // let anti_two =
        //     store.wrap_in_box::<NumBox, NumBox>(store.zero(), Color::Red, Natural::from(2_u32));
        // let neg_alpha_2 =
        //     store.wrap_in_box::<NumBox, PolynumBox>(anti_two, Color::Black, Natural::from(1_u32));
        // let expected = store.add(BoxState::Committed(one), BoxState::Committed(neg_alpha_2));
        // assert_eq!(prod, expected);
    }

    #[test]
    fn test_pixel() {
        // let mut store = BoxStore::new();

        // let one = store.one(); let two = store.from_u32(2);
        // let three = store.from_u32(3);

        // let p1 = store.pixel(one, two);

        // let p2 = store.pixel(two, three);

        // let p3 = store.mul_pixel(p1, p2);
        // let expected = store.pixel(one, three);

        // assert_eq!(p3, Some(expected));

        // let p4 = store.pixel(three, two);
        // let p5 = store.mul_pixel(p1, p4);
        // assert!(p5.is_none());
    }
}
