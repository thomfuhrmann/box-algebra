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

/// Kind of boxes that can exist in an arena
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

#[derive(Debug, Clone)]
pub struct AnyBox;
impl BoxType for AnyBox {
    const KIND: BoxKind = BoxKind::Box;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub struct NumBox;
impl BoxType for NumBox {
    const KIND: BoxKind = BoxKind::Num;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        todo!()
    }
}
impl Num for NumBox {}

#[derive(Debug, Clone)]
pub struct PolynumBox;
impl BoxType for PolynumBox {
    const KIND: BoxKind = BoxKind::Polynum;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        todo!()
    }
}
impl Num for PolynumBox {}
impl Polynum for PolynumBox {}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
#[repr(transparent)]
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
        BoxId::new(self.index)
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

/// Raw uncomitted mutable box data
#[derive(Debug)]
pub struct RawBoxMut<'a, T: BoxType> {
    colors: &'a mut [Color],
    multiplicities: &'a mut [Natural],
    lengths: &'a mut [u32],
    _marker: PhantomData<T>,
}

impl<'a, T: BoxType> RawBoxMut<'a, T> {
    fn new(
        colors: &'a mut [Color],
        multiplicities: &'a mut [Natural],
        lengths: &'a mut [u32],
    ) -> Self {
        Self {
            colors,
            multiplicities,
            lengths,
            _marker: PhantomData,
        }
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

impl<'a, T: BoxType> From<RawBoxMut<'a, T>> for RawBox<'a, T> {
    fn from(raw_mut: RawBoxMut<'a, T>) -> Self {
        RawBox::new(raw_mut.colors, raw_mut.multiplicities, raw_mut.lengths)
    }
}

#[derive(Debug)]
struct RawBoxOwned<T: BoxType> {
    colors: Vec<Color>,
    multiplicities: Vec<Natural>,
    lengths: Vec<u32>,
    _marker: PhantomData<T>,
}

impl<T: BoxType> RawBoxOwned<T> {
    pub fn new() -> Self {
        Self {
            colors: vec![],
            multiplicities: vec![],
            lengths: vec![],
            _marker: PhantomData,
        }
    }

    pub fn as_ref(&self) -> RawBox<'_, T> {
        RawBox::new(&self.colors, &self.multiplicities, &self.lengths)
    }

    pub fn as_mut(&mut self) -> RawBoxMut<'_, T> {
        RawBoxMut::new(
            &mut self.colors,
            &mut self.multiplicities,
            &mut self.lengths,
        )
    }
}

/// The color of a box: can be either black or red
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

/// Global arena for box computations
#[derive(Debug)]
pub struct BoxArena {
    /// Box colors
    pub colors: Vec<Color>,
    /// Multiplicities of boxes
    pub multiplicities: Vec<Natural>,
    /// Numbers of rows occupied by boxes
    pub lengths: Vec<u32>,
    /// Kinds of boxes
    pub kinds: Vec<BoxKind>,
    /// Pointers to starting indices of active boxes
    pub active_expressions: Vec<BoxId<AnyBox>>,
    /// Global box cache
    pub cache: RapidHashMap<u64, BoxId<AnyBox>>,
    /// Random state for hasher
    pub random_state: RandomState,
    /// Predefined constants
    pub constants: BoxConstants,
}

impl Default for BoxArena {
    fn default() -> Self {
        Self::new()
    }
}

const ARENA_INIT_CAPACITY: usize = 32;
impl BoxArena {
    /// Initializes the arena with elementary objects
    pub fn new() -> Self {
        let random_state = RandomState::new();

        let active_expressions = Vec::with_capacity(ARENA_INIT_CAPACITY);
        let mut cache = RapidHashMap::new();

        let mut kinds = Vec::with_capacity(ARENA_INIT_CAPACITY);
        let mut colors = Vec::with_capacity(ARENA_INIT_CAPACITY);
        let mut multiplicities = Vec::with_capacity(ARENA_INIT_CAPACITY);
        let mut lengths = Vec::with_capacity(ARENA_INIT_CAPACITY);

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
            active_expressions,
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
    pub fn zero(&self) -> BoxId<NumBox> {
        self.constants.zero
    }

    pub fn anti_zero(&self) -> BoxId<NumBox> {
        self.constants.anti_zero
    }

    pub fn one(&self) -> BoxId<NumBox> {
        self.constants.one
    }

    pub fn neg_one(&self) -> BoxId<NumBox> {
        self.constants.neg_one
    }

    pub fn anti_one(&self) -> BoxId<NumBox> {
        self.constants.anti_one
    }

    pub fn anti_neg_one(&self) -> BoxId<NumBox> {
        self.constants.anti_neg_one
    }

    pub fn alpha(&self) -> BoxId<PolynumBox> {
        self.constants.alpha
    }

    pub fn neg_alpha(&self) -> BoxId<PolynumBox> {
        self.constants.neg_alpha
    }

    pub fn anti_alpha(&self) -> BoxId<PolynumBox> {
        self.constants.anti_alpha
    }

    pub fn anti_neg_alpha(&self) -> BoxId<PolynumBox> {
        self.constants.anti_neg_alpha
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

    /// Commits a raw, uncommitted box view
    fn commit<T: BoxType>(&mut self, raw: RawBox<'_, T>) -> BoxId<T> {
        let hash = raw.hash(&self.random_state);

        // cache hit
        if let Some(&box_id) = self.cache.get(&hash) {
            return BoxId::<T>::new(box_id.index());
        }

        // cache miss - commit to arena
        let new_id = self.next_id();
        self.kinds.push(T::KIND);
        self.cache.insert(hash, new_id);
        self.active_expressions.push(new_id);

        self.colors.extend_from_slice(raw.colors);
        self.multiplicities.extend_from_slice(raw.multiplicities);
        self.lengths.extend_from_slice(raw.lengths);

        BoxId::<T>::new(new_id.index())
    }

    /// Sorts the box before commiting it
    fn commit_sorted<T: BoxType>(&mut self, mut raw: RawBoxMut<'_, T>) -> BoxId<T> {
        raw.sort_immediate_children();
        self.commit(RawBox::from(raw))
    }

    /// Wraps an existing expression in a new box container
    pub fn wrap_in_box<T: BoxType, U: BoxType>(
        &mut self,
        source: BoxId<T>,
        color: Color,
        multiplicity: Natural,
    ) -> BoxId<U> {
        let raw = self.get_raw(source);
        let source_len = raw.length() as usize;

        let mut colors = vec![color];
        let mut multiplicities = vec![Natural::from(1_u32)];
        let mut lengths = vec![1 + source_len as u32];

        // copy source elements
        for i in 0..source_len {
            colors.push(raw.colors[i]);

            if i == 0 {
                multiplicities.push(multiplicity.clone());
            } else {
                multiplicities.push(raw.multiplicities[i].clone());
            }

            lengths.push(raw.lengths[i]);
        }

        let raw = RawBox::new(&colors, &multiplicities, &lengths);

        self.commit(raw)
    }

    pub fn from_u32(&mut self, num: u32) -> BoxId<NumBox> {
        self.wrap_in_box(self.zero(), Color::Black, Natural::from(num))
    }

    pub fn from_u64(&mut self, num: u64) -> BoxId<NumBox> {
        self.wrap_in_box(self.zero(), Color::Black, Natural::from(num))
    }

    pub fn from_i32(&mut self, num: i32) -> BoxId<NumBox> {
        if num < 0 {
            self.wrap_in_box(
                self.anti_zero(),
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        } else {
            self.wrap_in_box(self.zero(), Color::Black, Natural::from(num.unsigned_abs()))
        }
    }

    pub fn from_i64(&mut self, num: i64) -> BoxId<NumBox> {
        if num < 0 {
            self.wrap_in_box(
                self.anti_zero(),
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        } else {
            self.wrap_in_box(self.zero(), Color::Black, Natural::from(num.unsigned_abs()))
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
            idx += self.lengths[idx as usize];
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

    /// Internal helper that does not commit to the arena
    fn add_scratch<L, R>(
        &self,
        lhs: BoxId<L>,
        rhs: BoxId<R>,
        scratch_box: &mut RawBoxOwned<L::Output>,
    ) where
        L: BoxType + BoxAdd<R>,
        R: BoxType,
    {
        let mut unique_children: RapidHashMap<u64, (BoxId<AnyBox>, Color, Natural)> =
            RapidHashMap::new();

        let mut process_child_boxes = |box_id: BoxId<AnyBox>| {
            let box_len = self.box_len(box_id);
            let start_idx = box_id.index();
            let mut curr = start_idx + 1;
            let end = start_idx + box_len;

            while curr < end {
                let curr_id = BoxId::new(curr);
                let curr_raw = self.get_raw(curr_id);
                let curr_mul = curr_raw.multiplicity();
                let curr_col = curr_raw.color();
                let curr_len = curr_raw.length();
                let struct_hash = curr_raw.hash_content(&self.random_state);

                let mut found_match = false;
                if let Some((other_id, other_col, other_mul)) =
                    unique_children.get_mut(&struct_hash)
                    && self.cmp_content(curr_id, *other_id)
                {
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

                if !found_match {
                    unique_children.insert(struct_hash, (curr_id, curr_col, curr_mul.clone()));
                }

                curr += curr_len;
            }
        };

        process_child_boxes(lhs.into_any());
        process_child_boxes(rhs.into_any());

        let lhs_col = self.box_color(lhs);
        let rhs_col = self.box_color(rhs);

        scratch_box.colors.push(lhs_col + rhs_col);
        scratch_box.multiplicities.push(Natural::from(1_u32));
        scratch_box.lengths.push(0);

        let mut written_len = 0;
        for (id, col, mul) in unique_children.values() {
            if *mul == 0 {
                continue;
            }

            let start = id.index() as usize;
            let len = self.box_len(*id) as usize;
            for i in 0..len {
                let src_idx = start + i;

                if i == 0 {
                    scratch_box.colors.push(*col);
                    scratch_box.multiplicities.push(mul.clone());
                } else {
                    scratch_box.colors.push(self.colors[src_idx]);
                    scratch_box
                        .multiplicities
                        .push(self.multiplicities[src_idx].clone());
                }

                scratch_box.lengths.push(self.lengths[src_idx]);
                written_len += 1;
            }
        }

        scratch_box.lengths[0] = (1 + written_len) as u32;
    }

    /// Adds two boxes (unordered)
    pub fn add<L, R>(&mut self, lhs: BoxId<L>, rhs: BoxId<R>) -> BoxId<L::Output>
    where
        L: BoxType + BoxAdd<R>,
        R: BoxType,
    {
        let mut scratch_box = RawBoxOwned::new();
        self.add_scratch(lhs, rhs, &mut scratch_box);
        let raw = scratch_box.as_mut();
        self.commit_sorted(raw)
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
                let mut curr_raw = RawBoxOwned::new();
                self.add_scratch(left_child, right_child, &mut curr_raw);

                let mut raw_mut = curr_raw.as_mut();
                raw_mut.sort_immediate_children();

                let curr_col = curr_raw.colors[0];
                let left_mul = self.box_multiplicity(left_child);
                let right_mul = self.box_multiplicity(right_child);
                let curr_mul = left_mul * right_mul;

                let curr_raw_ref = curr_raw.as_ref();
                let struct_hash = curr_raw_ref.hash_content(&self.random_state);

                let mut found_match = false;
                if let Some(other_raw) = unique_children.get_mut(&struct_hash) {
                    let other_raw_ref = other_raw.as_ref();
                    if curr_raw_ref.cmp_content(&other_raw_ref) {
                        let other_col = &mut other_raw.colors[0];
                        let other_mul = &mut other_raw.multiplicities[0];
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
                    curr_raw.multiplicities[0] = curr_mul.clone();
                    unique_children.insert(struct_hash, curr_raw);
                }
            }
        }

        let mut colors = vec![];
        let mut multiplicities = vec![];
        let mut lengths = vec![];

        let lhs_col = self.box_color(lhs);
        let rhs_col = self.box_color(rhs);
        let final_color = lhs_col + rhs_col;

        colors.push(final_color);
        multiplicities.push(Natural::from(1_u32));
        lengths.push(0);

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
                    colors.push(col);
                    multiplicities.push(mul.clone());
                } else {
                    colors.push(raw_box.colors[i]);
                    multiplicities.push(raw_box.multiplicities[i].clone());
                }

                lengths.push(raw_box.lengths[i]);
                written_len += 1;
            }
        }

        lengths[0] = (1 + written_len) as u32;

        let raw = RawBoxMut::new(&mut colors, &mut multiplicities, &mut lengths);
        self.commit_sorted(raw)
    }

    fn pixel_x(&self, pixel: BoxId<PixelBox>) -> BoxId<AnyBox> {
        let idx = pixel.index();
        BoxId::new(idx + 1)
    }

    fn pixel_y(&self, pixel: BoxId<PixelBox>) -> BoxId<AnyBox> {
        let idx = pixel.index();
        let idx_x = idx + 1;
        let len_x = self.lengths[idx_x as usize];
        BoxId::new(idx_x + len_x)
    }

    /// Multiplies two pixels
    pub fn mul_pixel(
        &mut self,
        left: BoxId<PixelBox>,
        right: BoxId<PixelBox>,
    ) -> Option<BoxId<PixelBox>> {
        let left_y = self.pixel_y(left);
        let right_x = self.pixel_x(right);

        let left_y_raw = self.get_raw(left_y);
        let right_x_raw = self.get_raw(right_x);

        if left_y_raw == right_x_raw {
            let left_x = self.pixel_x(left);
            let right_y = self.pixel_y(right);
            // return Some(self.pixel(left_x, right_y));
        }

        None
    }

    pub fn maxel(&mut self) -> BoxId<MaxelBox> {
        todo!()
    }
}

pub enum BoxView<'a, T: BoxType> {
    Id(BoxId<T>, &'a BoxArena),
    Transient(RawBox<'a, T>),
}

impl<'a, T: BoxType> BoxView<'a, T> {
    pub fn as_raw(&self) -> RawBox<'a, T> {
        match self {
            BoxView::Id(id, arena) => arena.get_raw(*id),
            BoxView::Transient(raw) => raw.clone(),
        }
    }
}

/// Workspace for building boxes with reusable scratch space to minimize heap allocations
pub struct BoxBuilder {
    pub colors: Vec<Color>,
    pub multiplicities: Vec<Natural>,
    pub lengths: Vec<u32>,
    pub scratch_x: Vec<Natural>,
    pub scratch_y: Vec<Natural>,
}

impl BoxBuilder {
    pub fn new() -> Self {
        Self {
            colors: Vec::with_capacity(64),
            multiplicities: Vec::with_capacity(64),
            lengths: Vec::with_capacity(64),
            scratch_x: Vec::with_capacity(4),
            scratch_y: Vec::with_capacity(4),
        }
    }

    #[inline]
    pub fn reset(&mut self) {
        self.colors.clear();
        self.multiplicities.clear();
        self.lengths.clear();
        self.scratch_x.clear();
        self.scratch_y.clear();
    }

    pub fn add_boxes<'a, T: BoxType>(
        &mut self,
        left: BoxView<'a, T>,
        right: BoxView<'a, T>,
    ) -> RawBox<'_, T> {
        todo!()
        // let mut arena = BoxArena::new();
        // let mut workspace = BoxBuilder::new();

        // let a = BoxView::Id(arena.constants.a, &arena);
        // let b = BoxView::Id(arena.constants.b, &arena);
        // let c = BoxView::Id(arena.constants.c, &arena);

        // let tmp_sum = workspace.add_boxes(a, b);

        // let final_raw_product = workspace.mul_boxes(BoxView::Transient(tmp_sum), c);

        // let result_id = arena.commit(final_raw_product);
    }
}

impl Default for BoxBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl BoxBuilder {
    pub fn mul_pixel(
        &mut self,
        arena: &BoxArena,
        left: BoxId<PixelBox>,
        right: BoxId<PixelBox>,
    ) -> Option<RawBox<'_, PixelBox>> {
        let left_y = arena.pixel_y(left);
        let right_x = arena.pixel_x(right);

        let left_y_raw = arena.get_raw(left_y);
        let right_x_raw = arena.get_raw(right_x);

        if left_y_raw == right_x_raw {
            let left_x = arena.pixel_x(left);
            let right_y = arena.pixel_y(right);
            let builder = PixelBuilder::new(arena, self);
            return Some(builder.build_pixel(left_x, right_y));
        }

        None
    }
}

pub enum BoxInput<'a, T: BoxType> {
    /// A permanent box that has been committed to the arena
    Committed(BoxId<T>),
    /// An uncommitted box view
    Transient(RawBox<'a, T>),
}

pub struct PixelBuilder<'a, 'b> {
    arena: &'a BoxArena,
    workspace: &'b mut BoxBuilder,
}

impl<'a, 'b> PixelBuilder<'a, 'b> {
    pub fn new(arena: &'a BoxArena, workspace: &'b mut BoxBuilder) -> Self {
        workspace.reset();
        Self { arena, workspace }
    }

    pub fn build_pixel<X, Y>(self, x: X, y: Y) -> RawBox<'b, PixelBox>
    where
        X: IntoRawBox,
        Y: IntoRawBox,
    {
        let x_raw = x.into_raw_box(self.arena, &mut self.workspace.scratch_x);
        let y_raw = y.into_raw_box(self.arena, &mut self.workspace.scratch_y);

        self.workspace.colors.push(Color::Black);
        self.workspace.multiplicities.push(Natural::from(1_u32));
        self.workspace
            .lengths
            .push(1 + x_raw.lengths.len() as u32 + y_raw.lengths.len() as u32);

        self.workspace.colors.extend_from_slice(x_raw.colors);
        self.workspace
            .multiplicities
            .extend_from_slice(x_raw.multiplicities);
        self.workspace.lengths.extend_from_slice(x_raw.lengths);

        self.workspace.colors.extend_from_slice(y_raw.colors);
        self.workspace
            .multiplicities
            .extend_from_slice(y_raw.multiplicities);
        self.workspace.lengths.extend_from_slice(y_raw.lengths);

        RawBox::new(
            &self.workspace.colors,
            &self.workspace.multiplicities,
            &self.workspace.lengths,
        )
    }
}

pub trait IntoRawBox {
    type Target: BoxType;

    /// Returns a raw view of the value without committing to the arena, for use in constructing new boxes
    fn into_raw_box<'a>(
        self,
        arena: &'a BoxArena,
        scratch: &'a mut Vec<Natural>,
    ) -> RawBox<'a, Self::Target>;
}

impl<T: BoxType> IntoRawBox for BoxId<T> {
    type Target = T;

    #[inline]
    fn into_raw_box<'a>(
        self,
        arena: &'a BoxArena,
        _scratch: &'a mut Vec<Natural>,
    ) -> RawBox<'a, Self::Target> {
        arena.get_raw(self)
    }
}

impl IntoRawBox for u32 {
    type Target = NumBox;

    #[inline]
    fn into_raw_box<'a>(
        self,
        _arena: &'a BoxArena,
        scratch: &'a mut Vec<Natural>,
    ) -> RawBox<'a, Self::Target> {
        scratch.push(Natural::from(1_u32));
        scratch.push(Natural::from(self));
        RawBox::<NumBox>::new(&[Color::Black, Color::Black], scratch, &[2, 1])
    }
}

pub trait IntoBox {
    type Target: BoxType;

    /// Convert or pass through the value into a box of the target type, committing to the arena if necessary
    fn into_box(self, arena: &mut BoxArena) -> BoxId<Self::Target>;
}

impl<T: BoxType> IntoBox for BoxId<T> {
    type Target = T;

    #[inline]
    fn into_box(self, _arena: &mut BoxArena) -> BoxId<Self::Target> {
        self
    }
}

impl IntoBox for u32 {
    type Target = NumBox;

    #[inline]
    fn into_box(self, arena: &mut BoxArena) -> BoxId<Self::Target> {
        arena.from_u32(self)
    }
}

#[macro_export]
macro_rules! pixel_alt {
    ($arena:expr, $x:expr, $y:expr) => {
        $arena::pixel($x, $y)
    };
}

#[macro_export]
macro_rules! maxel_alt {
    ($arena:ident, $([$x:expr, $y:expr]),* $(,)?) => {
        {
            let mut outer_box = $crate::MBox::new();
            $(
                let pix = $arena::pixel(($x).into(), ($y).into());
                outer_box.insert_box(pix);
            )*
            outer_box
        }
    };
}

impl BoxArena {
    /// Inspects the serialized manifest to safely reconstruct a strongly-typed handle
    pub fn get_typed_root<T: BoxType>(&self, index: u32) -> Option<BoxId<T>> {
        let kind = self.kinds.get(index as usize)?;

        if *kind == T::KIND {
            Some(BoxId::<T>::new(index))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut arena = BoxArena::new();
        let zero = arena.zero();

        let two = arena.add(arena.one(), arena.one());
        let expected = arena.wrap_in_box(zero, Color::Black, Natural::from(2_u32));
        assert_eq!(two, expected);

        let minus_two = arena.add(arena.neg_one(), arena.neg_one());
        let expected = arena.wrap_in_box(arena.anti_zero(), Color::Black, Natural::from(2_u32));
        assert_eq!(minus_two, expected);

        let minus_one = arena.wrap_in_box::<NumBox, NumBox>(
            arena.anti_zero(),
            Color::Black,
            Natural::from(1_u32),
        );
        let zero = arena.add(minus_one, arena.one());
        assert_eq!(zero, arena.zero());

        let two = arena.from_u32(2);
        let three = arena.from_u32(3);
        let five = arena.add(two, three);
        let expected = arena.from_u32(5);
        assert_eq!(five, expected);

        let minus_two = arena.from_i32(-2);
        let three = arena.from_u32(3);
        let one = arena.add(minus_two, three);
        let expected = arena.from_u32(1);
        assert_eq!(one, expected);

        let alpha = arena.alpha();
        let two_alpha = arena.add(alpha, alpha);
        let expected = arena.wrap_in_box::<NumBox, PolynumBox>(
            arena.one(),
            Color::Black,
            Natural::from(2_u32),
        );
        assert_eq!(two_alpha, expected);

        let alpha_1 =
            arena.wrap_in_box::<PolynumBox, MultinumBox>(alpha, Color::Black, Natural::from(1_u32));
        let two_alpha_1 =
            arena.wrap_in_box::<PolynumBox, MultinumBox>(alpha, Color::Black, Natural::from(2_u32));
        let sum = arena.add(alpha_1, two_alpha_1);
        let expected = arena.wrap_in_box(alpha, Color::Black, Natural::from(3_u32));
        assert_eq!(sum, expected);

        let one = arena.one();
        let alpha = arena.alpha();
        let sum_1 = arena.add(one, alpha);
        let sum_2 = arena.add(alpha, one);
        assert_eq!(sum_1, sum_2);
    }

    #[test]
    fn test_mul() {
        let mut arena = BoxArena::new();
        let two = arena.from_u32(2);
        let three = arena.from_u32(3);
        let six = arena.mul(two, three);
        let expected = arena.from_u32(6);
        assert_eq!(six, expected);

        let six_2 = arena.mul(three, two);
        assert_eq!(six, six_2);

        let m_three = arena.from_i32(-3);
        let m_six = arena.mul(two, m_three);
        let expected = arena.from_i32(-6);
        assert_eq!(m_six, expected);

        let alpha = arena.alpha();
        let alpha_2 = arena.mul(alpha, alpha);
        let two = arena.from_u32(2);
        let expected = arena.wrap_in_box(two, Color::Black, Natural::from(1_u32));
        assert_eq!(alpha_2, expected);

        let one = arena.one();
        let alpha = arena.alpha();
        let p1 = arena.add(one, alpha);
        let p2 = arena.add(one, arena.neg_alpha());
        let prod = arena.mul(p1, p2);
        let anti_two =
            arena.wrap_in_box::<NumBox, NumBox>(arena.zero(), Color::Red, Natural::from(2_u32));
        let neg_alpha_2 =
            arena.wrap_in_box::<NumBox, PolynumBox>(anti_two, Color::Black, Natural::from(1_u32));
        let expected = arena.add(one, neg_alpha_2);
        assert_eq!(prod, expected);
    }

    #[test]
    fn test_pixel() {
        let mut arena = BoxArena::new();
        let mut workspace = BoxBuilder::new();

        let one = arena.one();
        let two = arena.from_u32(2);
        let three = arena.from_u32(3);

        let raw_p1 = PixelBuilder::new(&arena, &mut workspace).build_pixel(one, two);
        let p1 = arena.commit(raw_p1);

        let raw_p2 = PixelBuilder::new(&arena, &mut workspace).build_pixel(two, three);
        let p2 = arena.commit(raw_p2);

        let p3 = arena.mul_pixel(p1, p2);
        let raw_expected = PixelBuilder::new(&arena, &mut workspace).build_pixel(one, three);
        let expected = arena.commit(raw_expected);

        assert_eq!(p3, Some(expected));

        let raw_p4 = PixelBuilder::new(&arena, &mut workspace).build_pixel(three, two);
        let p4 = arena.commit(raw_p4);
        let p5 = arena.mul_pixel(p1, p4);
        assert!(p5.is_none());
    }
}
