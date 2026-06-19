use malachite::{Natural, base::num::arithmetic::traits::SaturatingSub};

use std::{
    cmp::Ordering::Equal,
    hash::{BuildHasher, Hash, Hasher},
    marker::PhantomData,
    ops::{Add, Mul},
};

use rapidhash::{HashMapExt, RapidHashMap, fast::RandomState};

pub mod derivative;
pub mod display;
pub mod function;
pub mod maxel;
pub mod set;

/// Kind of boxes that can exist in a store
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum BoxKind {
    Box,
    Num,
    Polynum,
    Multinum,
    Unixel,
    Vexel,
    Pixel,
    Maxel,
    Set,
}

/// Traits that describes the type of a box
pub trait BoxType: Sized + Clone {
    const KIND: BoxKind;
}

pub trait Num: BoxType {}
pub trait Polynum: Num {}
pub trait Multinum: Polynum {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AnyBox;
impl BoxType for AnyBox {
    const KIND: BoxKind = BoxKind::Box;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NumBox;
impl BoxType for NumBox {
    const KIND: BoxKind = BoxKind::Num;
}
impl Num for NumBox {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PolynumBox;
impl BoxType for PolynumBox {
    const KIND: BoxKind = BoxKind::Polynum;
}
impl Num for PolynumBox {}
impl Polynum for PolynumBox {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MultinumBox;
impl BoxType for MultinumBox {
    const KIND: BoxKind = BoxKind::Multinum;
}
impl Num for MultinumBox {}
impl Polynum for MultinumBox {}
impl Multinum for MultinumBox {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PixelBox;
impl BoxType for PixelBox {
    const KIND: BoxKind = BoxKind::Pixel;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MaxelBox;
impl BoxType for MaxelBox {
    const KIND: BoxKind = BoxKind::Maxel;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnixelBox;
impl BoxType for UnixelBox {
    const KIND: BoxKind = BoxKind::Unixel;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VexelBox;
impl BoxType for VexelBox {
    const KIND: BoxKind = BoxKind::Vexel;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SetBox;
impl BoxType for SetBox {
    const KIND: BoxKind = BoxKind::Set;
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
#[derive(Debug, Clone, Copy)]
pub struct RawBox<'a, T: BoxType> {
    pub colors: &'a [Color],
    pub multiplicities: &'a [Natural],
    pub lengths: &'a [u32],
    _marker: PhantomData<T>,
}

impl<'a, T: BoxType> RawBox<'a, T> {
    pub fn new(colors: &'a [Color], multiplicities: &'a [Natural], lengths: &'a [u32]) -> Self {
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
        Hash::hash(self, &mut hasher);
        hasher.finish()
    }

    /// Hashes the box ignoring their outer color and multiplicity
    pub(crate) fn hash_content(&self, random_state: &RandomState) -> u64 {
        let mut hasher = random_state.build_hasher();
        T::KIND.hash(&mut hasher);

        self.colors.len().hash(&mut hasher);
        self.multiplicities.len().hash(&mut hasher);

        self.colors.get(1..).unwrap_or(&[]).hash(&mut hasher);
        self.multiplicities
            .get(1..)
            .unwrap_or(&[])
            .hash(&mut hasher);
        self.lengths.hash(&mut hasher);
        hasher.finish()
    }

    /// Compares box contents ignoring outer colors and multiplicities
    pub(crate) fn cmp_content(&self, other: Self) -> bool {
        let left_len = self.length(0) as usize;
        let right_len = other.length(0) as usize;

        if left_len != right_len {
            return false;
        }

        let left_inner = 1..left_len;
        let right_inner = 1..right_len;

        self.colors[left_inner.clone()] == other.colors[right_inner.clone()]
            && self.multiplicities[left_inner.clone()] == other.multiplicities[right_inner.clone()]
            && self.lengths[left_inner] == other.lengths[right_inner]
    }

    /// Returns the k-th color
    pub fn color(&self, index: usize) -> Color {
        self.colors.get(index).copied().unwrap_or(Color::Black)
    }

    /// Returns the k-th multiplicity
    pub fn multiplicity(&self, index: usize) -> Natural {
        self.multiplicities
            .get(index)
            .cloned()
            .unwrap_or(Natural::from(0_u32))
    }

    /// Returns the k-th length
    pub fn length(&self, index: usize) -> u32 {
        self.lengths.get(index).copied().unwrap_or(0)
    }

    /// Tests if the box is an anti-box
    pub fn is_anti(&self) -> bool {
        self.color(0) == Color::Red
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

impl<'a, T: BoxType> Hash for RawBox<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        T::KIND.hash(state);
        self.colors.hash(state);
        self.multiplicities.hash(state);
        self.lengths.hash(state);
    }
}

#[derive(Debug)]
pub struct RawBoxIter<'a, T: BoxType> {
    raw: RawBox<'a, T>,
    index: usize,
}

impl<'a, T: BoxType> Iterator for RawBoxIter<'a, T> {
    type Item = RawBox<'a, AnyBox>;

    fn next(&mut self) -> Option<Self::Item> {
        let total_len = self.raw.length(0) as usize;
        if self.index < total_len {
            let child_idx = self.index;
            let child_len = self.raw.lengths[self.index] as usize;
            self.index = child_idx + child_len;
            return Some(RawBox::new(
                &self.raw.colors[child_idx..child_idx + child_len],
                &self.raw.multiplicities[child_idx..child_idx + child_len],
                &self.raw.lengths[child_idx..child_idx + child_len],
            ));
        }

        None
    }
}

impl<'a, T: BoxType> IntoIterator for RawBox<'a, T> {
    type Item = RawBox<'a, AnyBox>;
    type IntoIter = RawBoxIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        RawBoxIter {
            raw: self,
            index: 1,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RawBoxOwned<T: BoxType> {
    pub(crate) colors: Vec<Color>,
    pub(crate) multiplicities: Vec<Natural>,
    pub(crate) lengths: Vec<u32>,
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

    /// Constructs a box from the given vectors
    pub fn new_with(colors: Vec<Color>, multiplicities: Vec<Natural>, lengths: Vec<u32>) -> Self {
        Self {
            colors,
            multiplicities,
            lengths,
            _marker: PhantomData,
        }
    }

    /// Constructs an empty black box
    pub fn empty() -> Self {
        let mut raw = Self::new();
        raw.colors.push(Color::Black);
        raw.multiplicities.push(Natural::from(1_u32));
        raw.lengths.push(1);
        raw
    }

    /// Returns the kind of box
    pub fn kind(&self) -> BoxKind {
        T::KIND
    }

    /// Casts this box to another box type
    pub fn cast<U: BoxType>(self) -> RawBoxOwned<U> {
        RawBoxOwned::<U>::new_with(self.colors, self.multiplicities, self.lengths)
    }

    /// Returns a reference to the raw box data
    pub fn as_raw(&self) -> RawBox<'_, T> {
        RawBox::new(&self.colors, &self.multiplicities, &self.lengths)
    }

    /// Hashes the box
    fn hash(&self, random_state: &RandomState) -> u64 {
        self.as_raw().hash(random_state)
    }

    /// Hashes the content of the box
    fn hash_content(&self, random_state: &RandomState) -> u64 {
        self.as_raw().hash_content(random_state)
    }

    /// Compares the content of the two boxes for equality
    pub(crate) fn cmp_content(&self, other: &Self) -> bool {
        self.as_raw().cmp_content(other.as_raw())
    }

    /// Sorts the immediate child boxes of this box
    pub(crate) fn sort_immediate_children(&mut self) {
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

    pub fn extend(&mut self, raw: RawBoxOwned<impl BoxType>) {
        if let Some(len) = self.lengths.get_mut(0) {
            *len += raw.length(0);
        }
        self.colors.extend(raw.colors);
        self.multiplicities.extend(raw.multiplicities);
        self.lengths.extend(raw.lengths);
    }

    pub fn extend_raw(&mut self, raw: RawBox<'_, impl BoxType>) {
        if let Some(len) = self.lengths.get_mut(0) {
            *len += raw.length(0);
        }
        self.colors.extend_from_slice(raw.colors);
        self.multiplicities.extend_from_slice(raw.multiplicities);
        self.lengths.extend_from_slice(raw.lengths);
    }

    pub fn extend_with_mult(&mut self, raw: RawBox<'_, impl BoxType>, mult: impl Into<Natural>) {
        if let Some(len) = self.lengths.get_mut(0) {
            *len += raw.length(0);
        }
        let mut owned = RawBoxOwned::from(raw);
        owned.set_multiplicity(mult, 0);

        self.colors.extend(owned.colors);
        self.multiplicities.extend(owned.multiplicities);
        self.lengths.extend(owned.lengths);
    }

    /// Returns the k-th color
    pub fn color(&self, index: usize) -> Color {
        self.colors.get(index).copied().unwrap_or(Color::Black)
    }

    /// Returns the k-th multiplicity
    pub fn multiplicity(&self, index: usize) -> Natural {
        self.multiplicities
            .get(index)
            .cloned()
            .unwrap_or(Natural::from(0_u32))
    }

    /// Returns the k-th length
    pub fn length(&self, index: usize) -> u32 {
        self.lengths.get(index).copied().unwrap_or(0)
    }

    /// Sets the k-th color
    pub fn set_color(&mut self, col: Color, index: usize) {
        if let Some(curr_col) = self.colors.get_mut(index) {
            *curr_col = col;
        }
    }

    /// Sets the k-th multiplicity
    pub fn set_multiplicity(&mut self, mult: impl Into<Natural>, index: usize) {
        if let Some(curr_mult) = self.multiplicities.get_mut(index) {
            *curr_mult = mult.into();
        }
    }

    /// Sets the k-th length
    pub fn set_length(&mut self, len: u32, index: usize) {
        if let Some(curr_len) = self.lengths.get_mut(index) {
            *curr_len = len;
        }
    }

    /// Removes the k-th row (without adjusting the lengths)
    pub fn remove(&mut self, index: usize) {
        self.colors.remove(index);
        self.multiplicities.remove(index);
        self.lengths.remove(index);
    }
}

impl<T: BoxType> From<RawBox<'_, T>> for RawBoxOwned<T> {
    fn from(value: RawBox<'_, T>) -> Self {
        Self {
            colors: Vec::from(value.colors),
            multiplicities: Vec::from(value.multiplicities),
            lengths: Vec::from(value.lengths),
            _marker: PhantomData,
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
    pub fn as_raw<'a>(&'a self, store: &'a BoxStore) -> RawBox<'a, T> {
        match self {
            BoxState::Uncommitted(owned) => owned.as_raw(),
            BoxState::Committed(box_id) => store.get_raw(*box_id),
        }
    }

    pub fn commit(self, store: &mut BoxStore) -> BoxState<T> {
        match self {
            BoxState::Uncommitted(raw_box) => store.commit(raw_box),
            comm => comm,
        }
    }

    pub fn sort_and_commit(self, store: &mut BoxStore) -> BoxState<T> {
        match self {
            BoxState::Uncommitted(raw_box) => store.sort_and_commit(raw_box),
            comm => comm,
        }
    }
}

/// Color of a box
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Color {
    Black,
    Red,
}

impl Color {
    pub fn invert(self) -> Self {
        match self {
            Color::Black => Color::Red,
            Color::Red => Color::Black,
        }
    }
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
    pub alpha: BoxId<PolynumBox>,
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

        let mut register_box = |kind: BoxKind, hash: u64, raw_box: RawBoxOwned<AnyBox>| {
            let index = lengths.len() as u32;
            let id = BoxId::<AnyBox>::new(index);

            cache.insert(hash, id);
            kinds.push(kind);

            colors.extend(raw_box.colors);
            multiplicities.extend(raw_box.multiplicities);
            lengths.extend(raw_box.lengths);
            index
        };

        let raw = RawBoxOwned::<NumBox>::new_with(
            vec![Color::Black],
            vec![Natural::from(1_u32)],
            vec![1],
        );
        let zero: BoxId<NumBox> = BoxId::<NumBox>::new(register_box(
            raw.kind(),
            raw.hash(&random_state),
            raw.cast(),
        ));

        let raw =
            RawBoxOwned::<NumBox>::new_with(vec![Color::Red], vec![Natural::from(1_u32)], vec![1]);
        let anti_zero: BoxId<NumBox> = BoxId::<NumBox>::new(register_box(
            raw.kind(),
            raw.hash(&random_state),
            raw.cast(),
        ));

        let raw = RawBoxOwned::<NumBox>::new_with(
            vec![Color::Black, Color::Black],
            vec![Natural::from(1_u32), Natural::from(1_u32)],
            vec![2, 1],
        );
        let one = BoxId::<NumBox>::new(register_box(
            raw.kind(),
            raw.hash(&random_state),
            raw.cast(),
        ));

        let raw = RawBoxOwned::<NumBox>::new_with(
            vec![Color::Black, Color::Red],
            vec![Natural::from(1_u32), Natural::from(1_u32)],
            vec![2, 1],
        );
        let neg_one = BoxId::<NumBox>::new(register_box(
            raw.kind(),
            raw.hash(&random_state),
            raw.cast(),
        ));

        let raw = RawBoxOwned::<PolynumBox>::new_with(
            vec![Color::Black, Color::Black, Color::Black],
            vec![
                Natural::from(1_u32),
                Natural::from(1_u32),
                Natural::from(1_u32),
            ],
            vec![3, 2, 1],
        );
        let alpha = BoxId::<PolynumBox>::new(register_box(
            raw.kind(),
            raw.hash(&random_state),
            raw.cast(),
        ));

        let constants = BoxConstants {
            zero,
            anti_zero,
            one,
            neg_one,
            alpha,
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

    pub fn alpha(&self) -> BoxState<PolynumBox> {
        BoxState::Committed(self.constants.alpha)
    }

    /// Returns the negative of a box
    pub fn negate<T: BoxType>(&self, box_state: &BoxState<T>) -> BoxState<T> {
        let mut raw_owned = RawBoxOwned::from(box_state.as_raw(self));
        if let Some(col) = raw_owned.colors.get_mut(1) {
            *col = col.invert();
        }
        BoxState::Uncommitted(raw_owned)
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
    pub(crate) fn commit<T: BoxType>(&mut self, raw: RawBoxOwned<T>) -> BoxState<T> {
        let hash = raw.hash(&self.random_state);

        // cache hit
        if let Some(&box_id) = self.cache.get(&hash) {
            return BoxState::Committed(BoxId::<T>::new(box_id.index()));
        }

        // cache miss - commit to store
        let new_id = self.next_id();
        self.kinds.push(T::KIND);
        self.cache.insert(hash, new_id);
        self.active_boxes.push(new_id);

        self.colors.extend(raw.colors);
        self.multiplicities.extend(raw.multiplicities);
        self.lengths.extend(raw.lengths);

        BoxState::Committed(BoxId::<T>::new(new_id.index()))
    }

    /// Sorts the box before commiting it
    fn sort_and_commit<T: BoxType>(&mut self, mut raw: RawBoxOwned<T>) -> BoxState<T> {
        raw.sort_immediate_children();
        self.commit(raw)
    }

    /// Wraps an existing box in a new container
    pub fn wrap_in_box_raw<T: BoxType, U: BoxType>(
        &self,
        raw: RawBox<'_, T>,
        color: Color,
        multiplicity: Natural,
    ) -> RawBoxOwned<U> {
        let source_len = raw.length(0);

        let mut result = RawBoxOwned::<U>::new();
        result.colors.push(color);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1 + source_len);

        result.colors.push(raw.color(0));
        result.multiplicities.push(multiplicity);
        result.lengths.push(source_len);

        if source_len > 1 {
            result.colors.extend_from_slice(&raw.colors[1..]);
            result
                .multiplicities
                .extend_from_slice(&raw.multiplicities[1..]);
            result.lengths.extend_from_slice(&raw.lengths[1..]);
        }

        result
    }

    pub fn wrap_in_box<U: BoxType>(
        &self,
        source: &BoxState<impl BoxType>,
        color: Color,
        multiplicity: impl Into<Natural>,
    ) -> BoxState<U> {
        BoxState::Uncommitted(self.wrap_in_box_raw(source.as_raw(self), color, multiplicity.into()))
    }

    pub fn from_u32(&self, num: u32) -> BoxState<NumBox> {
        if num == 0 {
            return self.zero();
        }
        self.wrap_in_box(&self.zero(), Color::Black, Natural::from(num))
    }

    pub fn from_u64(&self, num: u64) -> BoxState<NumBox> {
        if num == 0 {
            return self.zero();
        }
        self.wrap_in_box(&self.zero(), Color::Black, Natural::from(num))
    }

    pub fn from_i32(&self, num: i32) -> BoxState<NumBox> {
        if num == 0 {
            return self.zero();
        }
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

    pub fn from_i64(&self, num: i64) -> BoxState<NumBox> {
        if num == 0 {
            return self.zero();
        }
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

    fn add_child_boxes(
        &self,
        raw_box: RawBox<'_, AnyBox>,
        unique_children: &mut RapidHashMap<u64, RawBoxOwned<AnyBox>>,
    ) {
        for child_raw in raw_box {
            let child_col = child_raw.color(0);
            let child_mul = child_raw.multiplicity(0);
            let struct_hash = child_raw.hash_content(&self.random_state);

            if let Some(other) = unique_children.get_mut(&struct_hash)
                && child_raw.cmp_content(other.as_raw())
            {
                let other_col = other.color(0);
                let other_mul = other.multiplicity(0);
                if child_col + other_col == Color::Red {
                    if child_mul < other_mul {
                        other.set_multiplicity(other_mul.saturating_sub(child_mul), 0);
                    } else {
                        other.set_multiplicity(child_mul.saturating_sub(other_mul), 0);
                        other.set_color(child_col, 0);
                    }
                } else {
                    other.set_multiplicity(other_mul + child_mul, 0);
                }
            } else {
                unique_children.insert(struct_hash, RawBoxOwned::from(child_raw));
            }
        }
    }

    /// Internal helper that does not commit to the store
    pub fn add_raw<L, R>(
        &self,
        lhs: RawBox<'_, L>,
        rhs: RawBox<'_, R>,
    ) -> RawBoxOwned<<L as BoxAdd<R>>::Output>
    where
        L: BoxType + BoxAdd<R>,
        R: BoxType,
    {
        let mut result = RawBoxOwned::<L::Output>::new();
        let mut unique_children: RapidHashMap<u64, RawBoxOwned<AnyBox>> = RapidHashMap::new();

        let lhs_col = lhs.color(0);
        let rhs_col = rhs.color(0);

        result.colors.push(lhs_col + rhs_col);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1);

        self.add_child_boxes(lhs.cast(), &mut unique_children);
        self.add_child_boxes(rhs.cast(), &mut unique_children);

        for child in unique_children.into_values() {
            let mult = child.multiplicity(0);
            if mult == 0 {
                continue;
            }

            result.extend(child);
        }

        result.sort_immediate_children();
        result
    }

    /// Addition of boxes
    pub fn add<L, R>(&self, lhs: &BoxState<L>, rhs: &BoxState<R>) -> BoxState<L::Output>
    where
        L: BoxType + BoxAdd<R>,
        R: BoxType,
    {
        let lhs_raw = lhs.as_raw(self);
        let rhs_raw = rhs.as_raw(self);
        let result = self.add_raw(lhs_raw, rhs_raw);
        BoxState::Uncommitted(result)
    }

    pub fn mul_raw<L, R>(
        &self,
        lhs_raw: RawBox<'_, L>,
        rhs_raw: RawBox<'_, R>,
    ) -> RawBoxOwned<<L as BoxMul<R>>::Output>
    where
        L: BoxType + BoxMul<R>,
        R: BoxType,
    {
        let mut result = RawBoxOwned::new();
        let mut unique_children: RapidHashMap<u64, RawBoxOwned<AnyBox>> = RapidHashMap::new();

        let lhs_col = lhs_raw.color(0);
        let rhs_col = rhs_raw.color(0);

        for left_child in lhs_raw {
            for right_child in rhs_raw.clone() {
                let left_mul = left_child.multiplicity(0);
                let right_mul = right_child.multiplicity(0);
                let curr_mul = left_mul * right_mul;

                let mut curr_owned = self.add_raw(left_child, right_child);

                let curr_col = curr_owned.color(0);
                let struct_hash = curr_owned.hash_content(&self.random_state);

                if let Some(other) = unique_children.get_mut(&struct_hash)
                    && curr_owned.cmp_content(other)
                {
                    let other_col = other.color(0);
                    let other_mul = other.multiplicity(0);
                    if curr_col + other_col == Color::Red {
                        if curr_mul < other_mul {
                            other.set_multiplicity(other_mul.saturating_sub(curr_mul), 0);
                        } else {
                            other.set_multiplicity(curr_mul.saturating_sub(other_mul), 0);
                            other.set_color(curr_col, 0);
                        }
                    } else {
                        other.set_multiplicity(other_mul + curr_mul, 0);
                    }
                } else {
                    curr_owned.set_multiplicity(curr_mul, 0);
                    unique_children.insert(struct_hash, curr_owned);
                }
            }
        }

        result.colors.push(lhs_col + rhs_col);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1);

        for raw_box in unique_children.into_values() {
            let mul = raw_box.multiplicity(0);
            if mul == 0 {
                continue;
            }

            result.extend(raw_box);
        }

        result.sort_immediate_children();
        result
    }

    /// Multiplication of boxes
    pub fn mul<L, R>(&self, lhs: &BoxState<L>, rhs: &BoxState<R>) -> BoxState<L::Output>
    where
        L: BoxType + BoxMul<R>,
        R: BoxType,
    {
        let lhs_raw = lhs.as_raw(self);
        let rhs_raw = rhs.as_raw(self);
        let result = self.mul_raw(lhs_raw, rhs_raw);
        BoxState::Uncommitted(result)
    }
}

pub trait IntoBoxState<T: BoxType>: Sized {
    fn into_box_state(self, store: &BoxStore) -> BoxState<T>;
}

impl<T: BoxType> IntoBoxState<T> for BoxState<T> {
    fn into_box_state(self, _store: &BoxStore) -> BoxState<T> {
        self
    }
}

impl<T: BoxType> IntoBoxState<T> for RawBoxOwned<T> {
    fn into_box_state(self, _store: &BoxStore) -> BoxState<T> {
        BoxState::Uncommitted(self)
    }
}

impl IntoBoxState<NumBox> for u32 {
    fn into_box_state(self, store: &BoxStore) -> BoxState<NumBox> {
        store.from_u32(self)
    }
}

impl IntoBoxState<NumBox> for u64 {
    fn into_box_state(self, store: &BoxStore) -> BoxState<NumBox> {
        store.from_u64(self)
    }
}

impl IntoBoxState<NumBox> for i32 {
    fn into_box_state(self, store: &BoxStore) -> BoxState<NumBox> {
        store.from_i32(self)
    }
}

impl IntoBoxState<NumBox> for i64 {
    fn into_box_state(self, store: &BoxStore) -> BoxState<NumBox> {
        store.from_i64(self)
    }
}

#[macro_export]
macro_rules! into_pixel {
    ($store:expr, $x:expr, $y:expr) => {{ $store.pixel($x.into_box_state($store), $y.into_box_state($store)) }};
}

#[macro_export]
macro_rules! into_maxel {
    ($store:expr, [$([$x:expr, $y:expr]),* $(,)?]) => {
        {
            use $crate::IntoBoxState;
            let mut result = $crate::RawBoxOwned::<$crate::MaxelBox>::new();
            result.colors.push($crate::Color::Black);
            result.multiplicities.push(malachite::Natural::from(1_u32));
            result.lengths.push(1);
            $(
                let x_state = ($x).into_box_state($store);
                let y_state = ($y).into_box_state($store);
                let x_raw = x_state.as_raw($store);
                let y_raw = y_state.as_raw($store);
                let pix = $store.pixel_raw(x_raw, y_raw);
                result.extend(pix);
            )*
            $crate::BoxState::Uncommitted(result)
        }
    };
}

#[macro_export]
macro_rules! into_vexel {
    ($store:expr, [$($x:expr),* $(,)?]) => {
        {
            let mut result = $crate::RawBoxOwned::<VexelBox>::new();
            result.colors.push(Color::Black);
            result.multiplicities.push(Natural::from(1_u32));
            result.lengths.push(1);
            $(
                let state = ($x).into_box_state($store);
                let unix = $store.unix_raw(state.as_raw($store));
                result.extend(unix);
            )*
            result
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
        let store = BoxStore::new();

        let zero = store.zero();
        let one = store.one();
        let two = store.from_u32(2);
        let three = store.from_u32(3);
        let minus_one = store.negate(&one);
        let minus_two = store.from_i32(-2);
        let alpha = store.alpha();
        let beta_1 = store.wrap_in_box::<MultinumBox>(&alpha, Color::Black, 1_u32);
        let two_beta_1 = store.wrap_in_box::<MultinumBox>(&alpha, Color::Black, 2_u32);

        let sum = store.add(&store.one(), &store.one());
        let expected = store.wrap_in_box::<NumBox>(&zero, Color::Black, 2_u32);
        assert_eq!(sum, expected);

        let sum = store.add(&minus_one, &minus_one);
        let expected = store.wrap_in_box(&store.anti_zero(), Color::Black, 2_u32);
        assert_eq!(sum, expected);

        let sum = store.add(&minus_one, &one);
        assert_eq!(sum.as_raw(&store), zero.as_raw(&store));

        let sum = store.add(&two, &three);
        let expected = store.from_u32(5);
        assert_eq!(sum, expected);

        let sum = store.add(&minus_two, &three);
        let expected = store.from_u32(1);
        assert_eq!(sum, expected);

        let sum = store.add(&alpha, &alpha);
        let expected = store.wrap_in_box::<PolynumBox>(&store.one(), Color::Black, 2_u32);
        assert_eq!(sum, expected);

        let sum = store.add(&beta_1, &two_beta_1);
        let expected = store.wrap_in_box(&alpha, Color::Black, 3_u32);
        assert_eq!(sum, expected);

        let sum_1 = store.add(&one, &alpha);
        let sum_2 = store.add(&alpha, &one);
        assert_eq!(sum_1, sum_2);
    }

    #[test]
    fn test_mul() {
        let store = BoxStore::new();

        let zero = store.zero();
        let one = store.one();
        let two = store.from_u32(2);
        let three = store.from_u32(3);
        let m_three = store.from_i32(-3);
        let anti_one = store.wrap_in_box::<NumBox>(&zero, Color::Red, 1_u32);
        let alpha = store.alpha();
        let minus_alpha = store.wrap_in_box::<PolynumBox>(&anti_one, Color::Black, 1_u32);
        let anti_two = store.wrap_in_box::<NumBox>(&zero, Color::Red, 2_u32);
        let minus_2_alpha = store.wrap_in_box::<PolynumBox>(&anti_two, Color::Black, 1_u32);

        let prod = store.mul(&two, &three);
        let expected = store.from_u32(6);
        assert_eq!(prod, expected);

        let expected = store.mul(&three, &two);
        assert_eq!(prod, expected);

        let prod = store.mul(&two, &m_three);
        let expected = store.from_i32(-6);
        assert_eq!(prod, expected);

        let prod = store.mul(&alpha, &alpha);
        let expected = store.wrap_in_box(&two, Color::Black, 1_u32);
        assert_eq!(prod, expected);

        let s1 = store.add(&one, &alpha);
        let s2 = store.add(&one, &minus_alpha);
        let prod = store.mul(&s1, &s2);
        let expected = store.add(&one, &minus_2_alpha);
        assert_eq!(prod, expected);
    }

    #[test]
    fn test_pixel() {
        let store = BoxStore::new();

        let p1 = into_pixel!(&store, 1, 2);
        let p2 = into_pixel!(&store, 2, 3);
        let p3 = store.mul_pixel(&p1, &p2);
        let expected = into_pixel!(&store, 1, 3);

        assert_eq!(p3, Some(expected));

        let p4 = into_pixel!(&store, 3, 2);
        let p5 = store.mul_pixel(&p1, &p4);
        assert!(p5.is_none());
    }

    #[test]
    fn test_maxel() {
        let store = BoxStore::new();
        let a = into_maxel![&store, [[1, 1], [1, 2], [2, 2]]].into_box_state(&store);
        let b = into_maxel![&store, [[1, 2], [2, 1]]].into_box_state(&store);

        let prod = store.mul_maxel(&a, &b);
        let expected = into_maxel![&store, [[1, 1], [1, 2], [2, 1]]].into_box_state(&store);
        assert_eq!(prod, expected);
    }

    #[test]
    fn test_vexel() {
        let store = BoxStore::new();
        let v = into_vexel!(&store, [4, 2, 3]);
        let v_raw = v.as_raw();
        println!("{v_raw:#}");
    }
}
