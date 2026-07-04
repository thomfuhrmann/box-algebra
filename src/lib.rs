use malachite::Natural;
use strum::{EnumDiscriminants, EnumString};

use std::{
    cmp::Ordering::Equal,
    hash::{BuildHasher, Hash, Hasher},
    marker::PhantomData,
    ops::{Add, Mul},
};

use rapidhash::{HashMapExt, RapidHashMap, fast::RandomState};

pub mod add;
pub mod derivative;
pub mod display;
pub mod function;
pub mod maxel;
pub mod mul;
pub mod set;

/// Kind of boxes that can exist in a store
#[derive(Debug, Clone, Hash, EnumDiscriminants)]
#[strum_discriminants(name(BoxKind))]
#[strum_discriminants(derive(EnumString, Hash))]
pub enum BoxVariant {
    Box(BoxValue<AnyBox>),
    Num(BoxValue<NumBox>),
    Polynum(BoxValue<PolynumBox>),
    Multinum(BoxValue<MultinumBox>),
    Unixel(BoxValue<UnixelBox>),
    Vexel(BoxValue<VexelBox>),
    Pixel(BoxValue<PixelBox>),
    Maxel(BoxValue<MaxelBox>),
    Set(BoxValue<SetBox>),
}

impl BoxVariant {
    pub fn into_any_box(self) -> BoxValue<AnyBox> {
        match self {
            BoxVariant::Box(v) => v,
            BoxVariant::Num(v) => v.cast(),
            BoxVariant::Polynum(v) => v.cast(),
            BoxVariant::Multinum(v) => v.cast(),
            BoxVariant::Unixel(v) => v.cast(),
            BoxVariant::Vexel(v) => v.cast(),
            BoxVariant::Pixel(v) => v.cast(),
            BoxVariant::Maxel(v) => v.cast(),
            BoxVariant::Set(v) => v.cast(),
        }
    }

    pub fn is_numbox(&self) -> bool {
        matches!(self, BoxVariant::Num(_))
    }

    pub fn into_numbox(self) -> Result<BoxValue<NumBox>, Self> {
        match self {
            BoxVariant::Num(v) => Ok(v),
            other => Err(other),
        }
    }
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

/// Trait for the output type of box multiplication
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BoxValue<T: BoxType> {
    pub(crate) colors: Vec<Color>,
    pub(crate) multiplicities: Vec<Natural>,
    pub(crate) lengths: Vec<u32>,
    _marker: PhantomData<T>,
}

impl<T: BoxType> Default for BoxValue<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: BoxType> Hash for BoxValue<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        T::KIND.hash(state);
        self.colors.hash(state);
        self.multiplicities.hash(state);
        self.lengths.hash(state);
    }
}

impl<T: BoxType> BoxValue<T> {
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

    /// Returns the kind of box
    pub fn kind(&self) -> BoxKind {
        T::KIND
    }

    /// Tests if the box is an anti-box
    pub fn is_anti(&self) -> bool {
        self.get_color(0) == Color::Red
    }

    /// Casts this box to another box type
    pub fn cast<U: BoxType>(self) -> BoxValue<U> {
        BoxValue::<U>::new_with(self.colors, self.multiplicities, self.lengths)
    }

    /// Erase the type of the box
    pub fn erase_type(self) -> BoxValue<AnyBox> {
        self.cast::<AnyBox>()
    }

    /// Hashes the content of the box
    fn hash_content(&self, random_state: &RandomState) -> u64 {
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

    /// Compares the content of the two boxes for equality
    pub(crate) fn is_eq_content(&self, other: &Self) -> bool {
        let left_len = self.get_length(0) as usize;
        let right_len = other.get_length(0) as usize;

        if left_len != right_len {
            return false;
        }

        let left_inner = 1..left_len;
        let right_inner = 1..right_len;

        self.colors[left_inner.clone()] == other.colors[right_inner.clone()]
            && self.multiplicities[left_inner.clone()] == other.multiplicities[right_inner.clone()]
            && self.lengths[left_inner] == other.lengths[right_inner]
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

    /// Extends the box with another box
    pub fn extend(&mut self, value: BoxValue<impl BoxType>) {
        if let Some(len) = self.lengths.get_mut(0) {
            *len += value.get_length(0);
        }
        self.colors.extend(value.colors);
        self.multiplicities.extend(value.multiplicities);
        self.lengths.extend(value.lengths);
    }

    /// Extends the box with another box and multiplicity
    pub fn extend_with_mul(&mut self, mut value: BoxValue<impl BoxType>, mul: impl Into<Natural>) {
        value.set_multiplicity(0, mul);
        self.extend(value);
    }

    /// Returns the k-th color if it exists
    ///
    /// # Panics
    /// Panics if the index is out of bounds.
    pub fn get_color(&self, index: usize) -> Color {
        self.colors[index]
    }

    /// Returns the k-th multiplicity
    ///
    /// # Panics
    /// Panics if the index is out of bounds.
    pub fn get_multiplicity(&self, index: usize) -> Natural {
        self.multiplicities[index].clone()
    }

    /// Returns the k-th length
    ///
    /// # Panics
    /// Panics if the index is out of bounds.
    pub fn get_length(&self, index: usize) -> u32 {
        self.lengths[index]
    }

    /// Sets the k-th color
    ///
    /// # Panics
    /// Panics if the index is out of bounds.
    pub fn set_color(&mut self, index: usize, col: Color) {
        self.colors[index] = col;
    }

    /// Sets the k-th multiplicity
    ///
    /// # Panics
    /// Panics if the index is out of bounds.
    pub fn set_multiplicity(&mut self, index: usize, mul: impl Into<Natural>) {
        self.multiplicities[index] = mul.into();
    }

    /// Sets the k-th length
    ///
    /// # Panics
    /// Panics if the index is out of bounds.
    pub fn set_length(&mut self, index: usize, len: u32) {
        self.lengths[index] = len;
    }

    /// Removes the k-th row (without adjusting the lengths)
    pub fn remove(&mut self, index: usize) {
        self.colors.remove(index);
        self.multiplicities.remove(index);
        self.lengths.remove(index);
    }

    /// Wraps a box in another box
    pub fn wrap<U: BoxType>(mut self, mul: impl Into<Natural>) -> BoxValue<U> {
        self.set_multiplicity(0, mul);

        let mut result = BoxValue::<U>::new();
        result.colors.push(Color::Black);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1);

        result.extend(self);

        result
    }

    /// Inverts the color of the box
    pub fn into_anti(mut self) -> Self {
        let col = self.get_color(0);
        if col == Color::Black {
            self.set_color(0, Color::Red);
        } else {
            self.set_color(0, Color::Black);
        }
        self
    }
}

impl From<u32> for BoxValue<NumBox> {
    fn from(value: u32) -> Self {
        let zero = Self::zero();
        if value == 0 {
            return zero;
        }
        zero.wrap(value)
    }
}

impl From<u64> for BoxValue<NumBox> {
    fn from(value: u64) -> Self {
        let zero = Self::zero();
        if value == 0 {
            return zero;
        }
        zero.wrap(value)
    }
}

impl From<i32> for BoxValue<NumBox> {
    fn from(value: i32) -> Self {
        let zero = if value >= 0 {
            Self::zero()
        } else {
            Self::anti_zero()
        };

        if value == 0 {
            return zero;
        }

        zero.wrap(value.unsigned_abs())
    }
}

impl From<i64> for BoxValue<NumBox> {
    fn from(value: i64) -> Self {
        let zero = if value >= 0 {
            Self::zero()
        } else {
            Self::anti_zero()
        };

        if value == 0 {
            return zero;
        }

        zero.wrap(value.unsigned_abs())
    }
}

impl BoxValue<NumBox> {
    /// Constructs a black empty box
    pub fn zero() -> Self {
        let mut raw = BoxValue::new();
        raw.colors.push(Color::Black);
        raw.multiplicities.push(Natural::from(1_u32));
        raw.lengths.push(1);
        raw
    }

    /// Constructs a red empty box
    pub fn anti_zero() -> Self {
        let mut raw = BoxValue::new();
        raw.colors.push(Color::Red);
        raw.multiplicities.push(Natural::from(1_u32));
        raw.lengths.push(1);
        raw
    }

    pub fn one() -> Self {
        BoxValue::from(1_u32)
    }

    pub fn anti_one() -> Self {
        let anti_zero = Self::anti_zero();
        anti_zero.wrap(1_u32)
    }
}

impl BoxValue<PolynumBox> {
    pub fn alpha() -> Self {
        let one = BoxValue::one();
        one.wrap(1_u32)
    }

    pub fn anti_alpha() -> Self {
        let anti_one = BoxValue::anti_one();
        anti_one.wrap(1_u32)
    }
}

#[derive(Debug)]
pub struct BoxValueIter<T: BoxType> {
    raw: BoxValue<T>,
}

impl<T: BoxType> Iterator for BoxValueIter<T> {
    type Item = BoxValue<AnyBox>;

    fn next(&mut self) -> Option<Self::Item> {
        let child_len = match self.raw.lengths.first() {
            Some(&len) => len as usize,
            None => return None,
        };

        let colors: Vec<_> = self.raw.colors.drain(0..child_len).collect();
        let multiplicities: Vec<_> = self.raw.multiplicities.drain(0..child_len).collect();
        let lengths: Vec<_> = self.raw.lengths.drain(0..child_len).collect();

        Some(BoxValue::new_with(colors, multiplicities, lengths))
    }
}

impl<T: BoxType> IntoIterator for BoxValue<T> {
    type Item = BoxValue<AnyBox>;
    type IntoIter = BoxValueIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        let colors: Vec<_> = self.colors.into_iter().skip(1).collect();
        let multiplicities: Vec<_> = self.multiplicities.into_iter().skip(1).collect();
        let lengths: Vec<_> = self.lengths.into_iter().skip(1).collect();
        BoxValueIter {
            raw: BoxValue::new_with(colors, multiplicities, lengths),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash)]
pub struct BoxValueRef<'a> {
    pub colors: &'a [Color],
    pub multiplicities: &'a [Natural],
    pub lengths: &'a [u32],
}

pub struct BoxValueRefIter<'a> {
    colors: &'a [Color],
    multiplicities: &'a [Natural],
    lengths: &'a [u32],
}

impl<'a, T: BoxType> IntoIterator for &'a BoxValue<T> {
    type Item = BoxValueRef<'a>;
    type IntoIter = BoxValueRefIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        BoxValueRefIter {
            colors: &self.colors[1..],
            multiplicities: &self.multiplicities[1..],
            lengths: &self.lengths[1..],
        }
    }
}

impl<'a> Iterator for BoxValueRefIter<'a> {
    type Item = BoxValueRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.lengths.is_empty() {
            return None;
        }

        let current_len = self.lengths[0] as usize;

        let item = BoxValueRef {
            colors: &self.colors[..current_len],
            multiplicities: &self.multiplicities[..current_len],
            lengths: &self.lengths[..current_len],
        };

        self.colors = &self.colors[current_len..];
        self.multiplicities = &self.multiplicities[current_len..];
        self.lengths = &self.lengths[current_len..];

        Some(item)
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

/// Global store for box computations
#[derive(Debug)]
pub struct BoxStore {
    /// Store boxes by their hash
    pub boxes: RapidHashMap<u64, BoxVariant>,
    /// Look up table for variable names
    pub variables: RapidHashMap<String, u64>,
}

impl Default for BoxStore {
    fn default() -> Self {
        Self::new()
    }
}

impl BoxStore {
    /// Initialize the store
    pub fn new() -> Self {
        let boxes = RapidHashMap::new();
        let variables = RapidHashMap::new();

        Self { boxes, variables }
    }

    // Store a box by its hash
    pub fn store_box(&mut self, value: BoxVariant) {
        let hash = self.boxes.hasher().hash_one(&value);
        self.boxes.insert(hash, value);
    }

    /// Stores a box and binds it to a variable name
    pub fn store_box_with_name(&mut self, name: String, value: BoxVariant) {
        let hash = self.boxes.hasher().hash_one(&value);
        self.variables.insert(name, hash);
        self.boxes.insert(hash, value);
    }

    /// Fetch a box from the store by its name
    pub fn fetch_box_by_name(&self, name: &str) -> Option<BoxVariant> {
        let hash = self.variables.get(name)?;
        self.boxes.get(hash).cloned()
    }

    /// Fetch a box from the store by its hash
    pub fn fetch_box_by_hash(&self, hash: u64) -> Option<BoxVariant> {
        self.boxes.get(&hash).cloned()
    }
}
