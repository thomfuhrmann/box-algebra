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

pub trait BoxType: Sized {
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

#[derive(Debug)]
pub struct AnyBox;
impl BoxType for AnyBox {
    const KIND: BoxKind = BoxKind::Box;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        true
    }
}

#[derive(Debug)]
pub struct NumBox;
impl BoxType for NumBox {
    const KIND: BoxKind = BoxKind::Num;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        todo!()
    }
}
impl Num for NumBox {}

#[derive(Debug)]
pub struct PolynumBox;
impl BoxType for PolynumBox {
    const KIND: BoxKind = BoxKind::Polynum;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        todo!()
    }
}
impl Num for PolynumBox {}
impl Polynum for PolynumBox {}

#[derive(Debug)]
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

pub struct MaxelBox;
impl BoxType for MaxelBox {
    const KIND: BoxKind = BoxKind::Maxel;

    fn validate_layout(_raw: RawBox<'_, Self>) -> bool {
        todo!()
    }
}

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

    #[inline]
    pub fn into_any(self) -> BoxId<AnyBox> {
        BoxId::new(self.index)
    }
}

/// Raw uncomitted box data
#[derive(Debug)]
pub struct RawBox<'a, T: BoxType> {
    pub kind: BoxKind,
    pub colors: &'a [Color],
    pub multiplicities: &'a [Natural],
    pub lengths: &'a [u32],
    _marker: PhantomData<T>,
}

impl<'a, T: BoxType> RawBox<'a, T> {
    fn new(colors: &'a [Color], multiplicities: &'a [Natural], lengths: &'a [u32]) -> Self {
        Self {
            kind: T::KIND,
            colors,
            multiplicities,
            lengths,
            _marker: PhantomData,
        }
    }

    /// Hashes the box
    fn hash(&self, random_state: &RandomState) -> u64 {
        let mut hasher = random_state.build_hasher();
        self.kind.hash(&mut hasher);
        self.colors.hash(&mut hasher);
        self.multiplicities.hash(&mut hasher);
        self.lengths.hash(&mut hasher);
        hasher.finish()
    }

    /// Hashes the box ignoring their outer color and multiplicity
    fn hash_content(&self, random_state: &RandomState) -> u64 {
        let mut hasher = random_state.build_hasher();
        self.kind.hash(&mut hasher);
        self.colors[1..].hash(&mut hasher);
        self.multiplicities[1..].hash(&mut hasher);
        self.lengths.hash(&mut hasher);
        hasher.finish()
    }

    /// Compares box contents ignoring outer colors and multiplicities
    fn equal_content(&self, other: &Self) -> bool {
        let left_len = self.lengths[0] as usize;
        let right_len = other.lengths[0] as usize;

        if left_len != right_len {
            return false;
        }

        let left_range = 0..left_len;
        let right_range = 0..right_len;

        let left_inner = 1..left_len;
        let right_inner = 1..right_len;

        self.kind == other.kind
            && self.colors[left_inner.clone()] == other.colors[right_inner.clone()]
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
        self.kind == other.kind
            && self.colors == other.colors
            && self.multiplicities == other.multiplicities
            && self.lengths == other.lengths
    }
}

impl<'a, T: BoxType> Eq for RawBox<'a, T> {}

/// Raw uncomitted box data
#[derive(Debug)]
pub struct RawBoxMut<'a, T: BoxType> {
    kind: BoxKind,
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
            kind: T::KIND,
            colors,
            multiplicities,
            lengths,
            _marker: PhantomData,
        }
    }

    /// Sorts the content of this box
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
    kind: BoxKind,
    colors: Vec<Color>,
    multiplicities: Vec<Natural>,
    lengths: Vec<u32>,
    _marker: PhantomData<T>,
}

impl<T: BoxType> RawBoxOwned<T> {
    pub fn new() -> Self {
        Self {
            kind: T::KIND,
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

/// Global arena for box computations
#[derive(Debug)]
pub struct BoxArena {
    /// Box colors
    pub colors: Vec<Color>,
    /// Multiplicities of boxes
    pub multiplicities: Vec<Natural>,
    /// Numbers of rows occupied by boxes
    pub lengths: Vec<u32>,
    /// Pointers to starting indices of active boxes
    pub expression_roots: Vec<BoxId<AnyBox>>,
    /// Global box cache
    pub cache: RapidHashMap<u64, BoxId<AnyBox>>,
    /// Random state for hasher
    pub random_state: RandomState,
}

impl Default for BoxArena {
    fn default() -> Self {
        Self::new()
    }
}

const ARENA_INIT_CAPACITY: usize = 32;
impl BoxArena {
    pub const ZERO: BoxId<NumBox> = BoxId::new(0);
    pub const ANTI_ZERO: BoxId<NumBox> = BoxId::new(1);
    pub const ONE: BoxId<NumBox> = BoxId::new(2);
    pub const ANTI_ONE: BoxId<NumBox> = BoxId::new(4);
    pub const NEG_ONE: BoxId<NumBox> = BoxId::new(6);
    pub const ANTI_NEG_ONE: BoxId<NumBox> = BoxId::new(8);
    pub const ALPHA: BoxId<PolynumBox> = BoxId::new(10);
    pub const ANTI_ALPHA: BoxId<PolynumBox> = BoxId::new(13);
    pub const NEG_ALPHA: BoxId<PolynumBox> = BoxId::new(16);

    /// Initializes the arena with elementary objects
    pub fn new() -> Self {
        let random_state = RandomState::new();
        let mut cache = RapidHashMap::new();

        let mut colors = Vec::with_capacity(ARENA_INIT_CAPACITY);
        let mut multiplicities = Vec::with_capacity(ARENA_INIT_CAPACITY);
        let mut lengths = Vec::with_capacity(ARENA_INIT_CAPACITY);
        let mut expression_roots = Vec::with_capacity(ARENA_INIT_CAPACITY);

        let mut register_box = |id: BoxId<AnyBox>, raw_box: RawBox<'_, T>| {
            assert_eq!(
                colors.len(),
                id.index() as usize,
                "{}",
                format!("Index mismatch while registering box: {}", id.index())
            );

            // Compute structural hash
            let hash = raw_box.hash(&random_state);

            // Commit data
            cache.insert(hash, id);
            expression_roots.push(id);

            colors.extend_from_slice(raw_box.colors);
            multiplicities.extend_from_slice(raw_box.multiplicities);
            lengths.extend_from_slice(raw_box.lengths);
        };

        register_box(
            Self::ZERO.into_any(),
            RawBox::new(&[Color::Black], &[Natural::from(1_u32)], &[1]),
        );
        register_box(
            Self::ANTI_ZERO.into_any(),
            RawBox::new(&[Color::Red], &[Natural::from(1_u32)], &[1]),
        );
        register_box(
            Self::ONE.into_any(),
            RawBox::new(
                &[Color::Black, Color::Black],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::ANTI_ONE.into_any(),
            RawBox::new(
                &[Color::Red, Color::Black],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::NEG_ONE.into_any(),
            RawBox::new(
                &[Color::Black, Color::Red],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::ANTI_NEG_ONE.into_any(),
            RawBox::new(
                &[Color::Red, Color::Red],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::ALPHA.into_any(),
            RawBox::new(
                &[Color::Black, Color::Black, Color::Black],
                &[
                    Natural::from(1_u32),
                    Natural::from(1_u32),
                    Natural::from(1_u32),
                ],
                &[3, 2, 1],
            ),
        );
        register_box(
            Self::ANTI_ALPHA.into_any(),
            RawBox::new(
                &[Color::Red, Color::Black, Color::Black],
                &[
                    Natural::from(1_u32),
                    Natural::from(1_u32),
                    Natural::from(1_u32),
                ],
                &[3, 2, 1],
            ),
        );
        register_box(
            Self::NEG_ALPHA.into_any(),
            RawBox::new(
                &[Color::Black, Color::Red, Color::Black],
                &[
                    Natural::from(1_u32),
                    Natural::from(1_u32),
                    Natural::from(1_u32),
                ],
                &[3, 2, 1],
            ),
        );

        Self {
            colors,
            multiplicities,
            lengths,
            expression_roots,
            cache,
            random_state,
        }
    }

    /// Returns the next available ID
    #[inline(always)]
    fn next_id<T: BoxType>(&self) -> BoxId<T> {
        BoxId::new(self.colors.len() as u32)
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
        if let Some(&existing_id) = self.cache.get(&hash) {
            return BoxId::<T>::new(existing_id.index());
        }

        // cache miss
        let new_id = self.next_id();
        self.cache.insert(hash, new_id);
        self.expression_roots.push(new_id);

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
    pub fn wrap_in_box<T: BoxType>(
        &mut self,
        source: BoxId<T>,
        color: Color,
        multiplicity: Natural,
    ) -> BoxId<T> {
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

        let raw = RawBox::<T>::new(&colors, &multiplicities, &lengths);

        self.commit(raw)
    }

    pub fn from_u64(&mut self, num: u64) -> BoxId<NumBox> {
        self.wrap_in_box(BoxArena::ZERO, Color::Black, Natural::from(num))
    }

    pub fn from_i32(&mut self, num: i32) -> BoxId<NumBox> {
        if num < 0 {
            self.wrap_in_box(
                BoxArena::ANTI_ZERO,
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        } else {
            self.wrap_in_box(
                BoxArena::ZERO,
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        }
    }

    pub fn from_i64(&mut self, num: i64) -> BoxId<NumBox> {
        if num < 0 {
            self.wrap_in_box(
                BoxArena::ANTI_ZERO,
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        } else {
            self.wrap_in_box(
                BoxArena::ZERO,
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
    pub fn box_arity(&self, box_id: BoxId) -> u32 {
        let len = self.box_len(box_id);
        if len == 0 {
            return 0;
        }

        let mut count = 0;
        let mut idx = box_id.index() + 1;
        let end = idx + len as usize;

        while idx < end {
            count += 1;
            idx += self.lengths[idx] as usize;
        }
        count
    }

    /// Checks if two boxes have the same content structure ignoring their outer colors and multiplicities
    pub fn equal_content(&self, left: BoxId, right: BoxId) -> bool {
        let raw_left = self.get_raw(left);
        let raw_right = self.get_raw(right);
        raw_left.equal_content(&raw_right)
    }

    /// Hashes the content of a box ignoring its outer color and multiplicity
    pub fn hash_content(&self, box_id: BoxId) -> u64 {
        let raw = self.get_raw(box_id);
        raw.hash_content(&self.random_state)
    }

    /// Internal helper that does not commit to the arena
    fn add_scratch<T: BoxType>(
        &self,
        lhs: BoxId<T>,
        rhs: BoxId<T>,
        scratch_box: &mut RawBoxOwned<T>,
    ) {
        let mut unique_children: RapidHashMap<u64, (BoxId<T>, Color, Natural)> =
            RapidHashMap::new();

        let mut process_child_boxes = |box_id: BoxId<T>| {
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
                    && self.equal_content(curr_id, *other_id)
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

        process_child_boxes(lhs);
        process_child_boxes(rhs);

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
    pub fn add<T: BoxType>(&mut self, lhs: BoxId<T>, rhs: BoxId<T>) -> BoxId<T> {
        let mut scratch_box = RawBoxOwned::new();
        self.add_scratch(lhs, rhs, &mut scratch_box);
        let raw = scratch_box.as_mut();
        self.commit_sorted(raw)
    }

    /// Multiplication of boxes
    pub fn mul<T: BoxType>(&mut self, lhs: BoxId<T>, rhs: BoxId<T>) -> BoxId<T> {
        // extract left children
        let mut lhs_children = Vec::new();
        let lhs_start = lhs.id();
        let lhs_len = self.box_len(lhs);
        let mut curr = lhs_start + 1;
        let end = lhs_start + lhs_len;
        while curr < end {
            let child_id = BoxId::new(curr);
            lhs_children.push(child_id);
            curr += self.lengths[curr as usize];
        }

        // extract right children
        let mut rhs_children = Vec::new();
        let rhs_start = rhs.id();
        let rhs_len = self.box_len(rhs);
        let mut curr = rhs_start + 1;
        let end = rhs_start + rhs_len;
        while curr < end {
            let child_id = BoxId::new(curr);
            rhs_children.push(child_id);
            curr += self.lengths[curr as usize];
        }

        // let mut unique_children: RapidHashMap<u64, (BoxId, Color, Natural)> = RapidHashMap::new();
        let mut unique_children: RapidHashMap<u64, RawBoxOwned> = RapidHashMap::new();

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
                    if curr_raw_ref.equal_content(&other_raw_ref) {
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

        let mut is_ordered = vec![];
        let mut colors = vec![];
        let mut multiplicities = vec![];
        let mut lengths = vec![];

        let lhs_col = self.box_color(lhs);
        let rhs_col = self.box_color(rhs);
        let final_color = lhs_col + rhs_col;

        is_ordered.push(false);
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
                is_ordered.push(raw_box.is_ordered[i]);

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

        let raw = RawBoxMut::new(
            &mut is_ordered,
            &mut colors,
            &mut multiplicities,
            &mut lengths,
        );
        self.commit_sorted(raw)
    }

    /// Instantiates a pixel
    pub fn pixel(&mut self, x: BoxId, y: BoxId) -> BoxId {
        let x_raw = self.get_raw(x);
        let x_len = x_raw.length();

        let y_raw = self.get_raw(y);
        let y_len = y_raw.length();

        let mut is_ordered = vec![true];
        let mut colors = vec![Color::Black];
        let mut multiplicities = vec![Natural::from(1_u32)];
        let mut lengths = vec![1 + x_len + y_len];

        // copy source elements
        is_ordered.extend_from_slice(x_raw.is_ordered);
        colors.extend_from_slice(x_raw.colors);
        multiplicities.extend_from_slice(x_raw.multiplicities);
        lengths.extend_from_slice(x_raw.lengths);

        is_ordered.extend_from_slice(y_raw.is_ordered);
        colors.extend_from_slice(y_raw.colors);
        multiplicities.extend_from_slice(y_raw.multiplicities);
        lengths.extend_from_slice(y_raw.lengths);

        let raw = RawBox::new(&is_ordered, &colors, &multiplicities, &lengths);
        self.commit(raw)
    }

    fn pixel_x(&mut self, pixel: BoxId) -> BoxId {
        let idx = pixel.id();
        BoxId(idx + 1)
    }

    fn pixel_y(&mut self, pixel: BoxId) -> BoxId {
        let idx = pixel.id();
        let idx_x = idx + 1;
        let len_x = self.lengths[idx_x as usize];
        BoxId(idx_x + len_x)
    }

    /// Multiplies two pixels
    pub fn mul_pixel(&mut self, left: BoxId, right: BoxId) -> Option<BoxId> {
        let left_y = self.pixel_y(left);
        let right_x = self.pixel_x(right);

        let left_y_raw = self.get_raw(left_y);
        let right_x_raw = self.get_raw(right_x);

        if left_y_raw == right_x_raw {
            let left_x = self.pixel_x(left);
            let right_y = self.pixel_y(right);
            return Some(self.pixel(left_x, right_y));
        }

        None
    }
}

// struct BoxBuilder {
//     /// Box colors
//     pub colors: Vec<Color>,
//     /// Number of child nodes
//     pub arities: Vec<u32>,
//     /// Multiplicity of the node itself
//     pub multiplicities: Vec<u64>,
//     /// Number of flat nodes occupied by this subtree
//     pub lengths: Vec<u32>,
//     /// Pointers to the starting indices of active expressions
//     pub expression_roots: Vec<BoxId>,
//     /// Global box cache
//     pub cache: RapidHashMap<u64, BoxId>,
//     /// Random state for hasher
//     pub random_state: RandomState,
// }
//
// impl BoxBuilder {
//     /// Create a raw view window over a temporary expression in the scratch space
//     pub fn as_raw_box(&self, scratch_idx: usize) -> RawBox<'_> {
//         let len = self.lengths[scratch_idx] as usize;
//         let range = scratch_idx..(scratch_idx + len);
//
//         RawBox {
//             colors: &self.colors[range.clone()],
//             arities: &self.arities[range.clone()],
//             multiplicities: &self.multiplicities[range.clone()],
//             lengths: &self.lengths[range],
//         }
//     }
// }

// impl<'a> std::ops::Mul<BoxId> for BoxMut<'a> {
//     type Output = BoxId;
//
//     fn mul(self, rhs: BoxId) -> Self::Output {
//         self.arena.wrap_in_box(self.id, Color::Black, rhs.0 as u64)
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut arena = BoxArena::new();
        let zero = BoxArena::ZERO;

        let two = arena.add(BoxArena::ONE, BoxArena::ONE);
        let expected = arena.wrap_in_box(zero, Color::Black, Natural::from(2_u32));
        assert_eq!(two, expected);

        let minus_two = arena.add(BoxArena::NEG_ONE, BoxArena::NEG_ONE);
        let expected = arena.wrap_in_box(BoxArena::ANTI_ZERO, Color::Black, Natural::from(2_u32));
        assert_eq!(minus_two, expected);

        let minus_one = arena.wrap_in_box(BoxArena::ANTI_ZERO, Color::Black, Natural::from(1_u32));
        let zero = arena.add(minus_one, BoxArena::ONE);
        assert_eq!(zero, BoxArena::ZERO);

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

        let alpha = BoxArena::ALPHA;
        let two_alpha = arena.add(alpha, alpha);
        let expected = arena.wrap_in_box(BoxArena::ONE, false, Color::Black, Natural::from(2_u32));
        assert_eq!(two_alpha, expected);

        let alpha_1 = arena.wrap_in_box(alpha, false, Color::Black, Natural::from(1_u32));
        let two_alpha_1 = arena.wrap_in_box(alpha, false, Color::Black, Natural::from(2_u32));
        let sum = arena.add(alpha_1, two_alpha_1);
        let expected = arena.wrap_in_box(alpha, false, Color::Black, Natural::from(3_u32));
        assert_eq!(sum, expected);

        let one = BoxArena::ONE;
        let alpha = BoxArena::ALPHA;
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

        let alpha = BoxArena::ALPHA;
        let alpha_2 = arena.mul(alpha, alpha);
        let expected = arena.wrap_in_box(two, false, Color::Black, Natural::from(1_u32));
        assert_eq!(alpha_2, expected);

        let one = BoxArena::ONE;
        let alpha = BoxArena::ALPHA;
        let p1 = arena.add(one, alpha);
        let p2 = arena.add(one, BoxArena::NEG_ALPHA);
        let prod = arena.mul(p1, p2);
        let anti_two = arena.wrap_in_box(BoxArena::ZERO, false, Color::Red, Natural::from(2_u32));
        let neg_alpha_2 = arena.wrap_in_box(anti_two, false, Color::Black, Natural::from(1_u32));
        let expected = arena.add(one, neg_alpha_2);
        assert_eq!(prod, expected);
    }

    #[test]
    fn test_pixel() {
        let mut arena = BoxArena::new();
        let one = BoxArena::ONE;
        let two = arena.from_u32(2);
        let three = arena.from_u32(3);
        let p1 = arena.pixel(one, two);
        let p2 = arena.pixel(two, three);
        let p3 = arena.mul_pixel(p1, p2);
        let p4 = arena.pixel(three, two);
        let expected = Some(arena.pixel(one, three));
        assert_eq!(p3, expected);

        let p5 = arena.mul_pixel(p1, p4);
        assert!(p5.is_none());
    }
}
