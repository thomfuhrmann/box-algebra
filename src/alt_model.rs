use malachite::{
    Natural,
    base::num::arithmetic::traits::{SaturatingSub, SaturatingSubAssign},
};

use std::{
    cmp::Ordering::Equal,
    hash::{BuildHasher, Hash, Hasher},
    ops::{Add, Mul},
};

use rapidhash::{HashMapExt, RapidHashMap, fast::RandomState};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct BoxId(u32);

impl BoxId {
    #[inline(always)]
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn id(self) -> u32 {
        self.0
    }

    #[inline(always)]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for BoxId {
    #[inline(always)]
    fn from(value: usize) -> Self {
        BoxId(value as u32)
    }
}

/// Raw uncomitted box data
#[derive(Debug)]
pub struct RawBox<'a> {
    pub is_ordered: &'a [bool],
    pub colors: &'a [Color],
    pub multiplicities: &'a [Natural],
    pub lengths: &'a [u32],
}

impl<'a> RawBox<'a> {
    fn new(
        is_ordered: &'a [bool],
        colors: &'a [Color],
        multiplicities: &'a [Natural],
        lengths: &'a [u32],
    ) -> Self {
        Self {
            is_ordered,
            colors,
            multiplicities,
            lengths,
        }
    }

    fn hash(&self, random_state: &RandomState) -> u64 {
        let mut hasher = random_state.build_hasher();
        self.is_ordered.hash(&mut hasher);
        self.colors.hash(&mut hasher);
        self.multiplicities.hash(&mut hasher);
        self.lengths.hash(&mut hasher);
        hasher.finish()
    }
}

/// Raw uncomitted box data
#[derive(Debug)]
pub struct RawBoxMut<'a> {
    is_ordered: &'a mut [bool],
    colors: &'a mut [Color],
    multiplicities: &'a mut [Natural],
    lengths: &'a mut [u32],
}

impl<'a> RawBoxMut<'a> {
    fn new(
        is_ordered: &'a mut [bool],
        colors: &'a mut [Color],
        multiplicities: &'a mut [Natural],
        lengths: &'a mut [u32],
    ) -> Self {
        Self {
            is_ordered,
            colors,
            multiplicities,
            lengths,
        }
    }

    /// Sorts the content of this box
    fn sort_box(&mut self) {
        if self.lengths.is_empty() {
            return;
        }

        if self.is_ordered[0] {
            return;
        }

        let total_box_len = self.lengths[0] as usize;
        if total_box_len <= 1 {
            return;
        }

        let start_idx = 1;
        let end_idx = total_box_len;

        // Gather start indices and lengths of immediate children
        let mut child_meta = Vec::new();
        let mut curr = start_idx;
        while curr < end_idx {
            let len = self.lengths[curr] as usize;
            child_meta.push((curr, len));
            curr += len;
        }

        if child_meta.len() <= 1 {
            return;
        }

        // Sort child metadata permutation list
        child_meta.sort_by(|&(start_a, len_a), &(start_b, len_b)| {
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

        // Permute values into temporary scratch space
        let payload_len = end_idx - start_idx;
        let mut sorted_is_ordered = Vec::with_capacity(payload_len);
        let mut sorted_colors = Vec::with_capacity(payload_len);
        let mut sorted_lens = Vec::with_capacity(payload_len);
        let mut sorted_mults = Vec::with_capacity(payload_len);

        for &(start, len) in &child_meta {
            let range = start..(start + len);
            sorted_is_ordered.extend_from_slice(&self.is_ordered[range.clone()]);
            sorted_colors.extend_from_slice(&self.colors[range.clone()]);
            sorted_lens.extend_from_slice(&self.lengths[range.clone()]);

            for idx in range {
                let item = std::mem::take(&mut self.multiplicities[idx]);
                sorted_mults.push(item);
            }
        }

        let target_range = start_idx..end_idx;
        self.is_ordered[target_range.clone()].copy_from_slice(&sorted_is_ordered);
        self.colors[target_range.clone()].copy_from_slice(&sorted_colors);
        self.lengths[target_range.clone()].copy_from_slice(&sorted_lens);

        for (dest_idx, src_natural) in target_range.zip(sorted_mults) {
            self.multiplicities[dest_idx] = src_natural;
        }
    }
}

impl<'a> From<RawBoxMut<'a>> for RawBox<'a> {
    fn from(raw_mut: RawBoxMut<'a>) -> Self {
        RawBox {
            is_ordered: raw_mut.is_ordered,
            colors: raw_mut.colors,
            multiplicities: raw_mut.multiplicities,
            lengths: raw_mut.lengths,
        }
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

/// Global arena for computations
#[derive(Debug)]
pub struct BoxArena {
    /// Box colors
    pub colors: Vec<Color>,
    /// Flag to indicate if box is ordered
    pub is_ordered: Vec<bool>,
    /// Multiplicities of boxes
    pub multiplicities: Vec<Natural>,
    /// Numbers of rows occupied by boxes
    pub lengths: Vec<u32>,
    /// Pointers to starting indices of active boxes
    pub expression_roots: Vec<BoxId>,
    /// Global box cache
    pub cache: RapidHashMap<u64, BoxId>,
    /// Random state for hasher
    pub random_state: RandomState,
}

impl Default for BoxArena {
    fn default() -> Self {
        Self::new()
    }
}

const ARENA_INIT_CAPACITY: usize = 16;
impl BoxArena {
    pub const ZERO: BoxId = BoxId(0);
    pub const ANTI_ZERO: BoxId = BoxId(1);
    pub const ONE: BoxId = BoxId(2);
    pub const ANTI_ONE: BoxId = BoxId(4);
    pub const NEG_ONE: BoxId = BoxId(6);
    pub const ANTI_NEG_ONE: BoxId = BoxId(8);
    pub const ALPHA: BoxId = BoxId(10);
    pub const ANTI_ALPHA: BoxId = BoxId(13);
    pub const NEG_ALPHA: BoxId = BoxId(16);

    /// Initializes the arena with elementary objects
    pub fn new() -> Self {
        let random_state = RandomState::new();
        let mut cache = RapidHashMap::new();

        let mut is_ordered = Vec::with_capacity(ARENA_INIT_CAPACITY);
        let mut colors = Vec::with_capacity(ARENA_INIT_CAPACITY);
        let mut multiplicities = Vec::with_capacity(ARENA_INIT_CAPACITY);
        let mut lengths = Vec::with_capacity(ARENA_INIT_CAPACITY);
        let mut expression_roots = Vec::with_capacity(ARENA_INIT_CAPACITY);

        let mut register_box = |id: BoxId, raw_box: RawBox| {
            // Compute structural hash
            let hash = raw_box.hash(&random_state);

            // Commit data
            cache.insert(hash, id);
            expression_roots.push(id);

            is_ordered.extend_from_slice(raw_box.is_ordered);
            colors.extend_from_slice(raw_box.colors);
            multiplicities.extend_from_slice(raw_box.multiplicities);
            lengths.extend_from_slice(raw_box.lengths);
        };

        register_box(
            Self::ZERO,
            RawBox::new(&[false], &[Color::Black], &[Natural::from(1_u32)], &[1]),
        );
        register_box(
            Self::ANTI_ZERO,
            RawBox::new(&[false], &[Color::Red], &[Natural::from(1_u32)], &[1]),
        );
        register_box(
            Self::ONE,
            RawBox::new(
                &[false, false],
                &[Color::Black, Color::Black],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::ANTI_ONE,
            RawBox::new(
                &[false, false],
                &[Color::Red, Color::Black],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::NEG_ONE,
            RawBox::new(
                &[false, false],
                &[Color::Black, Color::Red],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::ANTI_NEG_ONE,
            RawBox::new(
                &[false, false],
                &[Color::Red, Color::Red],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::ALPHA,
            RawBox::new(
                &[false, false, false],
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
            Self::ANTI_ALPHA,
            RawBox::new(
                &[false, false, false],
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
            Self::NEG_ALPHA,
            RawBox::new(
                &[false, false, false],
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
            is_ordered,
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
    fn next_id(&self) -> BoxId {
        BoxId(self.colors.len() as u32)
    }

    /// Gets the raw data of a box given by its id
    pub fn get(&self, box_id: BoxId) -> RawBox<'_> {
        let idx = box_id.index();
        let len = self.lengths[idx] as usize;
        RawBox {
            is_ordered: &self.is_ordered[idx..(idx + len)],
            colors: &self.colors[idx..(idx + len)],
            multiplicities: &self.multiplicities[idx..(idx + len)],
            lengths: &self.lengths[idx..(idx + len)],
        }
    }

    /// Commits a raw, uncommitted box view
    fn commit(&mut self, raw: RawBox<'_>) -> BoxId {
        let hash = raw.hash(&self.random_state);

        // cache hit
        if let Some(&existing_id) = self.cache.get(&hash) {
            let view = self.get(existing_id);
            if view.is_ordered == raw.is_ordered
                && view.colors == raw.colors
                && view.multiplicities == raw.multiplicities
                && view.lengths == raw.lengths
            {
                return existing_id;
            }
        }

        // cache miss
        let new_id = self.next_id();
        self.cache.insert(hash, new_id);
        self.expression_roots.push(new_id);

        self.is_ordered.extend_from_slice(raw.is_ordered);
        self.colors.extend_from_slice(raw.colors);
        self.multiplicities.extend_from_slice(raw.multiplicities);
        self.lengths.extend_from_slice(raw.lengths);

        new_id
    }

    /// Sorts the box before commiting it
    fn commit_sorted(&mut self, mut raw: RawBoxMut<'_>) -> BoxId {
        raw.sort_box();
        self.commit(RawBox::from(raw))
    }

    /// Wraps an existing expression in a new box container
    pub fn wrap_in_box(
        &mut self,
        source: BoxId,
        ordered: bool,
        color: Color,
        multiplicity: Natural,
    ) -> BoxId {
        let source_idx = source.index();
        let source_len = self.lengths[source_idx] as usize;

        let mut is_ordered = vec![ordered];
        let mut colors = vec![color];
        let mut multiplicities = vec![Natural::from(1_u32)];
        let mut lengths = vec![1 + source_len as u32];

        // copy source elements
        for i in 0..source_len {
            let curr_idx = source_idx + i;

            is_ordered.push(self.is_ordered[curr_idx]);
            colors.push(self.colors[curr_idx]);

            if i == 0 {
                multiplicities.push(multiplicity.clone());
            } else {
                multiplicities.push(self.multiplicities[curr_idx].clone());
            }

            lengths.push(self.lengths[curr_idx]);
        }

        let raw = RawBox::new(&is_ordered, &colors, &multiplicities, &lengths);

        self.commit(raw)
    }

    pub fn from_u32(&mut self, num: u32) -> BoxId {
        self.wrap_in_box(BoxArena::ZERO, false, Color::Black, Natural::from(num))
    }

    pub fn from_u64(&mut self, num: u64) -> BoxId {
        self.wrap_in_box(BoxArena::ZERO, false, Color::Black, Natural::from(num))
    }

    pub fn from_i32(&mut self, num: i32) -> BoxId {
        if num < 0 {
            self.wrap_in_box(
                BoxArena::ANTI_ZERO,
                false,
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        } else {
            self.wrap_in_box(
                BoxArena::ZERO,
                false,
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        }
    }

    pub fn from_i64(&mut self, num: i64) -> BoxId {
        if num < 0 {
            self.wrap_in_box(
                BoxArena::ANTI_ZERO,
                false,
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        } else {
            self.wrap_in_box(
                BoxArena::ZERO,
                false,
                Color::Black,
                Natural::from(num.unsigned_abs()),
            )
        }
    }

    /// Returns if box is ordered
    pub fn is_box_ordered(&self, box_id: BoxId) -> bool {
        self.is_ordered[box_id.index()]
    }

    /// Returns the outer color of a box
    pub fn box_color(&self, box_id: BoxId) -> Color {
        self.colors[box_id.index()]
    }

    /// Returns the arity of a box
    pub fn box_multiplicity(&self, box_id: BoxId) -> Natural {
        self.multiplicities[box_id.index()].clone()
    }

    /// Returns the number of rows occupied by this box (including itself)
    pub fn box_len(&self, box_id: BoxId) -> u32 {
        self.lengths[box_id.index()]
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

    /// Checks if two boxes have the same content structure (ignoring their outer outer colors and multiplicities)
    pub fn equal_content(&self, left: BoxId, right: BoxId) -> bool {
        let left_len = self.box_len(left) as usize;
        let right_len = self.box_len(right) as usize;

        if left_len != right_len {
            return false;
        }

        let left_start = left.index();
        let right_start = right.index();

        let left_range = left_start..(left_start + left_len);
        let right_range = right_start..(right_start + right_len);

        let left_inner = left_start + 1..(left_start + left_len);
        let right_inner = right_start + 1..(right_start + right_len);

        self.colors[left_inner.clone()] == self.colors[right_inner.clone()]
            && self.multiplicities[left_inner] == self.multiplicities[right_inner]
            && self.lengths[left_range.clone()] == self.lengths[right_range.clone()]
            && self.is_ordered[left_range] == self.is_ordered[right_range]
    }

    /// Hashes the content of a box (ignoring its outer color and multiplicity)
    pub fn hash_content(&self, box_id: BoxId) -> u64 {
        let mut hasher = self.random_state.build_hasher();

        let start = box_id.index();
        let len = self.box_len(box_id) as usize;

        self.is_ordered[start..(start + len)].hash(&mut hasher);
        self.colors[start + 1..(start + len)].hash(&mut hasher);
        self.multiplicities[start + 1..(start + len)].hash(&mut hasher);
        self.lengths[start..(start + len)].hash(&mut hasher);

        hasher.finish()
    }

    /// Adds two boxes (unordered)
    pub fn add(&mut self, lhs: BoxId, rhs: BoxId) -> BoxId {
        let mut unique_children: RapidHashMap<u64, (BoxId, Color, Natural)> = RapidHashMap::new();

        let mut add_child_boxes = |box_id @ BoxId(start_idx): BoxId| {
            let box_len = self.box_len(box_id);
            let mut curr = start_idx + 1;
            let end = start_idx + box_len;

            while curr < end {
                let curr_id = BoxId::new(curr);
                let curr_mul = self.multiplicities[curr as usize].clone();
                let curr_col = self.colors[curr as usize];
                let curr_len = self.lengths[curr as usize];

                let struct_hash = self.hash_content(curr_id);

                // check if a box exists that has the same structure except for the color of the outer box
                let mut found_match = false;
                if let Some((other_id, other_col, other_mul)) =
                    unique_children.get_mut(&struct_hash)
                    && self.equal_content(curr_id, *other_id)
                {
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

                if !found_match {
                    unique_children.insert(struct_hash, (curr_id, curr_col, curr_mul));
                }

                curr += curr_len;
            }
        };

        // add child boxes
        add_child_boxes(lhs);
        add_child_boxes(rhs);

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

        // initialize data for unique children
        let mut written_len = 0;
        for (id, col, mul) in unique_children.values() {
            // skip boxes that got annihilated
            if *mul == 0 {
                continue;
            }

            let start = id.id();
            let len = self.box_len(*id);
            for i in 0..len {
                let src_idx = start + i;

                is_ordered.push(self.is_ordered[src_idx as usize]);

                if i == 0 {
                    colors.push(*col);
                    multiplicities.push(mul.clone());
                } else {
                    colors.push(self.colors[src_idx as usize]);
                    multiplicities.push(self.multiplicities[src_idx as usize].clone());
                }

                lengths.push(self.lengths[src_idx as usize]);
                written_len += 1;
            }
        }

        // update total length
        lengths[0] = (1 + written_len) as u32;

        let raw = RawBoxMut::new(
            &mut is_ordered,
            &mut colors,
            &mut multiplicities,
            &mut lengths,
        );
        self.commit_sorted(raw)
    }

    /// Multiplication of boxes
    pub fn mul(&mut self, lhs: BoxId, rhs: BoxId) -> BoxId {
        // extract left children
        let mut lhs_children = Vec::new();
        let BoxId(lhs_start) = lhs;
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
        let BoxId(rhs_start) = rhs;
        let rhs_len = self.box_len(rhs);
        let mut curr = rhs_start + 1;
        let end = rhs_start + rhs_len;
        while curr < end {
            let child_id = BoxId::new(curr);
            rhs_children.push(child_id);
            curr += self.lengths[curr as usize];
        }

        // This map mirrors the structure of your `add` function to handle identity and color rules.
        let mut unique_children: RapidHashMap<u64, (BoxId, Color, Natural)> = RapidHashMap::new();

        for &left_child in &lhs_children {
            for &right_child in &rhs_children {
                let child_id = self.add(left_child, right_child);

                let child_idx = child_id.id() as usize;
                let left_mul = self.box_multiplicity(left_child);
                let right_mul = self.box_multiplicity(right_child);
                let curr_mul = left_mul * right_mul;
                let curr_col = self.colors[child_idx];

                let struct_hash = self.hash_content(child_id);

                let mut found_match = false;
                if let Some((other_id, other_col, other_mul)) =
                    unique_children.get_mut(&struct_hash)
                    && self.equal_content(child_id, *other_id)
                {
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

                if !found_match {
                    unique_children.insert(struct_hash, (child_id, curr_col, curr_mul));
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
        for (id, col, mul) in unique_children.values() {
            if *mul == 0 {
                continue;
            }

            let start = id.id();
            let len = self.box_len(*id);
            for i in 0..len {
                let src_idx = (start + i) as usize;

                is_ordered.push(self.is_ordered[src_idx]);

                if i == 0 {
                    colors.push(*col);
                    multiplicities.push(mul.clone());
                } else {
                    colors.push(self.colors[src_idx]);
                    multiplicities.push(self.multiplicities[src_idx].clone());
                }

                lengths.push(self.lengths[src_idx]);
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
        let expected = arena.wrap_in_box(zero, false, Color::Black, Natural::from(2_u32));
        assert_eq!(two, expected);

        let minus_two = arena.add(BoxArena::NEG_ONE, BoxArena::NEG_ONE);
        let expected = arena.wrap_in_box(
            BoxArena::ANTI_ZERO,
            false,
            Color::Black,
            Natural::from(2_u32),
        );
        assert_eq!(minus_two, expected);

        let minus_one = arena.wrap_in_box(
            BoxArena::ANTI_ZERO,
            false,
            Color::Black,
            Natural::from(1_u32),
        );
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

        println!("{:?}", arena.get(prod));
        println!("{:?}", arena.get(expected));

        assert_eq!(prod, expected);
    }
}
