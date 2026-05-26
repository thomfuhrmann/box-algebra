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
    pub colors: &'a [Color],
    pub multiplicities: &'a [Natural],
    pub lengths: &'a [u32],
}

impl<'a> RawBox<'a> {
    pub fn new(colors: &'a [Color], multiplicities: &'a [Natural], lengths: &'a [u32]) -> Self {
        Self {
            colors,
            multiplicities,
            lengths,
        }
    }

    pub fn hash(&self, random_state: &RandomState) -> u64 {
        let mut hasher = random_state.build_hasher();
        self.colors.hash(&mut hasher);
        self.multiplicities.hash(&mut hasher);
        self.lengths.hash(&mut hasher);
        hasher.finish()
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

    /// Initializes the arena with elementary objects
    pub fn new() -> Self {
        let random_state = RandomState::new();
        let mut cache = RapidHashMap::new();

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

            colors.extend_from_slice(raw_box.colors);
            multiplicities.extend_from_slice(raw_box.multiplicities);
            lengths.extend_from_slice(raw_box.lengths);
        };

        register_box(
            Self::ZERO,
            RawBox::new(&[Color::Black], &[Natural::from(1_u32)], &[1]),
        );
        register_box(
            Self::ANTI_ZERO,
            RawBox::new(&[Color::Red], &[Natural::from(1_u32)], &[1]),
        );
        register_box(
            Self::ONE,
            RawBox::new(
                &[Color::Black, Color::Black],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::ANTI_ONE,
            RawBox::new(
                &[Color::Red, Color::Black],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::NEG_ONE,
            RawBox::new(
                &[Color::Black, Color::Red],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::ANTI_NEG_ONE,
            RawBox::new(
                &[Color::Red, Color::Red],
                &[Natural::from(1_u32), Natural::from(1_u32)],
                &[2, 1],
            ),
        );
        register_box(
            Self::ALPHA,
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
    fn next_id(&self) -> BoxId {
        BoxId(self.colors.len() as u32)
    }

    /// Gets the raw data of a box given by its id
    pub fn get(&self, box_id: BoxId) -> RawBox<'_> {
        let idx = box_id.index();
        let len = self.lengths[idx] as usize;
        RawBox {
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
            if view.colors == raw.colors
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

        self.colors.extend_from_slice(raw.colors);
        self.multiplicities.extend_from_slice(raw.multiplicities);
        self.lengths.extend_from_slice(raw.lengths);

        new_id
    }

    /// Wraps an existing expression in a new box container
    pub fn wrap_in_box(&mut self, source: BoxId, color: Color, multiplicity: Natural) -> BoxId {
        let source_idx = source.index();
        let source_len = self.lengths[source_idx] as usize;

        let mut colors = vec![color];
        let mut multiplicities = vec![Natural::from(1_u32)];
        let mut lengths = vec![1 + source_len as u32];

        // copy source elements
        for i in 0..source_len {
            let curr_idx = source_idx + i;

            colors.push(self.colors[curr_idx]);

            if i == 0 {
                multiplicities.push(multiplicity.clone());
            } else {
                multiplicities.push(self.multiplicities[curr_idx].clone());
            }

            lengths.push(self.lengths[curr_idx]);
        }

        let raw = RawBox::new(&colors, &multiplicities, &lengths);

        self.commit(raw)
    }

    pub fn from_u32(&mut self, num: u32) -> BoxId {
        self.wrap_in_box(BoxArena::ZERO, Color::Black, Natural::from(num))
    }

    pub fn from_u64(&mut self, num: u64) -> BoxId {
        self.wrap_in_box(BoxArena::ZERO, Color::Black, Natural::from(num))
    }

    pub fn from_i32(&mut self, num: i32) -> BoxId {
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

    pub fn from_i64(&mut self, num: i64) -> BoxId {
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
    pub fn get_box_color(&self, box_id: BoxId) -> Color {
        self.colors[box_id.index()]
    }

    /// Returns the arity of a box
    pub fn get_box_multiplicity(&self, box_id: BoxId) -> Natural {
        self.multiplicities[box_id.index()].clone()
    }

    /// Returns the number of rows occupied by this box (including itself)
    pub fn get_box_len(&self, box_id: BoxId) -> u32 {
        self.lengths[box_id.index()]
    }

    /// Computes the arity of a box
    pub fn arity(&self, box_id: BoxId) -> u32 {
        let len = self.get_box_len(box_id);
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
        let left_len = self.get_box_len(left) as usize;
        let right_len = self.get_box_len(right) as usize;

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
            && self.lengths[left_range] == self.lengths[right_range]
    }

    /// Hashes the content of a box (ignoring its outer color and multiplicity)
    pub fn hash_content(&self, box_id: BoxId) -> u64 {
        let mut hasher = self.random_state.build_hasher();

        let start = box_id.index();
        let len = self.get_box_len(box_id) as usize;

        self.colors[start + 1..(start + len)].hash(&mut hasher);
        self.multiplicities[start + 1..(start + len)].hash(&mut hasher);
        self.lengths[start..(start + len)].hash(&mut hasher);

        hasher.finish()
    }

    fn sort_box_content(colors: &mut [Color], multiplicities: &mut [Natural], lengths: &mut [u32]) {
        if lengths.is_empty() {
            return;
        }

        let total_box_len = lengths[0] as usize;
        if total_box_len <= 1 {
            return;
        }

        let start_idx = 1;
        let end_idx = total_box_len;

        // Gather start indices and lengths of immediate children
        let mut child_meta = Vec::new();
        let mut curr = start_idx;
        while curr < end_idx {
            let len = lengths[curr] as usize;
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

            let len_cmp = lengths[range_a.clone()].cmp(&lengths[range_b.clone()]);
            if len_cmp != Equal {
                return len_cmp;
            }

            let col_cmp = colors[range_a.clone()].cmp(&colors[range_b.clone()]);
            if col_cmp != Equal {
                return col_cmp;
            }

            multiplicities[range_a].cmp(&multiplicities[range_b])
        });

        // Permute values into temporary scratch space
        let payload_len = end_idx - start_idx;
        let mut sorted_colors = Vec::with_capacity(payload_len);
        let mut sorted_lens = Vec::with_capacity(payload_len);
        let mut sorted_mults = Vec::with_capacity(payload_len);

        for &(start, len) in &child_meta {
            let range = start..(start + len);
            sorted_colors.extend_from_slice(&colors[range.clone()]);
            sorted_lens.extend_from_slice(&lengths[range.clone()]);

            for idx in range {
                let item = std::mem::take(&mut multiplicities[idx]);
                sorted_mults.push(item);
            }
        }

        let target_range = start_idx..end_idx;
        colors[target_range.clone()].copy_from_slice(&sorted_colors);
        lengths[target_range.clone()].copy_from_slice(&sorted_lens);

        for (dest_idx, src_natural) in target_range.zip(sorted_mults) {
            multiplicities[dest_idx] = src_natural;
        }
    }

    /// Adds two boxes
    pub fn add(&mut self, lhs: BoxId, rhs: BoxId) -> BoxId {
        let mut unique_children: RapidHashMap<u64, (BoxId, Color, Natural)> = RapidHashMap::new();

        let mut add_child_boxes = |box_id @ BoxId(start_idx): BoxId| {
            let box_len = self.get_box_len(box_id);
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

        let mut colors = vec![];
        let mut multiplicities = vec![];
        let mut lengths = vec![];

        let lhs_col = self.get_box_color(lhs);
        let rhs_col = self.get_box_color(rhs);
        let final_color = lhs_col + rhs_col;

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
            let len = self.get_box_len(*id);
            for i in 0..len {
                let src_idx = start + i;

                colors.push(*col);

                if i == 0 {
                    multiplicities.push(mul.clone());
                } else {
                    multiplicities.push(self.multiplicities[src_idx as usize].clone());
                }

                lengths.push(self.lengths[src_idx as usize]);
                written_len += 1;
            }
        }

        // update total length
        lengths[0] = (1 + written_len) as u32;

        let raw = RawBox::new(&colors, &multiplicities, &lengths);
        self.commit(raw)
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
        let expected = arena.wrap_in_box(BoxArena::ONE, Color::Black, Natural::from(2_u32));
        assert_eq!(two_alpha, expected);

        let alpha_1 = arena.wrap_in_box(alpha, Color::Black, Natural::from(1_u32));
        let two_alpha_1 = arena.wrap_in_box(alpha, Color::Black, Natural::from(2_u32));
        let sum = arena.add(alpha_1, two_alpha_1);
        let expected = arena.wrap_in_box(alpha, Color::Black, Natural::from(3_u32));
        assert_eq!(sum, expected);
    }
}
