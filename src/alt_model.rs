// use malachite::Integer;
//
// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub enum Count {
//     /// Keeps the common case (counts < 2^63) entirely stack-allocated and ultra-fast.
//     Small(u64),
//     /// Gracefully scales to infinite precision when numbers explode.
//     Large(Integer),
// }
//
// impl std::ops::AddAssign<u64> for Count {
//     fn add_assign(&mut self, rhs: u64) {
//         match self {
//             Count::Small(lhs) => {
//                 if let Some(sum) = lhs.checked_add(rhs) {
//                     *lhs = sum;
//                 } else {
//                     // Overflow triggered! Upgrade to Malachite Big Integer
//                     let mut big_lhs = Integer::from(*lhs);
//                     big_lhs += Integer::from(rhs);
//                     *self = Count::Large(big_lhs);
//                 }
//             }
//             Count::Large(big_lhs) => {
//                 *big_lhs += Integer::from(rhs);
//             }
//         }
//     }
// }

// #[derive(Debug)]
// pub enum Count {
//     Small(u64)
// }

use std::{
    hash::{BuildHasher, Hash, Hasher},
    ops::{Add, Mul},
};

use rapidhash::{HashMapExt, RapidHashMap, fast::RandomState};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct BoxId(pub u32);

impl BoxId {
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
    pub arities: &'a [u32],
    pub multiplicities: &'a [u64],
    pub lengths: &'a [u32],
}

impl<'a> RawBox<'a> {
    pub fn new(
        colors: &'a [Color],
        arities: &'a [u32],
        multiplicities: &'a [u64],
        lengths: &'a [u32],
    ) -> Self {
        Self {
            colors,
            arities,
            multiplicities,
            lengths,
        }
    }

    pub fn hash(&self, random_state: &RandomState) -> u64 {
        let mut hasher = random_state.build_hasher();
        self.colors.hash(&mut hasher);
        self.arities.hash(&mut hasher);
        self.multiplicities.hash(&mut hasher);
        self.lengths.hash(&mut hasher);
        hasher.finish()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
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

#[derive(Debug)]
pub struct BoxArena {
    /// Box colors
    pub colors: Vec<Color>,
    /// Number of child nodes
    pub arities: Vec<u32>,
    /// Multiplicity of the node itself
    pub multiplicities: Vec<u64>,
    /// Number of flat nodes occupied by this subtree
    pub lengths: Vec<u32>,
    /// Pointers to the starting indices of active boxes
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

impl BoxArena {
    pub const ZERO: BoxId = BoxId(0);
    pub const ANTI_ZERO: BoxId = BoxId(1);
    pub const ONE: BoxId = BoxId(2);
    pub const ANTI_ONE: BoxId = BoxId(4);

    /// Initializes the arena with elementary objects
    pub fn new() -> Self {
        let random_state = RandomState::new();
        let mut cache = RapidHashMap::new();

        let mut colors = Vec::with_capacity(8);
        let mut arities = Vec::with_capacity(8);
        let mut multiplicities = Vec::with_capacity(8);
        let mut lengths = Vec::with_capacity(8);
        let mut expression_roots = Vec::with_capacity(8);

        let mut register_box = |id: BoxId, raw_box: RawBox| {
            // Compute structural hash
            let hash = raw_box.hash(&random_state);

            // Commit to vectors
            cache.insert(hash, id);
            colors.extend_from_slice(raw_box.colors);
            arities.extend_from_slice(raw_box.arities);
            multiplicities.extend_from_slice(raw_box.multiplicities);
            lengths.extend_from_slice(raw_box.lengths);
            expression_roots.push(id);
        };

        register_box(Self::ZERO, RawBox::new(&[Color::Black], &[0], &[1], &[1]));
        register_box(
            Self::ANTI_ZERO,
            RawBox::new(&[Color::Red], &[0], &[1], &[1]),
        );
        register_box(
            Self::ONE,
            RawBox::new(&[Color::Black, Color::Black], &[1, 0], &[1, 1], &[2, 1]),
        );
        register_box(
            Self::ANTI_ONE,
            RawBox::new(&[Color::Red, Color::Black], &[1, 0], &[1, 1], &[2, 1]),
        );

        Self {
            colors,
            arities,
            multiplicities,
            lengths,
            expression_roots,
            cache,
            random_state,
        }
    }

    /// Gets the raw data of a box given by its id
    pub fn get(&self, box_id: BoxId) -> RawBox<'_> {
        let idx = box_id.index();
        let len = self.lengths[idx] as usize;
        RawBox {
            colors: &self.colors[idx..(idx + len)],
            arities: &self.arities[idx..(idx + len)],
            multiplicities: &self.multiplicities[idx..(idx + len)],
            lengths: &self.lengths[idx..(idx + len)],
        }
    }

    /// Commits a raw, uncommitted box view
    pub fn commit(&mut self, raw: RawBox<'_>) -> BoxId {
        let hash = raw.hash(&self.random_state);

        // cache hit
        if let Some(&existing_id) = self.cache.get(&hash) {
            let view = self.get(existing_id);
            if view.colors == raw.colors
                && view.arities == raw.arities
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
        self.arities.extend_from_slice(raw.arities);
        self.multiplicities.extend_from_slice(raw.multiplicities);
        self.lengths.extend_from_slice(raw.lengths);

        new_id
    }

    #[inline(always)]
    fn next_id(&self) -> BoxId {
        BoxId(self.colors.len() as u32)
    }

    /// Wraps an existing expression in a new box container
    pub fn wrap_in_box(&mut self, source: BoxId, color: Color, multiplicity: u64) -> BoxId {
        let source_idx = source.index();
        let source_len = self.lengths[source_idx] as usize;

        let mut colors = vec![color];
        let mut arities = vec![1];
        let mut multiplicities = vec![1];
        let mut lengths = vec![1 + source_len as u32];

        // copy source elements
        for i in 0..source_len {
            let curr_idx = source_idx + i;

            colors.push(self.colors[curr_idx]);
            arities.push(self.arities[curr_idx]);

            if i == 0 {
                multiplicities.push(multiplicity);
            } else {
                multiplicities.push(self.multiplicities[curr_idx]);
            }

            lengths.push(self.lengths[curr_idx]);
        }

        let raw = RawBox::new(&colors, &arities, &multiplicities, &lengths);

        self.commit(raw)
    }

    pub fn one(&mut self) -> BoxId {
        self.wrap_in_box(BoxArena::ZERO, Color::Black, 1)
    }

    pub fn one_anti(&mut self) -> BoxId {
        self.wrap_in_box(BoxArena::ZERO, Color::Red, 1)
    }

    pub fn neg_one(&mut self) -> BoxId {
        self.wrap_in_box(BoxArena::ANTI_ZERO, Color::Black, 1)
    }

    pub fn neg_one_anti(&mut self) -> BoxId {
        self.wrap_in_box(BoxArena::ANTI_ZERO, Color::Red, 1)
    }

    pub fn add(&mut self, lhs: BoxId, rhs: BoxId) -> BoxId {
        let lhs_idx = lhs.index();
        let rhs_idx = rhs.index();

        let lhs_len = self.lengths[lhs_idx] as usize;
        let rhs_len = self.lengths[rhs_idx] as usize;

        let lhs_col = self.colors[lhs_idx];
        let rhs_col = self.colors[rhs_idx];

        let mut unique_children: RapidHashMap<u64, (usize, u64)> = RapidHashMap::new();
        let mut slices = Vec::new();

        let mut add_boxes = |start_idx: usize, total_len: usize, arena: &BoxArena| {
            let mut curr = start_idx + 1; // Skip the outer container token
            let end = start_idx + total_len;

            while curr < end {
                let child_len = arena.lengths[curr] as usize;
                let slice = BoxSlice {
                    start: curr,
                    len: child_len,
                };
                let struct_hash = slice.calculate_hash(arena);
                let current_mult = arena.multiplicities[curr];

                // check if a box exists that has the same structure except for the color of the outer box
                let mut found_match = false;
                if let Some(&(existing_plan_idx, _)) = unique_children.get(&struct_hash) {
                    // Double check for hash collisions
                    let match_slice: BoxSlice = slices[existing_plan_idx];
                    if slice.is_equal_to(&match_slice, arena) {
                        let current_col = self.colors[curr];
                        if let Some((_, mul)) = unique_children.get_mut(&struct_hash) {
                            if current_col == Color::Red {
                                *mul = mul.saturating_sub(current_mult);
                            } else {
                                *mul = mul.saturating_add(current_mult);
                            }
                            found_match = true;
                        }
                    }
                }

                if !found_match {
                    let slice_idx = slices.len();
                    slices.push(slice);
                    unique_children.insert(struct_hash, (slice_idx, current_mult));
                }

                curr += child_len;
            }
        };

        // add immediate children
        add_boxes(lhs_idx, lhs_len, self);
        add_boxes(rhs_idx, rhs_len, self);

        // push new consolidated container root
        let mut colors = vec![];
        let mut arities = vec![];
        let mut multiplicities = vec![];
        let mut lengths = vec![];

        let new_color = lhs_col + rhs_col;
        let final_arity = slices.len();

        colors.push(new_color);
        arities.push(final_arity as u32);
        multiplicities.push(1);
        lengths.push(1);

        // initialize data for unique slices
        let mut written_content_len = 0;
        for slice in slices {
            let struct_hash = slice.calculate_hash(self);
            let final_mul = unique_children.get(&struct_hash).unwrap().1 as u64;

            for i in 0..slice.len {
                let src_idx = slice.start + i;

                colors.push(self.colors[src_idx]);
                arities.push(self.arities[src_idx]);

                if i == 0 {
                    multiplicities.push(final_mul);
                } else {
                    multiplicities.push(self.multiplicities[src_idx]);
                }

                lengths.push(self.lengths[src_idx]);
                written_content_len += 1;
            }
        }

        // update total length
        lengths[0] = (1 + written_content_len) as u32;

        let raw = RawBox::new(&colors, &arities, &multiplicities, &lengths);
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

#[derive(Clone, Copy)]
struct BoxSlice {
    start: usize,
    len: usize,
}

impl BoxSlice {
    fn is_equal_to(&self, other: &BoxSlice, arena: &BoxArena) -> bool {
        if self.len != other.len {
            return false;
        }

        let range1 = self.start..(self.start + self.len);
        let range2 = other.start..(other.start + other.len);

        let colors_range1 = self.start + 1..(self.start + self.len);
        let colors_range2 = other.start + 1..(other.start + other.len);

        arena.colors[colors_range1] == arena.colors[colors_range2]
            && arena.arities[range1.clone()] == arena.arities[range2.clone()]
            && arena.lengths[range1] == arena.lengths[range2]
    }

    fn calculate_hash(&self, arena: &BoxArena) -> u64 {
        let mut hasher = arena.random_state.build_hasher();

        let range = self.start..(self.start + self.len);
        arena.colors[self.start + 1..(self.start + self.len)].hash(&mut hasher);
        arena.arities[range.clone()].hash(&mut hasher);
        arena.lengths[range].hash(&mut hasher);

        hasher.finish()
    }
}

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
        let expected = arena.wrap_in_box(zero, Color::Black, 2);

        assert_eq!(two, expected);
    }
}
