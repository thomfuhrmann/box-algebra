//! Box Algebra is a new way of understanding what arithmetic, and much of associated mathematics, is actually truly about. The most important new understandings are that
//! 1. the most powerful data structure for foundational work is not a set, and is not a list, but is rather a multiset, where elements are unordered and repetitions are allowed
//! 2. the particle / anti particle duality famously discovered by Paul Dirac in 20th century physics has a deep and remarkable analog in the foundations of arithmetic.
//!
//! When we put these two together, we get Box Arithmetic.
//!
//! This crate defines the fundamental data structure, a mathematical box, and associated methods for doing basic algebraic operations.
//!
//! # References
//!
//! The idea of Box Algebra and its arithmetic is being developed by Norman J. Wildberger.
//! He introduces the basic concepts and their applications in a Youtube video series which started in 2022:
//!
//! - <https://www.youtube.com/playlist?list=PLIljB45xT85B0aMG-G9oqj-NPIuBMnq8z>

use std::collections::BTreeMap;

#[macro_use]
pub mod maxel;
mod display;
pub mod function;
#[macro_use]
mod list;
#[macro_use]
pub mod polynumber;
pub mod alt_model;
pub mod derivative;
mod ord;
pub mod ordered_set;
pub mod set;

/// The fundamental data structure for mathematical boxes
#[derive(Debug, Clone)]
pub enum MBox {
    Box(BTreeMap<MBox, u64>),
    AntiBox(BTreeMap<MBox, u64>),
}

impl Default for MBox {
    fn default() -> Self {
        MBox::Box(BTreeMap::new())
    }
}

impl MBox {
    pub const ZERO: Self = MBox::Box(BTreeMap::new());
    pub const ANTI_ZERO: Self = MBox::AntiBox(BTreeMap::new());

    /// Creates a new empty box
    pub const fn new() -> Self {
        MBox::Box(BTreeMap::new())
    }

    /// Creates a new empty anti-box
    pub const fn new_anti() -> Self {
        MBox::AntiBox(BTreeMap::new())
    }

    /// Constructs the box that is one
    /// A box that just contains an empty box
    pub fn one() -> Self {
        let mut map = BTreeMap::new();
        map.insert(Self::ZERO, 1);
        MBox::Box(map)
    }

    /// Constructs the box that is anti-one
    /// An anti-box that just contains an empty box
    pub fn one_anti() -> Self {
        let mut map = BTreeMap::new();
        map.insert(Self::ZERO, 1);
        MBox::AntiBox(map)
    }

    /// Constructs the box that is negative one
    pub fn neg_one() -> Self {
        let mut map = BTreeMap::new();
        map.insert(Self::ANTI_ZERO, 1);
        MBox::Box(map)
    }

    /// Constructs the box that is anti negative one
    pub fn neg_one_anti() -> Self {
        let mut map = BTreeMap::new();
        map.insert(Self::ANTI_ZERO, 1);
        MBox::AntiBox(map)
    }

    /// Returns the type of the box: 1 for a box, and -1 for an anti-box
    pub const fn box_type(&self) -> i8 {
        match self {
            MBox::Box(_) => 1,
            MBox::AntiBox(_) => -1,
        }
    }

    /// Tests if the box is empty
    pub fn is_empty(&self) -> bool {
        match self {
            MBox::Box(map) | MBox::AntiBox(map) => map.is_empty(),
        }
    }

    /// Inverts the color of a box
    pub fn invert_box(self) -> Self {
        match self {
            MBox::Box(map) => MBox::AntiBox(map),
            MBox::AntiBox(map) => MBox::Box(map),
        }
    }

    /// Converts a box into a (non-anti) box
    /// It is the identity for (non-anti) boxes
    pub fn to_box(self) -> Self {
        match self {
            MBox::AntiBox(map) => MBox::Box(map),
            m_box => m_box,
        }
    }

    /// Converts a box into an anti-box
    /// It is the identity for anit-boxes
    pub fn to_anti_box(self) -> Self {
        match self {
            MBox::Box(map) => MBox::AntiBox(map),
            anti => anti,
        }
    }

    /// Tests if the box is a (non-anti) box
    pub const fn is_box(&self) -> bool {
        self.box_type() == 1
    }

    /// Tests if the box is an anti-box
    pub const fn is_anti_box(&self) -> bool {
        self.box_type() == -1
    }

    /// Tests if the box is empty
    pub fn is_zero(&self) -> bool {
        *self == Self::ZERO
    }

    /// Tests if the anti-box is empty
    pub fn is_anti_zero(&self) -> bool {
        *self == Self::ANTI_ZERO
    }

    /// Consumes the box returning its contained boxes
    pub fn into_boxes(self) -> BTreeMap<MBox, u64> {
        match self {
            MBox::Box(m) | MBox::AntiBox(m) => m,
        }
    }

    /// Returns a shared reference to the underlying `BTreeMap`
    pub fn as_boxes(&self) -> &BTreeMap<MBox, u64> {
        match self {
            MBox::Box(m) | MBox::AntiBox(m) => m,
        }
    }

    /// Returns an exclusive reference to the underlying `BTreeMap`
    pub fn as_boxes_mut(&mut self) -> &mut BTreeMap<MBox, u64> {
        match self {
            MBox::Box(m) | MBox::AntiBox(m) => m,
        }
    }

    /// Inserts a box into this box
    pub fn insert_box(&mut self, elem: MBox) {
        self.as_boxes_mut()
            .entry(elem)
            .and_modify(|count| *count += 1)
            .or_insert(1);
    }

    /// Wraps the box into another new box
    pub fn wrap(self) -> Self {
        let mut b = MBox::new();
        b.insert_box(self);
        b
    }

    /// Wraps the box into another new anti-box
    pub fn wrap_anti(self) -> Self {
        let mut b = MBox::new_anti();
        b.insert_box(self);
        b
    }

    /// Calculates the maximum depth of this box
    pub fn depth(&self) -> usize {
        self.as_boxes()
            .keys()
            .map(|k| k.depth() + 1)
            .max()
            .unwrap_or(0)
    }

    /// Calculates the total number of boxes contained in this box
    pub fn box_count(&self) -> u64 {
        let map = self.as_boxes();
        if map.is_empty() {
            return 0;
        }

        // Sum the leaves of all inner keys, multiplied by their counts/exponents
        map.iter()
            .map(|(key, &count)| key.box_count() * count)
            .sum()
    }

    /// Counts only the multiplicities of leaf nodes that reside at the maximum depth of this box
    pub fn outer_leaf_counts(&self) -> Vec<u64> {
        let max_depth = self.depth();

        if max_depth == 0 {
            return vec![0];
        }

        Self::count_at_depth(self, 0, max_depth)
    }

    fn count_at_depth(&self, current_depth: usize, target_depth: usize) -> Vec<u64> {
        // If we have reached the parent layer of the target depth,
        // every entry in this map is an outermost leaf node
        if current_depth + 1 == target_depth {
            let ann = self.clone().annihilate();
            let map = ann.into_boxes();
            return map.into_iter().map(|(_, value)| value).collect();
        }

        let map = self.as_boxes();
        map.iter()
            .flat_map(|(key, _)| Self::count_at_depth(key, current_depth + 1, target_depth))
            .collect()
    }

    /// Calculates the size of this box (number of elements including multiplicities)
    pub fn size(&self) -> u64 {
        self.as_boxes().iter().fold(0, |acc, (_, mul)| acc + mul)
    }

    /// Tests if this box is a number
    pub fn is_number(&self) -> bool {
        self.depth() == 1 && self.is_box()
    }

    /// Tests if this box is an anti-number
    pub fn is_anti_number(&self) -> bool {
        self.depth() == 1 && self.is_anti_box()
    }

    /// Returns n-times the box
    pub fn scale(&self, n: u64) -> Self {
        (0..n).fold(MBox::new(), |acc, _| acc + self.clone())
    }

    /// Exponentiation of boxes
    pub fn pow(self, exp: u32) -> Self {
        if exp > 0 {
            let mut m = self.clone();

            for _ in 0..(exp - 1) {
                m = m * self.clone();
            }

            m
        } else {
            MBox::from(1)
        }
    }

    /// Reduces the expression by annihilation of box-anti-box couples
    pub fn annihilate(self) -> Self {
        if self.is_empty() {
            return self;
        }

        let is_anti = self.is_anti_box();

        // i128 to prevent overflow
        let mut net_counts: BTreeMap<MBox, i128> = BTreeMap::new();

        for (child, count) in self.into_boxes() {
            let child = child.annihilate();

            // normalize
            let multiplier = if child.is_anti_box() { -1 } else { 1 };
            let normalized_child = if child.is_anti_box() {
                child.to_box()
            } else {
                child
            };

            *net_counts.entry(normalized_child).or_default() += (count as i128) * multiplier;
        }

        // reconstruct the map
        let mut final_map = BTreeMap::new();
        for (child, net) in net_counts {
            if net == 0 {
                continue;
            }

            // if net is negative, the child becomes an anti-box with a positive count
            let (final_child, final_count) = if net < 0 {
                (child.to_anti_box(), net.unsigned_abs() as u64)
            } else {
                (child, net as u64)
            };

            final_map.insert(final_child, final_count);
        }

        if is_anti {
            MBox::AntiBox(final_map)
        } else {
            MBox::Box(final_map)
        }
    }

    /// Truncates the box to the box given as an argument
    pub fn truncate(self, trunc: &Self) -> Self {
        let is_anti = self.is_anti_box();
        let inner = self
            .into_boxes()
            .into_iter()
            .filter(|(m_box, _)| m_box == trunc)
            .collect();

        if is_anti {
            MBox::AntiBox(inner)
        } else {
            MBox::Box(inner)
        }
    }
}

impl FromIterator<MBox> for MBox {
    fn from_iter<T: IntoIterator<Item = MBox>>(iter: T) -> Self {
        let mut result = MBox::new();
        iter.into_iter().for_each(|m_box| result.insert_box(m_box));
        result
    }
}

macro_rules! from_uint {
    ($($t:ty),*) => {
        $(
            impl From<$t> for MBox {
                /// Convert unsigned integer into MBox
                fn from(n: $t) -> Self {
                    let mut outer = MBox::new();
                    if n > 0 {
                        outer.as_boxes_mut().insert(MBox::ZERO, n as u64);
                    }
                    outer
                }
            }
        )*
    };
}

from_uint!(u8, u16, u32, u64, u128, usize);

macro_rules! from_int {
    ($($t:ty),*) => {
        $(
            impl From<$t> for MBox {
                /// Convert signed integer into MBox
                fn from(n: $t) -> Self {
                    let mut outer = MBox::new();
                    if n > 0 {
                        outer.as_boxes_mut().insert(MBox::ZERO, n as u64);
                    } else if n < 0 {
                        outer.as_boxes_mut().insert(MBox::ANTI_ZERO, (-n) as u64);
                    }
                    outer
                }
            }
        )*
    };
}

from_int!(i8, i16, i32, i64, i128, isize);

impl std::ops::Add for MBox {
    type Output = MBox;
    fn add(mut self, other: Self) -> Self {
        let result_type = self.box_type() * other.box_type();
        for (m, count) in other.into_boxes() {
            *self.as_boxes_mut().entry(m).or_default() += count;
        }

        if result_type < 0 {
            self.to_anti_box()
        } else {
            self.to_box()
        }
    }
}

impl std::ops::Add<&MBox> for &MBox {
    type Output = MBox;
    fn add(self, other: &MBox) -> MBox {
        self.clone() + other.clone()
    }
}

impl std::ops::Add<&MBox> for MBox {
    type Output = MBox;
    fn add(self, other: &MBox) -> MBox {
        self + other.clone()
    }
}

impl std::ops::Add<MBox> for &MBox {
    type Output = MBox;
    fn add(self, other: MBox) -> MBox {
        self.clone() + other
    }
}

impl std::ops::Mul for MBox {
    type Output = MBox;

    fn mul(self, other: MBox) -> MBox {
        let mut result = if self.box_type() * other.box_type() > 0 {
            MBox::new()
        } else {
            MBox::new_anti()
        };

        for (b1, v1) in self.into_boxes() {
            for (b2, v2) in other.as_boxes() {
                result
                    .as_boxes_mut()
                    .insert(b1.clone() + b2.clone(), v1 * v2);
            }
        }

        result
    }
}

impl std::ops::Mul<&MBox> for &MBox {
    type Output = MBox;
    fn mul(self, other: &MBox) -> MBox {
        self.clone() * other.clone()
    }
}

impl std::ops::Mul<&MBox> for MBox {
    type Output = MBox;
    fn mul(self, other: &MBox) -> MBox {
        self * other.clone()
    }
}

impl std::ops::Mul<MBox> for &MBox {
    type Output = MBox;
    fn mul(self, other: MBox) -> MBox {
        self.clone() * other
    }
}

#[macro_export]
macro_rules! m_box {
    ($($p:expr),* $(,)?) => {
        {
            let mut outer_box = $crate::MBox::new();
            $(
                outer_box.insert_box(($p).into());
            )*
            outer_box
        }
    };
}

#[macro_export]
macro_rules! num {
    ($e:expr) => {
        $crate::MBox::from($e)
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_depth() {
        let empty = MBox::new();
        assert_eq!(empty.depth(), 0);

        let one = num!(1);
        assert_eq!(one.depth(), 1);

        let x = var!(0);
        assert_eq!(x.depth(), 2);

        let v_1 = var!(1);
        assert_eq!(v_1.depth(), 3);

        let pix = pixel![1, 2];
        let dep = pix.depth();
        assert_eq!(dep, 3);
    }

    #[test]
    fn test_add_1() {
        let three = num!(3);
        let five = num!(5);
        let eight = num!(8);
        assert_eq!(three + five, eight);

        let four = num!(4);
        let alpha_5 = var!(0).pow(5);
        let mut b0 = MBox::new();
        let b1 = MBox::new();
        let b2 = num!(5);
        b0.as_boxes_mut().insert(b1, 4);
        b0.as_boxes_mut().insert(b2, 1);
        assert_eq!(four + alpha_5, b0);

        let b1 = MBox::new();
        let b2 = MBox::new();
        let b3 = MBox::new();
        assert_eq!(b1 + b2, b3);

        let b1 = num!(0);
        let b2 = num!(0);
        let b3 = num!(0);
        assert_eq!(b1 + b2, b3);

        assert_eq!(num!(0), MBox::new());

        let mut b1 = MBox::new();
        let b2 = MBox::from(5);
        let b3 = MBox::from(3);

        b1.as_boxes_mut().insert(b2, 1);
        b1.as_boxes_mut().insert(b3, 1);

        let mut b4 = MBox::new();
        let b5 = MBox::from(5);
        let b6 = MBox::from(3);

        b4.as_boxes_mut().insert(b6, 1);
        b4.as_boxes_mut().insert(b5, 1);

        assert_eq!(b1, b4);
    }

    #[test]
    fn test_ord_1() {
        let mut b1 = MBox::new();
        let b2 = MBox::from(2);

        b1.as_boxes_mut().insert(b2, 2);
        b1.as_boxes_mut().insert(MBox::new_anti(), 1);
        b1.as_boxes_mut().insert(MBox::new(), 3);

        let mut b3 = MBox::new();
        let b4 = MBox::from(3);

        b3.as_boxes_mut().insert(b4, 2);
        b3.as_boxes_mut().insert(MBox::new(), 2);

        assert!(b3 > b1);

        let b1_ann = b1.annihilate();

        assert!(b3 > b1_ann);
    }

    #[test]
    fn mult_1() {
        let b1 = MBox::from(3);
        let b2 = MBox::from(5);

        let b3 = MBox::from(15);

        assert_eq!(b1 * b2, b3);

        let one = MBox::from(1);
        let two = MBox::from(2);
        let alpha = var!(0);

        let mut b = MBox::new();
        b.as_boxes_mut().insert(MBox::new(), 1);
        b.as_boxes_mut().insert(one.clone(), 2);

        let p = one + two * alpha;

        assert_eq!(b, p);

        let b1 = MBox::new();
        let b2 = MBox::from(3);
        let mut b3 = MBox::new();
        b3.as_boxes_mut().insert(b1.clone(), 3);
        b3.as_boxes_mut().insert(b2.clone(), 1);

        let b4 = MBox::new();
        let b5 = MBox::from(2);
        let mut b6 = MBox::new();
        b6.as_boxes_mut().insert(b4.clone(), 2);
        b6.as_boxes_mut().insert(b5.clone(), 1);

        let two = MBox::from(2);
        let three = MBox::from(3);
        let six = MBox::from(6);
        let alpha = var!(0);
        let alpha_2 = alpha.clone().pow(2);
        let alpha_3 = alpha.clone().pow(3);
        let alpha_5 = alpha.clone().pow(5);
        let p = six + three * alpha_2 + two * alpha_3 + alpha_5;

        assert_eq!(b3 * b6, p);

        let anti_one = MBox::one_anti();
        let two = num!(2);
        let alpha = var!(0);

        let mut b = MBox::new_anti();
        b.as_boxes_mut().insert(MBox::new(), 1);
        b.as_boxes_mut().insert(MBox::from(1), 2);

        let p = anti_one + two * alpha;

        assert_eq!(b, p);
    }

    #[test]
    fn test_annihilate_1() {
        let mut b = MBox::new();
        b.as_boxes_mut().insert(MBox::new_anti(), 1);
        b.as_boxes_mut().insert(MBox::new(), 1);

        assert_eq!(b.annihilate(), MBox::new());

        let mut b = MBox::new();
        b.as_boxes_mut().insert(MBox::new_anti(), 1);
        b.as_boxes_mut().insert(MBox::new(), 1);
        let mut c = MBox::new();
        c.as_boxes_mut().insert(b.clone(), 1);

        let d = MBox::from(1);

        assert_eq!(c.annihilate(), d);

        let mut e = MBox::new_anti();
        e.as_boxes_mut().insert(MBox::new_anti(), 1);
        e.as_boxes_mut().insert(MBox::new(), 1);
        let mut f = MBox::new();
        f.as_boxes_mut().insert(b.clone(), 1);
        f.as_boxes_mut().insert(e.clone(), 1);

        assert_eq!(f.annihilate(), MBox::new());

        let mut a = MBox::new();
        a.as_boxes_mut().insert(MBox::new(), 1);
        a.as_boxes_mut().insert(MBox::new_anti(), 1);

        let mut b = MBox::new_anti();
        b.as_boxes_mut().insert(MBox::new(), 1);
        b.as_boxes_mut().insert(MBox::new_anti(), 1);

        let mut c = MBox::new();
        c.as_boxes_mut().insert(a, 1);
        c.as_boxes_mut().insert(b, 1);

        assert_eq!(c.annihilate(), MBox::new());
    }

    #[test]
    fn test_truncate_1() {
        let a = m_box![2, 3, 4, 4];
        let b = num![4];
        let c = a.truncate(&b);

        let expected = m_box![4, 4];
        assert_eq!(c, expected);
    }
}
