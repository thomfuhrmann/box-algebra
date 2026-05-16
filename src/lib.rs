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

use std::{cmp::Ordering, collections::BTreeMap};

#[macro_use]
mod maxel;
mod display;
mod function;
#[macro_use]
mod list;
#[macro_use]
mod polynumber;
mod derivative;
mod ordered_set;
mod set;

/// The fundamental data structure for mathematical boxes
#[derive(Debug, Clone)]
pub enum MBox {
    Box(BTreeMap<MBox, u64>),
    AntiBox(BTreeMap<MBox, u64>),
}

impl PartialEq for MBox {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for MBox {}

impl PartialOrd for MBox {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// Ordering is from anti-box to box and from lower nesting to higher nesting of boxes
impl Ord for MBox {
    fn cmp(&self, other: &Self) -> Ordering {
        // variant ordering first
        match (self, other) {
            (MBox::AntiBox(_), MBox::Box(_)) => return Ordering::Less,
            (MBox::Box(_), MBox::AntiBox(_)) => return Ordering::Greater,
            _ => {}
        }

        let l = self.boxes_ref();
        let r = other.boxes_ref();

        // structural equality check
        if l == r {
            return Ordering::Equal;
        }

        // depth comparison
        let l_depth = self.depth();
        let r_depth = other.depth();
        if l_depth != r_depth {
            return l_depth.cmp(&r_depth);
        }

        // net weight comparison
        let get_weight = |map: &BTreeMap<MBox, u64>| -> i128 {
            map.iter().fold(0i128, |acc, (m, &count)| {
                if m.is_anti_box() {
                    acc - count as i128
                } else {
                    acc + count as i128
                }
            })
        };

        let left_w = get_weight(l);
        let right_w = get_weight(r);

        if left_w != right_w {
            return left_w.cmp(&right_w);
        }

        // final tie-breaker: lexicographical comparison
        l.cmp(r)
    }
}

impl Default for MBox {
    fn default() -> Self {
        MBox::Box(BTreeMap::new())
    }
}

impl MBox {
    /// Creates a new empty box
    pub fn new() -> Self {
        Default::default()
    }

    /// Creates a new empty anti-box
    pub fn new_anti() -> Self {
        MBox::new().into_anti()
    }

    /// Returns the type of the box: 1 for a box, and -1 for an anti-box
    pub fn box_type(&self) -> i32 {
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

    /// Converts a box into a box
    pub fn into_box(self) -> Self {
        match self {
            MBox::AntiBox(m) => MBox::Box(m),
            m_box => m_box,
        }
    }

    /// Converts a box into an anti-box
    pub fn into_anti(self) -> Self {
        match self {
            MBox::Box(m) => MBox::AntiBox(m),
            anti => anti,
        }
    }

    /// Tests if the box is a box
    pub fn is_box(&self) -> bool {
        self.box_type() == 1
    }

    /// Tests if the box is an anti-box
    pub fn is_anti_box(&self) -> bool {
        self.box_type() == -1
    }

    /// Tests if the box is empty
    pub fn is_zero(&self) -> bool {
        self.is_box() && self.is_empty()
    }

    /// Tests if the anti-box is empty
    pub fn is_anti_zero(&self) -> bool {
        self.is_anti_box() && self.is_empty()
    }

    /// Wraps the boxes into a box container
    pub fn from_boxes(inner: BTreeMap<MBox, u64>) -> Self {
        MBox::Box(inner)
    }

    /// Wraps the boxes into an anti-box container
    pub fn from_boxes_anti(inner: BTreeMap<MBox, u64>) -> Self {
        MBox::AntiBox(inner)
    }

    /// Consumes the box returning its contained boxes
    pub fn into_boxes(self) -> BTreeMap<MBox, u64> {
        match self {
            MBox::Box(m) => m,
            MBox::AntiBox(m) => m,
        }
    }

    /// Returns a shared reference to the underlying `BTreeMap`
    pub fn boxes_ref(&self) -> &BTreeMap<MBox, u64> {
        match self {
            MBox::Box(m) | MBox::AntiBox(m) => m,
        }
    }

    /// Returns an exclusive reference to the underlying `BTreeMap`
    pub fn boxes_mut_ref(&mut self) -> &mut BTreeMap<MBox, u64> {
        match self {
            MBox::Box(m) | MBox::AntiBox(m) => m,
        }
    }

    /// Inserts a box into this box
    pub fn insert_box(&mut self, elem: MBox) {
        self.boxes_mut_ref()
            .entry(elem)
            .and_modify(|count| *count += 1)
            .or_insert(1);
    }

    /// Wraps the box into a new box
    pub fn wrap(self) -> Self {
        let mut b = MBox::new();
        b.insert_box(self);
        b
    }

    /// Wraps the box into a new anti-box
    pub fn wrap_anti(self) -> Self {
        let mut b = MBox::new_anti();
        b.insert_box(self);
        b
    }

    /// Calculates the maximum depth of this box
    pub fn depth(&self) -> usize {
        self.boxes_ref()
            .keys()
            .map(|m_box| m_box.depth())
            .max()
            .map(|max_sub_depth| max_sub_depth + 1)
            .unwrap_or(0)
    }

    /// Calculates the size of this box (number of elements including multiplicities)
    pub fn size(&self) -> u64 {
        self.boxes_ref().iter().fold(0, |acc, (_, mul)| acc + mul)
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
                child.into_box()
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
                (child.into_anti(), net.unsigned_abs() as u64)
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
                    let mut m = MBox::new();
                    if n > 0 {
                        m.boxes_mut_ref().insert(MBox::new(), n as u64);
                    }
                    m
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
                    let mut m = MBox::new();
                    if n > 0 {
                        m.boxes_mut_ref().insert(MBox::new(), n as u64);
                    } else if n < 0 {
                        m.boxes_mut_ref().insert(MBox::new_anti(), (-n) as u64);
                    }
                    m
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
            *self.boxes_mut_ref().entry(m).or_default() += count;
        }

        if result_type < 0 {
            self.into_anti()
        } else {
            self.into_box()
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
            for (b2, v2) in other.boxes_ref() {
                result
                    .boxes_mut_ref()
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
        let alpha_5 = var!().pow(5);
        let mut b0 = MBox::new();
        let b1 = MBox::new();
        let b2 = num!(5);
        b0.boxes_mut_ref().insert(b1, 4);
        b0.boxes_mut_ref().insert(b2, 1);
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

        b1.boxes_mut_ref().insert(b2, 1);
        b1.boxes_mut_ref().insert(b3, 1);

        let mut b4 = MBox::new();
        let b5 = MBox::from(5);
        let b6 = MBox::from(3);

        b4.boxes_mut_ref().insert(b6, 1);
        b4.boxes_mut_ref().insert(b5, 1);

        assert_eq!(b1, b4);
    }

    #[test]
    fn test_ord_1() {
        let mut b1 = MBox::new();
        let b2 = MBox::from(2);

        b1.boxes_mut_ref().insert(b2, 2);
        b1.boxes_mut_ref().insert(MBox::new_anti(), 1);
        b1.boxes_mut_ref().insert(MBox::new(), 3);

        let mut b3 = MBox::new();
        let b4 = MBox::from(3);

        b3.boxes_mut_ref().insert(b4, 2);
        b3.boxes_mut_ref().insert(MBox::new(), 2);

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
        let alpha = MBox::alpha();

        let mut b = MBox::new();
        b.boxes_mut_ref().insert(MBox::new(), 1);
        b.boxes_mut_ref().insert(one.clone(), 2);

        let p = one + two * alpha;

        assert_eq!(b, p);

        let b1 = MBox::new();
        let b2 = MBox::from(3);
        let mut b3 = MBox::new();
        b3.boxes_mut_ref().insert(b1.clone(), 3);
        b3.boxes_mut_ref().insert(b2.clone(), 1);

        let b4 = MBox::new();
        let b5 = MBox::from(2);
        let mut b6 = MBox::new();
        b6.boxes_mut_ref().insert(b4.clone(), 2);
        b6.boxes_mut_ref().insert(b5.clone(), 1);

        let two = MBox::from(2);
        let three = MBox::from(3);
        let six = MBox::from(6);
        let alpha = MBox::alpha();
        let alpha_2 = alpha.clone().pow(2);
        let alpha_3 = alpha.clone().pow(3);
        let alpha_5 = alpha.clone().pow(5);
        let p = six + three * alpha_2 + two * alpha_3 + alpha_5;

        assert_eq!(b3 * b6, p);

        let anti_one = MBox::from(1).into_anti();
        let two = MBox::from(2);
        let alpha = MBox::alpha();

        let mut b = MBox::new_anti();
        b.boxes_mut_ref().insert(MBox::new(), 1);
        b.boxes_mut_ref().insert(MBox::from(1), 2);

        let p = anti_one + two * alpha;

        assert_eq!(b, p);
    }

    #[test]
    fn test_annihilate_1() {
        let mut b = MBox::new();
        b.boxes_mut_ref().insert(MBox::new_anti(), 1);
        b.boxes_mut_ref().insert(MBox::new(), 1);

        assert_eq!(b.annihilate(), MBox::new());

        let mut b = MBox::new();
        b.boxes_mut_ref().insert(MBox::new_anti(), 1);
        b.boxes_mut_ref().insert(MBox::new(), 1);
        let mut c = MBox::new();
        c.boxes_mut_ref().insert(b.clone(), 1);

        let d = MBox::from(1);

        assert_eq!(c.annihilate(), d);

        let mut e = MBox::new_anti();
        e.boxes_mut_ref().insert(MBox::new_anti(), 1);
        e.boxes_mut_ref().insert(MBox::new(), 1);
        let mut f = MBox::new();
        f.boxes_mut_ref().insert(b.clone(), 1);
        f.boxes_mut_ref().insert(e.clone(), 1);

        assert_eq!(f.annihilate(), MBox::new());

        let mut a = MBox::new();
        a.boxes_mut_ref().insert(MBox::new(), 1);
        a.boxes_mut_ref().insert(MBox::new_anti(), 1);

        let mut b = MBox::new_anti();
        b.boxes_mut_ref().insert(MBox::new(), 1);
        b.boxes_mut_ref().insert(MBox::new_anti(), 1);

        let mut c = MBox::new();
        c.boxes_mut_ref().insert(a, 1);
        c.boxes_mut_ref().insert(b, 1);

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
