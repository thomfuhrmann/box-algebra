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

use std::{
    cmp::Ordering,
    collections::BTreeMap,
    ops::{Add, Mul},
};

mod maxel;

/// This is the fundamental data structure for mathematical boxes
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
    pub fn new() -> Self {
        Default::default()
    }

    pub fn is_empty(&self) -> bool {
        match self {
            MBox::Box(m) => m.is_empty(),
            MBox::AntiBox(m) => m.is_empty(),
        }
    }

    pub fn into_anti(self) -> Self {
        match self {
            MBox::Box(m) => MBox::AntiBox(m),
            MBox::AntiBox(m) => MBox::Box(m),
        }
    }

    pub fn new_anti() -> Self {
        MBox::new().into_anti()
    }

    pub fn box_type(&self) -> i32 {
        match self {
            MBox::Box(_) => 1,
            MBox::AntiBox(_) => -1,
        }
    }

    pub fn is_box(&self) -> bool {
        self.box_type() == 1
    }

    pub fn is_anti_box(&self) -> bool {
        self.box_type() == -1
    }

    pub fn is_zero(&self) -> bool {
        self.is_box() && self.is_empty()
    }

    pub fn is_anti_zero(&self) -> bool {
        self.is_anti_box() && self.is_empty()
    }

    pub fn into_boxes(self) -> BTreeMap<MBox, u64> {
        match self {
            MBox::Box(m) => m,
            MBox::AntiBox(m) => m,
        }
    }

    pub fn boxes_ref(&self) -> &BTreeMap<MBox, u64> {
        match self {
            MBox::Box(m) => m,
            MBox::AntiBox(m) => m,
        }
    }

    pub fn boxes_mut_ref(&mut self) -> &mut BTreeMap<MBox, u64> {
        match self {
            MBox::Box(m) => m,
            MBox::AntiBox(m) => m,
        }
    }

    pub fn wrap(self) -> Self {
        let mut b = MBox::new();
        b.insert_box(self);
        b
    }

    pub fn wrap_anti(self) -> Self {
        let mut b = MBox::new_anti();
        b.insert_box(self);
        b
    }

    pub fn insert_box(&mut self, elem: MBox) {
        self.boxes_mut_ref()
            .entry(elem)
            .and_modify(|count| *count += 1)
            .or_insert(1);
    }

    pub fn depth(&self) -> usize {
        self.boxes_ref()
            .keys()
            .map(|m_box| m_box.depth())
            .max()
            .map(|max_sub_depth| max_sub_depth + 1)
            .unwrap_or(0)
    }

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

    pub fn alpha() -> Self {
        MBox::from(1).wrap()
    }

    pub fn annihilate(self) -> Self {
        if self.is_zero() || self.is_anti_zero() {
            self
        } else {
            let mut outer = if self.is_box() {
                MBox::new()
            } else {
                MBox::new_anti()
            };

            for (b, v1) in self.into_boxes() {
                let b = b.annihilate();
                if b.is_anti_box() {
                    let anti = b.clone().into_anti();
                    // Check if corresponding box is already contained in outer box
                    if let Some(v2) = outer.boxes_mut_ref().get_mut(&anti) {
                        let v2_copy = *v2;
                        if v1 >= v2_copy {
                            outer.boxes_mut_ref().remove(&anti);
                            if v1 > v2_copy {
                                outer.boxes_mut_ref().insert(b, v1 - v2_copy);
                            }
                        } else {
                            *v2 = v2_copy - v1;
                        }
                    } else if let Some(v2) = outer.boxes_mut_ref().get_mut(&b) {
                        *v2 += v1;
                    } else {
                        outer.boxes_mut_ref().insert(b, v1);
                    }
                } else if let Some(v2) = outer.boxes_mut_ref().get_mut(&b) {
                    *v2 += v1;
                } else {
                    outer.boxes_mut_ref().insert(b, v1);
                }
            }

            outer
        }
    }

    pub fn list(entries: Vec<MBox>) -> Self {
        let mut result = Self::new();
        let mut current_sequence = Self::new();

        for e in entries {
            current_sequence.insert_box(e);
            result.insert_box(current_sequence.clone());
        }

        result
    }

    pub fn is_list(&self) -> bool {
        let sequences: Vec<&MBox> = self.boxes_ref().keys().collect();

        if sequences.is_empty() {
            return true;
        }

        for i in 0..sequences.len() - 1 {
            let current = sequences[i];
            let next = sequences[i + 1];

            for (curr_box, curr_mul) in current.boxes_ref() {
                if let Some(next_mul) = next.boxes_ref().get(curr_box) {
                    if next_mul != curr_mul && *next_mul != *curr_mul + 1 {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }

        true
    }

    /// A pixel is a 2-list of boxes
    pub fn pixel(a: MBox, b: MBox) -> Self {
        let mut result = Self::new();
        let mut current_sequence = Self::new();
        current_sequence.insert_box(a.clone());
        result.insert_box(current_sequence.clone());
        current_sequence.insert_box(b);
        result.insert_box(current_sequence);
        result
    }

    /// Test if the box is a pixel
    pub fn is_pixel(&self) -> bool {
        self.boxes_ref().len() == 2 && self.is_list()
    }

    /// Test if the box contains only one element
    pub fn is_singleton(&self) -> bool {
        self.boxes_ref().len() == 1
    }

    /// Test if the box is a vexel which is defined as a box of singletons
    pub fn is_vexel(&self) -> bool {
        self.boxes_ref().iter().all(|(b, _)| b.is_singleton())
    }

    /// Test if the box is a maxel which is defined as a box of pixels
    pub fn is_maxel(&self) -> bool {
        self.boxes_ref().iter().all(|(b, _)| b.is_pixel())
    }

    pub fn as_pixel_pair(&self) -> Option<(&MBox, &MBox)> {
        let mut iter = self.boxes_ref().keys();
        let first = iter.next()?;
        let first_box = first.boxes_ref().keys().next()?;

        let second = iter.next()?;
        let second_box = second
            .boxes_ref()
            .iter()
            .find(|&(m_box, &mul)| if m_box == first_box { mul > 1 } else { true })?
            .0;

        Some((first_box, second_box))
    }

    /// If a and b are pixels, computes the pixel product required for maxel multiplication
    pub fn pixel_product(a: &MBox, b: &MBox) -> Option<Self> {
        let (a_1, a_2) = a.as_pixel_pair()?;
        let (b_1, b_2) = b.as_pixel_pair()?;

        if a_2 == b_1 {
            Some(Self::pixel(a_1.clone(), b_2.clone()))
        } else {
            None
        }
    }
    /// Compute the product of two maxels
    pub fn maxel_product(a_box: &MBox, b_box: &MBox) -> Self {
        assert!(a_box.is_maxel());
        assert!(b_box.is_maxel());

        let mut result = Self::new();
        for (a_pix, a_mul) in a_box.boxes_ref() {
            for (b_pix, b_mul) in b_box.boxes_ref() {
                if let Some(pix) = Self::pixel_product(a_pix, b_pix) {
                    result.boxes_mut_ref().insert(pix, a_mul * b_mul);
                }
            }
        }

        result
    }
}

impl From<u32> for MBox {
    fn from(value: u32) -> Self {
        let e = MBox::new();
        let mut m = MBox::new();
        m.boxes_mut_ref().insert(e, value.into());
        m
    }
}

impl From<i32> for MBox {
    fn from(value: i32) -> Self {
        let mut m = MBox::new();
        if value >= 0 {
            let e = MBox::new();
            m.boxes_mut_ref().insert(e, value as u64);
            m
        } else {
            let e = MBox::new_anti();
            m.boxes_mut_ref().insert(e, (-value) as u64);
            m
        }
    }
}

impl Add for MBox {
    type Output = MBox;

    fn add(self, other: MBox) -> MBox {
        match self {
            MBox::Box(mut a) => match other {
                MBox::Box(b) => {
                    for (k, v) in b {
                        a.entry(k).and_modify(|curr| *curr += v).or_insert(v);
                    }
                    MBox::Box(a)
                }
                MBox::AntiBox(b) => {
                    for (k, v) in b {
                        a.entry(k).and_modify(|curr| *curr += v).or_insert(v);
                    }
                    MBox::AntiBox(a)
                }
            },
            MBox::AntiBox(mut a) => match other {
                MBox::Box(b) => {
                    for (k, v) in b {
                        a.entry(k).and_modify(|curr| *curr += v).or_insert(v);
                    }
                    MBox::AntiBox(a)
                }
                MBox::AntiBox(b) => {
                    for (k, v) in b {
                        a.entry(k).and_modify(|curr| *curr += v).or_insert(v);
                    }
                    MBox::Box(a)
                }
            },
        }
    }
}

impl Mul for MBox {
    type Output = MBox;

    fn mul(self, other: MBox) -> MBox {
        let mut b = if self.box_type() * other.box_type() == 1 {
            MBox::new()
        } else {
            MBox::new_anti()
        };

        for (b1, v1) in self.boxes_ref() {
            for (b2, v2) in other.boxes_ref() {
                b.boxes_mut_ref().insert(b1.clone() + b2.clone(), v1 * v2);
            }
        }

        b
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_depth() {
        let pix = MBox::pixel(MBox::from(1), MBox::from(2));
        let dep = pix.depth();
        assert_eq!(dep, 3);
    }

    #[test]
    fn add_1() {
        let three = MBox::from(3);
        let five = MBox::from(5);
        let eight = MBox::from(8);
        assert_eq!(three + five, eight);

        let four = MBox::from(4);
        let alpha_5 = MBox::alpha().pow(5);

        let mut b0 = MBox::new();
        let b1 = MBox::new();
        let b2 = MBox::from(5);
        b0.boxes_mut_ref().insert(b1, 4);
        b0.boxes_mut_ref().insert(b2, 1);

        assert_eq!(four + alpha_5, b0);

        let b1 = MBox::new();
        let b2 = MBox::new();
        let b3 = MBox::new();
        assert_eq!(b1 + b2, b3);

        let b1 = MBox::from(0);
        let b2 = MBox::from(0);
        let b3 = MBox::from(0);
        assert_eq!(b1 + b2, b3);

        let mut b1 = MBox::from(2);
        let b2 = MBox::from(5);
        b1.boxes_mut_ref().insert(b2, 1);

        let mut b3 = MBox::from(2);
        let b4 = MBox::from(5);
        b3.boxes_mut_ref().insert(b4, 1);
        assert_eq!(b1, b3);

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

        assert!(b3 < b1);
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
    fn test_pixel_product_1() {
        let a = MBox::pixel(MBox::from(2), MBox::from(2));
        let b = MBox::pixel(MBox::from(2), MBox::from(1));
        let c = MBox::pixel_product(&a, &b).unwrap();
        let expected = MBox::pixel(MBox::from(2), MBox::from(1));
        assert_eq!(c, expected);

        let a = MBox::pixel(MBox::from(1), MBox::from(2));
        let b = MBox::pixel(MBox::from(3), MBox::from(4));
        let none = MBox::pixel_product(&a, &b);
        assert!(none.is_none());
    }

    #[test]
    fn test_maxel_product_1() {
        let mut a = MBox::new();
        let a_11 = MBox::pixel(MBox::from(1), MBox::from(1));
        let a_12 = MBox::pixel(MBox::from(1), MBox::from(2));
        let a_22 = MBox::pixel(MBox::from(2), MBox::from(2));
        a.insert_box(a_11.clone());
        a.insert_box(a_12.clone());
        a.insert_box(a_22.clone());

        let mut b = MBox::new();
        let b_12 = MBox::pixel(MBox::from(1), MBox::from(2));
        let b_21 = MBox::pixel(MBox::from(2), MBox::from(1));
        b.insert_box(b_12.clone());
        b.insert_box(b_21.clone());

        let c = MBox::maxel_product(&a, &b);
        let mut expected = MBox::new();
        expected.insert_box(a_11);
        expected.insert_box(a_12);
        expected.insert_box(b_21);
        assert_eq!(c, expected);
    }
}
