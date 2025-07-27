use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashSet},
    ops::{Add, Deref, DerefMut, Mul},
};

trait Equality<Rhs = Self> {
    fn equals(&self, other: &Rhs) -> bool;
}

impl Equality for BTreeMap<MBox, u32> {
    fn equals(&self, other: &Self) -> bool {
        if self.len() == other.len() {
            if self.is_empty() && other.is_empty() {
                return true;
            } else {
                for (lk, lv) in self {
                    for (rk, rv) in other {
                        if (lv == rv) && (lk == rk) {
                            continue;
                        } else {
                            return false;
                        }
                    }
                }
                return true;
            }
        } else {
            return false;
        }
    }
}

/// This is the fundamental data structure for mathematical boxes
#[derive(Debug, Clone)]
pub enum MBox {
    Box(BTreeMap<MBox, u32>),
    AntiBox(BTreeMap<MBox, u32>),
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

/// Ordering is from box to anti-box and from lower nesting to higher nesting of boxes
impl Ord for MBox {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self {
            MBox::Box(l) => match other {
                MBox::Box(r) => {
                    if l.equals(r) {
                        return Ordering::Equal;
                    } else if l.is_empty() {
                        return Ordering::Less;
                    } else if r.is_empty() {
                        return Ordering::Greater;
                    } else {
                        return l.cmp(r);
                    }
                }
                MBox::AntiBox(_) => Ordering::Less,
            },
            MBox::AntiBox(l) => match other {
                MBox::Box(_) => Ordering::Greater,
                MBox::AntiBox(r) => {
                    if l.equals(r) {
                        return Ordering::Equal;
                    } else if l.is_empty() {
                        return Ordering::Less;
                    } else if r.is_empty() {
                        return Ordering::Greater;
                    } else {
                        return l.cmp(r);
                    }
                }
            },
        }
    }
}

impl MBox {
    pub fn new() -> Self {
        MBox::Box(BTreeMap::new())
    }

    pub fn new_anti() -> Self {
        MBox::new().into_anti()
    }

    pub fn into_anti(self) -> Self {
        match self {
            MBox::Box(m) => MBox::AntiBox(m),
            MBox::AntiBox(m) => MBox::Box(m),
        }
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

    pub fn boxes(self) -> BTreeMap<MBox, u32> {
        match self {
            MBox::Box(m) => m,
            MBox::AntiBox(m) => m,
        }
    }

    pub fn boxes_ref(&self) -> &BTreeMap<MBox, u32> {
        match self {
            MBox::Box(m) => m,
            MBox::AntiBox(m) => m,
        }
    }

    pub fn boxes_mut_ref(&mut self) -> &mut BTreeMap<MBox, u32> {
        match self {
            MBox::Box(m) => m,
            MBox::AntiBox(m) => m,
        }
    }

    pub fn wrap(self) -> Self {
        let mut b = MBox::new();
        b.insert(self, 1);
        b
    }

    pub fn wrap_anti(self) -> Self {
        let mut b = MBox::new_anti();
        b.insert(self, 1);
        b
    }

    pub fn pow(self, exp: u32) -> Self {
        if exp > 0 {
            let mut m = self.clone();

            for _ in 0..(exp - 1) {
                m = m * self.clone();
            }

            return m;
        } else {
            return MBox::from(1);
        }
    }

    pub fn alpha() -> Self {
        MBox::from(1).wrap()
    }

    pub fn annihilate(self) -> Self {
        if self.is_zero() || self.is_anti_zero() {
            return self;
        } else {
            let mut outer = if self.is_box() {
                MBox::new()
            } else {
                MBox::new_anti()
            };

            for (b, v1) in self.boxes() {
                let b = b.annihilate();

                if b.is_anti_box() {
                    let anti = b.clone().into_anti();
                    // Check if corresponding anti-box is already contained in outer box
                    if let Some(v2) = outer.get_mut(&anti) {
                        let v2_copy = *v2;
                        if v1 >= v2_copy {
                            outer.remove(&anti);
                            if v1 > v2_copy {
                                outer.insert(b, v1 - v2_copy);
                            }
                        } else {
                            *v2 = v2_copy - v1;
                        }
                    } else {
                        if let Some(v2) = outer.get_mut(&b) {
                            *v2 += v1;
                        } else {
                            outer.insert(b, v1);
                        }
                    }
                } else {
                    if let Some(v2) = outer.get_mut(&b) {
                        *v2 += v1;
                    } else {
                        outer.insert(b, v1);
                    }
                }
            }

            return outer;
        }
    }
}

impl Deref for MBox {
    type Target = BTreeMap<MBox, u32>;
    fn deref(&self) -> &Self::Target {
        match self {
            MBox::Box(m) => m,
            MBox::AntiBox(m) => m,
        }
    }
}

impl DerefMut for MBox {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            MBox::Box(m) => m,
            MBox::AntiBox(m) => m,
        }
    }
}

impl From<u32> for MBox {
    fn from(value: u32) -> Self {
        let e = MBox::new();
        let mut m = MBox::new();
        m.insert(e, value);
        m
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
                    return MBox::Box(a);
                }
                MBox::AntiBox(b) => {
                    for (k, v) in b {
                        a.entry(k).and_modify(|curr| *curr += v).or_insert(v);
                    }
                    return MBox::AntiBox(a);
                }
            },
            MBox::AntiBox(mut a) => match other {
                MBox::Box(b) => {
                    for (k, v) in b {
                        a.entry(k).and_modify(|curr| *curr += v).or_insert(v);
                    }
                    return MBox::AntiBox(a);
                }
                MBox::AntiBox(b) => {
                    for (k, v) in b {
                        a.entry(k).and_modify(|curr| *curr += v).or_insert(v);
                    }
                    return MBox::Box(a);
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

        for (b1, v1) in self.boxes() {
            for (b2, v2) in other.clone().boxes() {
                b.insert(b1.clone() + b2, v1 * v2);
            }
        }

        b
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_1() {
        let three = MBox::from(3);
        let five = MBox::from(5);
        let eight = MBox::from(8);
        assert_eq!(three + five, eight);
    }

    #[test]
    fn add_2() {
        let four = MBox::from(4);
        let alpha_5 = MBox::alpha().pow(5);

        let mut b0 = MBox::new();
        let b1 = MBox::new();
        let b2 = MBox::from(5);
        b0.insert(b1, 4);
        b0.insert(b2, 1);

        assert_eq!(four + alpha_5, b0);
    }

    #[test]
    fn add_3() {
        let b1 = MBox::new();
        let b2 = MBox::new();
        let b3 = MBox::new();
        assert_eq!(b1 + b2, b3);
    }

    #[test]
    fn add_4() {
        let b1 = MBox::from(0);
        let b2 = MBox::from(0);
        let b3 = MBox::from(0);
        assert_eq!(b1 + b2, b3);
    }

    #[test]
    fn test_equality_1() {
        let mut b1 = MBox::from(2);
        let b2 = MBox::from(5);
        b1.insert(b2, 1);

        let mut b3 = MBox::from(2);
        let b4 = MBox::from(5);
        b3.insert(b4, 1);
        assert_eq!(b1, b3);
    }

    #[test]
    fn test_equality_2() {
        let mut b1 = MBox::new();
        let b2 = MBox::from(5);
        let b3 = MBox::from(3);

        b1.insert(b2, 1);
        b1.insert(b3, 1);

        let mut b4 = MBox::new();
        let b5 = MBox::from(5);
        let b6 = MBox::from(3);

        b4.insert(b6, 1);
        b4.insert(b5, 1);

        assert_eq!(b1, b4);
    }

    #[test]
    fn mult_1() {
        let b1 = MBox::from(3);
        let b2 = MBox::from(5);

        let b3 = MBox::from(15);

        assert_eq!(b1 * b2, b3);
    }

    #[test]
    fn mult_2() {
        let one = MBox::from(1);
        let two = MBox::from(2);
        let alpha = MBox::alpha();

        let mut b = MBox::new();
        b.insert(MBox::new(), 1);
        b.insert(one.clone(), 2);

        let p = one + two * alpha;

        assert_eq!(b, p);
    }

    #[test]
    fn mult_3() {
        let b1 = MBox::new();
        let b2 = MBox::from(3);
        let mut b3 = MBox::new();
        b3.insert(b1.clone(), 3);
        b3.insert(b2.clone(), 1);

        let b4 = MBox::new();
        let b5 = MBox::from(2);
        let mut b6 = MBox::new();
        b6.insert(b4.clone(), 2);
        b6.insert(b5.clone(), 1);

        let two = MBox::from(2);
        let three = MBox::from(3);
        let six = MBox::from(6);
        let alpha = MBox::alpha();
        let alpha_2 = alpha.clone().pow(2);
        let alpha_3 = alpha.clone().pow(3);
        let alpha_5 = alpha.clone().pow(5);
        let p = six + three * alpha_2 + two * alpha_3 + alpha_5;

        assert_eq!(b3 * b6, p);
    }

    #[test]
    fn mult_4() {
        let anti_one = MBox::from(1).into_anti();
        let two = MBox::from(2);
        let alpha = MBox::alpha();

        let mut b = MBox::new_anti();
        b.insert(MBox::new(), 1);
        b.insert(MBox::from(1), 2);

        let p = anti_one + two * alpha;

        assert_eq!(b, p);
    }

    #[test]
    fn test_annihilate_1() {
        let mut b = MBox::new();
        b.insert(MBox::new_anti(), 1);
        b.insert(MBox::new(), 1);

        assert_eq!(b.annihilate(), MBox::new());
    }
}
