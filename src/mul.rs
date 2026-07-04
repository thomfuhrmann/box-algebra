use std::ops::Mul;

use malachite::{Natural, base::num::arithmetic::traits::SaturatingSub};
use rapidhash::RapidHashMap;

use crate::{AnyBox, BoxMul, BoxType, BoxValue, Color};

impl<L: BoxType + BoxMul<R>, R: BoxType> Mul<BoxValue<R>> for BoxValue<L> {
    type Output = BoxValue<L::Output>;

    /// Multiply two boxes
    fn mul(self, rhs: BoxValue<R>) -> Self::Output {
        let mut result = BoxValue::new();
        let mut unique_children: RapidHashMap<u64, BoxValue<AnyBox>> = RapidHashMap::default();

        let lhs_col = self.get_color(0);
        let rhs_col = rhs.get_color(0);

        for left_child in self {
            for right_child in rhs.clone() {
                let left_mul = left_child.get_multiplicity(0);
                let right_mul = right_child.get_multiplicity(0);
                let mul = left_mul * right_mul;

                let mut box_sum = left_child.clone() + right_child;

                let col = box_sum.get_color(0);
                let struct_hash = box_sum.hash_content(unique_children.hasher());

                if let Some(other) = unique_children.get_mut(&struct_hash)
                    && box_sum.is_eq_content(other)
                {
                    let other_col = other.get_color(0);
                    let other_mul = other.get_multiplicity(0);
                    if col + other_col == Color::Red {
                        if mul < other_mul {
                            other.set_multiplicity(0, other_mul.saturating_sub(mul));
                        } else {
                            other.set_multiplicity(0, mul.saturating_sub(other_mul));
                            other.set_color(0, col);
                        }
                    } else {
                        other.set_multiplicity(0, other_mul + mul);
                    }
                } else {
                    box_sum.set_multiplicity(0, mul);
                    unique_children.insert(struct_hash, box_sum);
                }
            }
        }

        result.colors.push(lhs_col + rhs_col);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1);

        for raw_box in unique_children.into_values() {
            let mul = raw_box.get_multiplicity(0);
            if mul == 0 {
                continue;
            }

            result.extend(raw_box);
        }

        result.sort_immediate_children();
        result
    }
}

impl<L: BoxType + BoxMul<R>, R: BoxType> Mul<&BoxValue<R>> for &BoxValue<L> {
    type Output = BoxValue<L::Output>;

    fn mul(self, rhs: &BoxValue<R>) -> Self::Output {
        self.clone() * rhs.clone()
    }
}

impl<'a, L: BoxType + BoxMul<R>, R: BoxType> Mul<&'a BoxValue<R>> for BoxValue<L> {
    type Output = BoxValue<L::Output>;
    fn mul(self, rhs: &'a BoxValue<R>) -> Self::Output {
        &self * rhs
    }
}

impl<L: BoxType + BoxMul<R>, R: BoxType> Mul<BoxValue<R>> for &BoxValue<L> {
    type Output = BoxValue<L::Output>;
    fn mul(self, rhs: BoxValue<R>) -> Self::Output {
        self * &rhs
    }
}

impl<T: BoxType> Mul<u32> for BoxValue<T> {
    type Output = Self;

    fn mul(self, rhs: u32) -> Self::Output {
        let mut result = BoxValue::<T>::new();
        result.colors.push(self.get_color(0));
        result.multiplicities.push(self.get_multiplicity(0));
        result.lengths.push(self.get_length(0));

        let rhs_nat = Natural::from(rhs);
        for mut child in self {
            let mul = child.get_multiplicity(0) * rhs_nat.clone();
            child.set_multiplicity(0, mul);
            result.extend(child);
        }
        result
    }
}

impl<T: BoxType> Mul<BoxValue<T>> for u32 {
    type Output = BoxValue<T>;

    #[inline]
    fn mul(self, rhs: BoxValue<T>) -> Self::Output {
        rhs * self
    }
}

impl<T: BoxType> Mul<u64> for BoxValue<T> {
    type Output = Self;

    fn mul(self, rhs: u64) -> Self::Output {
        let mut result = BoxValue::<T>::new();
        result.colors.push(self.get_color(0));
        result.multiplicities.push(self.get_multiplicity(0));
        result.lengths.push(self.get_length(0));

        let rhs_nat = Natural::from(rhs);
        for mut child in self {
            let mul = child.get_multiplicity(0) * rhs_nat.clone();
            child.set_multiplicity(0, mul);
            result.extend(child);
        }
        result
    }
}

impl<T: BoxType> Mul<BoxValue<T>> for u64 {
    type Output = BoxValue<T>;

    #[inline]
    fn mul(self, rhs: BoxValue<T>) -> Self::Output {
        rhs * self
    }
}

impl<T: BoxType> Mul<i32> for BoxValue<T> {
    type Output = Self;

    fn mul(self, rhs: i32) -> Self::Output {
        let mut result = BoxValue::<T>::new();
        result.colors.push(self.get_color(0));
        result.multiplicities.push(self.get_multiplicity(0));
        result.lengths.push(self.get_length(0));

        let rhs_nat = Natural::from(rhs.unsigned_abs());
        for mut child in self {
            let mul = child.get_multiplicity(0) * rhs_nat.clone();
            child.set_multiplicity(0, mul);
            child.set_color(0, Color::Red);
            result.extend(child);
        }
        result
    }
}

impl<T: BoxType> Mul<BoxValue<T>> for i32 {
    type Output = BoxValue<T>;

    #[inline]
    fn mul(self, rhs: BoxValue<T>) -> Self::Output {
        rhs * self
    }
}

impl<T: BoxType> Mul<i64> for BoxValue<T> {
    type Output = Self;

    fn mul(self, rhs: i64) -> Self::Output {
        let mut result = BoxValue::<T>::new();
        result.colors.push(self.get_color(0));
        result.multiplicities.push(self.get_multiplicity(0));
        result.lengths.push(self.get_length(0));

        let rhs_nat = Natural::from(rhs.unsigned_abs());
        for mut child in self {
            let mul = child.get_multiplicity(0) * rhs_nat.clone();
            child.set_multiplicity(0, mul);
            child.set_color(0, Color::Red);
            result.extend(child);
        }
        result
    }
}

impl<T: BoxType> Mul<BoxValue<T>> for i64 {
    type Output = BoxValue<T>;

    #[inline]
    fn mul(self, rhs: BoxValue<T>) -> Self::Output {
        rhs * self
    }
}

#[cfg(test)]
mod tests {

    use crate::*;

    #[test]
    fn test_mul() {
        let prod = BoxValue::from(2) * BoxValue::from(3);
        let expected = BoxValue::from(6);
        assert_eq!(prod, expected);

        let expected = BoxValue::from(3) * BoxValue::from(2);
        assert_eq!(prod, expected);

        let prod = BoxValue::from(2) * BoxValue::from(-3);
        let expected = BoxValue::from(-6);
        assert_eq!(prod, expected);

        let prod = BoxValue::alpha() * BoxValue::alpha();
        let expected = BoxValue::from(2).wrap(1_u32);
        assert_eq!(prod, expected);

        let s1 = BoxValue::one() + BoxValue::alpha();
        let minus_alpha = (-1) * BoxValue::alpha();
        let s2 = BoxValue::one() + minus_alpha;
        let prod = s1 * s2;
        let expected = BoxValue::from(1) + (-1) * BoxValue::alpha() * BoxValue::alpha();
        assert_eq!(prod, expected);
    }
}
