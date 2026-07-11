use std::ops::Add;

use malachite::{Natural, base::num::arithmetic::traits::SaturatingSub};
use rapidhash::RapidHashMap;

use crate::{AnyBox, BoxAdd, BoxType, BoxValue, Color};

impl<T: BoxType> BoxValue<T> {
    fn add_child_boxes(self, unique_children: &mut RapidHashMap<u64, BoxValue<AnyBox>>) {
        for child in self {
            let child_col = child.get_color(0);
            let child_mul = child.get_multiplicity(0);

            let hash = child.hash_content(unique_children.hasher());
            if let Some(other) = unique_children.get_mut(&hash)
                && child.is_eq_content(other)
            {
                let other_col = other.get_color(0);
                let other_mul = other.get_multiplicity(0);
                if child_col + other_col == Color::Red {
                    if child_mul < other_mul {
                        other.set_multiplicity(0, other_mul.saturating_sub(child_mul));
                    } else {
                        other.set_multiplicity(0, child_mul.saturating_sub(other_mul));
                        other.set_color(0, child_col);
                    }
                } else {
                    other.set_multiplicity(0, other_mul + child_mul);
                }
            } else {
                unique_children.insert(hash, child);
            }
        }
    }
}

impl<L: BoxType + BoxAdd<R>, R: BoxType> Add<BoxValue<R>> for BoxValue<L> {
    type Output = BoxValue<L::Output>;

    fn add(self, rhs: BoxValue<R>) -> Self::Output {
        let lhs_col = self.get_color(0);
        let rhs_col = rhs.get_color(0);

        let mut result = BoxValue::<L::Output>::new();
        result.colors.push(lhs_col + rhs_col);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1);

        let mut unique_children: RapidHashMap<u64, BoxValue<AnyBox>> = RapidHashMap::default();
        self.add_child_boxes(&mut unique_children);
        rhs.add_child_boxes(&mut unique_children);

        for child in unique_children.into_values() {
            let mult = child.get_multiplicity(0);
            if mult == 0 {
                continue;
            }

            result.extend(child);
        }

        result.sort_immediate_children();
        result
    }
}

impl<L: BoxType + BoxAdd<R>, R: BoxType> Add<&BoxValue<R>> for &BoxValue<L> {
    type Output = BoxValue<L::Output>;

    fn add(self, rhs: &BoxValue<R>) -> Self::Output {
        self.clone() + rhs.clone()
    }
}

impl<'a, L: BoxType + BoxAdd<R>, R: BoxType> Add<&'a BoxValue<R>> for BoxValue<L> {
    type Output = BoxValue<L::Output>;
    fn add(self, rhs: &'a BoxValue<R>) -> Self::Output {
        &self + rhs
    }
}

impl<L: BoxType + BoxAdd<R>, R: BoxType> Add<BoxValue<R>> for &BoxValue<L> {
    type Output = BoxValue<L::Output>;
    fn add(self, rhs: BoxValue<R>) -> Self::Output {
        self + &rhs
    }
}

#[cfg(test)]
mod tests {

    use crate::BoxValue;

    #[test]
    fn test_add() {
        let left = BoxValue::from(3);
        let right = BoxValue::from(5);
        let sum = left + right;
        let exp = BoxValue::from(8);
        assert_eq!(sum, exp);

        let left = BoxValue::from(-3);
        let right = BoxValue::from(5);
        let sum = left + right;
        let exp = BoxValue::from(2);
        assert_eq!(sum, exp);
    }
}
