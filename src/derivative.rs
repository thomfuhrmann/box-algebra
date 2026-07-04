use malachite::{Natural, base::num::arithmetic::traits::SaturatingSub};

use crate::{BoxValue, MultinumBox, PolynumBox};

impl BoxValue<PolynumBox> {
    /// Derivative of a polynumber
    pub fn derivative(self) -> Self {
        let mut result = BoxValue::<PolynumBox>::new();
        result.colors.push(self.get_color(0));
        result.multiplicities.push(self.get_multiplicity(0));
        result.lengths.push(1);
        for mut child in self {
            let len = child.get_length(0);
            if len == 2 {
                let coeff = child.get_multiplicity(0);
                let exp = child.get_multiplicity(1);
                child.set_multiplicity(0, coeff * exp.clone());
                let new_exp = exp.saturating_sub(Natural::from(1_u32));
                if new_exp == 0 {
                    child.remove(1);
                    child.set_length(0, 1);
                } else {
                    child.set_multiplicity(1, new_exp);
                }
                result.extend(child);
            }
        }
        result
    }
}

impl BoxValue<MultinumBox> {
    /// Derivative of a multinumber
    pub fn derivative(self, index: impl Into<Natural>) -> Self {
        let index = index.into();
        let mut result = BoxValue::<MultinumBox>::new();
        result.colors.push(self.get_color(0));
        result.multiplicities.push(self.get_multiplicity(0));
        result.lengths.push(1);
        for mut child in self {
            let len = child.get_length(0);
            if len == 3 {
                let var_index = child.get_multiplicity(2);

                if var_index == index {
                    let coeff = child.get_multiplicity(0);
                    let exp = child.get_multiplicity(1);
                    child.set_multiplicity(0, coeff * exp.clone());
                    let new_exp = exp.saturating_sub(Natural::from(1_u32));
                    if new_exp == 0 {
                        child.remove(2);
                        child.remove(1);
                        child.set_length(0, 1);
                    } else {
                        child.set_multiplicity(1, new_exp);
                    }
                    result.extend(child);
                }
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {

    use crate::{BoxValue, PolynumBox};

    #[test]
    fn test_der_uni() {
        let poly = BoxValue::alpha() + 3_u32 * BoxValue::alpha() * BoxValue::alpha();
        let der = poly.derivative();

        let exp = BoxValue::from(1) + 6_u32 * BoxValue::alpha();
        assert_eq!(der, exp);

        let poly = 3_u32 * BoxValue::alpha();
        let der = poly.derivative();
        let exp = BoxValue::from(3).cast::<PolynumBox>();
        assert_eq!(der, exp);
    }

    #[test]
    fn test_der_multi() {
        // let store = BoxStore::new();
        // let zero = store.zero();
        // let one = store.one();
        // let three = store.from_u32(3);
        // let six = store.from_u32(6);
        // let alpha = store.alpha();
        // let alpha_2 = store.mul(&alpha, &alpha);
        // let beta_2 = store.wrap_in_box::<MultinumBox>(&alpha_2, Color::Black, 1_u32);
        // let three_times_beta_2_exp_2 = store.mul(&three, &store.mul(&beta_2, &beta_2));

        // let der = store.derivative_multi_raw(beta_2.as_raw(&store), 2_u32);
        // let exp = one.as_raw(&store).cast::<MultinumBox>();
        // assert_eq!(der.as_raw(), exp);

        // let der = store.derivative_multi_raw(beta_2.as_raw(&store), 1_u32);
        // let exp = zero.as_raw(&store).cast::<MultinumBox>();
        // assert_eq!(der.as_raw(), exp);

        // let der = store.derivative_multi_raw(three_times_beta_2_exp_2.as_raw(&store), 2_u32);
        // let exp = store.mul(&six, &beta_2);
        // assert_eq!(der.as_raw(), exp.as_raw(&store));
    }
}
