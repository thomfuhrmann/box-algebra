use malachite::{Natural, base::num::arithmetic::traits::SaturatingSub};

use crate::lib_alt::{BoxStore, MultinumBox, PolynumBox, RawBox, RawBoxOwned};

impl BoxStore {
    /// Derivative of a univariate polynumber
    pub fn derivative_uni_raw(&self, poly: RawBox<'_, PolynumBox>) -> RawBoxOwned<PolynumBox> {
        let mut result = RawBoxOwned::<PolynumBox>::new();
        result.colors.push(poly.color(0));
        result.multiplicities.push(poly.multiplicity(0));
        result.lengths.push(1);
        for child in poly {
            let len = child.length(0);
            if len == 2 {
                let mut child_owned = RawBoxOwned::from(child);
                let coeff = child_owned.multiplicity(0);
                let exp = child_owned.multiplicity(1);
                child_owned.set_multiplicity(coeff * exp.clone(), 0);
                let new_exp = exp.saturating_sub(Natural::from(1_u32));
                if new_exp == 0 {
                    child_owned.remove(1);
                    child_owned.set_length(1, 0);
                } else {
                    child_owned.set_multiplicity(new_exp, 1);
                }
                result.append_owned(child_owned);
            }
        }
        result
    }

    /// Derivative of a multinumber
    pub fn derivative_multi_raw(
        &self,
        multi: RawBox<'_, MultinumBox>,
        index: Natural,
    ) -> RawBoxOwned<MultinumBox> {
        let mut result = RawBoxOwned::<MultinumBox>::new();
        result.colors.push(multi.color(0));
        result.multiplicities.push(multi.multiplicity(0));
        result.lengths.push(1);
        for child in multi {
            let len = child.length(0);
            if len == 3 {
                let var_index = child.multiplicity(2);

                if var_index == index {
                    let mut child_owned = RawBoxOwned::from(child);
                    let coeff = child_owned.multiplicity(0);
                    let exp = child_owned.multiplicity(1);
                    child_owned.set_multiplicity(coeff * exp.clone(), 0);
                    let new_exp = exp.saturating_sub(Natural::from(1_u32));
                    if new_exp == 0 {
                        child_owned.remove(2);
                        child_owned.remove(1);
                        child_owned.set_length(1, 0);
                    } else {
                        child_owned.set_multiplicity(new_exp, 1);
                    }
                    result.append_owned(child_owned);
                }
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use malachite::Natural;

    use crate::lib_alt::{BoxState, BoxStore, Color, MultinumBox, PolynumBox};

    #[test]
    fn test_der_uni() {
        let store = BoxStore::new();
        let one = store.one();
        let three = store.from_u32(3);
        let alpha = store.alpha();
        let alpha_2 = store.mul(&alpha, &alpha);
        let alpha_3 = store.wrap_in_box::<PolynumBox>(&three, Color::Black, Natural::from(1_u32));
        let poly = store.add(&alpha, &alpha_3);

        let der = BoxState::Uncommitted(store.derivative_uni_raw(poly.as_raw(&store)));
        let exp = store.add(&store.mul(&three, &alpha_2), &one);
        assert_eq!(der, exp);

        let poly = store.mul(&three, &alpha);
        let der = store.derivative_uni_raw(poly.as_raw(&store));
        let exp = three.as_raw(&store).cast::<PolynumBox>();
        assert_eq!(der.as_raw(), exp);
    }

    #[test]
    fn test_der_multi() {
        let store = BoxStore::new();
        let zero = store.zero();
        let one = store.one();
        let three = store.from_u32(3);
        let six = store.from_u32(6);
        let alpha = store.alpha();
        let alpha_2 = store.mul(&alpha, &alpha);
        let beta_2 = store.wrap_in_box::<MultinumBox>(&alpha_2, Color::Black, Natural::from(1_u32));
        let three_times_beta_2_exp_2 = store.mul(&three, &store.mul(&beta_2, &beta_2));

        let der = store.derivative_multi_raw(beta_2.as_raw(&store), Natural::from(2_u32));
        let exp = one.as_raw(&store).cast::<MultinumBox>();
        assert_eq!(der.as_raw(), exp);

        let der = store.derivative_multi_raw(beta_2.as_raw(&store), Natural::from(1_u32));
        let exp = zero.as_raw(&store).cast::<MultinumBox>();
        assert_eq!(der.as_raw(), exp);

        let der = store.derivative_multi_raw(
            three_times_beta_2_exp_2.as_raw(&store),
            Natural::from(2_u32),
        );
        let exp = store.mul(&six, &beta_2);
        assert_eq!(der.as_raw(), exp.as_raw(&store));
    }
}
