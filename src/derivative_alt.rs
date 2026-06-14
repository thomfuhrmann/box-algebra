use malachite::{Natural, base::num::arithmetic::traits::SaturatingSub};

use crate::lib_alt::{BoxState, BoxStore, MultinumBox, PolynumBox, RawBox, RawBoxOwned};

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

    /// Derivative of a multivariate polynumber
    pub fn derivative_multi_raw(&self, multi: RawBox<'_, MultinumBox>) -> RawBoxOwned<MultinumBox> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use malachite::Natural;

    use crate::lib_alt::{BoxState, BoxStore, Color, PolynumBox};

    #[test]
    fn test_der_1() {
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
}
