use crate::MBox;

impl MBox {
    /// Derivative of a univariate polynumber
    pub fn derivative_uni(&self) -> Option<MBox> {
        if self.depth() != 2 {
            return None;
        }

        let reduce_exp = |m_box: &MBox| -> Option<(MBox, u64)> {
            if m_box.depth() != 1 {
                return None;
            }

            // annihilate zeros with anti-zeros to obtain the reduced form
            let mut ann = m_box.clone().annihilate();
            let (zero, exp) = ann.as_boxes_mut().pop_first()?;

            let new_exp = if zero.is_anti_box() {
                // add one if exponent is negative
                exp.saturating_add(1)
            } else {
                // subtract one if exponent is positive
                exp.saturating_sub(1)
            };

            if new_exp > 0 {
                ann.as_boxes_mut().insert(zero, new_exp);
            } else {
                ann.as_boxes_mut().remove(&zero);
            }

            Some((ann, exp))
        };

        let filtered = self
            .as_boxes()
            .iter()
            .filter_map(|(m_box, mul)| {
                let (reduced, exp) = reduce_exp(m_box)?;
                Some((reduced, mul * exp))
            })
            .collect();

        if self.is_anti_box() {
            Some(MBox::AntiBox(filtered))
        } else {
            Some(MBox::Box(filtered))
        }
    }

    /// Derivative of a multivariate polynumber
    pub fn derivative_multi(&self, var: &MBox) -> Option<MBox> {
        if var.depth() != 3 || self.depth() != 3 {
            return None;
        }

        let _reduce_exp = |m_box: &MBox| -> Option<(MBox, u64)> {
            if m_box.depth() != 2 {
                return None;
            }

            // annihilate boxes with anti-boxes to obtain the reduced form
            let mut ann = m_box.clone().annihilate();
            let (zero, exp) = ann.as_boxes_mut().pop_first()?;

            let new_exp = if zero.is_anti_box() {
                // add one if exponent is negative
                exp.saturating_add(1)
            } else {
                // subtract one if exponent is positive
                exp.saturating_sub(1)
            };

            if new_exp > 0 {
                ann.as_boxes_mut().insert(zero, new_exp);
            } else {
                ann.as_boxes_mut().remove(&zero);
            }

            Some((ann, exp))
        };

        let filtered = self
            .as_boxes()
            .iter()
            .filter(|(key, _value)| !(key.depth() == 3 && key.outer_leaf_counts() == vec![0]))
            .map(|(k, v)| (k.clone(), *v))
            .collect();

        Some(MBox::Box(filtered))
    }

    /// Derivative of a poly/-multinumber
    pub fn derivative(&self, var: &MBox) -> Option<MBox> {
        match (self.depth(), var.depth()) {
            (2, 2) => Self::derivative_uni(self),
            (3, 3) => Self::derivative_multi(self, var),
            _ => panic!("Impossible variable"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::num;

    #[test]
    fn test_der_1() {
        let x = var!(0);
        let prod = var!(0) * var!(0) * var!(0) + var!(0);

        let der = prod.derivative(&x).unwrap().annihilate();
        let exp = num!(3) * var!(0) * var!(0) + num!(1);
        assert_eq!(der, exp);

        let x = num!(3) * var!(0);
        let der = x.derivative_uni().unwrap();
        let exp = num!(3);
        assert_eq!(der, exp);
    }
}
