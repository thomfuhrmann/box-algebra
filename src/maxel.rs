//! Maxel is an extension of matrices into the world of boxes
use crate::MBox;

impl MBox {
    /// A pixel is a 2-list of boxes
    pub fn pixel(a: Self, b: Self) -> Self {
        let mut result = Self::new();
        let mut current_sequence = Self::new();
        current_sequence.insert_box(a.clone());
        result.insert_box(current_sequence.clone());
        current_sequence.insert_box(b);
        result.insert_box(current_sequence);
        result
    }

    /// Tests if the box is a pixel
    pub fn is_pixel(&self) -> bool {
        self.as_boxes().len() == 2 && self.is_list()
    }

    /// Tests if the box contains only one element
    pub fn is_singleton(&self) -> bool {
        self.as_boxes().len() == 1
    }

    /// Tests if the box is a vexel which is defined as a box of singletons
    pub fn is_vexel(&self) -> bool {
        self.as_boxes().iter().all(|(b, _)| b.is_singleton())
    }

    /// Tests if the box is a maxel which is defined as a box of pixels
    pub fn is_maxel(&self) -> bool {
        self.as_boxes().iter().all(|(b, _)| b.is_pixel())
    }

    /// Converts to a pair of boxes
    pub fn as_pixel_pair(&self) -> Option<(&MBox, &MBox)> {
        let mut iter = self.as_boxes().keys();
        let first = iter.next()?;
        let first_box = first.as_boxes().keys().next()?;

        let second = iter.next()?;
        let second_box = second
            .as_boxes()
            .iter()
            .find(|&(m_box, &mul)| if m_box == first_box { mul > 1 } else { true })?
            .0;

        Some((first_box, second_box))
    }

    /// If a and b are pixels, computes their pixel product
    pub fn pixel_product(a_box: &MBox, b_box: &MBox) -> Option<Self> {
        let (a_1, a_2) = a_box.as_pixel_pair()?;
        let (b_1, b_2) = b_box.as_pixel_pair()?;

        if a_2 == b_1 {
            Some(Self::pixel(a_1.clone(), b_2.clone()))
        } else {
            None
        }
    }

    /// Computes the product of two maxels
    pub fn maxel_product(a_box: &MBox, b_box: &MBox) -> Option<Self> {
        if !a_box.is_maxel() || !b_box.is_maxel() {
            return None;
        }

        let mut result = Self::new();
        for (a_pix, a_mul) in a_box.as_boxes() {
            for (b_pix, b_mul) in b_box.as_boxes() {
                if let Some(pix) = Self::pixel_product(a_pix, b_pix) {
                    *result.as_boxes_mut().entry(pix).or_insert(0) += a_mul * b_mul;
                }
            }
        }

        Some(result)
    }
}

#[macro_export]
macro_rules! pixel {
    ($x:expr, $y:expr) => {
        $crate::MBox::pixel(($x).into(), ($y).into())
    };
}

#[macro_export]
macro_rules! maxel {
    ($([$x:expr, $y:expr]),* $(,)?) => {
        {
            let mut outer_box = $crate::MBox::new();
            $(
                let pix = $crate::MBox::pixel(($x).into(), ($y).into());
                outer_box.insert_box(pix);
            )*
            outer_box
        }
    };
}

#[cfg(test)]
mod tests {
    use crate::MBox;

    #[test]
    fn test_pixel_product_1() {
        let a = pixel![2, 2];
        let b = pixel![2, 1];
        let c = MBox::pixel_product(&a, &b).unwrap();
        let expected = pixel![2, 1];
        assert_eq!(c, expected);

        let a = pixel![1, 2];
        let b = pixel![3, 4];
        let none = MBox::pixel_product(&a, &b);
        assert!(none.is_none());

        let a = pixel![1, 2];
        let b = pixel![2, 1];
        let c = MBox::pixel_product(&a, &b).unwrap();

        let expected = MBox::pixel(MBox::from(1), MBox::from(1));
        assert_eq!(c, expected);
    }

    #[test]
    fn test_maxel_product_1() {
        let a = maxel![[1, 1], [1, 2], [2, 2]];
        let b = maxel![[1, 2], [2, 1]];
        let prod = MBox::maxel_product(&a, &b);
        let expected = maxel![[1, 1], [1, 2], [2, 1]];
        assert_eq!(prod, Some(expected));
    }
}
