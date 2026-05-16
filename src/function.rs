use crate::MBox;

impl MBox {
    /// Returns the domain of a function
    pub fn domain(&self) -> Self {
        self.k_proj_list_box(0)
    }

    /// Returns the range of a function
    pub fn range(&self) -> Self {
        self.k_proj_list_box(1).support()
    }

    /// Tests if the box is a function
    pub fn is_function(&self) -> bool {
        self.domain().is_set()
    }

    /// Tests if the function is bijective
    pub fn is_bijective(&self) -> bool {
        self.domain().size() == self.range().size()
    }
}

#[cfg(test)]
mod tests {
    use crate::{MBox, m_box, num, var};

    #[test]
    fn test_fn_1() {
        let f_box = maxel![
            [0, 3],
            [var!(), 1],
            [3, 1],
            [2, num!(6) + var!() * var!()],
            [4, 5]
        ];
        assert!(f_box.is_function());

        let dom = m_box![0, var!(), 3, 2, 4];
        assert_eq!(f_box.domain(), dom);

        let range = m_box![3, 1, num!(6) + var!() * var!(), 5];
        assert_eq!(f_box.range(), range);

        let g_box = maxel![[0, 0], [3, 8], [1, 8]];
        let prod = MBox::maxel_product(&f_box, &g_box);

        let expected = maxel![[0, 8], [var!(), 8], [3, 8]];
        assert_eq!(prod, Some(expected));
    }
}
