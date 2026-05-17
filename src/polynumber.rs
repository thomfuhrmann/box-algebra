//! Polynumber is an extension of polynomials into the world of boxes
use crate::MBox;

impl MBox {
    /// Constructs the building block for polynumbers
    pub fn alpha(n: u64) -> Self {
        MBox::from(n).wrap().wrap()
    }

    /// Constructs the negative of the building block for polynumbers
    pub fn neg_alpha(n: u64) -> Self {
        MBox::from(n).wrap_anti().wrap()
    }

    /// Constructs the anti-building block for polynumbers
    pub fn alpha_anti(n: u64) -> Self {
        MBox::from(n).wrap().wrap_anti()
    }

    /// Constructs the negative anti-building block for polynumbers
    pub fn neg_alpha_anti(n: u64) -> Self {
        MBox::from(n).wrap_anti().wrap_anti()
    }
}

#[macro_export]
macro_rules! var {
    ($num:expr) => {
        $crate::MBox::alpha($num)
    };
}

#[macro_export]
macro_rules! neg_var {
    ($num:expr) => {
        $crate::MBox::neg_alpha($num)
    };
}

#[macro_export]
macro_rules! var_anti {
    ($num:expr) => {
        $crate::MBox::alpha_anti($num)
    };
}

#[macro_export]
macro_rules! neg_var_anti {
    ($num:expr) => {
        $crate::MBox::neg_alpha_anti($num)
    };
}

#[cfg(test)]
mod tests {
    use crate::MBox;

    #[test]
    fn test_var_1() {
        let var = var!(0);
        let expected = MBox::one().wrap();
        println!("{var:#}");
        assert_eq!(var, expected);
    }
}
