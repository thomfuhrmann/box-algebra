//! Polynumber is an extension of polynomials into the world of boxes
use crate::MBox;

impl MBox {
    /// Constructs the building block of polynumbers
    pub fn alpha() -> Self {
        MBox::from(1).wrap()
    }

    /// Constructs the anti-building block of polynumbers
    pub fn alpha_anti() -> Self {
        MBox::from(1).wrap_anti()
    }
}

#[macro_export]
macro_rules! x {
    () => {
        $crate::MBox::alpha()
    };
}
