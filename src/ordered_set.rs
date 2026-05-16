use crate::MBox;

impl MBox {
    /// Tests if this box is an ordered set (a list in which every entry occurs once)
    pub fn is_ordered_set(&self) -> bool {
        self.is_list() && self.is_set()
    }
}
