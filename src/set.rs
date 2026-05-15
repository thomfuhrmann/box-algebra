use crate::MBox;

impl MBox {
    /// A set is a box (multi-set) with all its elements having multiplicity one
    pub fn is_set(&self) -> bool {
        self.boxes_ref().iter().all(|(_, mul)| *mul == 1)
    }

    /// Creates the supporting set of a box consisting of all its elements but with multiplicity one
    pub fn support(&self) -> Self {
        let inner = self
            .boxes_ref()
            .keys()
            .map(|m_box| (m_box.clone(), 1))
            .collect();
        if self.is_box() {
            Self::from_boxes(inner)
        } else {
            Self::from_boxes_anti(inner)
        }
    }

    /// Set union of two boxes
    pub fn union(a_box: &MBox, b_box: &MBox) -> Self {
        let mut result = a_box.clone();
        let a_map = result.boxes_mut_ref();
        let b_map = b_box.boxes_ref();

        for (key, &b_count) in b_map {
            a_map
                .entry(key.clone())
                .and_modify(|a_count| *a_count = (*a_count).max(b_count))
                .or_insert(b_count);
        }

        result
    }

    /// Set intersection of two boxes
    pub fn intersection(a_box: &MBox, b_box: &MBox) -> Self {
        let a_map = a_box.boxes_ref();
        let b_map = b_box.boxes_ref();

        let mut result = MBox::new();
        for (a_entry, a_count) in a_map {
            if let Some(b_count) = b_map.get(a_entry) {
                result
                    .boxes_mut_ref()
                    .insert(a_entry.clone(), *a_count.min(b_count));
            }
        }

        result
    }
}
