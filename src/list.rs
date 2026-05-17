use std::collections::BTreeMap;

use crate::MBox;

impl MBox {
    /// Converts from a list of boxes into a box representation of this list
    pub fn from_list(entries: Vec<MBox>) -> Self {
        let mut result = Self::new();
        let mut current_sequence = Self::new();

        for e in entries {
            current_sequence.insert_box(e);
            result.insert_box(current_sequence.clone());
        }

        result
    }

    /// Converts from a box into its list form (if the box is a list)
    pub fn into_list(self) -> Option<Vec<MBox>> {
        if !self.is_list() {
            return None;
        }
        let mut sequences: Vec<MBox> = self.into_boxes().into_keys().collect();
        sequences.sort_by_key(|m| m.as_boxes().len());

        let mut result = Vec::new();
        let mut previous_map: BTreeMap<MBox, u64> = BTreeMap::new();

        for seq in sequences {
            let current_map = seq.into_boxes();

            for (mbox, &count) in &current_map {
                let prev_count = previous_map.get(mbox).unwrap_or(&0);

                if count > *prev_count {
                    // add to result as many times as the count increased
                    for _ in 0..(count - prev_count) {
                        result.push(mbox.clone());
                    }
                }
            }

            previous_map = current_map;
        }

        Some(result)
    }

    /// Test if this box is a list
    pub fn is_list(&self) -> bool {
        let sequences: Vec<&MBox> = self.as_boxes().keys().collect();

        if sequences.is_empty() {
            return true;
        }

        for i in 0..sequences.len() - 1 {
            let current = sequences[i];
            let next = sequences[i + 1];

            for (curr_box, curr_mul) in current.as_boxes() {
                if let Some(next_mul) = next.as_boxes().get(curr_box) {
                    if next_mul != curr_mul && *next_mul != *curr_mul + 1 {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }

        true
    }

    /// Tests if the box is a box of lists
    pub fn is_list_box(&self) -> bool {
        self.as_boxes().iter().all(|(m_box, _)| m_box.is_list())
    }

    /// Returns the first item that is in self but not in other
    fn diff_one(&self, other: &Self) -> Option<Self> {
        for (item, count) in self.as_boxes() {
            if let Some((_, other_count)) = other.as_boxes().get_key_value(item) {
                if count > other_count {
                    return Some(item.clone());
                }
            } else {
                return Some(item.clone());
            }
        }
        None
    }

    /// Returns the k-th element of a list
    pub fn get_kth(&self, k: usize) -> Option<Self> {
        if !self.is_list() {
            return None;
        }

        let mut sequences: Vec<MBox> = self.clone().into_boxes().into_keys().collect();
        sequences.sort_by_key(|m| m.as_boxes().len());

        if k >= sequences.len() {
            return None;
        }

        if k == 0
            && let Some((m_box, _)) = sequences[0].as_boxes().first_key_value()
        {
            return Some(m_box.clone());
        }

        let pre_box = &sequences[k - 1];
        let curr_box = &sequences[k];

        curr_box.diff_one(pre_box)
    }

    /// Returns the box consisting of all the k-th entries of its contained lists
    pub fn k_proj_list_box(&self, k: usize) -> Self {
        if !self.is_list_box() {
            if self.is_anti_box() {
                return Self::new_anti();
            } else {
                return Self::new();
            }
        }

        self.as_boxes()
            .iter()
            .filter_map(|(m_box, _)| m_box.get_kth(k))
            .collect()
    }
}

#[macro_export]
macro_rules! list {
    ($($x:expr),* $(,)?) => {
        {
            let list = vec![ $( ($x).into() ),* ];
            $crate::MBox::from_list(list)
        }
    };
}

#[cfg(test)]
mod tests {
    use crate::{MBox, m_box};

    #[test]
    fn test_list_1() {
        let m_box_1 = list![1, 2, 3];
        assert_eq!(m_box_1.get_kth(0), Some(MBox::from(1)));
        assert_eq!(m_box_1.get_kth(1), Some(MBox::from(2)));
        assert_eq!(m_box_1.get_kth(2), Some(MBox::from(3)));

        let m_box_2 = list![2, 5, 3];
        assert_eq!(m_box_2.get_kth(1), Some(MBox::from(5)));

        let proj = m_box![m_box_1, m_box_2].k_proj_list_box(1);
        let expected = m_box![2, 5];
        assert_eq!(proj, expected);
    }
}
