use std::{cmp::Ordering, collections::BTreeMap};

use crate::MBox;

impl PartialOrd for MBox {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// Ordering is from anti-box to box and from lower nesting to higher nesting of boxes
impl Ord for MBox {
    fn cmp(&self, other: &Self) -> Ordering {
        // variant ordering first
        match (self, other) {
            (MBox::AntiBox(_), MBox::Box(_)) => return Ordering::Less,
            (MBox::Box(_), MBox::AntiBox(_)) => return Ordering::Greater,
            _ => {}
        }

        let l = self.as_boxes();
        let r = other.as_boxes();

        // structural equality check
        if l == r {
            return Ordering::Equal;
        }

        // depth comparison
        let l_depth = self.depth();
        let r_depth = other.depth();
        if l_depth != r_depth {
            return l_depth.cmp(&r_depth);
        }

        // net weight comparison
        let get_weight = |map: &BTreeMap<MBox, u64>| -> i128 {
            map.iter().fold(0i128, |acc, (m, &count)| {
                if m.is_anti_box() {
                    acc - count as i128
                } else {
                    acc + count as i128
                }
            })
        };

        let left_w = get_weight(l);
        let right_w = get_weight(r);

        if left_w != right_w {
            return left_w.cmp(&right_w);
        }

        // final tie-breaker: lexicographical comparison
        l.cmp(r)
    }
}

impl PartialEq for MBox {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for MBox {}
