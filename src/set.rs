use malachite::Natural;
use rapidhash::{HashMapExt, RapidHashMap};

use crate::{AnyBox, BoxState, BoxStore, RawBoxOwned, SetBox};

impl BoxStore {
    /// A set is a box with all its elements having multiplicity one
    pub fn is_set(&self, state: BoxState<AnyBox>) -> bool {
        let raw_box = state.as_raw(self);
        for child in raw_box {
            let mult = child.multiplicity(0);
            if mult != 1 {
                return false;
            }
        }
        true
    }

    /// Creates the supporting set of a box consisting of all its elements but with multiplicity one
    pub fn support(&self, state: BoxState<AnyBox>) -> BoxState<SetBox> {
        let raw_box = state.as_raw(self);
        let mut result = RawBoxOwned::<SetBox>::new();
        for child in raw_box {
            let mut child_owned = RawBoxOwned::from(child);
            child_owned.set_multiplicity(Natural::from(1_u32), 0);
            result.extend(child_owned);
        }
        BoxState::Uncommitted(result)
    }

    /// Set union of two boxes
    pub fn union(&self, left: &BoxState<AnyBox>, right: &BoxState<AnyBox>) -> BoxState<AnyBox> {
        let mut unique_children: RapidHashMap<u64, RawBoxOwned<AnyBox>> = RapidHashMap::new();
        let left_raw = left.as_raw(self);
        let right_raw = right.as_raw(self);
        for left_child in left_raw {
            let hash = left_child.hash_content(&self.random_state);
            if let Some(other) = unique_children.get_mut(&hash)
                && left_raw.cmp_content(other.as_raw())
                && left_child.color(0) == other.color(0)
            {
                let left_mult = left_child.multiplicity(0);
                let other_mult = other.multiplicity(0);
                other.set_multiplicity(left_mult.max(other_mult), 0);
            } else {
                unique_children.insert(hash, RawBoxOwned::from(left_child));
            }
        }
        for right_child in right_raw {
            let hash = right_child.hash_content(&self.random_state);
            if let Some(other) = unique_children.get_mut(&hash)
                && left_raw.cmp_content(other.as_raw())
                && right_child.color(0) == other.color(0)
            {
                let left_mult = right_child.multiplicity(0);
                let other_mult = other.multiplicity(0);
                other.set_multiplicity(left_mult.max(other_mult), 0);
            } else {
                unique_children.insert(hash, RawBoxOwned::from(right_child));
            }
        }

        let mut result = RawBoxOwned::<AnyBox>::new();
        let color = left_raw.color(0) + right_raw.color(0);
        result.colors.push(color);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1);
        for (_, child) in unique_children.into_iter() {
            result.extend(child);
        }
        BoxState::Uncommitted(result)
    }

    /// Set intersection of two boxes
    pub fn intersection(
        &self,
        left: &BoxState<AnyBox>,
        right: &BoxState<AnyBox>,
    ) -> BoxState<AnyBox> {
        let mut left_unique: RapidHashMap<u64, RawBoxOwned<AnyBox>> = RapidHashMap::new();
        let left_raw = left.as_raw(self);
        for left_child in left_raw {
            let hash = left_child.hash_content(&self.random_state);
            left_unique.insert(hash, RawBoxOwned::from(left_child));
        }

        let mut right_unique: RapidHashMap<u64, RawBoxOwned<AnyBox>> = RapidHashMap::new();
        let right_raw = right.as_raw(self);
        for right_child in right_raw {
            let hash = right_child.hash_content(&self.random_state);
            right_unique.insert(hash, RawBoxOwned::from(right_child));
        }

        let mut result = RawBoxOwned::<AnyBox>::new();
        let color = left_raw.color(0) + right_raw.color(0);
        result.colors.push(color);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1);

        for (left_hash, mut left_child) in left_unique.into_iter() {
            if let Some(right_child) = right_unique.get_mut(&left_hash)
                && right_child.cmp_content(&left_child)
                && right_child.color(0) == left_child.color(0)
            {
                let right_mult = right_child.multiplicity(0);
                let left_mult = left_child.multiplicity(0);
                left_child.set_multiplicity(left_mult.min(right_mult), 0);
                result.extend(left_child);
            }
        }
        BoxState::Uncommitted(result)
    }
}

#[cfg(test)]
mod tests {

    use crate::{AnyBox, BoxState, BoxStore, RawBoxOwned};

    #[test]
    fn test_set_ops() {
        let mut store = BoxStore::new();
        let one = store.one();
        let one_raw = one.as_raw(&store);
        let two = store.from_u32(2);
        let two_raw = two.as_raw(&store);
        let three = store.from_u32(3);
        let three_raw = three.as_raw(&store);
        let four = store.from_u32(4);
        let four_raw = four.as_raw(&store);
        let mut m_owned = RawBoxOwned::empty();
        m_owned.extend_with_mult(one_raw, 4_u32);
        m_owned.extend_with_mult(two_raw, 2_u32);
        m_owned.extend_with_mult(three_raw, 1_u32);
        let mut n_owned = RawBoxOwned::empty();
        n_owned.extend_with_mult(one_raw, 7_u32);
        n_owned.extend_with_mult(three_raw, 3_u32);
        n_owned.extend(RawBoxOwned::from(four_raw));
        let m = BoxState::Uncommitted(m_owned);
        let n = BoxState::Uncommitted(n_owned);
        let union = store.union(&m, &n);
        let mut exp = RawBoxOwned::<AnyBox>::empty();
        exp.extend_with_mult(one_raw, 7_u32);
        exp.extend_with_mult(two_raw, 2_u32);
        exp.extend_with_mult(three_raw, 3_u32);
        exp.extend_with_mult(four_raw, 1_u32);
        exp.sort_immediate_children();
        let exp_comm = store.commit(exp);
        let union_comm = union.sort_and_commit(&mut store);
        assert_eq!(union_comm, exp_comm);

        let one_raw = one.as_raw(&store);
        let three_raw = three.as_raw(&store);
        let int = store.intersection(&m, &n);
        let mut exp = RawBoxOwned::<AnyBox>::empty();
        exp.extend_with_mult(one_raw, 4_u32);
        exp.extend_with_mult(three_raw, 1_u32);
        exp.sort_immediate_children();
        let exp_comm = store.commit(exp);
        let int_comm = int.sort_and_commit(&mut store);
        assert_eq!(int_comm.as_raw(&store), exp_comm.as_raw(&store));
    }
}
