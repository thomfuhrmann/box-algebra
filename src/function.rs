use rapidhash::RapidHashSet;

use crate::{AnyBox, BoxState, BoxStore, MaxelBox, PixelBox, RawBox};

impl BoxStore {
    /// Returns the domain of a function
    pub fn domain<'a>(&'a self, box_state: &'a BoxState<MaxelBox>) -> Vec<RawBox<'a, AnyBox>> {
        let raw = box_state.as_raw(self);
        let mut result = Vec::new();
        for child in raw {
            let x = self.x_raw(child.cast::<PixelBox>());
            result.push(x);
        }
        result
    }

    /// Returns the range of a function
    pub fn range<'a>(&'a self, box_state: &'a BoxState<MaxelBox>) -> Vec<RawBox<'a, AnyBox>> {
        let raw = box_state.as_raw(self);
        let mut result = Vec::new();
        for child in raw {
            let x = self.y_raw(child.cast::<PixelBox>());
            result.push(x);
        }
        result
    }

    /// Tests if the box is a function
    pub fn is_function(&self, box_state: &BoxState<MaxelBox>) -> bool {
        let mut unique_children = RapidHashSet::default();
        let raw = box_state.as_raw(self);
        let mut num_children = 0;
        for child in raw {
            num_children += 1;
            unique_children.insert(child);
        }
        unique_children.len() == num_children
    }

    /// Tests if the function is bijective
    pub fn is_bijective(&self, box_state: &BoxState<MaxelBox>) -> bool {
        self.domain(box_state).len() == self.range(box_state).len()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        maxel, {AnyBox, BoxStore, IntoBoxState},
    };

    #[test]
    fn test_fn_1() {
        let mut store = BoxStore::new();
        let zero = store.zero();
        let one = store.one();
        let two = store.from_u32(2);
        let three = store.from_u32(3);
        let four = store.from_u32(4);
        let five = store.from_u32(5);
        let six = store.from_u32(6);
        let alpha = store.alpha();
        let alpha_2 = store.mul(&alpha, &alpha);
        let poly = store.add(&six, &alpha_2);
        let f_box =
            maxel![&store, [[0, 3], [alpha, 1], [3, 1], [2, poly], [4, 5]]].into_box_state(&store);
        assert!(store.is_function(&f_box));

        let alpha = store.alpha();
        let dom = vec![
            zero.as_raw(&store).cast::<AnyBox>(),
            alpha.as_raw(&store).cast::<AnyBox>(),
            three.as_raw(&store).cast::<AnyBox>(),
            two.as_raw(&store).cast::<AnyBox>(),
            four.as_raw(&store).cast::<AnyBox>(),
        ];
        assert_eq!(store.domain(&f_box), dom);

        let poly = store.add(&six, &alpha_2);
        let range = vec![
            three.as_raw(&store).cast::<AnyBox>(),
            one.as_raw(&store).cast::<AnyBox>(),
            one.as_raw(&store).cast::<AnyBox>(),
            poly.as_raw(&store).cast::<AnyBox>(),
            five.as_raw(&store).cast::<AnyBox>(),
        ];
        assert_eq!(store.range(&f_box), range);

        let g_box = maxel![&store, [[0, 0], [3, 8], [1, 8]]].into_box_state(&store);
        let prod = store.mul_maxel(&f_box, &g_box);
        let prod_comm = prod.commit(&mut store);
        let exp = maxel![&store, [[0, 8], [alpha, 8], [3, 8]]];
        let exp_comm = exp.sort_and_commit(&mut store);
        assert_eq!(prod_comm, exp_comm);
    }
}
