//! Maxel is an extension of matrices into the world of boxes

use malachite::Natural;

use crate::{
    AnyBox, BoxState, BoxStore, BoxType, Color, MaxelBox, PixelBox, RawBox, RawBoxOwned, UnixelBox,
    VexelBox,
};
impl BoxStore {
    /// Returns a raw pixel
    pub fn pixel_raw<T: BoxType, U: BoxType>(
        &self,
        x_raw: RawBox<'_, T>,
        y_raw: RawBox<'_, U>,
    ) -> RawBoxOwned<PixelBox> {
        let x_len = x_raw.length(0);
        let y_len = y_raw.length(0);

        let mut result = RawBoxOwned::<PixelBox>::new();
        result.colors.push(Color::Black);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1 + x_len + y_len);

        result.colors.extend_from_slice(x_raw.colors);
        result
            .multiplicities
            .extend_from_slice(x_raw.multiplicities);
        result.lengths.extend_from_slice(x_raw.lengths);

        result.colors.extend_from_slice(y_raw.colors);
        result
            .multiplicities
            .extend_from_slice(y_raw.multiplicities);
        result.lengths.extend_from_slice(y_raw.lengths);

        result
    }

    /// Returns a pixel
    pub fn pixel<T: BoxType, U: BoxType>(
        &self,
        x: BoxState<T>,
        y: BoxState<U>,
    ) -> BoxState<PixelBox> {
        let x_raw = x.as_raw(self);
        let y_raw = y.as_raw(self);
        BoxState::Uncommitted(self.pixel_raw(x_raw, y_raw))
    }

    /// Returns the first child of a box
    pub fn x_raw<'a>(&'a self, raw: RawBox<'a, PixelBox>) -> RawBox<'a, AnyBox> {
        let x_len = raw.length(1) as usize;
        RawBox::new(
            &raw.colors[1..1 + x_len],
            &raw.multiplicities[1..1 + x_len],
            &raw.lengths[1..1 + x_len],
        )
    }

    /// Returns the second child of a box
    pub fn y_raw<'a>(&'a self, raw: RawBox<'a, PixelBox>) -> RawBox<'a, AnyBox> {
        let x_len = raw.length(1) as usize;
        let y_idx = 1 + x_len;
        let y_len = raw.length(y_idx) as usize;
        RawBox::new(
            &raw.colors[y_idx..y_idx + y_len],
            &raw.multiplicities[y_idx..y_idx + y_len],
            &raw.lengths[y_idx..y_idx + y_len],
        )
    }

    /// Multiplies two pixels
    pub fn mul_pixel_raw(
        &self,
        left: RawBox<PixelBox>,
        right: RawBox<PixelBox>,
    ) -> Option<RawBoxOwned<PixelBox>> {
        let left_y = self.y_raw(left);
        let right_x = self.x_raw(right);

        if left_y == right_x {
            let left_x = self.x_raw(left);
            let right_y = self.y_raw(right);
            return Some(self.pixel_raw(left_x, right_y));
        }

        None
    }

    pub fn mul_pixel(
        &self,
        left: &BoxState<PixelBox>,
        right: &BoxState<PixelBox>,
    ) -> Option<BoxState<PixelBox>> {
        let left_raw = left.as_raw(self);
        let right_raw = right.as_raw(self);
        let result = self.mul_pixel_raw(left_raw, right_raw);
        result.map(BoxState::Uncommitted)
    }

    pub fn mul_maxel_raw(
        &self,
        left: RawBox<MaxelBox>,
        right: RawBox<MaxelBox>,
    ) -> RawBoxOwned<MaxelBox> {
        let mut result = RawBoxOwned::<MaxelBox>::new();
        result.colors.push(Color::Black);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1);
        for left_pix in left {
            for right_pix in right {
                if let Some(mul) =
                    self.mul_pixel_raw(left_pix.cast::<PixelBox>(), right_pix.cast::<PixelBox>())
                {
                    result.extend(mul);
                }
            }
        }
        result.sort_immediate_children();
        result
    }

    pub fn mul_maxel(
        &self,
        left: &BoxState<MaxelBox>,
        right: &BoxState<MaxelBox>,
    ) -> BoxState<MaxelBox> {
        let left_raw = left.as_raw(self);
        let right_raw = right.as_raw(self);
        let result = self.mul_maxel_raw(left_raw, right_raw);
        BoxState::Uncommitted(result)
    }

    pub fn unix_raw<T: BoxType>(&self, x_raw: RawBox<T>) -> RawBoxOwned<UnixelBox> {
        let x_len = x_raw.length(0);

        let mut result = RawBoxOwned::<UnixelBox>::new();
        result.colors.push(Color::Black);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1 + x_len);

        result.colors.extend_from_slice(x_raw.colors);
        result
            .multiplicities
            .extend_from_slice(x_raw.multiplicities);
        result.lengths.extend_from_slice(x_raw.lengths);

        result
    }

    pub fn unixel<T: BoxType>(&self, x: BoxState<T>) -> BoxState<UnixelBox> {
        BoxState::Uncommitted(self.unix_raw(x.as_raw(self)))
    }

    fn mul_pix_unix(
        &self,
        pix: RawBox<'_, PixelBox>,
        unix: RawBox<'_, UnixelBox>,
    ) -> Option<RawBoxOwned<UnixelBox>> {
        let left_y = self.y_raw(pix);
        let right_x = self.x_raw(unix.cast::<PixelBox>());

        if left_y == right_x {
            return Some(self.unix_raw(self.x_raw(pix)));
        }

        None
    }

    fn mul_max_vex_raw(
        &self,
        max_raw: RawBox<MaxelBox>,
        vex_raw: RawBox<VexelBox>,
    ) -> RawBoxOwned<VexelBox> {
        let mut result = RawBoxOwned::<VexelBox>::new();
        result.colors.push(Color::Black);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1);
        for left_pix in max_raw {
            for right_unix in vex_raw {
                if let Some(mul) =
                    self.mul_pix_unix(left_pix.cast::<PixelBox>(), right_unix.cast::<UnixelBox>())
                {
                    result.extend(mul);
                }
            }
        }
        result.sort_immediate_children();
        result
    }

    pub fn mul_max_vex(
        &self,
        max: &BoxState<MaxelBox>,
        vex: &BoxState<VexelBox>,
    ) -> BoxState<VexelBox> {
        let max_raw = max.as_raw(self);
        let vex_raw = vex.as_raw(self);
        let result = self.mul_max_vex_raw(max_raw, vex_raw);
        BoxState::Uncommitted(result)
    }
}
