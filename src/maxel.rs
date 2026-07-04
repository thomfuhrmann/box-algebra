//! Maxel is an extension of matrices into the world of boxes

use malachite::Natural;

impl BoxValue<UnixelBox> {
    /// Create a unixel out of a box
    pub fn unixel<T: BoxType>(value: BoxValue<T>) -> Self {
        let len = value.get_length(0);

        let mut result = BoxValue::<UnixelBox>::new();
        result.colors.push(Color::Black);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1 + len);

        result.colors.extend(value.colors);
        result.multiplicities.extend(value.multiplicities);
        result.lengths.extend(value.lengths);

        result
    }

    /// Returns the box inside the unixel
    pub fn x(&self) -> BoxValue<AnyBox> {
        let x_len = self.get_length(1) as usize;
        BoxValue::new_with(
            self.colors[1..1 + x_len].to_vec(),
            self.multiplicities[1..1 + x_len].to_vec(),
            self.lengths[1..1 + x_len].to_vec(),
        )
    }
}

use crate::{AnyBox, BoxType, BoxValue, Color, MaxelBox, PixelBox, UnixelBox, VexelBox};
impl BoxValue<PixelBox> {
    /// Create a pixel out of two boxes
    pub fn pixel<X: BoxType, Y: BoxType>(x: BoxValue<X>, y: BoxValue<Y>) -> Self {
        let x_len = x.get_length(0);
        let y_len = y.get_length(0);

        let mut result = BoxValue::<PixelBox>::new();
        result.colors.push(Color::Black);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1 + x_len + y_len);

        result.colors.extend(x.colors);
        result.multiplicities.extend(x.multiplicities);
        result.lengths.extend(x.lengths);

        result.colors.extend(y.colors);
        result.multiplicities.extend(y.multiplicities);
        result.lengths.extend(y.lengths);

        result
    }

    /// Returns the first child of a pixel
    pub fn x(&self) -> BoxValue<AnyBox> {
        let x_len = self.get_length(1) as usize;
        BoxValue::new_with(
            self.colors[1..1 + x_len].to_vec(),
            self.multiplicities[1..1 + x_len].to_vec(),
            self.lengths[1..1 + x_len].to_vec(),
        )
    }

    /// Returns the second child of a pixel
    pub fn y(&self) -> BoxValue<AnyBox> {
        let x_len = self.get_length(1) as usize;
        let y_idx = 1 + x_len;
        let y_len = self.get_length(y_idx) as usize;
        BoxValue::new_with(
            self.colors[y_idx..y_idx + y_len].to_vec(),
            self.multiplicities[y_idx..y_idx + y_len].to_vec(),
            self.lengths[y_idx..y_idx + y_len].to_vec(),
        )
    }

    /// Multiplies two pixels
    pub fn mul_pix(left: Self, right: Self) -> Option<Self> {
        let left_y = left.y();
        let right_x = right.x();

        if left_y == right_x {
            let left_x = left.x();
            let right_y = right.y();
            return Some(Self::pixel(left_x, right_y));
        }

        None
    }

    /// Multiply a pixel with a unixel
    fn mul_pix_unix(self, unix: BoxValue<UnixelBox>) -> Option<BoxValue<UnixelBox>> {
        let pix_y = self.y();
        let unix_x = unix.x();

        if pix_y == unix_x {
            return Some(BoxValue::<UnixelBox>::unixel(self.x()));
        }

        None
    }
}

impl BoxValue<MaxelBox> {
    /// Multiply two maxels
    pub fn mul_max(left: Self, right: Self) -> Self {
        let mut result = BoxValue::<MaxelBox>::new();
        result.colors.push(Color::Black);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1);
        for left_pix in left {
            for right_pix in right.clone() {
                if let Some(mul) = BoxValue::<PixelBox>::mul_pix(
                    left_pix.clone().cast::<PixelBox>(),
                    right_pix.cast::<PixelBox>(),
                ) {
                    result.extend(mul);
                }
            }
        }
        result.sort_immediate_children();
        result
    }

    /// Multiply a maxel with a vexel
    pub fn mul_max_vex(self, vex: BoxValue<VexelBox>) -> BoxValue<VexelBox> {
        let mut result = BoxValue::<VexelBox>::new();
        result.colors.push(Color::Black);
        result.multiplicities.push(Natural::from(1_u32));
        result.lengths.push(1);
        for left_pix in self {
            for right_unix in vex.clone() {
                if let Some(mul) = left_pix
                    .clone()
                    .cast::<PixelBox>()
                    .mul_pix_unix(right_unix.cast::<UnixelBox>())
                {
                    result.extend(mul);
                }
            }
        }
        result.sort_immediate_children();
        result
    }
}

#[macro_export]
macro_rules! pixel {
    ($x:expr, $y:expr) => {{ $crate::BoxValue::<$crate::maxel::PixelBox>::pixel($x.into(), $y.into()) }};
}

#[macro_export]
macro_rules! vexel {
     ([$($x:expr),* $(,)?]) => {
         {
             let mut result = $crate::BoxValue::<$crate::VexelBox>::new();
             result.colors.push($crate::Color::Black);
             result.multiplicities.push(malachite::Natural::from(1_u32));
             result.lengths.push(1);
             $(
                 let unix = $crate::BoxValue::<$crate::maxel::UnixelBox>::unixel(($x).into());
                 result.extend(unix);
             )*
             result
         }
     };
 }

#[macro_export]
macro_rules! maxel {
     ([$([$x:expr, $y:expr]),* $(,)?]) => {
         {
             let mut result = $crate::BoxValue::<$crate::MaxelBox>::new();
             result.colors.push($crate::Color::Black);
             result.multiplicities.push(malachite::Natural::from(1_u32));
             result.lengths.push(1);
             $(
                 let pix = $crate::BoxValue::<$crate::PixelBox>::pixel(($x).into(), ($y).into());
                 result.extend(pix);
             )*
             result
         }
     };
 }

#[cfg(test)]
mod tests {
    use crate::BoxValue;

    #[test]
    fn test_pixel() {
        let p1 = pixel!(1, 2);
        let p2 = pixel!(2, 3);
        let p3 = BoxValue::mul_pix(p1.clone(), p2);
        let expected = pixel!(1, 3);

        assert_eq!(p3, Some(expected));

        let p4 = pixel!(3, 2);
        let p5 = BoxValue::mul_pix(p1, p4);
        assert!(p5.is_none());
    }

    #[test]
    fn test_maxel() {
        let a = maxel![[[1, 1], [1, 2], [2, 2]]];
        let b = maxel![[[1, 2], [2, 1]]];

        let prod = BoxValue::mul_max(a, b);
        let expected = maxel![[[1, 1], [1, 2], [2, 1]]];
        assert_eq!(prod, expected);

        let m = maxel![[[1, 1], [2, 2], [3, 3]]];
        let v = vexel!([1, 2, 3]);
        let prod = m.mul_max_vex(v);
        let expected = vexel!([1, 2, 3]);
        assert_eq!(prod, expected);
    }
}
