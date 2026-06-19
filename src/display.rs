use std::fmt::Display;

use colored::Colorize;
use malachite::Natural;

use crate::{BoxType, RawBox};

/// Helper function to display multiplicities as subscripts
fn to_subscript(num: Natural) -> String {
    num.to_string()
        .chars()
        .map(|c| match c {
            '0' => '₀',
            '1' => '₁',
            '2' => '₂',
            '3' => '₃',
            '4' => '₄',
            '5' => '₅',
            '6' => '₆',
            '7' => '₇',
            '8' => '₈',
            '9' => '₉',
            _ => c,
        })
        .collect()
}

impl<T: BoxType> Display for RawBox<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ⌊ ... ⌋
        let open = if self.is_anti() {
            "⌊".red()
        } else {
            "⌊".black()
        };
        let close = if self.is_anti() {
            "⌋".red()
        } else {
            "⌋".black()
        };

        write!(f, "{}", open)?;

        let mut first = true;
        for child in self.clone() {
            if !first {
                write!(f, " ")?;
            }
            first = false;

            let len = child.length(0);
            let mult = child.multiplicity(0);
            if len > 1 {
                if f.alternate() {
                    if mult > 1 {
                        write!(f, "{}", to_subscript(mult))?;
                    }

                    child.fmt(f)?;
                } else if let Ok(count) = usize::try_from(&mult) {
                    for i in 0..count {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        child.fmt(f)?;
                    }
                }
            } else {
                // ■ □
                let symbol = if child.is_anti() {
                    "□".red()
                } else {
                    "□".black()
                };

                if f.alternate() {
                    if mult > 1 {
                        write!(f, "{}", to_subscript(mult))?;
                    }

                    write!(f, "{}", symbol)?;
                } else if let Ok(count) = usize::try_from(&mult) {
                    for i in 0..count {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{}", symbol)?;
                    }
                }
            }
        }

        write!(f, "{}", close)
    }
}

#[cfg(test)]
mod tests {
    use malachite::Natural;

    use crate::{BoxStore, Color, NumBox};

    #[test]
    fn test_display() {
        let store = BoxStore::new();
        let three = store.from_u32(3);
        let three_raw = three.as_raw(&store);
        println!("{three_raw:#}");

        let minus_two = store.from_i32(-2);
        let minus_two_raw = minus_two.as_raw(&store);
        println!("{minus_two_raw}");

        let sum = store.add_raw(three_raw, minus_two_raw);
        let sum_raw = sum.as_raw();
        println!("{sum_raw}");

        let alpha = store.alpha();
        let alpha_raw = alpha.as_raw(&store);
        println!("{alpha_raw}");

        let poly_1 = store.add_raw(minus_two_raw, alpha_raw);
        let poly_2 = store.add_raw(poly_1.as_raw(), alpha_raw);
        let alpha_2 = store.mul_raw(alpha_raw, alpha_raw);
        let poly_3 = store.add_raw(poly_2.as_raw(), alpha_2.as_raw());
        let poly_4 = store.add_raw(poly_3.as_raw(), three_raw);
        let poly_4_raw = poly_4.as_raw();
        println!("{poly_4_raw}");
        println!("{poly_4_raw:#}");

        let anti_box = store.wrap_in_box::<NumBox>(&store.zero(), Color::Red, Natural::from(4_u32));
        let anti_box_raw = anti_box.as_raw(&store);
        println!("{anti_box_raw:#}");
    }
}
