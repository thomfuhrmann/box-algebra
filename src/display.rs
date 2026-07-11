use std::fmt::Display;

use colored::Colorize;
use malachite::Natural;

use crate::{BoxKind, BoxType, BoxValue, PixelBox};

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

impl<T: BoxType> Display for BoxValue<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ⎣...⎦
        // ⎡...⎤
        let kind = self.kind();
        let open_bracket = if kind == BoxKind::Vexel || kind == BoxKind::Pixel {
            "⎡"
        } else {
            "⎣"
        };
        let close_bracket = if kind == BoxKind::Vexel || kind == BoxKind::Pixel {
            "⎤"
        } else {
            "⎦"
        };
        let open = if self.is_anti() {
            open_bracket.red()
        } else {
            open_bracket.black()
        };
        let close = if self.is_anti() {
            close_bracket.red()
        } else {
            close_bracket.black()
        };

        write!(f, "{}", open)?;

        let mut first = true;
        for child in self.clone() {
            if !first {
                write!(f, ",")?;
            }
            first = false;

            let len = child.get_length(0);
            let mult = child.get_multiplicity(0);
            if len > 1 {
                if f.alternate() {
                    if mult > 1 {
                        write!(f, "{}", to_subscript(mult))?;
                    }

                    child.fmt(f)?;
                } else if let Ok(count) = usize::try_from(&mult) {
                    for i in 0..count {
                        if i > 0 {
                            write!(f, ",")?;
                        }
                        child.fmt(f)?;
                    }
                }
            } else {
                // ■ □ ⧠
                let symbol = if child.is_anti() {
                    "⧠".red()
                } else {
                    "⧠".black()
                };

                if f.alternate() {
                    if mult > 1 {
                        write!(f, "{}", to_subscript(mult))?;
                    }

                    write!(f, "{}", symbol)?;
                } else if let Ok(count) = usize::try_from(&mult) {
                    for i in 0..count {
                        if i > 0 {
                            write!(f, ",")?;
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

    use crate::BoxValue;

    #[test]
    fn test_display() {
        let three = BoxValue::from(3);
        println!("{three}");
        println!("{three:#}");

        let minus_two = BoxValue::from(-2);
        println!("{minus_two}");
        println!("{minus_two:#}");

        let sum = three + minus_two.clone();
        println!("{sum}");

        let alpha = BoxValue::alpha();
        println!("{alpha}");

        let poly = minus_two + 2_u32 * alpha + BoxValue::alpha() * BoxValue::alpha();
        println!("{poly}");
        println!("{poly:#}");

        let anti_box = BoxValue::from(1).into_anti();
        println!("{anti_box}");
    }
}
