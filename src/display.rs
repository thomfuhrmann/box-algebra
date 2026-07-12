use std::fmt::Display;

use colored::Colorize;
use malachite::Natural;

use crate::{AnyBox, BoxKind, BoxType, BoxValue, BoxVariant};

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
        let kind = self.get_kind(0);

        let open_bracket = if kind == BoxKind::Unixel || kind == BoxKind::Pixel {
            "⎡"
        } else {
            "⎣"
        };

        let close_bracket = if kind == BoxKind::Unixel || kind == BoxKind::Pixel {
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

        let is_num = self.get_length(0) == 2;

        if !is_num {
            write!(f, "{}", open)?;
        }

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
                if child.is_anti() {
                    write!(f, "-")?;
                }
                write!(f, "{}", mult)?;
            }
        }

        if !is_num {
            write!(f, "{}", close)
        } else {
            write!(f, "")
        }
    }
}

impl Display for BoxVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BoxVariant::Any(inner) => {
                if f.alternate() {
                    write!(f, "{:#}", inner)
                } else {
                    write!(f, "{}", inner)
                }
            }
            BoxVariant::Empty(inner) => {
                if f.alternate() {
                    write!(f, "{:#}", inner)
                } else {
                    write!(f, "{}", inner)
                }
            }
            BoxVariant::Num(inner) => {
                if f.alternate() {
                    write!(f, "{:#}", inner)
                } else {
                    write!(f, "{}", inner)
                }
            }
            BoxVariant::Polynum(inner) => {
                if f.alternate() {
                    write!(f, "{:#}", inner)
                } else {
                    write!(f, "{}", inner)
                }
            }
            BoxVariant::Multinum(inner) => {
                if f.alternate() {
                    write!(f, "{:#}", inner)
                } else {
                    write!(f, "{}", inner)
                }
            }
            BoxVariant::Unixel(inner) => {
                if f.alternate() {
                    write!(f, "{:#}", inner)
                } else {
                    write!(f, "{}", inner)
                }
            }
            BoxVariant::Vexel(inner) => {
                if f.alternate() {
                    write!(f, "{:#}", inner)
                } else {
                    write!(f, "{}", inner)
                }
            }
            BoxVariant::Pixel(inner) => {
                if f.alternate() {
                    write!(f, "{:#}", inner)
                } else {
                    write!(f, "{}", inner)
                }
            }
            BoxVariant::Maxel(inner) => {
                if f.alternate() {
                    write!(f, "{:#}", inner)
                } else {
                    write!(f, "{}", inner)
                }
            }
            BoxVariant::Set(inner) => {
                if f.alternate() {
                    write!(f, "{:#}", inner)
                } else {
                    write!(f, "{}", inner)
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct BoxDisplay<T: BoxType>(pub(crate) BoxValue<T>);

impl<T: BoxType> BoxDisplay<T> {
    pub fn new(variant: BoxValue<T>) -> Self {
        Self(variant)
    }
}

impl<'a> From<&'a BoxVariant> for BoxDisplay<AnyBox> {
    fn from(value: &'a BoxVariant) -> Self {
        let raw_any = value.clone().into_any_raw();
        BoxDisplay::new(raw_any)
    }
}

impl<T: BoxType> Display for BoxDisplay<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ⎣...⎦
        // ⎡...⎤
        let kind = self.0.get_kind(0);

        let open_bracket = if kind == BoxKind::Unixel || kind == BoxKind::Pixel {
            "⎡"
        } else {
            "⎣"
        };

        let close_bracket = if kind == BoxKind::Unixel || kind == BoxKind::Pixel {
            "⎤"
        } else {
            "⎦"
        };

        let open = if self.0.is_anti() {
            open_bracket.red()
        } else {
            open_bracket.black()
        };

        let close = if self.0.is_anti() {
            close_bracket.red()
        } else {
            close_bracket.black()
        };

        write!(f, "{}", open)?;

        let mut first = true;
        for child in self.0.clone() {
            if !first {
                write!(f, ",")?;
            }
            first = false;

            let len = child.get_length(0);
            let mult = child.get_multiplicity(0);
            if len > 1 {
                let child = BoxDisplay::new(child);
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

    use crate::{AnyBox, BoxVariant, display::BoxDisplay, maxel, vexel};

    #[test]
    fn test_display() {
        let three = BoxDisplay::<AnyBox>::from(&BoxVariant::from(3));
        println!("{three}");
        println!("{three:#}");

        let three = BoxVariant::from(3);
        println!("{three}");
        println!("{three:#}");

        let minus_two = BoxVariant::from(-2);
        println!("{minus_two}");
        println!("{minus_two:#}");

        let sum = three + minus_two.clone();
        println!("{sum}");
        println!("{sum:#}");

        let alpha = BoxVariant::alpha();
        println!("{alpha}");
        println!("{alpha:#}");

        let poly = minus_two + 2_u32 * alpha + BoxVariant::alpha() * BoxVariant::alpha();
        println!("{poly}");
        println!("{poly:#}");

        let anti_box = BoxVariant::from(1).into_anti();
        println!("{anti_box}");
        println!("{anti_box:#}");

        let a = maxel![[[1, 1], [1, 2], [2, 2], [2, 2]]];
        println!("{a}");
        println!("{a:#}");

        let a = vexel![[1, 2, 3, 3]];
        println!("{a}");
        println!("{a:#}");

        let a = vexel![[1, 2, 3, 3]];
        let a = BoxDisplay::<AnyBox>::from(&a.into());
        println!("{a}");
        println!("{a:#}");
    }
}
