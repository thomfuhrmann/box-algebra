use std::fmt::Display;

use colored::Colorize;

use crate::MBox;

/// Helper function to display multiplicities as subscripts
fn to_subscript(n: u64) -> String {
    n.to_string()
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

impl Display for MBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ⌊ ... ⌋
        let open = if self.is_anti_box() {
            "⌊".red()
        } else {
            "⌊".black()
        };
        let close = if self.is_anti_box() {
            "⌋".red()
        } else {
            "⌋".black()
        };

        write!(f, "{}", open)?;

        let map = self.as_boxes();
        let mut first = true;
        for (m_box, count) in map.iter() {
            if !first {
                write!(f, " ")?;
            }
            first = false;
            // Recurse if the inner box has content
            if !m_box.is_empty() {
                if f.alternate() {
                    if *count > 1 {
                        write!(f, "{}", to_subscript(*count))?;
                    }

                    if *count > 0 {
                        m_box.fmt(f)?;
                    }
                } else {
                    for i in 0..*count {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        m_box.fmt(f)?;
                    }
                }
            } else {
                // Print the block symbols based on multiplicity
                let symbol = if m_box.is_anti_box() {
                    "□".red()
                } else {
                    // ■ □
                    "□".black()
                };

                if f.alternate() {
                    if *count > 1 {
                        write!(f, "{}", to_subscript(*count))?;
                    }
                    if *count > 0 {
                        write!(f, "{}", symbol)?;
                    }
                } else {
                    for i in 0..*count {
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
    use crate::{MBox, num, var};

    #[test]
    fn test_display() {
        let three = MBox::from(3);
        println!("{three:#}");

        let anti_two = MBox::from(-2);
        println!("{anti_two}");

        let sum = &three + &anti_two;
        println!("{sum}");

        let ann = sum.annihilate();
        println!("{ann}");

        let alpha = var!(0);
        println!("{alpha}");

        let poly = &anti_two + &alpha + &alpha + &alpha * &alpha + num!(1);
        println!("{poly}");
        println!("{poly:#}");

        let poly_ann = poly.annihilate();
        println!("{poly_ann:#}");

        let anti_box = MBox::from(4).to_anti_box();
        println!("{anti_box:#}");
    }
}
