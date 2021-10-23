pub fn is_letter(c: char) -> bool {
    let ch: u8 = c as u8;
    (ch >= b'a' && ch <= b'z') || (ch >= b'A' && ch <= b'Z') || ch == b'_'
}

pub fn is_digit(c: char) -> bool {
    let i = c as u8;
    i >= b'0' && i <= b'9'
}