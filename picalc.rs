use std::ops::DivAssign;

const DIGITS: usize = 10_000;
type Digit = u64;
type Double = u128;

/*
 * Number represents a positive number between 0 (incl.) and 1 (excl.). It uses fixed precision
 * with DIGITS digits, each of base 2^64. For DIGITS = 10_000, this means 160_000 hexadecimal or
 * 640_000 binary digits. We only implement methods needed for the algorithm, which includes
 * division by a small (64 bit) unsigned integer, addition and subtraction of Numbers and
 * multiplication by 4.
 */
#[derive(Clone)]
struct Number {
    digits: Vec<Digit>,
}

impl Number {
    fn zero() -> Number {
        // Create Number that equals zero.
        Number {
            digits: vec![0; DIGITS],
        }
    }

    fn from_inv(x: Digit) -> Number {
        // Create number as inverse of given digit. Since 1.0 can not be represented, we can not
        // simply use the existing devision method, although the code is quite similar.
        let x = x as Double;
        let mut rem: Double = 1;
        let mut result = Number::zero();
        for i in 0..DIGITS {
            let nom = rem << 64;
            result.digits[i] = (nom / x) as Digit;
            rem = nom % x;
        }
        result
    }

    fn is_zero(&self) -> bool {
        for i in 0..DIGITS {
            if self.digits[i] != 0 {
                return false;
            }
        }
        return true;
    }

    fn mul4(&mut self) {
        // Multiply value by 4
        let mut carry: Double = 0;
        for i in (0..DIGITS).rev() {
            carry += 4*self.digits[i] as Double;
            self.digits[i] = carry as Digit;
            carry >>= 64;
        }
    }

    fn set_to_div(&mut self, x: &Self, d: Digit) -> &Self{
        // self = x / d
        let d = d as Double;
        let mut rem: Double = 0;
        for i in 0..DIGITS {
            let num = (rem << 64) + x.digits[i] as Double;
            self.digits[i] = (num / d) as Digit;
            rem = num % d;
        }
        self
    }

    fn add_assign(&mut self, rhs: &Self) {
        // self += rhs
        // These are not implemented with trait AddAssign because that one expects the rhs to be
        // copied or moved, but we want to borrow it.
        let mut carry: Double = 0;
        for i in (0..DIGITS).rev() {
            let res = carry + self.digits[i] as Double + rhs.digits[i] as Double;
            self.digits[i] = res as Digit;
            carry = res >> 64;
        }
    }

    fn sub_assign(&mut self, rhs: &Self) {
        // self -= rhs
        let mut carry: Double = 0;
        for i in (0..DIGITS).rev() {
            let mut res = carry + self.digits[i] as Double + (!rhs.digits[i]) as Double;
            if i == DIGITS-1 {
                res += 1;
            }
            self.digits[i] = res as Digit;
            carry = res >> 64;
        }
    }

    fn print(&self) {
        // Print Number as hexadecimal
        for i in 0..DIGITS {
            print!("{:016x} ", self.digits[i]);
            if i%4 == 3 {
                println!("")
            }
        }
        println!("")
    }
}

impl DivAssign<Digit> for Number {
    fn div_assign(&mut self, x: Digit) {
        // self /= x
        let x = x as Double;
        let mut rem: Double = 0;
        for i in 0..DIGITS {
            let num = (rem << 64) + self.digits[i] as Double;
            self.digits[i] = (num / x) as Digit;
            rem = num % x;
        }
    }
}

fn ataninv(x: Digit) -> Number {
    // Calculate atan(1/x) using Taylor expansion
    let mut result = Number::from_inv(x);
    let x2 = x*x;
    let mut term = result.clone();
    let mut tmp = Number::zero();
    let mut denom = 1;
    while !term.is_zero() {
        //println!("{}", term.first_nonzero());
        denom += 2;
        term /= x2;
        tmp.set_to_div(&term, denom);
        result.sub_assign(&tmp);

        denom += 2;
        term /= x2;
        tmp.set_to_div(&term, denom);
        result.add_assign(&tmp);
    }
    result
}

fn main() {
    // Calculate pi using pi/4 = 4atan(1/5)-atan(1/239)
    let mut pi = ataninv(5);
    pi.mul4();
    pi.sub_assign(&ataninv(239));
    // Note that this takes the number outside the representable range by creating a value larger
    // than one, which overflows and drops the integer part, but that one is known to be 3.
    pi.mul4();
    pi.print();
}
