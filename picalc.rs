use std::ops::DivAssign;

const DIGITS: usize = 10000;

#[derive(Clone)]
struct Number {
    digits: Vec<u64>,
}

impl Number {
    fn zero() -> Number {
        Number {
            digits: vec![0; DIGITS],
        }
    }

    fn from_inv(x: u128) -> Number {
        let mut rem: u128 = 1;
        let mut result = Number::zero();
        for i in 0..DIGITS {
            let nom = rem << 64;
            result.digits[i] = (nom / x) as u64;
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
        let mut carry: u128 = 0;
        for i in (0..DIGITS).rev() {
            carry += 4*self.digits[i] as u128;
            self.digits[i] = carry as u64;
            carry >>= 64;
        }
    }

    fn div(&mut self, x: &Self, d: u128) -> &Self{
        // self = x / d
        let mut rem: u128 = 0;
        for i in 0..DIGITS {
            let num = (rem << 64) + x.digits[i] as u128;
            self.digits[i] = (num / d) as u64;
            rem = num % d;
        }
        self
    }

    fn add_assign(&mut self, rhs: &Self) {
        let mut carry: u128 = 0;
        for i in (0..DIGITS).rev() {
            let res = carry + self.digits[i] as u128 + rhs.digits[i] as u128;
            self.digits[i] = res as u64;
            carry = res >> 64;
        }
    }

    fn sub_assign(&mut self, rhs: &Self) {
        let mut carry: u128 = 0;
        for i in (0..DIGITS).rev() {
            let mut res = carry + self.digits[i] as u128 + (!rhs.digits[i]) as u128;
            if i == DIGITS-1 {
                res += 1;
            }
            self.digits[i] = res as u64;
            carry = res >> 64;
        }
    }

    fn print(&self) {
        for i in 0..DIGITS {
            print!("{:016x} ", self.digits[i]);
            if i%4 == 3 {
                println!("")
            }
        }
        println!("")
    }

    //fn first_nonzero(&self) -> usize {
    //    for i in 0..DIGITS {
    //        if self.digits[i] != 0 {
    //            return i;
    //        }
    //    }
    //    DIGITS
    //}
}

impl DivAssign<u128> for Number {
    fn div_assign(&mut self, x: u128) {
        let mut rem: u128 = 0;
        for i in 0..DIGITS {
            let num = (rem << 64) + self.digits[i] as u128;
            self.digits[i] = (num / x) as u64;
            rem = num % x;
        }
    }
}

fn ataninv(x: u128) -> Number {
    // Calculate atan(1/x)
    let mut result = Number::from_inv(x);
    let x2 = x*x;
    let mut term = result.clone();
    let mut tmp = Number::zero();
    let mut denom = 1;
    while !term.is_zero() {
        //println!("{}", term.first_nonzero());
        denom += 2;
        term /= x2;
        tmp.div(&term, denom);
        result.sub_assign(&tmp);

        denom += 2;
        term /= x2;
        tmp.div(&term, denom);
        result.add_assign(&tmp);
    }
    result
}

fn main() {
    let mut pi = ataninv(5);
    pi.mul4();
    pi.sub_assign(&ataninv(239));
    pi.mul4();
    pi.print();
}
