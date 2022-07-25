use std::ops::DivAssign;
use std::cmp::{min,max};
use std::thread;
use std::vec::Vec;
use crossbeam::{channel::{unbounded,Receiver,Sender}};
use std::time::Instant;

const DIGITS: usize = 20_000;
type Digit = u64;
type Double = u128;

const MAXTHREADS: usize = 7;

/*
 * Number represents a number between -0.5 (incl.) and 0.5 (excl.). It uses fixed precision
 * with DIGITS digits, each of base 2^64. For DIGITS = 10_000, this means 160_000 hexadecimal or
 * 640_000 binary digits. We only implement methods needed for the algorithm, which includes
 * a) addition and subtraction and
 * b) multiplication by 4 and division by a small (u64) number (only for positiv Numbers.
 */
#[derive(Clone)]
struct Number {
    zeros: usize, // At least the first N digits are zeros
    digits: Vec<Digit>,
}

impl Number {
    fn zero() -> Number {
        // Create Number that equals zero.
        Number {
            digits: vec![0; DIGITS],
            zeros: DIGITS,
        }
    }

    fn from_inv(x: Digit) -> Number {
        // Create number as inverse of given digit. Since 1.0 can not be represented, we can not
        // simply use the existing division method, although the code is quite similar.
        let x = x as Double;
        let mut rem: Double = 1;
        let mut result = Number::zero();
        for i in 0..DIGITS {
            let nom = rem << 64;
            result.digits[i] = (nom / x) as Digit;
            rem = nom % x;
        }
        result.update_zeros();
        result
    }

    fn set(&mut self, rhs: &Number) {
        for i in 0..DIGITS {
            self.digits[i] = rhs.digits[i];
        }
        self.zeros = rhs.zeros;
    }

    #[allow(dead_code)]
    fn set_zero(&mut self) {
        for i in 0..DIGITS {
            self.digits[i] = 0;
        }
        self.zeros = DIGITS;
    }

    fn update_zeros_min(&mut self, min: usize) {
        self.zeros = DIGITS;
        for i in min..DIGITS {
            if self.digits[i] != 0 {
                self.zeros = i;
                break;
            }
        }
    }

    fn update_zeros(&mut self) {
        self.update_zeros_min(0);
    }

    fn is_zero(&self) -> bool {
        self.zeros == DIGITS
    }

    fn mul4(&mut self) {
        // Multiply value by 4
        let mut carry: Double = 0;
        for i in (0..DIGITS).rev() {
            carry += 4*self.digits[i] as Double;
            self.digits[i] = carry as Digit;
            carry >>= 64;
        }
        self.update_zeros();
    }

    fn set_to_div(&mut self, x: &Self, d: Digit) -> &Self{
        // self = x / d
        let d = d as Double;
        let mut rem: Double = 0;
        for i in self.zeros..x.zeros {
            self.digits[i] = 0;
        }

        for i in x.zeros..DIGITS {
            let num = (rem << 64) + x.digits[i] as Double;
            self.digits[i] = (num / d) as Digit;
            rem = num % d;
        }
        self.update_zeros_min(x.zeros);
        self
    }

    fn add_assign(&mut self, rhs: &Self) {
        // self += rhs
        // These are not implemented with trait AddAssign because that one expects the rhs to be
        // copied or moved, but we want to borrow it.
        let mut carry: Double = 0;
        for i in (rhs.zeros..DIGITS).rev() {
            let res = carry + self.digits[i] as Double + rhs.digits[i] as Double;
            self.digits[i] = res as Digit;
            carry = res >> 64;
        }
        self.update_zeros_min(max(1, min(self.zeros, rhs.zeros))-1);
    }

    fn sub_assign(&mut self, rhs: &Self) {
        // self -= rhs
        let mut carry: Double = 1;
        for i in (0..DIGITS).rev() {
            if i < rhs.zeros && carry == 1 {
                // The rest of the operations will not change anything, can return
                self.update_zeros_min(min(self.zeros, i+1));
                return;
            }
            let res = carry + self.digits[i] as Double + (!rhs.digits[i]) as Double;
            self.digits[i] = res as Digit;
            carry = res >> 64;
        }
        self.update_zeros();
    }

    #[allow(dead_code)]
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
        for i in self.zeros..DIGITS {
            let num = (rem << 64) + self.digits[i] as Double;
            self.digits[i] = (num / x) as Digit;
            rem = num % x;
        }
        self.update_zeros_min(self.zeros);
    }
}

struct Term {
    // One val in the Taylor series, i.e. 1/x^n, where n is an odd number. This structure is
    // passed to the threads as workspace that is returned when no longer needed. In each step, the
    // thread reduces the given value to the currently needed one, which only works for not too
    // large steps (i.e., the divisor needs to be u64).

    // Current denominator in Taylor series
    denom: Digit,
    // Number that always holds 1/x^denom
    val: Number,
}

impl Term {
    fn init(xinv: &Number) -> Self {
        // Initialize
        Term {
            denom: 1,
            val: xinv.clone(),
        }
    }
    fn copy_from(&mut self, rhs: &Term) {
        self.val.set(&rhs.val);
        self.denom = rhs.denom;
    }
    fn update(&mut self, x2: Digit, denom: Digit) {
        let divisor = match x2.checked_pow(((denom - self.denom)/2) as u32) {
            Some(d) => d,
            None => {
                println!("{} {} {}", x2, denom, self.denom);
                panic!("Difference too large");
            }
        };
        self.val /= divisor;
        self.denom = denom;
    }
}

fn calc(rcv: Receiver<(bool, Digit, Term)>, snd: Sender<Term>,
        snd_res: Sender<Number>) {
    // Worker thread. Iteratively receive a term and divisor and add or subtract the resulting
    // Taylor term to the result. Once no more terms are received, pass the result to the main
    // thread, which sums them together.
    let mut result = Number::zero();
    let mut tmp = Number::zero();
    loop {
        let (neg, div, term) = match rcv.recv() {
            Ok(x) => x,
            Err(_) => break,
        };
        tmp.set_to_div(&term.val, div);
        snd.send(term).unwrap();
        if neg {
            result.sub_assign(&tmp);
        } else {
            result.add_assign(&tmp);
        };
    }
    snd_res.send(result).unwrap();
}

fn ataninv(x: Digit) -> Number {
    // Calculate atan(1/x) using Taylor expansion
    let mut result = Number::from_inv(x);
    let x2 = x*x;
    let mut denom: Digit = 1;
    let mut negative = false;
    let mut running = 0;
    // Reference term. This starts with 1/x. Every time a task is created, we check if the target
    // term can be obtained from this using a division by a u64 number. If that is not possible,
    // because the divisor becomes too big, the reference term is updated to a smaller value, to
    // make the jump distance smaller.
    let mut refterm = Term::init(&result);

    let (snd_main, rcv_thrd) = unbounded();
    let (snd_thrd, rcv_main) = unbounded();
    let (snd_res, rcv_res) = unbounded();

    let mut terms = Vec::new();
    for _ in 0..MAXTHREADS+5 {
        terms.push(Term::init(&result));
    }
    for _ in 0..MAXTHREADS {
        let rcv = rcv_thrd.clone();
        let snd = snd_thrd.clone();
        let snd_res = snd_res.clone();
        thread::spawn(move || {
            calc(rcv, snd, snd_res);
        });
    }

    drop(rcv_thrd);
    drop(snd_thrd);
    drop(snd_res);

    loop {
        let mut term = match terms.pop() {
            Some(x) => {
                running += 1;
                x
            },
            None => match rcv_main.recv() {
                Ok(x) => x,
                Err(_) => break,
            },
        };
        if term.val.is_zero() {
            running -= 1;
            if running == 0 {
                drop(snd_main);
                break;
            }
            continue;
        }

        denom += 2;
        negative = !negative;
        let mut div = denom as Double *
            match x2.checked_pow(((denom-refterm.denom)/2) as u32) {
                Some(x) => x as Double,
                None => 0,
            };
        if div != 0 && (div >> 64) != 0 {
            // Move refterm forward, the step is too large
            refterm.update(x2, denom);
            div = denom as Double;
        } else if div  == 0 {
            // Even before multiplying with denom, it is too large.
            refterm.update(x2, denom-2);
            div = (denom * x2) as Double;
        }
        if term.denom < refterm.denom {
            term.copy_from(&refterm);
        }
        snd_main.send((negative, div as Digit, term)).unwrap();
    }
    for val in rcv_res {
        result.add_assign(&val);
    }
    println!("{}", denom);
    result
}

fn calc_term(tmp: &mut Number, term: &mut Term, denom: Digit, x2: Digit) {
    // Set tmp to 1/(denom*x^denom). If the step is too large, update term.
    // Case 1: denom*x^(denom-term.denom) fits into u64.
    //         We leave term as it is and only need one division.
    // Case 2: It does not, but x^(denom-term.denom) does.
    //         We update term so term.denom becomes denom and calculate tmp from there.
    // Case 3: Neither does. Update term s.t. term.denom becomes denom-2, calculate tmp.
    let divisor = denom as Double *
        match x2.checked_pow(((denom - term.denom)/2) as u32) {
            Some(d) => d,
            None => 0,
        } as Double;
    if divisor != 0 && (divisor >> 64) == 0 {
        tmp.set_to_div(&term.val, divisor as Digit);
    } else if divisor != 0 {
        term.update(x2, denom);
        tmp.set_to_div(&term.val, denom);
    } else {
        term.update(x2, denom-2);
        tmp.set_to_div(&term.val, denom*x2);
    }
}

#[allow(dead_code)]
fn ataninv_scalar(x: Digit) -> Number {
    let mut result = Number::from_inv(x);
    let x2 = x*x;
    let mut term = Term {
        val: result.clone(),
        denom: 1,
    };
    let mut tmp = Number::zero();
    let mut denom = 1;
    let starttime = Instant::now();
    let mut iters = 0;
    while !term.val.is_zero() {
        denom += 2;
        calc_term(&mut tmp, &mut term, denom, x2);
        result.sub_assign(&tmp);

        denom += 2;
        calc_term(&mut tmp, &mut term, denom, x2);
        result.add_assign(&tmp);
        iters += 1;
    }
    let elapsed = starttime.elapsed();
    println!("{} {}.{}", iters, elapsed.as_secs(), elapsed.subsec_millis());
    result
}

fn main() {
    // Calculate pi using pi/4 = 4atan(1/5)-atan(1/239)
    let mut start = Instant::now();
    let mut pi = ataninv(5);
    pi.mul4();
    let t1 = start.elapsed();
    start = Instant::now();
    pi.sub_assign(&ataninv(239));
    let t2 = start.elapsed();
    // Note that this takes the number outside the representable range by creating a value larger
    // than one, which overflows and drops the integer part, but that one is known to be 3.
    pi.mul4();
    pi.print();
    println!("{}.{:03} {}.{:03}",
             t1.as_secs(), t1.subsec_millis(), t2.as_secs(), t2.subsec_millis());
}
