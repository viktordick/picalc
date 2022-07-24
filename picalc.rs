use std::ops::DivAssign;
use std::cmp::{min,max};
use std::thread;
use crossbeam::{channel::{unbounded,Receiver,Sender}};

const DIGITS: usize = 100_000;
type Digit = u64;
type Double = u128;

const MAXTHREADS: usize = 8;

/*
 * Number represents a positive number between 0 (incl.) and 1 (excl.). It uses fixed precision
 * with DIGITS digits, each of base 2^64. For DIGITS = 10_000, this means 160_000 hexadecimal or
 * 640_000 binary digits. We only implement methods needed for the algorithm, which includes
 * division by a small (64 bit) unsigned integer, addition and subtraction of Numbers and
 * multiplication by 4.
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

    //fn set_zero(&mut self) {
    //    for i in 0..DIGITS {
    //        self.digits[i] = 0;
    //    }
    //    self.zeros = DIGITS;
    //}

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

struct Workspace {
    // Collection of Numbers and additional info that are needed by a thread to compute a term in
    // the Taylor sum. A Workspace is moved through the channel to a worker that is ready to do
    // some work, which returns it once it is finished.

    // Current denominator in Taylor series
    cur_denom: Digit,
    // Number that always holds 1/x^denom
    term: Number,
    // Denominator that is to be computed
    denom: Digit,
    // squared argument to ataninv
    x2: Digit,
    // Result number - 1/(denom * x^denom)
    result: Number,
    // Flag that term is zero
    term_zero: bool,
    // If term is to be understood to be negative
    negative: bool,
}

impl Workspace {
    fn init(x2: Digit, xinv: &Number, denom: Digit, negative: bool) -> Self {
        // Initialize workspace
        Workspace {
            cur_denom: 1,
            denom: denom,
            x2: x2,
            term: xinv.clone(),
            result: Number::zero(),
            term_zero: false,
            negative: negative,
        }
    }
    fn proc(&mut self) {
        let divisor = match self.x2.checked_pow(((self.denom - self.cur_denom)/2) as u32) {
            Some(d) => d,
            None => {
                println!("{} {} {}", self.x2, self.denom, self.cur_denom);
                panic!("Difference too lange");
            }
        };
        self.term /= divisor;
        self.cur_denom = self.denom;
        self.result.set_to_div(&self.term, self.denom);
        self.term_zero = self.term.is_zero();
    }
}

fn calc(rcv: Receiver<Workspace>, snd: Sender<Workspace>) {
    loop {
        // Receive a workspace
        match rcv.recv() {
            Ok(mut wsp) => {
                // Process
                wsp.proc();
                // Send result
                snd.send(wsp).unwrap();
            },
            Err(_) => break,
        }
    }
}

fn ataninv(x: Digit) -> Number {
    // Calculate atan(1/x) using Taylor expansion
    let mut result = Number::from_inv(x);
    let x2 = x*x;
    let mut denom: Digit = 1;
    let mut negative = false;
    let mut running = 0;
    // We always store a copy of the term of the completed Workspace with the highest denominator,
    // so we can push Workspaces to this denominator before they are started again, so the jump is
    // not too large - the reason being that we need to divide the term by x2^(diff of denoms),
    // which might become too large for u64.
    let mut refdenom = 1;
    let mut refterm = Number::zero();
    refterm.set(&result);

    let (snd_main, rcv_thrd) = unbounded();
    let (snd_thrd, rcv_main) = unbounded();
    for _ in 0..MAXTHREADS+1 {
        denom += 2;
        negative = !negative;
        let wsp = Workspace::init(x2, &result, denom, negative);
        snd_main.send(wsp).unwrap();
        running += 1;
        // If the number of threads requires a too large jump in the divisor, we break even though
        // we might use more threads.
        match x2.checked_pow(((denom+1)/2) as u32) {
            Some(_) => (),
            None => break
        }
    }
    
    println!("Created {} tasks.", running);
    for _ in 0..min(running, MAXTHREADS) {
        let rcv = rcv_thrd.clone();
        let snd = snd_thrd.clone();
        thread::spawn(move || {
            calc(rcv, snd);
        });
    }

    let mut refoutput = 1000;

    for mut wsp in rcv_main {
        if wsp.negative {
            result.sub_assign(&wsp.result);
        } else {
            result.add_assign(&wsp.result);
        }
        if wsp.term.zeros >= refoutput {
            println!("{}", wsp.term.zeros);
            refoutput += 1000;
        }
        if wsp.term_zero {
            running -= 1;
        } else {
            if wsp.cur_denom > refdenom {
                refterm.set(&wsp.term);
                refdenom = wsp.cur_denom;
            }
            if refdenom > wsp.cur_denom {
                wsp.term.set(&refterm);
                wsp.cur_denom = refdenom;
            }
            denom += 2;
            negative = !negative;
            wsp.denom = denom;
            wsp.negative = negative;
            snd_main.send(wsp).unwrap();
        }
        if running == 0 {
            drop(snd_main);
            break
        }
    }
    result
}

//fn ataninv(x: Digit) -> Number {
//    let mut result = Number::from_inv(x);
//    let x2 = x*x;
//    let mut term = result.clone();
//    let mut tmp = Number::zero();
//    let mut denom = 1;
//    let starttime = Instant::now();
//    let mut iters = 0;
//    while !term.is_zero() {
//        denom += 2;
//        term /= x2;
//        tmp.set_to_div(&term, denom);
//        result.sub_assign(&tmp);

//        denom += 2;
//        term /= x2;
//        tmp.set_to_div(&term, denom);
//        result.add_assign(&tmp);
//        iters += 1;
//    }
//    let elapsed = starttime.elapsed();
//    println!("{} {}.{}", iters, elapsed.as_secs(), elapsed.subsec_millis());
//    result
//}

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
