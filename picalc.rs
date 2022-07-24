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

struct Thread {
    result: Number,
    tmp: Number,
    x2: Digit,
    snd: Sender<Term>,
}

impl Thread {
    fn step(&mut self, mut denom: Digit, mut t1: Term, mut t2: Term) {
        t1.update(self.x2, denom);
        self.tmp.set_to_div(&t1.val, denom);
        // we ignore the value that t2 came with, t1 is closer to the target denom
        t2.copy_from(&t1);
        self.snd.send(t1).unwrap();
        self.result.add_assign(&self.tmp);

        denom += 2;
        t2.update(self.x2, denom);
        self.tmp.set_to_div(&t2.val, denom);
        self.snd.send(t2).unwrap();
        self.result.sub_assign(&self.tmp);
    }
}

fn calc(x2: Digit, rcv: Receiver<(Digit, Term, Term)>, snd: Sender<Term>,
        snd_res: Sender<Number>) {
    // Worker loop. Collect result by iteratively receiving two terms, updating them as necessary
    // to compute 1/dx^d - 1/(d+2)x^(d+2), which is added to the result. Send the terms back
    // separately as soon as possible so the intermediate result can more quickly be used by
    // another thread.
    // Send the final result back to the main thread once the loop is done.
    let mut me = Thread {
        result: Number::zero(),
        tmp: Number::zero(),
        x2: x2,
        snd: snd,
    };
    loop {
        let (denom, t1, t2) = match rcv.recv() {
            Ok(x) => x,
            Err(_) => break,
        };
        if t1.denom > t2.denom {
            me.step(denom, t1, t2);
        } else {
            me.step(denom, t2, t1);
        }
    }
    snd_res.send(me.result).unwrap();
}

fn ataninv(x: Digit) -> Number {
    // Calculate atan(1/x) using Taylor expansion
    let mut result = Number::from_inv(x);
    let x2 = x*x;
    let mut denom: Digit = 3;
    let mut running = 0;
    // We always store a copy of the val of the completed Term with the highest denominator,
    // so we can push Terms to this denominator before they are started again, so the jump is
    // not too large - the reason being that we need to divide the val by x2^(diff of denoms),
    // which might become too large for u64.
    let mut refdenom = 1;
    let mut refval = Number::zero();
    refval.set(&result);

    let (snd_main, rcv_thrd) = unbounded();
    let (snd_thrd, rcv_main) = unbounded();
    let (snd_res, rcv_res) = unbounded();

    for _ in 0..MAXTHREADS+1 {
        snd_main.send((denom, Term::init(&result), Term::init(&result))).unwrap();
        running += 1;
        denom += 4;

        // If the number of threads requires a too large jump in the divisor, we break even though
        // we might use more threads.
        match x2.checked_pow(((denom-1)/2) as u32) {
            Some(_) => (),
            None => break
        }
    }
    
    println!("Created {} tasks.", running);
    for _ in 0..min(running, MAXTHREADS) {
        let rcv = rcv_thrd.clone();
        let snd = snd_thrd.clone();
        let snd_res = snd_res.clone();
        thread::spawn(move || {
            calc(x2, rcv, snd, snd_res);
        });
    }

    drop(rcv_thrd);
    drop(snd_thrd);
    drop(snd_res);
    running *= 2;

    let mut refoutput = 1000;

    let mut buffer: Option<Term> = None;
    for mut term in rcv_main {
        running -= 1;
        if term.val.zeros >= refoutput {
            refoutput += 1000;
        }
        if term.val.is_zero() {
            if running == 0 {
                drop(snd_main);
                break;
            }
            continue;
        }

        if term.denom > refdenom {
            refval.set(&term.val);
            refdenom = term.denom;
        }
        if refdenom > term.denom {
            term.val.set(&refval);
            term.denom = refdenom;
        }
        if buffer.is_some() {
            snd_main.send((denom, buffer.take().unwrap(), term)).unwrap();
            denom += 4;
            running += 2;
        } else {
            buffer.get_or_insert(term);
        }
    }
    for val in rcv_res {
        result.sub_assign(&val);
    }
    result
}

//fn ataninv(x: Digit) -> Number {
//    let mut result = Number::from_inv(x);
//    let x2 = x*x;
//    let mut val = result.clone();
//    let mut tmp = Number::zero();
//    let mut denom = 1;
//    let starttime = Instant::now();
//    let mut iters = 0;
//    while !val.is_zero() {
//        denom += 2;
//        val /= x2;
//        tmp.set_to_div(&val, denom);
//        result.sub_assign(&tmp);

//        denom += 2;
//        val /= x2;
//        tmp.set_to_div(&val, denom);
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
