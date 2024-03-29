use std::ops::DivAssign;
use std::cmp::{min,max};
use std::thread;
use std::env;
use std::vec::Vec;
use crossbeam::{channel::{unbounded,Receiver,Sender}};

const DIGITS: usize = 20000;
type Digit = u64;
type Double = u128;

/*
 * Number represents a number between -0.5 (incl.) and 0.5 (excl.). It uses fixed precision
 * with DIGITS digits, each of base 2^64. For DIGITS = 10_000, this means 160_000 hexadecimal or
 * 640_000 binary digits. We only implement methods needed for the algorithm, which includes
 * a) addition and subtraction and
 * b) multiplication by 4 and division by a small (u64) number (only for positive Numbers).
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
            let nom = rem << Digit::BITS;
            result.digits[i] = (nom / x) as Digit;
            rem = nom % x;
        }
        result.update_zeros();
        result
    }

    fn copy_from(&mut self, rhs: &Number) {
        for i in 0..DIGITS {
            self.digits[i] = rhs.digits[i];
        }
        self.zeros = rhs.zeros;
    }

    fn update_zeros_min(&mut self, min: usize) {
        // Update how many leading digits are zeros, under the assumption that there are at least
        // min
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
            carry >>= Digit::BITS;
        }
        self.update_zeros();
    }

    fn set_to_div(&mut self, x: &Self, d: Digit) {
        // self = x / d
        let d = d as Double;
        let mut rem: Double = 0;
        for i in self.zeros..x.zeros {
            self.digits[i] = 0;
        }

        for i in x.zeros..DIGITS {
            let num = (rem << Digit::BITS) + x.digits[i] as Double;
            self.digits[i] = (num / d) as Digit;
            rem = num % d;
        }
        self.update_zeros_min(x.zeros);
    }

    fn add_assign(&mut self, rhs: &Self) {
        // self += rhs
        // These are not implemented with trait AddAssign because that one expects the rhs to be
        // copied or moved, but we want to borrow it.
        let mut carry: Double = 0;
        for i in (rhs.zeros..DIGITS).rev() {
            let res = carry + self.digits[i] as Double + rhs.digits[i] as Double;
            self.digits[i] = res as Digit;
            carry = res >> Digit::BITS;
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
            carry = res >> Digit::BITS;
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

fn ataninv_scalar(x: Digit) -> Number {
    /* Scalar version of computing atan(1/x) as alternating sum over 1/(kx^k) with k iterating over
     * odd numbers.
     */
    let x2 = x*x;
    let mut result = Number::from_inv(x);
    // refterm is always 1/x^n with some odd n that is not necessarily the same as k since we can
    // sometimes get away with only one division - computing 1/(kx^k)=refterm/(kx^(k-n)).  Only if
    // the denominator becomes too large for a u64, we update the refterm such that n=k.
    let mut refterm = result.clone();
    let mut tmp = Number::zero();
    // the counting variable, k in the term 1/(kx^k)
    let mut denom: Digit = 1;
    // x^(k-n), this indicates how far refterm lags behind
    let mut stepsize: Digit = 1;
    let mut neg = true;
    while !refterm.is_zero() {
        denom += 2;
        stepsize *= x2;
        let mut divisor = denom as Double * stepsize as Double;
        if divisor > Digit::MAX.into() {
            refterm /= stepsize;
            stepsize = 1;
            divisor = denom as Double;
        }
        tmp.set_to_div(&refterm, divisor as Digit);
        if neg {
            result.sub_assign(&tmp);
        } else {
            result.add_assign(&tmp);
        }
        neg = !neg;
    }
    result
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
        self.val.copy_from(&rhs.val);
        self.denom = rhs.denom;
    }
}

enum Msg {
    Number(Number),
    Term(Term),
}

fn calc(rcv: Receiver<(bool, Digit, Term)>, snd: Sender<Msg>,) {
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
        if tmp.is_zero() {
            snd.send(Msg::Number(result)).unwrap();
            break;
        }
        snd.send(Msg::Term(term)).unwrap();
        if neg {
            result.sub_assign(&tmp);
        } else {
            result.add_assign(&tmp);
        };
    }
}

#[allow(dead_code)]
fn ataninv_threaded(x: Digit, nthreads: usize) -> Number {
    // Calculate atan(1/x) using Taylor expansion. This keeps the calculation of the reference term
    // in the main thread. Only the final division by the factor k that does not help in updating
    // the reference term and the summing is done inside the worker thread.

    let mut result = Number::from_inv(x);
    // Reference term. This starts with 1/x. Every time a task is created, we check if the target
    // term can be obtained from this using a division by a u64 number. If that is not possible,
    // because the divisor becomes too large, the reference term is updated to a smaller value, to
    // make the jump distance smaller.
    let mut refterm = Term::init(&result);

    let (snd_main, rcv_thrd) = unbounded();
    let (snd_thrd, rcv_main) = unbounded();

    let mut terms = Vec::new();
    for _ in 0..nthreads+2 {
        terms.push(Term::init(&result));
    }
    for _ in 0..nthreads {
        let rcv = rcv_thrd.clone();
        let snd = snd_thrd.clone();
        thread::spawn(move || {
            calc(rcv, snd);
        });
    }

    drop(rcv_thrd);
    drop(snd_thrd);

    let x2 = x*x;
    // current power of x for the Taylor series
    let mut denom: Digit = 1;
    // x^(denom-refterm.denom)
    let mut stepsize: Digit = 1;
    // current sign of the next term.
    let mut negative = false;
    loop {
        // Push a few tasks from the vector. Once it is depleted, create tasks by reusing terms
        // that were given back by a processing thread.
        let mut term = match terms.pop() {
            Some(x) => x,
            None => match rcv_main.recv() {
                Ok(msg) => match msg {
                    Msg::Term(x) => x,
                    Msg::Number(x) => {
                        result.add_assign(&x);
                        continue;
                    },
                },
                Err(_) => break,
            },
        };

        denom += 2;
        negative = !negative;
        stepsize *= x2;
        if denom as Double * stepsize as Double > Digit::MAX.into() {
            refterm.val /= stepsize;
            refterm.denom = denom;
            stepsize = 1;
        };
        if term.denom < refterm.denom {
            term.copy_from(&refterm);
        }
        // Errors here are not a problem. All threads already encountered a zero term and
        // terminated. We prepared too many terms, but they will also be zero.
        let _ = snd_main.send((negative, denom*stepsize, term));
    }
    result
}


// Alternate strategy - update refterm inside worker thread
struct TaskParams {
    neg: bool,
    // Divisor
    div: Digit,
}

enum Task {
    UpdateTerm,
    UpdateRef,
}

fn worker(rcv: Receiver<(Task, TaskParams, Number)>,
          snd: Sender<(Task, TaskParams, Number, bool)>) {
    let mut finished = false;
    while !finished {
        let received = rcv.recv();
        let (task, params, mut term) = match received {
            Ok(x) => x,
            Err(_) => break,
        };

        term /= params.div;
        finished = term.is_zero();

        snd.send((task, params, term, finished)).unwrap();
    }
}

fn ataninv_threaded2(x: Digit, nthreads: usize) -> Number {
    let (snd_main, rcv_thrd) = unbounded();
    let (snd_thrd, rcv_main) = unbounded();

    let x2 = x*x;

    for _ in 0..nthreads {
        let rcv = rcv_thrd.clone();
        let snd = snd_thrd.clone();
        thread::spawn(move || { worker(rcv, snd); });
    };

    drop(rcv_thrd);
    drop(snd_thrd);

    let mut result = Number::from_inv(x);
    let mut terms = Vec::new();

    let mut refterm = result.clone();
    // Index of refterm power
    let mut refidx: Digit = 0;

    let mut awaiting_nextrefterm = false;
    let mut neg = false;
    let mut running = true;
    let mut tasks = 0;

    while running || tasks > 0 {
        if running && !awaiting_nextrefterm {
            // Compute all divisors that can be used from the current to the next refterm.
            // But create the task that computes the next refterm before creating the division
            // tasks.
            let mut divs = Vec::new();

            let mut div: Digit = 1;
            while let Some(fulldiv) = div.checked_mul(x2*(2*refidx+3)) {
                div = div.checked_mul(x2).unwrap();
                divs.push(fulldiv);
                refidx += 1;
            }
            // Make sure there are enough workspaces for all needed tasks.
            for _ in terms.len()..=divs.len() {
                terms.push(Number::zero());
            }
            let mut term = terms.pop().unwrap();
            term.copy_from(&refterm);
            if let Ok(_) = snd_main.send((Task::UpdateRef, TaskParams{neg: false, div: div}, term)) {
                tasks += 1;
            }
            for i in 0..divs.len() {
                neg = !neg;
                let mut term = terms.pop().unwrap();
                term.copy_from(&refterm);
                if let Ok(_) = snd_main.send((Task::UpdateTerm, TaskParams{neg: neg, div: divs[i]}, term)) {
                    tasks += 1;
                }
            }
            awaiting_nextrefterm = true;
        }
        match rcv_main.recv() {
            Ok((task, params, term, finished)) => {
                tasks -= 1;
                if finished {
                    running = false;
                }
                match task {
                    Task::UpdateRef => {
                        refterm.copy_from(&term);
                        awaiting_nextrefterm = false;
                    },
                    Task::UpdateTerm => {
                        if params.neg {
                            result.sub_assign(&term)
                        } else {
                            result.add_assign(&term)
                        }
                    },
                }
                terms.push(term);
            },
            Err(_) => break,
        };

    };
    result
}

fn ataninv(x: Digit, nthreads: usize) -> Number {
    if nthreads == 0 {
        ataninv_scalar(x)
    } else {
        ataninv_threaded2(x, nthreads)
    }
}

fn main() {
    // Calculate pi using pi/4 = 4atan(1/5)-atan(1/239)
    let args: Vec<String> = env::args().collect();
    let nt = args[1].parse::<usize>().unwrap();
    let (snd, rcv) = unbounded();
    thread::spawn(move || {
        snd.send(ataninv(239, nt)).unwrap();
    });

    let mut pi = ataninv(5, nt);
    pi.mul4();
    pi.sub_assign(&rcv.recv().unwrap());
    // Note that this takes the number outside the representable range by creating a value larger
    // than one, which overflows and drops the integer part, but that one is known to be 3.
    pi.mul4();
    //pi.print();
}
