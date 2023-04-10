#include <vector>
#include <iostream>
#include <iomanip>

const int DIGITS = 10000;

typedef __uint64_t Digit;
typedef __uint128_t Double;

struct Number {
    std::vector<Digit> digits;
    int zeros;

    Number() {
        digits.resize(DIGITS);
        zeros = DIGITS;
    };

    void set_zero() {
        digits.resize(0);
        digits.resize(DIGITS);
        zeros = DIGITS;
    }

    void update_zeros(int min = 0) {
        zeros = DIGITS;
        for (int i=min; i<DIGITS; i++)
            if (digits[i]) {
                zeros = i;
                return;
            }
    }

    bool is_zero() const {
        return zeros == DIGITS;
    }

    void set_inv(int x) {
        Double rem = 1;
        set_zero();

        for (auto &d: digits) {
            auto nom = rem << 64;
            d = nom / x;
            rem = nom % x;
        }
        update_zeros();
    }

    void mul4() {
        Double carry = 0;
        for (int i=DIGITS-1; i>=0; i--) {
            carry += 4 * Double(digits[i]);
            digits[i] = carry;
            carry >>= 64;
        }
        update_zeros();
    }

    void set_to_div(const Number &x, Double d) {
        Double rem = 0;
        for (int i=zeros; i<x.zeros; i++)
            digits[i] = 0;

        for (int i=x.zeros; i<DIGITS; i++) {
            auto num = (rem << 64) + x.digits[i];
            digits[i] = num / d;
            rem = num % d;
        }
        update_zeros(x.zeros);
    }

    Number &operator+=(const Number &rhs) {
        Double carry = 0;
        for (int i=DIGITS-1; i>=rhs.zeros; i--) {
            Double res = carry + digits[i] + rhs.digits[i];
            digits[i] = res;
            carry = res >> 64;
        }
        update_zeros(std::max(1, std::min(zeros, rhs.zeros))-1);
        return *this;
    }

    Number &operator-=(const Number &rhs) {
        Double carry = 0;
        for (int i=DIGITS-1; i>=0; i--) {
            if (i < rhs.zeros && carry == 1) {
                update_zeros(std::min(zeros, i+1));
                return *this;
            }
            Double res = carry + digits[i] + (~rhs.digits[i]);
            if (i == DIGITS-1)
                res += 1;
            digits[i] = res;
            carry = res >> 64;
        }
        update_zeros();
        return *this;
    }

    Number &operator/=(Double x) {
        Double rem = 0;
        for (int i=zeros; i<DIGITS; i++) {
            auto num = (rem << 64) + digits[i];
            digits[i] = num / x;
            rem = num % x;
        }
        update_zeros(zeros);
        return *this;
    }

};

//std::ostream &operator<<(std::ostream &os, Number &x) {

//    for (int i=0; i<DIGITS; i++) {
//        os << std::hex << std::setw(16) << std::setfill('0') << x.digits[i] << ' ';
//        if (i % 4 == 3)
//            os << '\n';
//    }
//    return os;
//};

Number ataninv(int x) {
    Number result;
    result.set_inv(x);
    Double x2 = x*x;
    Number term = result;
    Number tmp;
    Double denom = 1;
    
    while (!term.is_zero()) {
        denom += 2;
        term /= x2;
        tmp.set_to_div(term, denom);
        result -= tmp;

        denom += 2;
        term /= x2;
        tmp.set_to_div(term, denom);
        result += tmp;
    }
    std::cout << uint32_t(denom) << std::endl;
    return result;
};

int main() {
    Number x = ataninv(5);
    x.mul4();
    x -= ataninv(239);
    x.mul4();
    //std::cout << x << std::endl;
}
