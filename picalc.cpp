#include <vector>
#include <iostream>
#include <iomanip>

const int DIGITS = 10000;

typedef __uint128_t Double;
typedef __uint64_t Single;

struct Number {
    std::vector<Single> digits;
    Number() {
        digits.resize(DIGITS);
    };

    void set_zero() {
        digits.resize(0);
        digits.resize(DIGITS);
    }

    bool is_zero() const {
        for (auto d: digits)
            if (d)
                return false;
        return true;
    }

    void set_inv(int x) {
        Double rem = 1;
        set_zero();

        for (auto &d: digits) {
            auto nom = rem << 64;
            d = nom / x;
            rem = nom % x;
        }
    }

    void mul4() {
        Double carry = 0;
        for (int i=DIGITS-1; i>=0; i--) {
            carry += 4 * Double(digits[i]);
            digits[i] = carry;
            carry >>= 64;
        }
    }

    void div(const Number &x, Double d) {
        Double rem = 0;
        for (int i=0; i<DIGITS; i++) {
            auto num = (rem << 64) + x.digits[i];
            digits[i] = num / d;
            rem = num % d;
        }
    }

    Number &operator+=(const Number &rhs) {
        Double carry = 0;
        for (int i=DIGITS-1; i>=0; i--) {
            Double res = carry + digits[i] + rhs.digits[i];
            digits[i] = res;
            carry = res >> 64;
        }
        return *this;
    }

    Number &operator-=(const Number &rhs) {
        Double carry = 0;
        for (int i=DIGITS-1; i>=0; i--) {
            Double res = carry + digits[i] + (~rhs.digits[i]);
            if (i == DIGITS-1)
                res += 1;
            digits[i] = res;
            carry = res >> 64;
        }
        return *this;
    }

    Number &operator/=(Double x) {
        Double rem = 0;
        for (auto &d: digits) {
            auto num = (rem << 64) + d;
            d = num / x;
            rem = num % x;
        }
        return *this;
    }

};

std::ostream &operator<<(std::ostream &os, Number &x) {

    for (int i=0; i<DIGITS; i++) {
        os << std::hex << std::setw(16) << std::setfill('0') << x.digits[i] << ' ';
        if (i % 4 == 3)
            os << '\n';
    }
    return os;
};

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
        tmp.div(term, denom);
        result -= tmp;

        denom += 2;
        term /= x2;
        tmp.div(term, denom);
        result += tmp;
    }

    return result;
};

int main() {
    Number x = ataninv(5);
    x.mul4();
    x -= ataninv(239);
    x.mul4();
    std::cout << x << std::endl;
}
