#[derive(Debug)]
struct Float {
    bits: u64,
}

impl Float {
    fn from_bits(bits: u64) -> Self {
        Float { bits }
    }

    fn new(value: f64) -> Self {
        Float {
            bits: value.to_bits(),
        }
    }

    fn to_f64(&self) -> f64 {
        f64::from_bits(self.bits)
    }

    fn get_sign(&self) -> bool {
        (self.bits >> 63) & 1 == 1 // false for positive, true for negative
    }

    fn get_exponent(&self) -> i16 {
        let exp_bits = ((self.bits >> 52) & ((1 << 11) - 1)) as i16;
        exp_bits - 1023 // Subtracting the bias
    }

    fn get_mantissa(&self) -> u64 {
        self.bits & ((1 << 52) - 1) // last 52 bits
    }

    fn negate(&mut self) {
        self.bits ^= 1 << 63; // flip the sign bit by XORing because 1^0=1 and 1^1=0
    }

    // thank you william kahan todo: consider negative numbers
    fn less_than(&self, other: &Float) -> bool {
        self.bits < other.bits
    }
    fn greater_than(&self, other: &Float) -> bool {
        self.bits > other.bits
    }
    fn equals(&self, other: &Float) -> bool {
        self.bits == other.bits
    }

    fn from_parts(sign: bool, exponent: i16, mantissa: u64) -> Self {
        Float {
            bits: (
                (sign as u64) << 63) |
                ((((exponent + 1023) as u64) & ((1 << 11)-1)) << 52) | // select lower 11 bits of exponent and shift
                (mantissa & ((1 << 52) - 1) // select lower 52 bits of mantissa
            ),
        }
    }

    fn is_zero(&self) -> bool {
        self.get_exponent() == -1023 && self.get_mantissa() == 0
    }

    fn is_nan(&self) -> bool {
        self.get_exponent() == 1024 && self.get_mantissa() != 0
    }

    fn is_infinity(&self) -> bool {
        self.get_exponent() == 1024 && self.get_mantissa() == 0
    }

    fn nan() -> Float {
        Float::from_bits(0x7FF8000000000000)
    }

    fn infinity(sign: bool) -> Float {
        Float::from_bits((sign as u64) << 63 | (0x7FF << 52)) // infinity
    }

    fn copy(&self) -> Float {
        Float { bits: self.bits }
    }

    // fn largest_finite() -> float {
    //     float::from_bits(0x7FEFFFFFFFFFFFFF) // largest finite number
    // }
    // fn smallest_normal() -> float {
    //     float::from_bits(0x0010000000000000) // smallest normal number
    // }
    // fn smallest_subnormal() -> float {
    //     float::from_bits(0x0000000000000001) // smallest subnormal number
    // }

    fn nan_logic(&self, other: &Float) -> Option<Float> {
        // this nan logic is not super important but matches apple's cpu behavior
        // the rule is that signaling nans take precedence over quiet nans,
        // that if both are the same type the first operand takes precedence,
        // and that if one is a nan and the other is not, the nan is returned.
        let self_is_nan = self.is_nan();
        let other_is_nan = other.is_nan();
        if self_is_nan || other_is_nan {
            let chosen_nan = if other_is_nan
                && (other.get_mantissa() >> 51) == 0
                && !(self_is_nan && (self.get_mantissa() >> 51) == 0)
            {
                // other is signaling nan and self is not signaling nan
                other.bits
            } else if self_is_nan {
                self.bits
            } else {
                other.bits
            };
            return Some(Float::from_bits(chosen_nan | 1 << 51)); // quiet nan
        }
        None
    }

    // returns mantissa with implicit leading 1 and adjusts exponent for subnormals
    fn get_full_mantissa(&self, exponent: &mut i16) -> u64 {
        let is_normal = (((self.bits >> 52) & ((1 << 11) - 1)) != 0) as u64; // exponent bits non-zero
        *exponent += 1 - is_normal as i16; // adjust exponent for subnormal (interpreted as -1022)
        self.get_mantissa() | (is_normal << 52) // implicit leading 1
    }

    fn multiply(&self, other: &Float) -> Float {
        if let Some(nan) = self.nan_logic(other) {
            return nan;
        }

        let sign = self.get_sign() ^ other.get_sign(); // same sign means pos, else neg

        if self.is_infinity() || other.is_infinity() {
            if self.is_zero() || other.is_zero() {
                return Float::nan(); // infinity * 0 = nan
            }
            return Float::infinity(sign);
        }

        let mut exponent = self.get_exponent() + other.get_exponent();

        let mut mantissa_full = u128::from(self.get_full_mantissa(&mut exponent)) * u128::from(other.get_full_mantissa(&mut exponent)); // 53 + 53 = 106 bits

        // println!("Mantissa full: {:0106b}", mantissa_full);

        // if-else block normalizes mantissa_full so that the 105th bit is set.
        // why bit 105? because we're going to shift down by 52 and so the implicit 1 will be correctly at bit 53.
        if mantissa_full >> 105 != 0 {
            // is 106th bit set? this means we overflowed.
            // println!("Normalizing mantissa, shifting right");
            exponent += 1;
            mantissa_full >>= 1; // todo: technically this could affect rounding??
        } else {
            // this case only happens when subnormals are involved, since min normal mantissa is 2^52 and 2^52 * 2^52 = 2^104, which has the 105th bit set.
            // todo: handle upper case by using leading zeros too?
            let shift_amt = mantissa_full.leading_zeros() - (128 - 105); // this will never be negative since we handled that case above. we want 23 leading zeros.
            mantissa_full <<= shift_amt;
            exponent -= shift_amt as i16;
        }

        let shift_and_round = |mantissa_full: u128, shift: u32| -> u64 {
            let mantissa = (mantissa_full >> shift) as u64;
            let remainder = mantissa_full & ((1u128 << shift) - 1);
            let half_way = 1u128 << (shift - 1);

            if remainder > half_way || (remainder == half_way && mantissa & 1 == 1) {
                // if past halfway or exactly halfway and mantissa is odd (add instead of subtract since other case rounds down.)
                mantissa + 1
            } else {
                // round down (truncate)
                mantissa
            }
        };

        if exponent >= 1024 { // overflow to infinity
            return Float::infinity(sign);
        }

        let mut shift = 52; // we want to shift right by 52 to get 53 bits (including implicit leading 1). another way to think of this is that when we multiplied the mantissas we did an implicit mult by 2^52.

        if exponent <= -1023 {
            // can we create a subnormal number?
            if exponent < -1075 {
                // min subnormal is 2^-52 * 2^-1022 = 2^-1074. we still allow exponent -1075 because we might round up to that value
                // underflow to zero
                return Float::from_bits((sign as u64) << 63); // zero
            }
            shift += (-1023 + 1 - exponent) as u32; // correct by induction: if exponent is -1023, we want to shift by 1 extra since -1022 is the exponent this subnormal will be interpreted as having. if exponent is -1024 we want to shift by 2 extra, etc.
            exponent = -1023; // mark as subnormal
        }
        // from parts selects the lower 52 bits of the mantissa for us.
        Float::from_parts(sign, exponent, shift_and_round(mantissa_full, shift) as u64)
    }

    fn add(&self, other: &Float) -> Float {
        if let Some(nan) = self.nan_logic(other) {
            return nan;
        }

        if self.is_zero() {
            return other.copy();
        }
        if other.is_zero() {
            return self.copy();
        }
        if self.is_infinity() {
            if other.is_infinity() {
                if self.get_sign() != other.get_sign() {
                    return Float::nan(); // infinity + -infinity = nan
                }
            }
            return self.copy();
        }
        if other.is_infinity() {
            return other.copy();
        }
        
        // both are finite and non-zero

        let (mut a, mut b) = if self.get_exponent() > other.get_exponent() {
            (self.copy(), other.copy())
        } else {
            (other.copy(), self.copy())
        }; // a has the larger exponent
        let mut exp_a = a.get_exponent();
        let mut exp_b = b.get_exponent();

        let sign = a.get_sign(); // sign of the result is the sign of the larger exponent
        let mut mantissa_a = a.get_full_mantissa(&mut exp_a);
        let mut mantissa_b = b.get_full_mantissa(&mut exp_b);

        let exp_diff = (exp_a - exp_b) as u32;

        // todo: think about signs and rounding.

        let shifted_out = mantissa_b & ((1 << exp_diff) - 1); // for rounding

        mantissa_b = if exp_diff >= 64 { // we could choose a smaller number such as 54 here since each mantissa is at most 53 bits.
            0
        } else {
            mantissa_b >> exp_diff
        };

        let mantissa = mantissa_a + mantissa_b; // 53 + 53 = 54 bits

        // Float::from_parts(sign, exponent, mantissa_a + mantissa_b)
        return Float::nan(); // todo
    }

            // if exp_diff != 0 {
        //     if exp_diff > 53 { // each mantissa is at most 53 bits.
        //         // mantissa_b will be shifted out completely
        //         mantissa_b = 0; // todo: think about rounding
        //     } else {
        //         // shift right with jamming
                // if shifted_out != 0 {
        //     mantissa_b |= 1; // jam bit
        // }
            // }
        // }

    // fn divide(&self, other: &Float) -> Float {
    //     if let Some(nan) = self.nan_logic(other) {
    //         return nan;
    //     }
    //     // division by zero and zero divided by zero both yield NaN
    //     if other.is_zero() {
    //         return Float::nan();
    //     }
        
    //     let sign = self.get_sign() ^ other.get_sign(); // same sign means pos, else neg
        
    //     if self.is_zero() {
    //         return Float::from_bits((sign as u64) << 63); // zero
    //     }
    //     if self.is_infinity() {
    //         if other.is_infinity() {
    //             return Float::nan(); // infinity / infinity = nan
    //         }
    //         return Float::infinity(sign); // infinity / finite = infinity
    //     }
    //     if other.is_infinity() {
    //         return Float::from_bits((sign as u64) << 63); // finite / infinity = 0
    //     }

    //     let mut exponent = self.get_exponent() - other.get_exponent();
    //     let mut mantissa_full = {
    //         // mutable because closure borrows exponent mutably
    //         let mut get_full_mantissa = |f: &Float| -> u64 {
    //             // branchless version. should profile to see if this is actually faster.
    //             let is_normal = (((f.bits >> 52) & ((1 << 11) - 1)) != 0) as u64; // exponent bits non-zero
    //             exponent += 1 - is_normal as i16; // adjust exponent for subnormal (interpreted as -1022)
    //             f.get_mantissa() | (is_normal << 52) // implicit leading 1
    //         };
    //         (u128::from(get_full_mantissa(self)) << 52) / u128::from(get_full_mantissa(other))
    //         // shift by 52 to keep precision.
    //     };
    //     println!("Mantissa full: {:0106b}", mantissa_full);
    //     // if-else block normalizes mantissa_full so that the 105th bit is set.

    //     // todo: think about rounding.
        
    //     return Float::from_parts(sign, exponent, mantissa_full as u64); // todo
    // }

    fn print_bits(&self) {
        println!("{:064b}", self.bits);
    }

    fn print_parts(&self) {
        println!(
            "Sign: {}, Exponent: {}, Mantissa: {:052b}",
            self.get_sign(),
            self.get_exponent(),
            self.get_mantissa()
        );
    }
}

fn mult_check_print(a: Float, b: Float, print: bool) {
    let result = a.multiply(&b);
    let expected = a.to_f64() * b.to_f64();
    let actual = result.to_f64();

    if print {
        a.print_parts();
        b.print_parts();
        result.print_parts();
    }

    if expected.to_bits() != actual.to_bits() {
        println!("Mismatch!");
        println!("x: {}, y: {}", a.to_f64(), b.to_f64());
        println!("expected: {:e}, actual: {:e}", expected, actual);
        a.print_parts();
        b.print_parts();
        result.print_parts();
        Float::new(expected).print_parts();
        panic!("Test failed");
    } else if print {
        println!("Match!");
        println!("x: {}, y: {}", a.to_f64(), b.to_f64());
        println!("expected: {:e}, actual: {:e}", expected, actual);
    }
}

fn mult_stress_test() {
    use rand::Rng;
    let mut rng = rand::rng();
    for _ in 0..10_000_000 {
        let fx = Float::from_bits(rng.random());
        let fy = Float::from_bits(rng.random());
        mult_check_print(fx, fy, false);
    }
    println!("Stress test passed!");
}

fn main() {
    let a = Float::new(1.1);
    // let a = Float::new(-1.02735137937997933477e+00);
    println!("{:?}", a.to_f64());
    a.print_parts();
    a.print_bits();
    let b = Float::new(1.1);
    // let b = Float::new(-1.02735137937997933477e+00);
    println!("{:?}", b.to_f64());
    b.print_parts();
    b.print_bits();

    let c = a.multiply(&b);
    println!("{:?}", c.to_f64());
    c.print_parts();
    c.print_bits();

    // b.print_parts();
    // c.print_parts();
    // println!("{:?}", c.to_f64());

    // let expected = a.to_f64() * b.to_f64();
    // println!("Expected: {:?}", expected);
    // Float::new(expected).print_parts();

    // mult_stress_test();
    mult_benchmark();
    // mult_tie_test();

    // let c = a.divide(&b);
}

fn mult_benchmark() {

    let n = 100_000_000;

    use std::time::Instant;
    // let a = Float::new(1.1);
    // let b = Float::new(1.1);

    // test with subnormals
    let a = Float::from_parts(false, -1023, 1); // smallest subnormal
    let b = Float::new(1.0);

    let start = Instant::now();
    for _ in 0..n {
        let _ = a.multiply(&b);
    }
    let duration1 = start.elapsed();
    println!("Time elapsed in multiplication: {:?}", duration1);

    let a_f = a.to_f64();
    let b_f = b.to_f64();
    let start = Instant::now();
    for _ in 0..n {
        let _ = a_f * b_f;
    }
    let duration2 = start.elapsed();
    println!("Time elapsed in f64 multiplication: {:?}", duration2);

    println!("Software is {} times slower", duration1.as_secs_f64() / duration2.as_secs_f64());
}

fn mult_tie_test() {
    // mantissa1 * mantissa2 = (some_value << 52) + (1 << 51)
    // mantissa1 = 2^26, mantissa2 = 2^26 + 2^25, product = 2^52 + 2^51
    let mantissa1 = 1 << 26; // 2^26
    let mantissa2 = (1 << 26) + (1 << 25); // 2^26 + 2^25
                                           // let mantissa2 = 1 << 25;

    let a = Float::from_parts(false, 0, mantissa1);
    let b = Float::from_parts(false, 0, mantissa2);

    mult_check_print(a, b, true);

    // a.print_parts();
    // b.print_parts();

    // let result = a.multiply(&b);
    // println!("Result = {:.17e}", result.to_f64());
    // result.print_parts();
    // let expected = a.to_f64() * b.to_f64();
    // println!("Expected = {:.17e}", expected);
    // Float::new(expected).print_parts();
}
