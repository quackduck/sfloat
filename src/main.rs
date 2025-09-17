
#[derive(Debug)]
struct Float {
    bits: u64,
}

impl Float {
    fn from_bits(bits: u64) -> Self {
        Float { bits }
    }

    fn new(value: f64) -> Self {
        Float { bits: value.to_bits() }
    }

    fn to_f64(&self) -> f64 {
        f64::from_bits(self.bits)
    }

    fn get_sign(&self) -> bool {
        (self.bits >> 63) & 1 == 1 // false for positive, true for negative
    }

    fn get_exponent(&self) -> i16 {
        let exp_bits = ((self.bits >> 52) & ((1<<11)-1)) as i16;
        exp_bits - 1023 // Subtracting the bias
        // exp_bits
    }

    fn get_mantissa(&self) -> u64 {
        self.bits & ((1 << 52) - 1) // last 52 bits
    }

    fn negate(&mut self) {
        self.bits ^= 1 << 63; // flip the sign bit by XORing because 1^0=1 and 1^1=0
    }

    // thank you william kahan
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


    fn multiply(&self, other: &Float) -> Float {
        if self.is_nan() {
            return Float::from_bits(self.bits | 1 << 51) // set 52nd bit to indicate qNaN
        }
        if other.is_nan() {
            return Float::from_bits(other.bits | 1 << 51)
        }

        let sign = self.get_sign() ^ other.get_sign();

        if self.is_infinity() || other.is_infinity() {
            if self.is_zero() || other.is_zero() {
                return Float::nan();
            }
            return Float::infinity(sign);
        }

        let mut exponent = self.get_exponent() + other.get_exponent();

        // add implicit leading 1s
        // let mut mantissa_full = u128::from(self.get_mantissa() | (1 << 52)) * u128::from(other.get_mantissa() | (1 << 52));

        let mut mantissa_full = {
            let a_full = if self.get_exponent() == -1023 { // subnormal
                // todo: better way to handle subnormals?
                exponent += 1; // adjust exponent for subnormal
                self.get_mantissa()
            } else {
                self.get_mantissa() | (1 << 52)
            };
            let b_full = if other.get_exponent() == -1023 { // subnormal
                exponent += 1; // adjust exponent for subnormal
                other.get_mantissa()
            } else {
                other.get_mantissa() | (1 << 52)
            };
            u128::from(a_full) * u128::from(b_full)
        };

        // println!("Mantissa full: {:0106b}", mantissa_full);

        if mantissa_full >> 105 != 0 { // is 106th bit set?
            // println!("Normalizing mantissa, shifting right");
            // we need to shift right
            exponent += 1;
            mantissa_full >>= 1;
        }

        // move mantissa_full up until 105th bit is set
        while mantissa_full >> 104 == 0 && mantissa_full != 0 {
            // println!("Normalizing mantissa, shifting left");
            mantissa_full <<= 1;
            exponent -= 1;
        } // now either 105th bit is set or mantissa_full is zero. we could exit early here if mantissa_full is zero...

        let mut mantissa = mantissa_full >> 52; // shift down to get 53 bits (including implicit leading 1)
        {
            let mantissa_lower52 = mantissa_full & ((1 << 52) - 1);
            if mantissa_lower52 == (1 << 51) { // tie, so round to even case.
                println!("TIE!");
                if mantissa & 1 == 1 {
                    mantissa += 1; // round up to make even
                }
                // else truncate, so do nothing
            } else if mantissa_lower52 > (1 << 51) {
                mantissa += 1; // round up
            } // else truncate, so do nothing
        }

        if exponent <= -1023 {
            // can we create a subnormal number?
            if exponent < -1074 {
                // underflow to zero
                return Float::from_bits((sign as u64) << 63); // zero
            }
            // subnormal
            let shift = (-1023 + 1 - exponent) as u32; // how much to shift right to make exponent -1023
            mantissa >>= shift; // todo: handle rounding here
            exponent = -1023;
            return Float::from_parts(sign, exponent, mantissa as u64);
        }
        if exponent >= 1024 {
            // overflow to infinity
            return Float::from_bits((sign as u64) << 63 | (0x7FF << 52)); // infinity
        }

        Float::from_parts(sign, exponent, mantissa as u64)
    }

    fn print_bits(&self) {
        println!("{:064b}", self.bits);
    }

    fn print_parts(&self) {
        println!("Sign: {}, Exponent: {}, Mantissa: {:052b}", self.get_sign(), self.get_exponent(), self.get_mantissa());
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
    } else if print {
        println!("Match!");
        println!("x: {}, y: {}", a.to_f64(), b.to_f64());
        println!("expected: {:e}, actual: {:e}", expected, actual);
    }
    
}

fn mult_stress_test() {
    use rand::Rng;
    let mut rng = rand::rng();
    for _ in 0..1_000_000 {
        let fx = Float::from_bits(rng.random());
        let fy = Float::from_bits(rng.random());
        mult_check_print(fx, fy, false);
    }
}


fn main() {
    // let a = float::new(0.01);
    let a = Float::new(-1.02735137937997933477e+00);
    println!("{:?}", a.to_f64());
    a.print_parts();
    a.print_bits();
    // let b = float::new(-0.03);
    let b = Float::new(-1.02735137937997933477e+00);
    println!("{:?}", b.to_f64());
    b.print_parts();
    b.print_bits();

    let c = a.multiply(&b);

    b.print_parts();
    c.print_parts();
    println!("{:?}", c.to_f64());

    let expected = a.to_f64() * b.to_f64();
    println!("Expected: {:?}", expected);
    Float::new(expected).print_parts();

    mult_stress_test();
    mult_tie_test();
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