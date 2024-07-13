import whooie/float
import gleam/order.{ Eq, Gt, Lt }

/// Main data type describing every complex number `z` as a real and imaginary
/// part, i.e. `z = x + i y` where `x` and `y` are real numbers.
pub type Complex {
  Complex(re: Float, im: Float)
}

/// Create a new `Complex`.
pub fn new(re: Float, im: Float) -> Complex { Complex(re, im) }

/// Convert a `Float` to a `Complex` with zero imaginary part.
pub fn of_float(re: Float) -> Complex { Complex(re, 0.0) }

/// Convert a pair of `Float`s to a `Complex`, with the first being the real
/// part.
pub fn of_pair(z: #(Float, Float)) -> Complex { Complex(z.0, z.1) }

/// Construct a `Complex` from radial coordinates.
pub fn of_polar(r: Float, ph: Float) -> Complex {
  Complex(r *. float.cos(ph), r *. float.sin(ph))
}

/// Convert a `Complex` to a polar representation of the form `#(|z|, arg(z))`.
pub fn to_polar(z: Complex) -> #(Float, Float) { #(norm(z), arg(z)) }

/// Imaginary unit.
pub const i: Complex = Complex(0.0, 1.0)

/// Real unit.
pub const one: Complex = Complex(1.0, 0.0)

/// Complex zero.
pub const zero: Complex = Complex(0.0, 0.0)

/// Return `True` if two `Complex`es are equal.
pub fn eq(l: Complex, r: Complex) -> Bool {
  case float.cmp(l.re, r.re), float.cmp(l.im, r.im) {
    Eq, Eq -> True
    _, _ -> False
  }
}

/// Return `True` if two `Complex`es are not equal.
pub fn neq(l: Complex, r: Complex) -> Bool { !eq(l, r) }

/// Return the additive inverse of `z`.
pub fn neg(z: Complex) -> Complex { Complex(float.neg(z.re), float.neg(z.im)) }

/// Add a `Complex` to another.
pub fn add(l: Complex, r: Complex) -> Complex {
  Complex(l.re +. r.re, l.im +. r.im)
}

/// Add a `Complex` to a `Float`. The `Float` is treated as a real value.
pub fn addf(l: Complex, r: Float) -> Complex { Complex(l.re +. r, l.im) }

/// Add a `Float` to a `Complex`. The `Complex` is treated as a real value.
pub fn fadd(l: Float, r: Complex) -> Complex { Complex(l +. r.re, r.im) }

/// Subtract a `Complex` from another.
pub fn sub(l: Complex, r: Complex) -> Complex {
  Complex(l.re -. r.re, l.im -. r.im)
}

/// Subtract a `Float` from a `Complex`. The `Float` is treated as a real
/// value.
pub fn subf(l: Complex, r: Float) -> Complex { Complex(l.re -. r, l.im) }

/// Subtract a `Complex` from a `Float`. The `Float` is treated as a real
/// value.
pub fn fsub(l: Float, r: Complex) -> Complex {
  Complex(l -. r.re, float.neg(r.im))
}

/// Multiply a `Complex` by another.
pub fn mul(l: Complex, r: Complex) -> Complex {
  Complex(
    l.re *. r.re -. l.im *. r.im,
    l.re *. r.im +. l.im *. r.re,
  )
}

/// Multiply a `Complex` by a `Float`. The `Float` is treated as a real value.
pub fn mulf(l: Complex, r: Float) -> Complex { Complex(l.re *. r, l.im *. r) }

/// Multiply a `Float` by a `Complex`. The `Float` is treated as a real value.
pub fn fmul(l: Float, r: Complex) -> Complex { Complex(l *. r.re, l *. r.im) }

/// Divide a `Complex` by another.
pub fn div(l: Complex, r: Complex) -> Complex {
  of_polar(float.div(norm(l), norm(r)), arg(l) -. arg(r))
}

/// Divide a `Complex` by a `Float`. The `Float` is treated as a real value.
pub fn divf(l: Complex, r: Float) -> Complex {
  Complex(float.div(l.re, r), float.div(l.im, r))
}

/// Divide a `Float` by a `Complex`. The `Float` is treated as a real value.
pub fn fdiv(l: Float, r: Complex) -> Complex {
  case float.cmp(l, 0.0) {
    Eq -> Complex(0.0, 0.0)
    Gt -> of_polar(l /. norm(r), float.neg(arg(r)))
    Lt -> of_polar(float.abs(l) /. norm(r), float.pi -. arg(r))
  }
}

/// Return the modulus of `z` as a `Complex`.
///
/// See also [`norm`](#norm).
pub fn abs(z: Complex) -> Complex { norm(z) |> of_float }

/// Modulus of a difference.
pub fn abs_sub(l: Complex, r: Complex) -> Complex { abs(sub(l, r)) }

/// Arc cosine.
pub fn acos(z: Complex) -> Complex {
  mul(
    neg(i),
    ln(
      add(
        mul(
          i,
          sqrt(
            fsub(
              1.0,
              mul(z, z)
            )
          )
        ),
        z
      )
    )
  )
}

/// Inverse hyperbolic cosine.
pub fn acosh(z: Complex) -> Complex {
  fmul(
    2.0,
    ln(
      add(
        sqrt(
          divf(
            addf(z, 1.0),
            2.0
          )
        ),
        sqrt(
          divf(
            subf(z, 1.0),
            2.0
          )
        )
      )
    )
  )
}

/// Argument.
///
/// The output of this function is also called the phase angle.
pub fn arg(z: Complex) -> Float {
  case float.cmp(z.re, 0.0), float.cmp(z.im, 0.0) {
    Eq, Eq -> 0.0
    Eq, Gt -> float.pi /. 2.0
    Eq, Lt -> float.neg(float.pi /. 2.0)
    _, _ -> float.atan2(z.im, z.re)
  }
}

/// Arc sine.
pub fn asin(z: Complex) -> Complex {
  mul(neg(i), ln(add(sqrt(fsub(1.0, mul(z, z))), mul(i, z))))
}

/// Inverse hyperbolic sine.
pub fn asinh(z: Complex) -> Complex {
  ln(add(z, sqrt(fadd(1.0, mul(z, z)))))
}

/// Arc tangent.
pub fn atan(z: Complex) -> Complex {
  case eq(z, i), eq(z, neg(i)) {
    True, _ | _, True -> panic as "invalid argument in atan"
    _, _ -> {
      div(
        sub(
          ln(
            fadd(
              1.0,
              mul(i, z)
            )
          ),
          ln(
            fsub(
              1.0,
              mul(i, z)
            )
          )
        ),
        fmul(
          2.0,
          i
        )
      )
    }
  }
}

/// Inverse hyperbolic tangent.
pub fn atanh(z: Complex) -> Complex {
  case eq(z, Complex(1.0, 0.0)), eq(z, Complex(-1.0, 0.0)) {
    True, _ | _, True -> panic as "invalid argument in atanh"
    _, _ -> {
      divf(
        sub(
          ln(
            fadd(1.0, z)
          ),
          ln(
            fsub(1.0, z)
          ),
        ),
        2.0,
      )
    }
  }
}

/// Create a `Complex` of unit modulus and argument `ph`.
pub fn cis(ph: Float) -> Complex { Complex(float.cos(ph), float.sin(ph)) }

/// Cosine.
pub fn cos(z: Complex) -> Complex {
  Complex(
    float.cos(z.re) *. float.cosh(z.im),
    float.neg(float.sin(z.re) *. float.sinh(z.im)),
  )
}

/// Hyperbolic cosine.
pub fn cosh(z: Complex) -> Complex {
  Complex(
    float.cosh(z.re) *. float.cos(z.im),
    float.sinh(z.re) *. float.sin(z.im),
  )
}

/// Exponential function.
pub fn exp(z: Complex) -> Complex { of_polar(float.exp(z.re), z.im) }

/// Raise a real base to a complex power.
pub fn fexp(b: Float, z: Complex) -> Complex {
  case eq(z, Complex(0.0, 0.0)) {
    True -> Complex(0.0, 0.0)
    False -> of_polar(float.pow(b, z.re), z.im *. float.ln(b))
  }
}

/// Exponential function base 2.
pub fn exp2(z: Complex) -> Complex {
  of_polar(float.exp2(z.re), float.ln2 *. z.im)
}

/// Exponeential function base 10.
pub fn exp10(z: Complex) -> Complex {
  of_polar(float.exp10(z.re), float.ln10 *. z.im)
}

/// Natural logarithm.
pub fn ln(z: Complex) -> Complex { Complex(float.ln(norm(z)), arg(z)) }

/// Logarithm with aribtrary real base.
pub fn log(z: Complex, base: Float) -> Complex {
  Complex(float.log(norm(z), base), arg(z) /. float.ln(base))
}

/// Base-10 logarithm.
pub fn log10(z: Complex) -> Complex { divf(ln(z), float.ln10) }

/// Base-2 logarithm.
pub fn log2(z: Complex) -> Complex { divf(ln(z), float.ln2) }

/// Modulus of `z`.
pub fn norm(z: Complex) -> Float { float.sqrt(z.re *. z.re +. z.im *. z.im) }

/// Raise a base `z` to an exponent `e`.
pub fn pow(z: Complex, e: Complex) -> Complex {
  case eq(e, Complex(0.0, 0.0)) {
    True -> Complex(1.0, 0.0)
    False -> exp(mul(e, ln(z)))
  }
}

/// Raise a complex base to a real exponent.
pub fn powf(z: Complex, e: Float) -> Complex {
  case float.eq(e, 0.0) {
    True -> Complex(1.0, 0.0)
    False -> of_polar(float.pow(norm(z), e), e *. arg(z))
  }
}

/// Sine.
pub fn sin(z: Complex) -> Complex {
  Complex(
    float.sin(z.re) *. float.cosh(z.im),
    float.cos(z.re) *. float.sinh(z.im),
  )
}

/// Hyperbolic sine.
pub fn sinh(z: Complex) -> Complex {
  Complex(
    float.sinh(z.re) *. float.cos(z.im),
    float.cosh(z.re) *. float.sin(z.im),
  )
}

/// Square root.
pub fn sqrt(z: Complex) -> Complex {
  case float.cmp(z.re, 0.0), float.cmp(z.im, 0.0) {
    Gt, Eq -> Complex(float.sqrt(z.re), z.im)
    _,  Eq -> Complex(0.0, float.sqrt(float.neg(z.re)))
    Eq, Gt -> {
      let x = float.sqrt(float.abs(z.im) /. 2.0)
      Complex(x, x)
    }
    Eq, _ -> {
      let x = float.sqrt(float.abs(z.im) /. 2.0)
      Complex(x, float.neg(x))
    }
    _, _ -> Complex(float.sqrt(norm(z)), arg(z) /. 2.0)
  }
}

/// Tangent.
pub fn tan(z: Complex) -> Complex {
  let two_re = z.re +. z.re
  let two_im = z.im +. z.im
  let denom = float.cos(two_re) +. float.cosh(two_im)
  Complex(
    float.sin(two_re) /. denom,
    float.sinh(two_im) /. denom,
  )
}

/// Hyperbolic tangent.
pub fn tanh(z: Complex) -> Complex {
  let two_re = z.re +. z.re
  let two_im = z.im +. z.im
  let denom = float.cosh(two_re) +. float.cos(two_im)
  Complex(
    float.sinh(two_re) /. denom,
    float.sin(two_im) /. denom,
  )
}

