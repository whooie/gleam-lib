import gleam/float
import gleam/int
import gleam/order.{ type Order, Eq, Lt, Gt }

/// Machine epsilon for floating-point arithmetic.
pub const epsilon: Float = 2.220446049250313e-16

/// Natural logarithm of 10.
pub const ln10: Float = 2.302585092994046

/// Natural logarithm of 2.
pub const ln2: Float = 0.6931471805599453

/// Circle constant.
pub const pi: Float = 3.141592653589793

/// Euler's number.
pub const e: Float = 2.718281828459045

/// At the time of writing, `gleam/float` does not yet have implementations of
/// several basic floating-point math functions, so they have to be implemented
/// here. We'll calculate them via power series, summing until convergence to
/// the machine-epsilon level.
fn do_series(
  acc: Float,
  n: Int,
  x: Float,
  summand: fn(Int, Float) -> Float
) -> Float
{
  let term = summand(n, x)
  case float.absolute_value(term) <. epsilon {
    True -> acc +. term
    False -> do_series(acc +. term, n + 1, x, summand)
  }
}

fn do_factorial(acc: Int, n: Int) -> Int {
  case n <= 1 {
    True -> 1
    False -> do_factorial(acc * n, n - 1)
  }
}

fn factorial(n: Int) -> Int { do_factorial(1, n) }

fn sq(x: Float) -> Float { x *. x }

/// Compare two `Float`s up to machine epsilon for 64-bit floating-point
/// numbers.
pub fn cmp(l: Float, r: Float) -> Order {
  float.loosely_compare(l, r, epsilon)
}

/// Compare two `Float`s up to an arbitrary positive epsilon.
pub fn cmp_eps(l: Float, r: Float, eps: Float) -> Order {
  case cmp(eps, 0.0) {
    Gt -> float.loosely_compare(l, r, eps)
    _ -> panic as "invalid argument in cmp_eps"
  }
}

/// Return `True` if the output of [`cmp`](#cmp) is `Eq`.
pub fn eq(l: Float, r: Float) -> Bool {
  case cmp(l, r) {
    Eq -> True
    _ -> False
  }
}

/// Return `True` if the output of [`cmp`](#cmp) is not `Eq`.
pub fn neq(l: Float, r: Float) -> Bool {
  case cmp(l, r) {
    Eq -> False
    _ -> True
  }
}

/// Return `True` if the output of [`cmp`](#cmp) is `Lt`.
pub fn lt(l: Float, r: Float) -> Bool {
  case cmp(l, r) {
    Lt -> True
    _ -> False
  }
}

/// Return `True` if the output of [`cmp`](#cmp) is `Gt`.
pub fn gt(l: Float, r: Float) -> Bool {
  case cmp(l, r) {
    Gt -> True
    _ -> False
  }
}

/// Return `True` if the output of [`cmp`](#cmp) is `Lt` or `Eq`.
pub fn leq(l: Float, r: Float) -> Bool {
  case cmp(l, r) {
    Lt | Eq -> True
    _ -> False
  }
}

/// Return `True` if the output of [`cmp`](#cmp) is `Gt` or `Eq`.
pub fn geq(l: Float, r: Float) -> Bool {
  case cmp(l, r) {
    Gt | Eq -> True
    _ -> False
  }
}

/// Return the additive inverse of `x`.
pub fn neg(x: Float) -> Float { float.negate(x) }

/// Add two `Float`s.
pub fn add(l: Float, r: Float) -> Float { l +. r }

/// Subtract two `Float`s.
pub fn sub(l: Float, r: Float) -> Float { l -. r }

/// Multiply two `Float`s.
pub fn mul(l: Float, r: Float) -> Float { l *. r }

/// Divide two `Float`s. Panics if `r` is zero.
pub fn div(l: Float, r: Float) -> Float {
  case eq(r, 0.0) {
    True -> panic as "encountered divide by zero"
    False -> l /. r
  }
}

/// Absolute value.
pub fn abs(x: Float) -> Float { float.absolute_value(x) }

/// Absolute difference.
pub fn abs_sub(l: Float, r: Float) -> Float { float.absolute_value(l -. r) }

/// Arc cosine.
pub fn acos(x: Float) -> Float {
  let summand = fn(n: Int, x: Float) -> Float {
    let f_n = int.to_float(n)
    let f_2np1 = 2.0 *. f_n +. 1.0
    int.to_float(factorial(2 * n))
      /. sq(pow(2.0, f_n) *. f_n)
      *. pow(x, f_2np1)
      /. f_2np1
  }
  let asin =
    case cmp(abs(x), 1.0) {
      Gt -> panic as "invalid argument in acos"
      _ -> do_series(0.0, 0, x, summand)
    }
  pi /. 2.0 -. asin
}

/// Inverse hyperbolic cosine.
pub fn acosh(x: Float) -> Float {
  case cmp(x, 1.0) {
    Lt -> panic as "invalid argument in acosh"
    _ -> ln(x +. sqrt(sq(x) -. 1.0))
  }
}

/// Arc sine.
pub fn asin(x: Float) -> Float {
  let summand = fn(n: Int, x: Float) -> Float {
    let f_n = int.to_float(n)
    let f_2np1 = 2.0 *. f_n +. 1.0
    int.to_float(factorial(2 * n))
      /. sq(pow(2.0, f_n) *. f_n)
      *. pow(x, f_2np1)
      /. f_2np1
  }
  case cmp(abs(x), 1.0) {
    Gt -> panic as "invalid argument in asin"
    _ -> do_series(0.0, 0, x, summand)
  }
}

/// Inverse hyperbolic sine.
pub fn asinh(x: Float) -> Float { ln(x +. sqrt(sq(x) +. 1.0)) }

/// Arc tangent.
pub fn atan(x: Float) -> Float {
  let summand = fn(n: Int, x: Float) -> Float {
    let f_n = int.to_float(n)
    pow(2.0, 2.0 *. f_n)
      *. sq(int.to_float(factorial(n)))
      /. int.to_float(factorial(2 * n + 1))
      *. pow(x, 2.0 *. f_n +. 1.0)
      /. pow(1.0 +. sq(x), f_n +. 1.0)
  }
  do_series(0.0, 0, x, summand)
}

/// Arc tangent with quadrant correction.
pub fn atan2(y: Float, x: Float) -> Float {
  let yx =
    case cmp(abs(x), 0.0) {
      Eq -> panic as "invalid denominator argument in atan2"
      _ -> y /. x
    }
  let z = atan(yx)
  case #(cmp(y, 0.0), cmp(x, 0.0)) {
    #(Gt, Lt) -> z +. pi
    #(Lt, Lt) -> z -. pi
    #(Eq, Lt) -> z +. pi
    _ -> z
  }
}

/// Inverse hyperbolic tangent.
pub fn atanh(x: Float) -> Float {
  case cmp(abs(x), 1.0) {
    Lt -> ln({ 1.0 +. x } /. { 1.0 -. x }) /. 2.0
    _ -> panic as "invalid argument in atanh"
  }
}

/// Ceiling function.
pub fn ceil(x: Float) -> Float { float.ceiling(x) }

/// Cosine.
pub fn cos(x: Float) -> Float {
  let summand = fn(n: Int, x: Float) -> Float {
    let sign =
      case int.is_even(n) {
        True -> 1.0
        False -> -1.0
      }
    let denom = int.to_float(factorial(2 * n))
    let num = pow(x, 2.0 *. int.to_float(n))
    sign *. num /. denom
  }
  do_series(0.0, 0, x, summand)
}

/// Hyperbolic cosine.
pub fn cosh(x: Float) -> Float { { exp(x) +. exp(neg(x)) } /. 2.0 }

/// Exponential function.
pub fn exp(x: Float) -> Float { pow(e, x) }

/// Exponential (base 2).
pub fn exp2(x: Float) -> Float { pow(2.0, x) }

/// Exponential (base 10).
pub fn exp10(x: Float) -> Float { pow(10.0, x) }

/// Floor function.
pub fn floor(x: Float) -> Float { float.floor(x) }

/// Natural logarithm.
pub fn ln(x: Float) -> Float {
  case cmp(x, 0.0) {
    Gt -> {
      let summand = fn(n: Int, x: Float) -> Float {
        let f_n = int.to_float(n)
        let f_2np1 = 2.0 *. f_n +. 1.0
        { 2.0 /. f_2np1 } *. pow({ x -. 1.0 } /. { x +. 1.0 }, f_2np1)
      }
      do_series(0.0, 0, x, summand)
    }
    _ -> panic as "invalid argument in ln"
  }
}

/// Logarithm to arbitrary base. Requires positive base and argument.
pub fn log(x: Float, base: Float) -> Float {
  case #(cmp(x, 0.0), cmp(base, 0.0)) {
    #(Gt, Gt) -> ln(x) /. ln(base)
    _ -> panic as "invalid argument in log"
  }
}

/// Logarithm base 10.
pub fn log10(x: Float) -> Float {
  case cmp(x, 0.0) {
    Gt -> ln(x) /. ln10
    _ -> panic as "invalid argument in log10"
  }
}

/// Logarithm base 2.
pub fn log2(x: Float) -> Float {
  case cmp(x, 0.0) {
    Gt -> ln(x) /. ln2
    _ -> panic as "invalid argument in log2"
  }
}

/// Maximum of two `Float`s.
pub fn max(x: Float, y: Float) -> Float { float.max(x, y) }

/// Minimum of two `Float`s.
pub fn min(x: Float, y: Float) -> Float { float.min(x, y) }

/// Raise a base to an exponent.
pub fn pow(base: Float, exp: Float) -> Float {
  case float.power(base, exp) {
    Ok(res) -> res
    _ -> panic as "invalid argument in pow"
  }
}

/// Round to the nearest integer.
pub fn round(x: Float) -> Float { float.round(x) |> int.to_float }

/// Sine.
pub fn sin(x: Float) -> Float {
  let summand = fn(n: Int, x: Float) -> Float {
    let sign =
      case int.is_even(n) {
        True -> 1.0
        False -> -1.0
      }
    let denom = int.to_float(factorial(2 * n + 1))
    let num = pow(x, 2.0 *. int.to_float(n) +. 1.0)
    sign *. num /. denom
  }
  do_series(0.0, 0, x, summand)
}

/// Sine and cosine.
pub fn sin_cos(x: Float) -> #(Float, Float) { #(sin(x), cos(x)) }

/// Hyperbolic sine.
pub fn sinh(x: Float) -> Float { { exp(x) -. exp(neg(x)) } /. 2.0 }

/// Square root.
pub fn sqrt(x: Float) -> Float {
  case float.square_root(x) {
    Ok(res) -> res
    _ -> panic as "invalid argument in sqrt"
  }
}

/// Tangent.
pub fn tan(x: Float) -> Float {
  let s = sin(x)
  let c = cos(x)
  case cmp(c, 0.0) {
    Eq -> panic as "invalid argument in tan"
    _ -> s /. c
  }
}

/// Hyperbolic tangent.
pub fn tanh(x: Float) -> Float { sinh(x) /. cosh(x) }

/// Truncate to only the integer part of a `Float`.
pub fn trunc(x: Float) -> Float { float.truncate(x) |> int.to_float }

