import whooie/float
import gleam/order

fn square(x: Float) -> Float { x *. x }

/// Main type. In order to ensure that error values are always positive, use
/// [`new`](#new) to construct values.
pub opaque type ExpVal {
  ExpVal(v: Float, e: Float)
}

/// Create a new `ExpVal`.
pub fn new(v: Float, e: Float) -> ExpVal { ExpVal(v, float.abs(e)) }

/// Get the mean value of an `ExpVal`.
pub fn val(x: ExpVal) -> Float { x.v }

/// Get the error value of an `ExpVal`.
pub fn err(x: ExpVal) -> Float { x.e }

/// Convert from a `Float`, setting the associated error to zero.
pub fn of_float(v: Float) -> ExpVal { ExpVal(v, 0.0) }

/// Convert from an ordinary `#(Float, Float)` pair, taking the second item to
/// be the error value.
pub fn of_pair(ev: #(Float, Float)) -> ExpVal { new(ev.0, ev.1) }

/// Compare two `ExpVal`s by only their mean values.
pub fn compare(l: ExpVal, r: ExpVal) -> order.Order { float.cmp(l.v, r.v) }

/// Return `True` if the output of [`compare`](#compare) is `Eq`.
pub fn eq(l: ExpVal, r: ExpVal) -> Bool {
  case compare(l, r) {
    order.Eq -> True
    _ -> False
  }
}

/// Return `True` if the output of [`compare`](#compare) is not `Eq`.
pub fn neq(l: ExpVal, r: ExpVal) -> Bool { !eq(l, r) }

/// Return `True` if the output of [`compare`](#compare) is `Gt`.
pub fn gt(l: ExpVal, r: ExpVal) -> Bool {
  case compare(l, r) {
    order.Gt -> True
    _ -> False
  }
}

/// Return `True` if the output of [`compare`](#compare) is `Lt`.
pub fn lt(l: ExpVal, r: ExpVal) -> Bool {
  case compare(l, r) {
    order.Lt -> True
    _ -> False
  }
}

/// Return `True` if the output of [`compare`](#compare) is `Gt` or `Eq`.
pub fn geq(l: ExpVal, r: ExpVal) -> Bool {
  case compare(l, r) {
    order.Gt | order.Eq -> True
    _ -> False
  }
}

/// Return `True` if the output of [`compare`](#compare) is `Lt` or `Eq`.
pub fn leq(l: ExpVal, r: ExpVal) -> Bool {
  case compare(l, r) {
    order.Lt | order.Eq -> True
    _ -> False
  }
}

/// Return the additive inverse of `ev`.
pub fn neg(x: ExpVal) -> ExpVal { ExpVal(float.neg(x.v), x.e) }

/// Add an `ExpVal` to another.
pub fn add(l: ExpVal, r: ExpVal) -> ExpVal {
  ExpVal(
    l.v +. r.v,
    float.sqrt(square(l.e) +. square(r.e)),
  )
}

/// Add an `ExpVal` to a `Float`.
pub fn addf(l: ExpVal, r: Float) -> ExpVal { ExpVal(l.v +. r, l.e) }

/// Add a `Float` to an `ExpVal`.
pub fn fadd(l: Float, r: ExpVal) -> ExpVal { ExpVal(l +. r.v, r.e) }

/// Subtract an `ExpVal` from another.
pub fn sub(l: ExpVal, r: ExpVal) -> ExpVal {
  ExpVal(
    l.v -. r.v,
    float.sqrt(square(l.e) +. square(r.e)),
  )
}

/// Subtract a `Float` from an `ExpVal`.
pub fn subf(l: ExpVal, r: Float) -> ExpVal { ExpVal(l.v -. r, l.e) }

/// Subtract an `ExpVal` from a `Float`.
pub fn fsub(l: Float, r: ExpVal) -> ExpVal { ExpVal(l -. r.v, r.e) }

/// Multiply an `ExpVal` by another.
pub fn mul(l: ExpVal, r: ExpVal) -> ExpVal {
  ExpVal(
    l.v *. r.v,
    float.sqrt(square(l.e *. r.v) +. square(l.v *. r.e)),
  )
}

/// Multiply an `ExpVal` by a `Float`.
pub fn mulf(l: ExpVal, r: Float) -> ExpVal {
  ExpVal(
    l.v *. r,
    l.e *. float.abs(r),
  )
}

/// Multiply a `Float` by an `ExpVal`.
pub fn fmul(l: Float, r: ExpVal) -> ExpVal {
  ExpVal(
    l *. r.v,
    float.abs(l) *. r.e,
  )
}

/// Divide an `ExpVal` by another.
pub fn div(l: ExpVal, r: ExpVal) -> ExpVal {
  ExpVal(
    l.v /. r.v,
    float.sqrt(square(l.e /. r.v) +. square(r.e *. l.v /. square(r.v)))
  )
}

/// Divide an `ExpVal` by a `Float`.
pub fn divf(l: ExpVal, r: Float) -> ExpVal {
  ExpVal(
    l.v /. r,
    l.e /. float.abs(r),
  )
}

/// Divide a `Float` by an `ExpVal`.
pub fn fdiv(l: Float, r: ExpVal) -> ExpVal {
  ExpVal(
    l /. r.v,
    r.e *. float.abs(l) /. square(r.v),
  )
}

/// Absolute value.
pub fn abs(x: ExpVal) -> ExpVal { ExpVal(float.abs(x.v), x.e) }

/// Absolute difference.
pub fn abs_sub(l: ExpVal, r: ExpVal) -> ExpVal {
  ExpVal(
    float.abs(l.v -. r.v),
    float.sqrt(square(l.e) +. square(r.e))
  )
}

/// Arc cosine.
pub fn acos(x: ExpVal) -> ExpVal {
  ExpVal(
    float.acos(x.v),
    x.e /. float.sqrt(1.0 -. square(x.v)),
  )
}

/// Inverse hyperbolic cosine.
pub fn acosh(x: ExpVal) -> ExpVal {
  ExpVal(
    float.acosh(x.v),
    x.e /. float.sqrt(square(x.v) -. 1.0),
  )
}

/// Arc sine.
pub fn asin(x: ExpVal) -> ExpVal {
  ExpVal(
    float.asin(x.v),
    x.e /. float.sqrt(1.0 -. square(x.v)),
  )
}

/// Inverse hyperbolic sine.
pub fn asinh(x: ExpVal) -> ExpVal {
  ExpVal(
    float.asinh(x.v),
    x.e /. float.sqrt(square(x.v) +. 1.0),
  )
}

/// Arc tangent.
pub fn atan(x: ExpVal) -> ExpVal {
  ExpVal(
    float.atan(x.v),
    x.e /. { square(x.v) +. 1.0 },
  )
}

/// Arc tangent with quadrant correction.
pub fn atan2(y: ExpVal, x: ExpVal) -> ExpVal {
  ExpVal(
    float.atan2(y.v, x.v),
    float.sqrt(square(y.v *. x.e) +. square(y.e *. x.v))
      /. float.sqrt(square(y.v) +. square(x.v)),
  )
}

/// Inverse hyperbolic tangent.
pub fn atanh(x: ExpVal) -> ExpVal {
  ExpVal(
    float.atanh(x.v),
    x.e /. float.abs(square(x.v) -. 1.0),
  )
}

/// Ceiling function.
pub fn ceil(x: ExpVal) -> ExpVal { ExpVal(float.ceil(x.v), 0.0) }

/// Cosine.
pub fn cos(x: ExpVal) -> ExpVal {
  ExpVal(
    float.cos(x.v),
    x.e *. float.abs(float.sin(x.v)),
  )
}

/// Hyperbolic cosine.
pub fn cosh(x: ExpVal) -> ExpVal {
  ExpVal(
    float.cosh(x.v),
    x.e *. float.abs(float.sinh(x.v)),
  )
}

/// Exponential function.
pub fn exp(x: ExpVal) -> ExpVal {
  let ex = float.exp(x.v)
  ExpVal(
    ex,
    x.e *. ex,
  )
}

/// Exponential function with base 2.
pub fn exp2(x: ExpVal) -> ExpVal {
  let ex2 = float.exp2(x.v)
  ExpVal(
    ex2,
    x.e *. float.ln2 *. ex2,
  )
}

/// Exponential function with base 10.
pub fn exp10(x: ExpVal) -> ExpVal {
  let ex10 = float.exp10(x.v)
  ExpVal(
    ex10,
    x.e *. float.ln10 *. ex10,
  )
}

/// Floor function.
pub fn floor(x: ExpVal) -> ExpVal { ExpVal(float.floor(x.v), 0.0) }

/// Natural logarithm.
pub fn ln(x: ExpVal) -> ExpVal {
  ExpVal(
    float.ln(x.v),
    x.e /. float.abs(x.v),
  )
}

/// Logarithm.
pub fn log(x: ExpVal, base: ExpVal) -> ExpVal {
  let v = float.log(x.v, base.v)
  ExpVal(
    v,
    float.sqrt(
      {
        square(x.e /. x.v)
        +. square(base.e *. v /. base.v)
      } /. square(float.ln(base.v))
    ),
  )
}

/// Base-10 logarithm.
pub fn log10(x: ExpVal) -> ExpVal {
  ExpVal(
    float.log10(x.v),
    x.e /. float.ln10 /. float.abs(x.v),
  )
}

/// Base-2 logarithm.
pub fn log2(x: ExpVal) -> ExpVal {
  ExpVal(
    float.log2(x.v),
    x.e /. float.ln2 /. float.abs(x.v),
  )
}

/// Maximum by mean value only.
pub fn max(x: ExpVal, y: ExpVal) -> ExpVal {
  case compare(x, y) {
    order.Eq -> x
    order.Lt -> y
    order.Gt -> x
  }
}

/// Minimum by mean value only.
pub fn min(x: ExpVal, y: ExpVal) -> ExpVal {
  case compare(x, y) {
    order.Eq -> x
    order.Lt -> x
    order.Gt -> y
  }
}

/// Raise a base to a power.
pub fn pow(base: ExpVal, pow: ExpVal) -> ExpVal {
  ExpVal(
    float.pow(base.v, pow.v),
    float.pow(base.v, pow.v -. 1.0)
      *. float.sqrt(
        square(base.e *. pow.v)
        +. square(pow.e *. base.v *. float.ln(base.v))
      ),
  )
}

/// Round to the nearest integer value.
pub fn round(x: ExpVal) -> ExpVal { ExpVal(float.round(x.v), 0.0) }

/// Sine.
pub fn sin(x: ExpVal) -> ExpVal {
  ExpVal(
    float.sin(x.v),
    x.e *. float.abs(float.cos(x.v)),
  )
}

/// Sine and cosine.
pub fn sin_cos(x: ExpVal) -> #(ExpVal, ExpVal) { #(sin(x), cos(x)) }

/// Hyperbolic sine.
pub fn sinh(x: ExpVal) -> ExpVal {
  ExpVal(
    float.sinh(x.v),
    x.e *. float.cosh(x.v),
  )
}

/// Square root.
pub fn sqrt(x: ExpVal) -> ExpVal {
  let sq = float.sqrt(x.v)
  ExpVal(
    sq,
    x.e /. sq /. 2.0,
  )
}

/// Tangent.
pub fn tan(x: ExpVal) -> ExpVal {
  ExpVal(
    float.tan(x.v),
    x.e /. square(float.cos(x.v)),
  )
}

/// Hyperbolic tangent.
pub fn tanh(x: ExpVal) -> ExpVal {
  ExpVal(
    float.tanh(x.v),
    x.e /. square(float.cosh(x.v)),
  )
}

/// Truncate to just the integer.
pub fn trunc(x: ExpVal) -> ExpVal { ExpVal(float.trunc(x.v), 0.0) }

