import gleam/int
import gleam/order.{ type Order }

/// Main type representing a total-spin quantum number.
///
/// Operations are performed using integer arithmetic.
pub opaque type SpinTotal {
  SpinTotal(j: Int)
}

/// Create a new `SpinTotal` from the number of half-spin quanta.
///
/// Negative values are disallowed.
pub fn new(halves: Int) -> SpinTotal {
  case halves < 0 {
    True -> panic as "total spin number must be positive"
    False -> SpinTotal(halves)
  }
}

/// Return the quantum number as a bare number of halves.
pub fn halves(j: SpinTotal) -> Int { j.j }

/// Return the quantum number as an ordinary floating-point value.
pub fn to_float(j: SpinTotal) -> Float { int.to_float(j.j) /. 2.0 }

/// Generate a string representation.
///
/// This should be used only for display purposes.
pub fn to_string(j: SpinTotal) -> String {
  case int.is_even(j.j) {
    True -> "tot:" <> int.to_string(j.j / 2)
    False -> "tot:" <> int.to_string(j.j) <> "/2"
  }
}

/// Compare two projection numbers.
pub fn cmp(l: SpinTotal, r: SpinTotal) -> Order { int.compare(l.j, r.j) }

