import gleam/int
import gleam/order.{ type Order }

/// Main type representing a spin-projection quantum number.
///
/// Operations are performed using integer arithmetic.
pub opaque type SpinProj {
  SpinProj(m: Int)
}

/// Create a new `SpinProj` from the number of half-spin quanta.
pub fn new(halves: Int) -> SpinProj { SpinProj(halves) }

/// Flip the sign of a projection number.
pub fn refl(m: SpinProj) -> SpinProj { SpinProj(-m.m) }

/// Increase the projection number by 1.
pub fn raise(m: SpinProj) -> SpinProj { SpinProj(m.m + 2) }

/// Decrease the projection number by 1.
pub fn lower(m: SpinProj) -> SpinProj { SpinProj(m.m - 2) }

/// Return the quantum number as a bare number of halves.
pub fn halves(m: SpinProj) -> Int { m.m }

/// Return the quantum number as an ordinary floating-point value.
pub fn to_float(m: SpinProj) -> Float { int.to_float(m.m) /. 2.0 }

/// Generate a string representation.
///
/// This should be used only for display purposes.
pub fn to_string(m: SpinProj) -> String {
  case int.is_even(m.m) {
    True -> "proj:" <> int.to_string(m.m / 2)
    False -> "proj:" <> int.to_string(m.m) <> "/2"
  }
}

/// Compare two projection numbers.
pub fn cmp(l: SpinProj, r: SpinProj) -> Order { int.compare(l.m, r.m) }

