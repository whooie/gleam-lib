import gleam/int
import gleam/iterator.{ type Iterator, type Step, Next, Done }
import gleam/option.{ type Option, Some, None }
import gleam/order.{ type Order }
import whooie/float
import whooie/phys/spinproj.{ type SpinProj }
import whooie/phys/spintotal.{ type SpinTotal }
import whooie/res.{ unwrap }

fn rem(x: Int, m: Int) -> Int { int.remainder(x, m) |> unwrap } 

fn product(it_a: Iterator(a), it_b: Iterator(b)) -> Iterator(#(a, b)) {
  it_a
  |> iterator.flat_map(fn(a) {
    it_b |> iterator.map(fn(b) { #(a, b) })
  })
}

/// Main type, representing a total spin-spin projection pair.
pub type Spin {
  Spin(tot: SpinTotal, proj: SpinProj)
}

pub type SpinError {
  InvalidProj(String)
  InvalidRaise(String)
  InvalidLower(String)
}

pub type SpinResult(a) = Result(a, SpinError)

/// Create a new `Spin`, checking that the projection number does not exceed
/// the total in magnitude, and that both numbers have the same parity.
pub fn new(tot: SpinTotal, proj: SpinProj) -> SpinResult(Spin) {
  let j = spintotal.halves(tot)
  let m = spinproj.halves(proj)
  case rem(j, 2) != int.absolute_value(rem(m, 2)) {
    True -> {
      let msg =
        "spin numbers ("
        <> spintotal.to_string(tot)
        <> ", "
        <> spinproj.to_string(proj)
        <> ") must have equal parity"
      Error(InvalidProj(msg))
    }
    False -> case m < -j || m > j {
      True -> {
        let msg =
          "spin projection "
          <> spinproj.to_string(proj)
          <> " must not exceed total "
          <> spintotal.to_string(tot)
          <> " in magnitude"
        Error(InvalidProj(msg))
      }
      False -> Ok(Spin(tot, proj))
    }
  }
}

pub type StretchedState {
  Pos
  Neg
}

/// Create a new streteched state where the projection number is equal to the
/// total in magnitude and oriented in the *+z* direction.
pub fn new_stretched_pos(tot: SpinTotal) -> Spin {
  new(tot, spinproj.new(spintotal.halves(tot))) |> unwrap
}

/// Create a new stretched state where the projection number is equal to the
/// total in magnitude and oriented in the *-z* direction.
pub fn new_stretched_neg(tot: SpinTotal) -> Spin {
  new(tot, spinproj.new(-spintotal.halves(tot))) |> unwrap
}

/// Create a new stretched state.
pub fn new_stretched(tot: SpinTotal, dir: StretchedState) -> Spin {
  case dir {
    Pos -> new_stretched_pos(tot)
    Neg -> new_stretched_neg(tot)
  }
}

/// Create a new `Spin` from raw halves.
pub fn of_halves(tot: Int, proj: Int) -> SpinResult(Spin) {
  new(spintotal.new(tot), spinproj.new(proj))
}

/// Create a new `Spin` from a pair of raw halves.
pub fn of_halves_pair(tp: #(Int, Int)) -> SpinResult(Spin) {
  let #(tot, proj) = tp
  of_halves(tot, proj)
}

/// Convert to a pair of ordinary floating-point values.
pub fn to_floats(s: Spin) -> #(Float, Float) {
  #(spintotal.to_float(s.tot), spinproj.to_float(s.proj))
}

/// Generate a string representation.
///
/// This should be used only for display purposes.
pub fn to_string(s: Spin) -> String {
  "(" <> spintotal.to_string(s.tot) <> ", " <> spinproj.to_string(s.proj) <> ")"
}

/// Return the quantum numbers as a pair of bare halves.
pub fn halves(s: Spin) -> #(Int, Int) {
  #(spintotal.halves(s.tot), spinproj.halves(s.proj))
}

/// Return `True` if `s` is a stretched state pointing in the *-z* direction.
pub fn is_stretched_neg(s: Spin) -> Bool {
  spinproj.halves(s.proj) == -spintotal.halves(s.tot)
}

/// Return `True` if `s` is a stretched state pointing in the *+z* direction.
pub fn is_stretched_pos(s: Spin) -> Bool {
  spinproj.halves(s.proj) == spintotal.halves(s.tot)
}

/// Return `True` if `s` is a stretched state.
pub fn is_stretched(s: Spin) -> Bool {
  int.absolute_value(spinproj.halves(s.proj)) == spintotal.halves(s.tot)
}

/// Flip the sign of the projection number.
pub fn refl(s: Spin) -> Spin { Spin(s.tot, spinproj.refl(s.proj)) }

/// Increase the projection number by 1 if `s` is not already a positively
/// stretched state.
pub fn raise(s: Spin) -> SpinResult(Spin) {
  case is_stretched_pos(s) {
    True -> {
      let msg = "cannot raise stretched state " <> to_string(s)
      Error(InvalidRaise(msg))
    }
    False -> Ok(Spin(s.tot, spinproj.raise(s.proj)))
  }
}

/// Decrease the projection number by 1 if `s` is not already a negatively
/// stretched state.
pub fn lower(s: Spin) -> SpinResult(Spin) {
  case is_stretched_neg(s) {
    True -> {
      let msg = "cannot lower stretched state " <> to_string(s)
      Error(InvalidLower(msg))
    }
    False -> Ok(Spin(s.tot, spinproj.lower(s.proj)))
  }
}

/// Return an iterator over all available spin states with total spin `tot`,
/// starting from most negative.
pub fn projections(tot: SpinTotal) -> Iterator(Spin) {
  iterator.unfold(
    new_stretched_neg(tot),
    fn(prev: Spin) -> Step(Spin, Spin) {
      case raise(prev) {
        Ok(next) -> Next(next, next)
        Error(_) -> Done
      }
    }
  )
}

/// Return an iterator over all available spin states with total spin `tot`,
/// starting from most positive.
pub fn projections_rev(tot: SpinTotal) -> Iterator(Spin) {
  iterator.unfold(
    new_stretched_pos(tot),
    fn(prev: Spin) -> Step(Spin, Spin) {
      case lower(prev) {
        Ok(next) -> Next(next, next)
        Error(_) -> Done
      }
    }
  )
}

/// Compare the projection numbers of two states if they have the same total
/// spin.
pub fn cmp(l: Spin, r: Spin) -> Option(Order) {
  case #(spintotal.cmp(l.tot, r.tot), spinproj.cmp(l.proj, r.proj)) {
    #(order.Eq, o) -> Some(o)
    _ -> None
  }
}

fn do_factorial(acc: Int, n: Int) -> Int {
  case n <= 1 {
    True -> 1
    False -> do_factorial(acc * n, n - 1)
  }
}

fn factorial_float(n: Int) -> Float { do_factorial(1, n) |> int.to_float }

/// Compute the Clebsch-Gordan coefficient `⟨jm1, jm2∣jm12⟩`.
pub fn cg(jm1: Spin, jm2: Spin, jm12: Spin) -> Float {
  let #(j1, m1) = halves(jm1)
  let #(j2, m2) = halves(jm2)
  let #(j12, m12) = halves(jm12)
  case m1 + m2 != m12 || rem(j1 + j2, 2) != rem(j12, 2) {
    True -> 0.0
    False -> {
      let kmin =
        0
        |> int.max(-{ j12 - j2 + m1 } / 2)
        |> int.max(-{ j12 - j1 - m2 } / 2)
      let kmax =
        { { j1 + j2 - j12 } / 2 }
        |> int.min({ j1 - m1 } / 2)
        |> int.min({ j2 + m2 } / 2)
      case kmax < kmin {
        True -> 0.0
        False -> {
          let summand = fn(k: Int) -> Float {
            let sign =
              case int.is_even(k) {
                True -> 1.0
                False -> -1.0
              }
            let f_k = factorial_float(k)
            let f_j1_pj2_mj12_mk = factorial_float({ j1 + j2 - j12 } / 2 - k)
            let f_j1_mm1_mk = factorial_float({ j1 - m1 } / 2 - k)
            let f_j2_pm2_mk = factorial_float({ j2 + m2 } / 2 - k)
            let f_j12_mj2_pm1_pk = factorial_float({ j12 - j2 + m1 } / 2 + k)
            let f_j12_mj1_mm2_pk = factorial_float({ j12 - j1 - m2 } / 2 + k)
            sign
              /. f_k
              /. f_j1_pj2_mj12_mk
              /. f_j1_mm1_mk
              /. f_j2_pm2_mk
              /. f_j12_mj2_pm1_pk
              /. f_j12_mj1_mm2_pk
          }
          let sum =
            iterator.range(kmin, kmax)
            |> iterator.map(summand)
            |> iterator.fold(0.0, float.add)
          let j12t2_p1 = j12 + 1 |> int.to_float
          let f_j12_pj1_mj2 = factorial_float({ j12 + j1 - j2 } / 2)
          let f_j12_mj1_pj2 = factorial_float({ j12 - j1 + j2 } / 2)
          let f_j1_pj2_mj12 = factorial_float({ j1 + j2 - j12 } / 2)
          let f_j1_pj2_pj12_p1 = factorial_float({ j1 + j2 + j12 } / 2 + 1)
          let f_j12_pm12 = factorial_float({ j12 + m12 } / 2)
          let f_j12_mm12 = factorial_float({ j12 - m12 } / 2)
          let f_j1_mm1 = factorial_float({ j1 - m1 } / 2)
          let f_j1_pm1 = factorial_float({ j1 + m1 } / 2)
          let f_j2_mm2 = factorial_float({ j2 - m2 } / 2)
          let f_j2_pm2 = factorial_float({ j2 + m2 } / 2)
          float.sqrt(
            j12t2_p1
            *. f_j12_pj1_mj2
            *. f_j12_mj1_pj2
            *. f_j1_pj2_mj12
            *. f_j12_pm12
            *. f_j12_mm12
            *. f_j1_mm1
            *. f_j1_pm1
            *. f_j2_mm2
            *. f_j2_pm2
            /. f_j1_pj2_pj12_p1
          ) *. sum
        }
      }
    }
  }
}

/// Return `True` if `jm1`, `jm2`, and `jm3` satisfy the selection rules of the
/// Wigner 3*j* symbol `(jm1 jm2 jm3)`.
pub fn w3j_sel(jm1: Spin, jm2: Spin, jm3: Spin) -> Bool {
  let #(j1, m1) = halves(jm1)
  let #(j2, m2) = halves(jm2)
  let #(j3, m3) = halves(jm3)
  m1 + m2 + m3 == 0
    && int.absolute_value(j1 - j2) <= j3
    && j3 <= j1 + j2
    && !{ m1 == 0 && m2 == 0 && m3 == 0 } && int.is_even({ j1 + j2 + j3 } / 2)
}

/// Compute the Wigner 3*j* symbol `(jm1 jm2 jm3)`.
pub fn w3j(jm1: Spin, jm2: Spin, jm3: Spin) -> Float {
  case !w3j_sel(jm1, jm2, jm3) {
    True -> 0.0
    False -> {
      let j1 = spintotal.halves(jm1.tot)
      let j2 = spintotal.halves(jm2.tot)
      let j3 = spintotal.halves(jm3.tot)
      let m3 = spinproj.halves(jm3.proj)
      let sign =
        case int.is_even({ j1 - j2 - m3 } / 2) {
          True -> 1.0
          False -> -1.0
        }
      let denom = float.sqrt(int.to_float(j3) +. 1.0)
      let cg = cg(jm1, jm2, refl(jm3))
      sign *. cg /. denom
    }
  }
}

/// Compute the Wigner 6*j* symbol `{j1 j2 j3; j4 j5 j6}` (where `j1`, ...,
/// `j3` are in the top row).
pub fn w6j(
  j1: SpinTotal,
  j2: SpinTotal,
  j3: SpinTotal,
  j4: SpinTotal,
  j5: SpinTotal,
  j6: SpinTotal,
) -> Float
{
  let term_filter = fn(jm: #(#(#(#(#(Spin, Spin), Spin), Spin), Spin), Spin)) {
    let #(#(#(#(#(jm1, jm2), jm3), jm4), jm5), jm6) = jm
    w3j_sel(refl(jm1), refl(jm2), refl(jm3))
      && w3j_sel(jm1, refl(jm5), jm6)
      && w3j_sel(jm4, jm2, refl(jm6))
      && w3j_sel(refl(jm4), jm5, jm3)
  }
  let sign = fn(jm: #(#(#(#(#(Spin, Spin), Spin), Spin), Spin), Spin)) {
    let #(#(#(#(#(jm1, jm2), jm3), jm4), jm5), jm6) = jm
    let #(j1, m1) = halves(jm1)
    let #(j2, m2) = halves(jm2)
    let #(j3, m3) = halves(jm3)
    let #(j4, m4) = halves(jm4)
    let #(j5, m5) = halves(jm5)
    let #(j6, m6) = halves(jm6)
    let k = { j1 - m1 + j2 - m2 + j3 - m3 + j4 - m4 + j5 - m5 + j6 - m6 } / 2
    case int.is_even(k) {
      True -> 1.0
      False -> -1.0
    }
  }
  let term_map = fn(jm: #(#(#(#(#(Spin, Spin), Spin), Spin), Spin), Spin)) {
    let #(#(#(#(#(jm1, jm2), jm3), jm4), jm5), jm6) = jm
    sign(jm)
      *. w3j(refl(jm1), refl(jm2), refl(jm3))
      *. w3j(jm1, refl(jm5), jm6)
      *. w3j(jm4, jm2, refl(jm6))
      *. w3j(refl(jm4), jm5, jm3)
  }
  projections(j1)
  |> product(projections(j2))
  |> product(projections(j3))
  |> product(projections(j4))
  |> product(projections(j5))
  |> product(projections(j6))
  |> iterator.filter(term_filter)
  |> iterator.map(term_map)
  |> iterator.fold(0.0, float.add)
}

