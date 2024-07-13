import gleam/iterator.{ type Iterator, Next, Done }
import gleam/option.{ type Option, Some, None }

/// Return `True` if `opt` is `Some`.
pub fn is_some(opt: Option(a)) -> Bool {
  case opt {
    Some(_) -> True
    None -> False
  }
}

/// Return `True` if `opt` is `Some` and the wrapped value satisfies `f`.
pub fn is_some_and(opt: Option(a), f: fn(a) -> Bool) -> Bool {
  case opt {
    Some(a) -> f(a)
    None -> False
  }
}

/// Return `True` if `opt` is `None`.
pub fn is_none(opt: Option(a)) -> Bool {
  case opt {
    Some(_) -> False
    None -> True
  }
}

/// Return `opt_b` if `opt_a` is `Some`, otherwise `None`.
pub fn and(opt_a: Option(a), opt_b: Option(b)) -> Option(b) {
  case opt_a {
    Some(_) -> opt_b
    None -> None
  }
}

/// If `opt` is `Some`, call `f` on the wrapped value, otherwise return the
/// original `None`.
pub fn and_then(opt: Option(a), f: fn(a) -> Option(b)) -> Option(b) {
  case opt {
    Some(a) -> f(a)
    None -> None
  }
}

/// Return `opt_b` if `opt_a` is `None`, otherwise the `Some` value of `opt_a`.
pub fn or(opt_a: Option(a), opt_b: Option(a)) -> Option(a) {
  case opt_a {
    Some(a) -> Some(a)
    None -> opt_b
  }
}

/// If `opt` is `None`, call `f`, otherwise return the original `Some`.
pub fn or_else(opt: Option(a), f: fn() -> Option(a)) -> Option(a) {
  case opt {
    Some(a) -> Some(a)
    None -> f()
  }
}

/// If only one of both arguments is `Some`, return that value, otherwise
/// `None`.
pub fn xor(opt_a: Option(a), opt_b: Option(a)) -> Option(a) {
  case opt_a, opt_b {
    Some(a), None -> Some(a)
    None, Some(b) -> Some(b)
    _, _ -> None
  }
}

/// If both arguments are `Some`, return a `Some` of both wrapped values,
/// otherwise `None`.
pub fn zip(opt_a: Option(a), opt_b: Option(b)) -> Option(#(a, b)) {
  case opt_a, opt_b {
    Some(a), Some(b) -> Some(#(a, b))
    _, _ -> None
  }
}

/// If `opt` is `Some`, split is wrapped value into two `Some`s, otherwise two
/// `None`s.
pub fn unzip(opt: Option(#(a, b))) -> #(Option(a), Option(b)) {
  case opt {
    Some(#(a, b)) -> #(Some(a), Some(b))
    None -> #(None, None)
  }
}

/// Convert a `Some(Some(a))` into `Some(a)`, returning `None` in all other
/// cases.
pub fn flatten(opt: Option(Option(a))) -> Option(a) {
  case opt {
    Some(Some(a)) -> Some(a)
    _ -> None
  }
}

/// If `opt` is `Some`, call `f` on the wrapped value and re-wrap the output as
/// `Some`, otherwise return `None`.
pub fn map(opt: Option(a), f: fn(a) -> b) -> Option(b) {
  case opt {
    Some(a) -> Some(f(a))
    None -> None
  }
}

/// If `opt` is `Some`, call `f` on the wrapped value and return the output,
/// otherwise return `default`.
pub fn map_or(opt: Option(a), f: fn(a) -> b, default: b) -> b {
  case opt {
    Some(a) -> f(a)
    None -> default
  }
}

/// If `opt` is `Some`, call `f_some` on the wrapped value and return the
/// output. If `opt` is `None`, call `f_none` instead.
pub fn map_or_else(
  opt: Option(a),
  f_some: fn(a) -> b,
  f_none: fn() -> b,
) -> b
{
  case opt {
    Some(a) -> f_some(a)
    None -> f_none()
  }
}

/// Convert `opt` to a `Result`, mapping `Some` to `Ok` and `None` to `Error`
/// with the provided error value.
pub fn ok_or(opt: Option(a), err: e) -> Result(a, e) {
  case opt {
    Some(a) -> Ok(a)
    None -> Error(err)
  }
}

/// Convert `opt` to a `Result`, mapping `Some` to `Ok` and `None` to `Error`
/// with the output of `f`.
pub fn ok_or_else(opt: Option(a), f: fn() -> e) -> Result(a, e) {
  case opt {
    Some(a) -> Ok(a)
    None -> Error(f())
  }
}

/// If `opt` is `Some`, return the wrapped value, otherwise return `default`.
pub fn unwrap_or(opt: Option(a), default: a) -> a {
  case opt {
    Some(a) -> a
    None -> default
  }
}

/// If `opt` is `Some`, return the wrapped value, otherwise return the output
/// of `f`.
pub fn unwrap_or_else(opt: Option(a), f: fn() -> a) -> a {
  case opt {
    Some(a) -> a
    None -> f()
  }
}

/// If `opt` is `Some`, return the wrapped value, otherwise panic.
pub fn unwrap(opt: Option(a)) -> a {
  case opt {
    Some(a) -> a
    None -> panic as "called `unwrap` on a `None` value"
  }
}

/// If `opt` is `Some`, return the wrapped value, otherwise panic with a custom
/// message.
pub fn expect(opt: Option(a), msg: String) -> a {
  case opt {
    Some(a) -> a
    None -> panic as msg
  }
}

/// If all items in `items` are `Some`, return a `Some` of all wrapped values,
/// otherwise `None`.
pub fn collect_list(items: List(Option(a))) -> Option(List(a)) {
  case items {
    [] -> Some([])
    [None, ..] -> None
    [Some(item), ..tail] -> {
      case collect_list(tail) {
        None -> None
        Some(rec_res) -> Some([item, ..rec_res])
      }
    }
  }
}

/// If all items in `items` are `Some`, return a `Some` of a `List` of all
/// wrapped values, otherwise `None`.
pub fn collect_list_of_iter(items: Iterator(Option(a))) -> Option(List(a)) {
  case iterator.step(items) {
    Done -> Some([])
    Next(None, _) -> None
    Next(Some(item), rest) -> {
      case collect_list_of_iter(rest) {
        None -> None
        Some(rec_res) -> Some([item, ..rec_res])
      }
    }
  }
}

