import gleam/io
import gleam/option.{ Some }
import whooie/opt

pub fn main() {
  io.debug([Some(0), Some(1), Some(2)] |> opt.collect_list)
}

