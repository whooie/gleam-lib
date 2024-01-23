import whooie/float.{ pi }
import whooie/phys/consts.{ h, c }

/// Saturation parameter needed to achieve a particular Rabi frequency.
pub fn freq_to_saturation(rabi_freq: Float, linewidth: Float) -> Float {
  float.pow(rabi_freq /. linewidth, 2.0) *. 2.0
}

/// Rabi frequency produced by a particular saturation parameter.
pub fn saturation_to_freq(saturation: Float, linewidth: Float) -> Float {
  float.sqrt(saturation /. 2.0) *. linewidth
}

/// Saturation intensity of a transition.
pub fn saturation_intensity(linewidth: Float, wavelength: Float) -> Float {
  pi /. 2.0 *. h *. c /. float.pow(wavelength, 3.0) *. linewidth
}

