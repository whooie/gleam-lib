import whooie/float.{ pi }

/// Rayleigh range.
pub fn rayleigh(waist_radius: Float, wavelength: Float) -> Float {
  pi *. waist_radius *. waist_radius /. wavelength
}

/// Beam 1/e^2 radius for a given `z` position. `z = 0` corresponds to the
/// waist.
pub fn radius(z: Float, waist_radius: Float, wavelength: Float) -> Float {
  let zr = rayleigh(waist_radius, wavelength)
  waist_radius *. float.sqrt(1.0 +. float.pow(z /. zr, 2.0))
}

/// Peak intensity for a given total power.
pub fn peak_intensity(power: Float, radius: Float) -> Float {
  2.0 *. power /. { pi *. float.pow(radius, 2.0) }
}

/// Total power for a given peak intensity.
pub fn power(peak_intensity: Float, radius: Float) -> Float {
  pi *. float.pow(radius, 2.0) *. peak_intensity /. 2.0
}

/// Zero-to-one weighting factor on the local intensity due to only the radial
/// (Gaussian) power distribution in the beam.
pub fn radial_weight(r: Float, radius: Float) -> Float {
  float.exp(float.neg(2.0) *. float.pow(r /. radius, 2.0))
}

/// Zero-to-one weighting factor on the local intensity due to only the axial
/// (Lorentzian) power distribution in the beam.
pub fn axial_weight(z: Float, rayl: Float) -> Float {
  1.0 /. { 1.0 +. float.pow(z /. rayl, 2.0) }
}

/// Total zero-to-one power distribution in the beam, normalized to a maximum
/// value of `1`.
pub fn dist(
  r: Float,
  z: Float,
  waist_radius: Float,
  wavelength: Float,
) -> Float
{
  let zr = rayleigh(waist_radius, wavelength)
  let rad = radius(z, waist_radius, wavelength)
  let w_rad = radial_weight(r, rad)
  let w_ax = axial_weight(z, zr)
  w_rad *. w_ax
}

