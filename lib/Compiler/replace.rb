for fname in Dir['builtin/*.smdl']
  text1 = File.read fname
  text2 = text1.gsub /\b_albedo_lut\b/, '_AlbedoLUT'
  text2 = text2.gsub /\b_material_instance\b/, '_MaterialInstance'
  text2 = text2.gsub /\bjit_struct\b/, 'ptr'
  #text2 = text2.gsub /\b_polyline_lerp\b/, '_polylineLerp'
  #text2 = text2.gsub /\b_samples_to_color\b/, '_samplesToColor'
  #text2 = text2.gsub /\b_spectral_curve_to_color\b/, '_spectralCurveToColor'
  #text2 = text2.gsub /\b_default_bsdf\b/, '_DefaultBSDF'
  #text2 = text2.gsub /\b_default_vdf\b/, '_DefaultVDF'
  if text2 != text1
    puts fname
  end
end
