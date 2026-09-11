/// \file
/// Writing what the render produced.
#pragma once

#include <string>
#include <vector>

#include "smdl/Compiler.h"
#include "smdl/RenderUtil/SpectralFilm.h"

struct Options;
struct Frame;
struct ResolvedGrid;
struct ResumedSequence;
class EnvLight;
class Response;
class STree;

/// The picture as linear sRGB, developed the way the camera develops it:
/// the observer's develop of the spectral film, or a physical sensor's
/// of its band film read out noise-free, at the stated ISO or the one
/// this film meters to. For a checkpoint, so nothing is logged and the
/// preview differs from the final picture in its samples and its noise
/// alone. `bandFilm` is the band film, which a physical sensor has.
[[nodiscard]] std::vector<float>
developPreview(const Options &opts, const Frame &frame,
               const ResolvedGrid &grid, smdl::Compiler &compiler,
               const smdl::SpectralFilm &film,
               const smdl::SpectralFilm *bandFilm);

/// Write everything the command line asked for: the linear RGB floating
/// point image, the spectral ENVI pair, the guide tree beside it, the
/// readout, and the tone mapped 8-bit image.
///
/// The picture is developed by mode: the observer's develop of the
/// spectral film, or a physical sensor's of its readout, which runs
/// whether or not the digital numbers are written, since the picture is
/// made from them. Both then take the same tail: the firefly filter, the
/// floating point write, the tone map, and the 8-bit write.
///
/// The film must already hold every sample the session took, resumed
/// ones included. `outputSpectrum` is the resolved spectral path, empty
/// for none, which `-resume` implies back to the file it read.
/// `sdtree` is the tree to write beside it, or null to write none.
///
/// `resumed.header` is stamped with this session's fingerprint on the
/// way out, since the settings a later resume compares itself against
/// are the ones the samples now in the film were drawn under.
///
/// `envLight` is the environment the render used, or null for none: the
/// spectral header advertises where the sun stood and how hard it shone
/// when there was a procedural one. That is written for a reader and
/// never read back, which is why it is not part of `resumed.header`.
///
/// `response` and `bandFilm` are the detector's response and the film
/// its bands accumulated into, both null for none; the band film is
/// written as its own ENVI pair beside the spectral one, at
/// `bandFilmFileName()`, with the bands named and the response's
/// fingerprint in its header for a later resume to check.
void writeOutputs(const Options &opts, const Frame &frame,
                  const ResolvedGrid &grid, smdl::Compiler &compiler,
                  const EnvLight *envLight, const smdl::SpectralFilm &film,
                  const Response *response, const smdl::SpectralFilm *bandFilm,
                  ResumedSequence &resumed, const std::string &outputSpectrum,
                  const STree *sdtree);
