/// \file
/// Writing what the render produced.
#pragma once

#include <string>

#include "smdl/Compiler.h"
#include "smdl/RenderUtil/SpectralFilm.h"

struct Options;
struct Frame;
struct ResolvedGrid;
struct ResumedSequence;
class STree;

/// Write everything the command line asked for: the linear RGB floating
/// point image, the spectral ENVI pair, the guide tree beside it, and
/// the tone mapped 8-bit image.
///
/// The film must already hold every sample the session took, resumed
/// ones included. `outputSpectrum` is the resolved spectral path, empty
/// for none, which `-resume` implies back to the file it read.
/// `sdtree` is the tree to write beside it, or null to write none.
///
/// `resumed.header` is stamped with this session's fingerprint on the
/// way out, since the settings a later resume compares itself against
/// are the ones the samples now in the film were drawn under.
void writeOutputs(const Options &opts, const Frame &frame,
                  const ResolvedGrid &grid, smdl::Compiler &compiler,
                  const smdl::SpectralFilm &film, ResumedSequence &resumed,
                  const std::string &outputSpectrum, const STree *sdtree);
