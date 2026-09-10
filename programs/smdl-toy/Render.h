/// \file
/// Drawing the samples.
#pragma once

#include <memory>
#include <string>

#include "smdl/Compiler.h"
#include "smdl/RenderUtil/SpectralFilm.h"

struct Options;
struct Frame;
struct ResolvedGrid;
struct ResumedSequence;
class Response;
class StagedScene;
class STree;

/// Will this session leave a guide tree behind?
///
/// Whenever guiding is on and the spectrum accumulation is being
/// written: the tree pairs with that file, and a session that resumes it
/// inherits the training.
[[nodiscard]] bool savesGuideTree(const Options &opts, const Frame &frame,
                                  const std::string &outputSpectrum);

/// Draw this session's samples into `film`.
///
/// Guiding splits the budget into geometrically growing passes and
/// combines them; everything else is one pass. Either way the film holds
/// every sample the session took when this returns, resumed ones
/// included, and `resumed.header` has been charged for the time it cost.
///
/// `response`, `bandFilm`, and `bandSquares` are the detector's
/// response, the film its bands accumulate into, and the film of their
/// squares, all null for none: every sample's radiance is projected onto
/// the bands beside the spectral sum, at every pass alike, since the
/// guided combination resolves to the plain mean anyway. When this
/// returns the band films hold every sample the spectral film does,
/// resumed ones included.
///
/// `outputSpectrum` is where a checkpoint writes, empty for none, and
/// `sdtree` is filled in when guiding is on, whether or not it is going
/// to be written.
void renderSamples(const Options &opts, const Frame &frame,
                   const ResolvedGrid &grid, smdl::Compiler &compiler,
                   const StagedScene &staged, ResumedSequence &resumed,
                   smdl::SpectralFilm &film, const Response *response,
                   smdl::SpectralFilm *bandFilm,
                   smdl::SpectralFilm *bandSquares,
                   const std::string &outputSpectrum,
                   std::unique_ptr<STree> &sdtree);
