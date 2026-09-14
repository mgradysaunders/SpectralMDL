/// \file
/// Drawing the samples.
#pragma once

#include <memory>
#include <string>
#include <vector>

#include "smdl/Compiler.h"

struct Options;
struct Frame;
struct ResumedSequence;
class RenderFilm;
class StagedScene;
class STree;

/// How a sample budget is split into passes.
///
/// Without guiding, one pass of the whole budget. With guiding the passes
/// grow geometrically (1, 2, 4, ...), which bounds what is spent while the
/// tree is immature, and the remainder becomes the final pass once it is
/// less than twice the next pass would be. The final pass is therefore
/// never smaller than the one before it, and always more than a third of
/// the budget though not always half: the warmup costs 2^k - 1 samples
/// whatever the budget is, so a budget just past a power of two spends
/// most of itself warming up.
///
/// A session that resumes a tree `trainedSpp` samples already trained
/// starts at the largest power of two at or below `trainedSpp`, skipping
/// the warmup it has outgrown. Solved up front rather than as the loop
/// runs, so that the progress bar can say which pass of how many.
[[nodiscard]] std::vector<size_t> solveSamplePasses(size_t spp, bool useGuiding,
                                                    size_t trainedSpp);

/// Will this session leave a guide tree behind?
///
/// Whenever guiding is on and the film is being written: the tree pairs
/// with that file, and a session that resumes it inherits the training.
[[nodiscard]] bool savesGuideTree(const Options &opts, const Frame &frame,
                                  const std::string &outputBands);

/// Meter the scene when the shot needs it, then draw this session's
/// samples into the film.
///
/// `target` is the observer's spectral film, or the band film a physical
/// sensor's response projects every sample onto; see `RenderFilm`.
/// Guiding splits the budget into geometrically growing passes and
/// combines them; everything else is one pass. Either way the film holds
/// every sample the session took when this returns, resumed ones
/// included, and `resumed.header` has been charged for the time it cost.
///
/// A sensor whose ISO nothing states is metered before the first
/// sample, once for the sequence: a metering pass over the window,
/// thrown away, decides the rung `resumed.header` records and every
/// later session reads back. See `Sensor/Meter.h`.
///
/// `outputBands` is where the film goes, empty for none, and `sdtree`
/// is filled in when guiding is on, whether or not it is going to be
/// written.
void renderSamples(const Options &opts, const Frame &frame,
                   smdl::Compiler &compiler, const StagedScene &staged,
                   ResumedSequence &resumed, RenderFilm &target,
                   const std::string &outputBands,
                   std::unique_ptr<STree> &sdtree);
