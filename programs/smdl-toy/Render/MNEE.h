/// \file
/// The transport side of the manifold estimators: the estimator that
/// runs over the solver glue in `Render/Manifold.h`. `gatherDirect()`
/// gathers direct lighting at one path vertex, routing a blocked or
/// caster-marked light sample through a manifold connection instead of
/// reading as occluded, and `MNEECoverage` carries what those gathers
/// have claimed since the last receiver, so that the walk's own
/// arrivals at lights are weighed against them rather than counted
/// twice.
///
/// The walk itself is `Render/PathTracing.cc`'s: it builds a
/// `PathVertex` at every scattering vertex, hands it here, and reads
/// back the shares (`claimedShareOf()`, `receiverShareOf()`) its
/// arrivals must drop.
#pragma once

#include <array>

#include "Render/Context.h"

class DTree;

/// The scattering role of a path vertex: a surface BSDF, a volume phase
/// function, or the hair BSDF at a curve hit whose material binds
/// `material.hair`.
enum class VertexKind { SURFACE, VOLUME, HAIR };

/// The share of a receiver's bounce its receiving lobes carry, deferred
/// to the light it is read against: the smooth lobes receive any light
/// and the glossy ones a light their width is wide enough for
/// (`MNEE_RECEIVER_EXTENT_RATIO` of its angular radius from the
/// receiver), so the share is one of two colors, chosen by the light.
/// Zero throughout where nothing receives.
struct ReceiverShare final {
  /// The narrowest width of the glossy lobes, `INFINITY` without one, so
  /// that the glossy question never arises; see `manifoldGlossyWidth()`.
  float glossyWidth{INFINITY};

  /// The share with the glossy lobes receiving, and without them.
  Color withGlossy{};
  Color withoutGlossy{};

  /// The share against a light of angular radius `angularRadius` from
  /// the receiver.
  [[nodiscard]] const Color &at(float angularRadius) const noexcept {
    return glossyWidth >= MNEE_RECEIVER_EXTENT_RATIO * angularRadius
               ? withGlossy
               : withoutGlossy;
  }

  /// The share of a bounce every lobe of which receives.
  [[nodiscard]] static ReceiverShare whole() noexcept {
    ReceiverShare share{};
    share.withGlossy.fill(1.0f);
    share.withoutGlossy.fill(1.0f);
    return share;
  }
};

/// One vertex of the walk, as the gathers and the manifold estimators
/// see it: where it is, what arrived there, what scatters there, and
/// what the estimators may claim of it.
struct PathVertex final {
  /// What scatters here: the material instance at a surface or hair
  /// vertex, the phase function of the collision at a volume one. It has
  /// no default, so a vertex is always built around one.
  Scatterer scatterer;

  VertexKind kind{};

  float3 point{};

  /// The direction back along the segment that arrived here.
  float3 wo{};

  /// The instance scattering here at a surface vertex; null at a volume
  /// or hair vertex, which has no interface.
  const MeshInstance *instance{};

  /// The nested-medium stack the vertex sits in. A gather ray leaving it
  /// starts in `receiver().mediumToward()` of its direction.
  const MediumStack *mediumStack{};

  /// The SD-tree cell participating here, or null where the continuation
  /// samples the BSDF alone; see `guidingCellAt()`.
  const DTree *dtree{};

  /// The probability the continuation draws from the BSDF rather than
  /// the cell, meaningful only with a `dtree`; see `bsdfFractionAt()`.
  float bsdfFraction{1.0f};

  /// What the manifold estimators claim at this vertex, already narrowed
  /// to what the gathers behind it can reach from here; see
  /// `MNEECoverage::reach()`.
  ManifoldClaim reachableClaim{};

  /// Whether a receiver behind this vertex ran a gather, so that the
  /// claimed lobes are that gather's to estimate and light sampling here
  /// covers the rest.
  bool isArmedBehind{};

  /// Whether this vertex is one the gathers run from at all; see
  /// `manifoldReceiverLobes()`.
  bool isReceiver{true};

  /// The finite lobes this vertex scatters with and the narrowest width
  /// of its glossy ones (see `manifoldGlossyWidth()`), from which
  /// `gatherDirect()` decides per light which lobes receive it: the
  /// smooth ones always, the glossy ones when wide enough for the
  /// light's angular radius (`MNEE_RECEIVER_EXTENT_RATIO`). It leaves
  /// the answer in `receiveMask`, a mask over the lobes the gathers value
  /// the receiver with, `DF_ALL` where every finite lobe receives. The
  /// arrivals behind keep the share of this vertex's bounce the other
  /// lobes carried, read against their own light the same way, so the
  /// two halves partition the transport as the casters' claims do.
  int finiteLobes{};
  float glossyWidth{INFINITY};
  int receiveMask{smdl::DF_ALL};

  /// Whether `gatherDirect()` ran the manifold gathers here, which is
  /// also whether it drew its light sample by area; the walk carries it
  /// to the arrival sites to recompute the density the gather drew with.
  bool ranManifold{};

  /// The share of the bounce behind this vertex its receiver's receiving
  /// lobes carried, with the point it is read from: what that receiver's
  /// reflective gather claims of a reflection here (the previous
  /// vertex's), and what the armed receiver's chain gather claims of a
  /// transmission here (the armed receiver's). Light sampling here keeps
  /// the rest of the claimed lobes. Whole where nothing is partitioned.
  ReceiverShare reflectShareBehind{ReceiverShare::whole()};
  float3 reflectPointBehind{};
  ReceiverShare refractShareBehind{ReceiverShare::whole()};
  float3 refractPointBehind{};

  /// The vertex as the receiver a connection leaves from; see
  /// `MNEEReceiver`.
  [[nodiscard]] MNEEReceiver receiver() const noexcept {
    return MNEEReceiver{point, wo, mediumStack,
                        kind == VertexKind::SURFACE ? &scatterer.material()
                                                    : nullptr,
                        instance};
  }
};

/// The MNEE coverage the camera walk carries: armed at every vertex
/// whose gather could attempt a manifold connection, along with that
/// vertex and the identity of every claimed transmission the walk has
/// taken unbroken since. An arrival at a light through a Dirac
/// chain is weighed against the gather at the receiver by `coverWeight()`;
/// one through a glossy chain is claimed outright, to the share of the
/// throughput the chain's claimed lobes carry; any other bounce breaks the
/// chain and restores the ordinary weights. Either way the claim extends
/// only to the share of the receiver's own bounce its receiving lobes
/// carried (`receiverShare()`).
class MNEECoverage final {
public:
  /// What the chain since the receiver is made of. A chain of one kind is
  /// what the gathers estimate; a mixed one nobody claims.
  enum class ChainKind { NONE, DIRAC, GLOSSY, MIXED };

  /// Begin a fresh receiver, the vertex a gather could connect from.
  /// `isEnabled` is false when manifold NEE is off, which leaves the state
  /// permanently disarmed. `share` is the share of the receiver's bounce
  /// its receiving lobes carry (see `ReceiverShare`), read against the
  /// light at the arrival, which every claim through this chain is scaled
  /// by: a Dirac chain's arrival keeps its ordinary weight for the rest.
  void arm(bool isEnabled, const MNEEReceiver &receiver, float pdf,
           const ReceiverShare &share) noexcept {
    mIsArmed = isEnabled;
    mFamily = MNEEChainFamily{};
    mChainKind = ChainKind::NONE;
    mChainShare = Color(1.0f);
    mReceiverShare = share;
    mReceiver = receiver;
    mReceiverPdf = pdf;
  }

  /// Disarm, which is all a fresh path needs: `arm()` sets everything
  /// else before any reader consults it, and the chain arrays are read
  /// only below the length it resets.
  void disarm() noexcept { mIsArmed = false; }

  [[nodiscard]] bool isArmed() const noexcept { return mIsArmed; }

  /// Extend the chain across a claimed transmission: Dirac, or glossy with
  /// the share of the crossing's throughput its claimed lobe carries. The
  /// family keeps counting past `MANIFOLD_MAX_DEPTH` so that an overlong
  /// chain reads as uncovered rather than as a shorter one.
  void extend(const Hit &hit, bool isGlossy,
              const Color &claimedShare) noexcept {
    if (mFamily.count < MANIFOLD_MAX_DEPTH) mChainHits[mFamily.count] = hit;
    mFamily.append(hit);
    const ChainKind kind{isGlossy ? ChainKind::GLOSSY : ChainKind::DIRAC};
    mChainKind = mChainKind == ChainKind::NONE || mChainKind == kind
                     ? kind
                     : ChainKind::MIXED;
    if (isGlossy) mChainShare *= claimedShare;
  }

  /// Is there a Dirac chain of connectable length for the gather to
  /// compete with, so that `coverWeight()` replaces the ordinary weight?
  [[nodiscard]]
  bool coversDirac(const MNEEOptions &mneeOptions) const noexcept {
    return covers(ChainKind::DIRAC, mneeOptions);
  }

  /// Is there a glossy chain of connectable length, which the gather at
  /// the receiver claims to the share `chainShare()`?
  [[nodiscard]]
  bool coversGlossy(const MNEEOptions &mneeOptions) const noexcept {
    return covers(ChainKind::GLOSSY, mneeOptions);
  }

  [[nodiscard]] const Color &chainShare() const noexcept { return mChainShare; }

  /// The share of the armed receiver's bounce its receiving lobes carry,
  /// read against the light; see `arm()`.
  [[nodiscard]] const ReceiverShare &receiverShare() const noexcept {
    return mReceiverShare;
  }

  [[nodiscard]] ChainKind chainKind() const noexcept { return mChainKind; }

  [[nodiscard]] int chainLength() const noexcept { return mFamily.count; }

  /// What the gathers behind this vertex can reach of the vertex's own
  /// claim, which is what the vertex's gather leaves to them and what the
  /// next arrival drops. The reflection kinds are the previous vertex's
  /// reflective gather's, which ran there and weighed its connections
  /// with the finite lobes the path then bounced through, so a Dirac
  /// bounce there is outside it; the transmission kinds are the chain
  /// receiver's refractive gather's, which reaches this vertex only as
  /// the next crossing of a chain of one kind within its depth. Nothing
  /// is reachable while disarmed.
  [[nodiscard]] ManifoldClaim reach(const ManifoldClaim &claim,
                                    const MNEEOptions &mneeOptions,
                                    bool isPrevDirac) const noexcept {
    ManifoldClaim reachable{};
    if (!mIsArmed) return reachable;
    if (!isPrevDirac) reachable.reflectLobes = claim.reflectLobes;
    if (mFamily.count < mneeOptions.depth) {
      switch (mChainKind) {
      case ChainKind::NONE:
        reachable.refractLobes = claim.refractLobes;
        break;
      case ChainKind::DIRAC:
        reachable.refractLobes = claim.refractLobes & smdl::DF_DIRAC_BTDF;
        break;
      case ChainKind::GLOSSY:
        reachable.refractLobes = claim.refractLobes & smdl::DF_GLOSSY_BTDF;
        break;
      case ChainKind::MIXED:
        break;
      }
    }
    return reachable;
  }

  /// The receiver the chain leaves from.
  [[nodiscard]] const MNEEReceiver &receiver() const noexcept {
    return mReceiver;
  }

  /// The instance of the chain's first crossing, meaningful while the
  /// chain has one: what the caster refractive gather's membership is
  /// asked of.
  [[nodiscard]] uint32_t firstInstIndex() const noexcept {
    return mFamily.instances[0];
  }

  /// The MIS weight of a BSDF-side arrival at a light, an environment
  /// escape or an emitter hit, through a Dirac chain of eligible refractive
  /// interfaces, by re-walk MIS (Hanika et al. 2015, section 5): re-run
  /// the discovery and the deterministic manifold walk the gather at the
  /// receiver runs for this target, and only when the walk converges to the
  /// same crossings the path actually took does the gather compete;
  /// otherwise (a different chain family, a different fold solution, a
  /// failed walk, or a light the sampler cannot draw) the arrival keeps
  /// weight 1 instead of silently losing its transport. The competing
  /// densities are per unit solid angle of the straight line toward the
  /// light: the gather's is the light sampling density, the arrival's is
  /// the receiver's recorded continuation density times the interfaces'
  /// own selection chances times the transfer Jacobian; the gather applies
  /// the complementary weight with the same formula, so the pair sums to
  /// one.
  ///
  /// That last factor is taken from the walk run here rather than
  /// re-evaluated on the crossings the path actually took. The two agree
  /// only to the convergence tolerance, and the pair sums to one exactly
  /// when both sides weigh the same number, so the number to weigh by is
  /// the one the gather would compute: this walk IS the gather's walk, for
  /// this target, from the same discovery (`discoverStraightChain()`) of
  /// the same line.
  ///
  /// Where the chain starts on a caster the caster refractive gather
  /// samples (`isCasterChain`), that gather owns every family of the caster's
  /// chains but the straight one, exclusively, and an arrival through such
  /// a family is dropped instead of kept: every exit before the solve,
  /// where the discovery does not reach the light through the crossings
  /// the path took, returns 0. The exits after the solve keep the weights
  /// above, because the path's family IS the straight one there and the
  /// straight-line gather owns it, whether or not its walk reached this
  /// solution.
  ///
  /// In the biased claimed mode the straight-line gather's clustered walks
  /// claim the straight family outright, so an arrival through it is
  /// dropped where the solve would otherwise run, and every other family
  /// keeps the rule above: the caster gather's are dropped, and one no
  /// gather reaches keeps weight 1 rather than losing its transport.
  [[nodiscard]] float coverWeight(const RenderContext &render,
                                  PathContext &path,
                                  const ManifoldTarget &target, float lightPdf,
                                  bool isCasterChain) const;

private:
  [[nodiscard]] bool covers(ChainKind kind,
                            const MNEEOptions &mneeOptions) const noexcept {
    return mIsArmed && mChainKind == kind && mFamily.count >= 1 &&
           mFamily.count <= mneeOptions.depth;
  }

  bool mIsArmed{};
  ChainKind mChainKind{ChainKind::NONE};
  Color mChainShare{1.0f};
  MNEEReceiver mReceiver{};
  float mReceiverPdf{};
  ReceiverShare mReceiverShare{};

  /// The crossings the path took since the receiver, as a family and as
  /// the hits themselves, the latter below `MANIFOLD_MAX_DEPTH` only.
  MNEEChainFamily mFamily{};
  std::array<Hit, MANIFOLD_MAX_DEPTH> mChainHits{};
};

/// Gather direct lighting at one path vertex by light sampling: sample a
/// light, evaluate the BSDF, and test visibility against the sampled
/// point. The BSDF-sampling half of the MIS pair is the walk's own
/// continuation segment, whose emitter hits and environment escapes
/// `tracePath` weighs against the density this gather would have
/// produced. Returns the estimate WITHOUT the path throughput, which is
/// exactly what the guiding trainer records. `gatherState` is a pristine
/// state carrying only the render-wide fields: light sampling applies the
/// light hit's own geometry, and the LOD fields stay zero so emission
/// evaluates at full fidelity.
///
/// With the manifold estimators enabled, a light sample whose straight
/// segment is blocked by claimed refractive interfaces routes through
/// `MNEEGather::gatherStraightRefraction()` instead of reading as
/// occluded, and the caster gathers sample the marked casters besides,
/// for a reflection off one and for a refraction through one. Hair
/// vertices keep plain gathering: the manifold estimator's MIS is not
/// wired through the hair BSDF.
///
/// Where the vertex both claims lobes and stands behind an armed
/// receiver, those lobes are the receiver's gather's to estimate, so
/// light sampling here covers the other lobes only, weighed against the
/// unmasked continuation density exactly as the continuation's arrivals
/// are weighed for those lobes (`tracePath()` keeps their share of each
/// arrival and drops the claimed share). The receiver's gather produces
/// only the share of its own bounce its receiving lobes carried, so
/// light sampling keeps the rest of the claimed lobes to match.
[[nodiscard]]
Color gatherDirect(const RenderContext &render, PathContext &path,
                   const smdl::State &gatherState, PathVertex &vertex);

/// The share of one bounce's throughput the manifold estimators claim,
/// per wavelength, given what the gathers behind this vertex can reach:
/// all of a Dirac reflection of a claimed kind, and of a finite bounce
/// the part of its value the claimed lobes carry, which is the value
/// without them over the value with them. A Dirac transmission is not a
/// share but a chain, weighed against by re-walk MIS, and reports zero
/// here.
[[nodiscard]]
Color claimedShareOf(const smdl::JIT::Material &material,
                     const ManifoldClaim &reachable, const float3 &wo,
                     const float3 &wNext, const Color &f, bool isDiracBounce,
                     bool transmits, int sampledLobe);

/// The share of one bounce's throughput the vertex's receiving lobes
/// carry, in both readings a light can give it (see `ReceiverShare`): the
/// value without the finite lobes that do not receive over the value with
/// them, which is what the gathers at the vertex estimate of the
/// transport through this bounce and what the arrivals behind therefore
/// drop of it; see `PathVertex::receiveMask`. With the glossy lobes
/// receiving every finite lobe does, so that reading is one; without
/// them it is the smooth lobes' part, which needs the material only where
/// it has both kinds. Zero for a Dirac bounce, which no lobe receives
/// with, and at a vertex that is no receiver.
[[nodiscard]]
ReceiverShare
receiverShareOf(const smdl::JIT::Material &material, int finiteLobes,
                float glossyWidth, const float3 &wo, const float3 &wNext,
                const Color &f, bool isDiracBounce, bool isReceiver);
