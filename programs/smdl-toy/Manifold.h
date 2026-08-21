/// \file
/// The Newton solve behind manifold next-event estimation (Hanika,
/// Droske & Fascione, "Manifold Next Event Estimation", EGSR 2015):
/// given a receiver point and a distant light direction, find the
/// points on a chain of smooth refractive interfaces where the
/// connection obeys Snell's law at every crossing. The transport-side
/// work (Fresnel, medium attenuation, MIS bookkeeping) lives with the
/// path tracer; this is the geometry.
#pragma once

#include <algorithm>
#include <array>
#include <vector>

// For the nested-medium stack the per-side refractive indices resolve
// against.
#include "Medium.h"

/// The most refractive interfaces a connection may cross.
constexpr int MNEE_MAX_DEPTH{4};

/// How far a converged crossing may sit from the one a path actually
/// took and still count as the same solution, as a fraction of the
/// distance from the receiver.
///
/// This one number calibrates the whole estimator. The arrival side
/// identifies solutions with it, deciding whether the gather can produce
/// what a path found, and the walk converges against a fraction of it,
/// so neither side can be more precise than the other needs. It used to
/// be a literal on the arrival side facing an unrelated residual
/// tolerance in the walk, and the two disagreeing is what made coverage
/// unreliable.
constexpr float MANIFOLD_IDENTITY_FRACTION{1e-2f};

/// The lobes a material instance can produce anywhere in its
/// scattering tree, both sides together. What every eligibility question
/// here and in the path tracer is asked of.
[[nodiscard]] inline int
dfLobesOf(const smdl::JIT::MaterialInstance &mat) noexcept {
  return mat.instance.df_lobes_surface | mat.instance.df_lobes_backface;
}

/// One interface of a seed chain, as discovered along the straight
/// shadow segment: where it was hit, the absolute refractive indices on
/// the receiver-facing and light-facing sides (resolved against the
/// medium stack as of the crossing), and which side of the shading
/// normal the segment arrived from, so a walk that migrates across a
/// silhouette is rejected rather than solved with swapped indices.
class ManifoldVertexSeed final {
public:
  Hit hit{};
  float etaFront{};
  float etaBack{};
  float sideSign{};
};

/// A seed chain: the eligible interfaces the straight shadow segment
/// crosses, in order from the receiver.
class ManifoldChain final {
public:
  std::array<ManifoldVertexSeed, MNEE_MAX_DEPTH> vertices{};
  int count{};
};

/// One interface of a converged connection.
class ManifoldConnectionVertex final {
public:
  /// The interface vertex the walk converged to.
  Hit hit{};

  /// The differential geometry at the vertex.
  ManifoldGeometry geometry{};

  /// The unit direction toward the previous vertex (or the receiver).
  float3 wFront{};

  /// The unit direction toward the next vertex (or the light).
  float3 wBack{};

  /// The cosine of `wFront` against the shading normal, positive.
  float cosFront{};

  /// The cosine of `wBack` against the shading normal, positive.
  float cosBack{};
};

/// A converged refractive connection.
class ManifoldConnection final {
public:
  std::array<ManifoldConnectionVertex, MNEE_MAX_DEPTH> vertices{};

  int count{};

  /// The unit direction from the receiver toward the first vertex.
  float3 wr{};

  /// The transfer Jacobian `|d omega_r / d omega_l|`: the solid angle
  /// the connection subtends at the receiver per unit solid angle of
  /// the light direction, through the whole chain. For a finite light
  /// the light-direction measure is the solid angle of the straight
  /// line from the receiver to the light point, which is exactly the
  /// measure the light sampler's density and radiance are expressed
  /// in, so the estimator keeps the same form as the distant case.
  /// This is the purely geometric factor; the per-crossing radiance
  /// compression `eta^2` that rides with refracted radiance is
  /// deliberately not included, so the caller applies the same
  /// convention the specular BSDF uses.
  float transfer{};
};

/// The light side of a manifold connection: a distant direction (the
/// environment) or a finite light point (a punctual light or a point
/// on an area light). `wl` is always the unit direction of the
/// STRAIGHT segment from the receiver, which for a finite target must
/// equal `normalize(point - receiver)`.
class ManifoldTarget final {
public:
  float3 wl{};
  float3 point{};
  bool infinite{true};
};

/// Is the material at a blocking hit an interface a connection may
/// refract through: a solid carrying a Dirac transmission that actually
/// bends light? Thin walls transmit without bending and emitters are
/// light, not glass; either disqualifies, and so does an index-matched
/// boundary, whose transport has no refraction to solve and stays with
/// the ordinary estimators.
///
/// The test is on the Dirac transmission LOBE, `DF_DELTA_BTDF`, and not
/// on the material being purely specular. A material may carry any other
/// lobes alongside it: the chain claims only the Dirac transmission,
/// asking the material for that one lobe by mask, and everything else the
/// interface does is transport the ordinary estimators carry. Testing
/// the two axes separately would not do, since a Dirac reflection over a
/// diffuse transmission has the same domains and the same kinds as a
/// Dirac transmission over a diffuse reflection and no manifold to
/// solve.
///
/// A material that remaps `geometry.normal` disqualifies: the walk
/// differentiates the mesh shading-normal field, so the BSDF would
/// refract about a frame the walk does not solve for, and gathering on
/// the unremapped manifold while the path side refracts about the
/// remapped one silently over-counts. `stateNormal` is the internal-space
/// shading normal of the state the instance was evaluated with; the test
/// is byte equality, not a tolerance, because an untouched
/// `geometry.normal` is initialized from `$state.normal` and a remap
/// genuinely moves the manifold.
///
/// The instance's exterior IOR must already be resolved against the
/// medium stack, since the contrast is measured against it.
///
/// This is a static, whole-tree question, so it can only say that the
/// interface HAS a Dirac transmission, never that this crossing reaches
/// it. Both sides of the estimator confirm that with a masked
/// `scatterSample` at the converged geometry and decline complementarily
/// when it fails.
///
/// Several Dirac transmissions mixed at one interface are the one case
/// that query cannot settle: they are indistinguishable by direction, so
/// each side reports the chance of the branch its own draw took rather
/// than the sum over every branch that produces the direction. Each side
/// still estimates its own transport correctly; it is the two MIS weights
/// that stop summing to exactly one.
[[nodiscard]] bool isManifoldInterface(const smdl::JIT::MaterialInstance &mat,
                                       const float3 &stateNormal);

/// Seed one chain vertex from an interface the straight segment
/// crosses: resolve the instance's exterior IOR against `medium`,
/// admit the interface only if `isManifoldInterface()` does, and fill
/// in the per-side indices and the side of the shading normal the
/// segment arrived from.
///
/// Both halves of the manifold estimator go through here: the gather
/// discovering its chain and the arrival-side re-walk reconstructing
/// it. Their seeds must agree vertex for vertex or the two MIS weights
/// stop summing to one, so the eligibility test and the index
/// assignment deliberately have exactly one implementation.
///
/// `wl` is the direction of travel along the straight segment, toward
/// the light. `mat` is modified in place by the exterior
/// IOR resolution, and `state` must be the state it was evaluated
/// with, for the normal-remap eligibility test.
[[nodiscard]] bool makeManifoldSeed(const MediumStack *medium,
                                    smdl::JIT::MaterialInstance &mat,
                                    const smdl::State &state, const Hit &hit,
                                    const float3 &wl, ManifoldVertexSeed &seed);

/// Solve the refractive connection from `receiver` to the light target
/// through the seed chain, by damped Newton iteration on the
/// block-coupled per-vertex constraints. Steps re-anchor onto the real
/// surfaces by re-casting each vertex from its (already updated)
/// predecessor, so a converged connection's segments are known to see
/// their endpoints (null interfaces excepted). Returns true on
/// convergence to a transmission configuration on the seed's own side
/// of every interface; failure (divergence, leaving a seed instance,
/// total internal reflection, a silhouette migration, a grazing or
/// degenerate frame) means no contribution, never a wrong one.
[[nodiscard]] bool solveManifoldConnection(const Scene &scene,
                                           const float3 &receiver,
                                           const ManifoldTarget &target,
                                           const ManifoldChain &chain,
                                           ManifoldConnection &connection);

/// Is a converged crossing one the estimator accepts? The two segments
/// must lie on opposite sides of the shading normal, which is what makes
/// the crossing a transmission, and the arriving segment must be on the
/// side the seed crossed from, which rejects a solution that migrated
/// across a silhouette and would otherwise be weighed with swapped
/// indices.
///
/// Both halves of the estimator apply this to whatever crossing they
/// have in hand, the gather to each solution and the arrival-side
/// cancelation to the crossing the path actually took, which is what
/// keeps the two coverage sets identical. `wFront` points toward the
/// receiver and `wBack` toward the light.
[[nodiscard]] bool isManifoldCrossing(const ManifoldVertexSeed &seed,
                                      const float3 &normal,
                                      const float3 &wFront,
                                      const float3 &wBack);

/// Evaluate the transfer Jacobian of a chain that already satisfies the
/// constraints, e.g., the actual crossings of a path that refracted
/// through the interfaces, without running the Newton walk. The chain's
/// hits must be the crossing points in order from `receiver`, with the
/// target past the last one. Fails on a degenerate frame or a chain
/// whose residual says it is not actually a solution.
[[nodiscard]] bool evaluateManifoldTransfer(const Scene &scene,
                                            const float3 &receiver,
                                            const ManifoldTarget &target,
                                            const ManifoldChain &chain,
                                            float &transfer);
