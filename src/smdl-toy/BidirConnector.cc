#include "BidirConnector.h"

void BidirConnector::connect(std::vector<Vertex> &cameraPath,
                             std::vector<Vertex> &lightPath) const {
  //
  for (size_t m = 2; m <= cameraPath.size(); m++) {
    truncate_camera_path(cameraPath[m - 1]);
    if (!lightPath.empty()) {
      complete_camera_path_by_resampling_light(cameraPath[m - 1]);
    }
  }
  for (size_t n = 2; n <= lightPath.size(); n++) {
    truncate_light_path(lightPath[n - 1]);
    if (!cameraPath.empty()) {
      complete_light_path_by_resampling_camera(lightPath[n - 1]);
    }
  }
  for (size_t m = 2; m <= cameraPath.size(); m++) {
    for (size_t n = 2; n <= lightPath.size(); n++) {
      connect_camera_path_to_light_path(cameraPath[m - 1], lightPath[n - 1]);
    }
  }
}

void BidirConnector::truncate_camera_path(Vertex &lastCameraVertex) const {
  // TODO
}

void BidirConnector::truncate_light_path(Vertex &) const {
  // Do nothing! It is currently impossible for a light path to be
  // complete by itself because there is no intersectible camera geometry.
}

void BidirConnector::complete_camera_path_by_resampling_light(
    Vertex &lastCameraVertex) const {
  // TODO
}

void BidirConnector::complete_light_path_by_resampling_camera(
    Vertex &lastLightVertex) const {
  // TODO
}

void BidirConnector::connect_camera_path_to_light_path(
    Vertex &lastCameraVertex, Vertex &lastLightVertex) const {
  // TODO
#if 0
    // Connect paths.
    auto isConnectible = [&]() {
      return !vertexA.runtime.flags.isIncomplete && vertexA.material.hasScattering() && //
             !vertexB.runtime.flags.isIncomplete && vertexB.material.hasScattering();
    }();
    if (!isConnectible) return;
    Vector3d omegaI{vertexA.omega(vertexB)};
    Spectrum fA{spectrumZerosLike(vertexA.runtime.ratio)};
    Spectrum fB{spectrumZerosLike(vertexB.runtime.ratio)};
    vertexA.runtime.scatteringPDF = vertexA.material.scatter(random, vertexA.runtime.omegaO, +omegaI, fA);
    vertexB.runtime.scatteringPDF = vertexB.material.scatter(random, vertexB.runtime.omegaO, -omegaI, fB);
    if (Spectrum L{
          vertexA.runtime.ratio * fA * //
          vertexB.runtime.ratio * fB * //
          (1.0 / magnitudeSquare(vertexA.position - vertexB.position))};
        isPositiveAndFinite(L) && mVisibilityTester(vertexA, vertexB, L)) {
      receiver(pathA, pathB, multipleImportanceWeight(pathA, pathB), L);
    }
#endif
}

#if 0
  struct PathBackup {
  public:
    PathBackup(PathView pathView) : mPathView(pathView) {
      for (int i = 0, j = max(int(mPathView.size()) - 2, 0); j < int(mPathView.size()); i++, j++) mRuntimes[i] = mPathView[j].runtime;
    }
    ~PathBackup() {
      for (int i = 0, j = max(int(mPathView.size()) - 2, 0); j < int(mPathView.size()); i++, j++) mPathView[j].runtime = std::move(mRuntimes[i]);
    }

  private:
    PathView mPathView;

    // It is only ever necessary to modify the runtimes of the last two (at most).
    Path::Vertex::Runtime mRuntimes[2]{};
  };

void PathConnector::connectTerm(Random &random, PathView pathA, PathView pathB, const Receiver &receiver) const {
  auto doTruncation = [&](Path::Vertex &vertexP) -> void {
    Path::Kind kind{vertexP.runtime.kind};
    if (mTruncater(vertexP)) {
      if (vertexP.runtime.kind != kind) [[unlikely]]
        throw Error(std::logic_error("Call to PathConnector::connectTerm() failed! Reason: Truncation operator must return same kind of vertex!"));
      receiver(pathA, pathB, multipleImportanceWeight(pathA, pathB), vertexP.runtime.ratio);
    }
  };
  auto doCompletion = [&](Path::Vertex &vertexP, Path::Vertex &vertexQ) -> std::optional<Spectrum> {
    if (mCompleter(vertexP, vertexQ)) {
      if (vertexP.runtime.kind == vertexQ.runtime.kind) [[unlikely]]
        throw Error(std::logic_error("Call to PathConnector::connectTerm() failed! Reason: Completion operator must return opposite kind of vertex!"));
      Vector3d omegaI{vertexP.omega(vertexQ)};
      Spectrum fP{spectrumZerosLike(vertexP.runtime.ratio)};
      vertexP.runtime.scatteringPDF = vertexP.material.scatter(random, vertexP.runtime.omegaO, omegaI, fP);
      if (Spectrum L{
            vertexP.runtime.ratio * fP * //
            vertexQ.runtime.ratio};
          isPositiveAndFinite(L) && mVisibilityTester(vertexP, vertexQ, L)) {
        return L;
      }
    }
    return {};
  };
  const PathBackup backups[2]{PathBackup(pathA), PathBackup(pathB)};
  if (pathA.empty() && pathB.empty()) [[unlikely]] {
    return;
  } else if (pathA.empty()) {
    doTruncation(pathB.back()); // Apply truncation to path B when path A is empty.
  } else if (pathB.empty()) {
    doTruncation(pathA.back()); // Apply truncation to path A when path B is empty.
  } else if (pathA.size() == 1 && pathB.size() == 1) [[unlikely]] {
    return;
  } else if (pathA.size() == 1) { // Implied: && pathB.size() > 1
    Path::Vertex vertexA;
    if (auto L = doCompletion(pathB.back(), vertexA)) receiver(PathView(&vertexA, 1), pathB, multipleImportanceWeight(PathView(&vertexA, 1), pathB), *L);
  } else if (pathB.size() == 1) { // Implied: && pathA.size() > 1
    Path::Vertex vertexB;
    if (auto L = doCompletion(pathA.back(), vertexB)) receiver(pathA, PathView(&vertexB, 1), multipleImportanceWeight(pathA, PathView(&vertexB, 1)), *L);
  } else {

  }
}

double PathConnector::multipleImportanceWeight(PathView pathA, PathView pathB) const {
  if (pathA.size() > 0 && pathB.size() == 1)
    return 1;
  return 0;
  // Recalculate the relevant conjugate path-space PDFs.
  size_t nA{pathA.size()};
  size_t nB{pathB.size()};
  if (nA > 0 && nB > 0) {
    pathA[nA - 1].recalculateReversePathPDF(pathB[nB - 1]);
    pathB[nB - 1].recalculateReversePathPDF(pathA[nA - 1]);
  }
  if (nA > 1) pathA[nA - 2].recalculateReversePathPDF(pathA[nA - 1]);
  if (nB > 1) pathB[nB - 2].recalculateReversePathPDF(pathB[nB - 1]);

  // Determine the multiple importance weight as per the balance heuristic. Ordinarily, the balance heuristic
  // calculation looks like a basic sum-normalization. Suppose you sampled something according to density P, but
  // you could have sampled it from some other densities Q and R as well. Then the weight for the way you actually
  // sampled it with P is P/(P+Q+R). If you work out the probabilities for bidirectional connections in this way,
  // you can eventually arrive at the calculation here. The way we actually sampled this path is the product of all
  // of the forward path-space PDFs. We want to weight that against every other way we could have legitimately
  // sampled it. Consider this configuration:
  //
  //    A0 ----> A1 ~ ~ ~ B1 <---- B0
  //
  // We have two paths A and B with two vertices each. And the way we connected the entire path this time is by
  // sampling A1 from A0, sampling B1 from B0, and then connecting A1 to B1. If the entire path has N vertices in
  // general, then there are N + 1 ways of sampling it at most. It is N + 1 because we can form the path completely
  // "forward" and completely "reverse", and also via connection at each of its N - 1 edges. The other four strategies
  // in this case would be:
  //
  //    A0  ----> A1  ----> B1* ~ ~ ~ B0    (Connect B0 and B1)
  //    A0  ----> A1  ----> B1* ----> B0*   (Forward)
  //    A0  ~ ~ ~ A1* <---- B1  <---- B0    (Connect A0 and A1)
  //    A0* <---- A1* <---- B1  <---- B0    (Reverse)
  //
  // where the star notation indicates where we need the conjugate path-space probability density. As this suggests,
  // we need to conjugate each subpath cumulatively and in reverse order. Now notice that whenever applying the
  // balance heuristic, as in the initial example with P/(P+Q+R), we can divide through by the numerator to obtain
  // an equivalent expression 1/(1+D) where D=Q/P+R/P. If we do that, we obtain signficant term cancellation that
  // results in a nested arithmetic expression (1+Ri)Rj where Ri is the ratio of the reverse to forward PDFs.
  double denomA{1};
  double denomB{1};
  for (auto &vertex : pathA) {
    if (!vertex.runtime.flags.isDeltaScattering) {
      denomA *= vertex.runtime.pathPDF.reverse / vertex.runtime.pathPDF.forward;
      denomA += vertex.runtime.flags.isIncomplete ? 0 : 1;
    }
  }
  for (auto &vertex : pathB) {
    if (!vertex.runtime.flags.isDeltaScattering) {
      denomB *= vertex.runtime.pathPDF.reverse / vertex.runtime.pathPDF.forward;
      denomB += vertex.runtime.flags.isIncomplete ? 0 : 1;
    }
  }
  return finiteOrZero(1 / (denomA + denomB - 1));
}
#endif
