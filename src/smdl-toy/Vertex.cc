#include "Vertex.h"

bool Vertex::scatter(const smdl::float3 &w, float &dirPdf, float &dirPdfAdjoint,
                     Color &f) const {
  return material->scatter_evaluate(materialInstance, transportMode, wPrev, w,
                                    dirPdf, dirPdfAdjoint, &f[0]);
}

bool Vertex::scatter_sample(const smdl::float4 &xi, smdl::float3 &w,
                            float &dirPdf, float &dirPdfAdjoint, Color &f,
                            int &isDelta) const {
  return material->scatter_sample(materialInstance, transportMode, xi, wPrev, w,
                                  dirPdf, dirPdfAdjoint, &f[0], isDelta);
}

float Vertex::convert_direction_pdf_to_point_pdf(
    float pdf, const Vertex &nextVertex) const {
  if (!nextVertex.isAtInfinity) {
    auto sep{nextVertex.point - point};
    auto invDistance2{smdl::finite_or_zero(1 / smdl::length_squared(sep))};
    pdf *= invDistance2;
    if (nextVertex.intersection)
      pdf *= smdl::abs_dot(nextVertex.intersection->geometryNormal,
                           sep * std::sqrt(invDistance2));
  }
  return pdf;
}

bool Vertex::reconnect(const Vertex &nextVertex, float dirPdfAdjoint,
                       Color &f) const {
  pdfAdjoint =
      nextVertex.convert_direction_pdf_to_point_pdf(dirPdfAdjoint, *this);
  if (float unused{}; scatter(smdl::normalize(nextVertex.point - point), unused,
                              dirPdfAdjoint, f)) {
    if (prevVertex && //
        prevVertex->prevVertex) {
      prevVertex->pdfAdjoint =
          convert_direction_pdf_to_point_pdf(dirPdfAdjoint, *prevVertex);
    }
    return true;
  }
  return false;
}

#if 0
[[nodiscard]]
static float multiple_importance_weight(const Vertex *cameraVertex,
                                        const Vertex *lightVertex) {
  float termSum{0.0f};
  if (cameraVertex) {
    for (float term{1.0f}; cameraVertex->prevVertex;
         cameraVertex = cameraVertex->prevVertex) {
      term *= cameraVertex->pdfAdjoint / cameraVertex->pdf;
      termSum += term;
    }
  }
  if (lightVertex) {
    for (float term{1.0f}; lightVertex->prevVertex;
         lightVertex = lightVertex->prevVertex) {
      term *= lightVertex->pdfAdjoint / lightVertex->pdf;
      termSum += term;
    }
  }
  return 1.0f / (1.0f + termSum);
}

bool connect_bidirectional(const Scene &scene,
                           smdl::BumpPtrAllocator &allocator,
                           const std::function<float()> &rngf,
                           const Color &wavelengthBase, Vertex *cameraVertex,
                           Vertex *lightVertex, Color &beta, float &misWeight,
                           smdl::float2 &pixelCoord) {
  if (!cameraVertex) {
    return false;
  }
  if (!lightVertex) {
    return false;
  }
  SMDL_PRESERVE(*cameraVertex, *lightVertex);
  SMDL_SANITY_CHECK(cameraVertex->source == smdl::TRANSPORT_MODE_RADIANCE);
  SMDL_SANITY_CHECK(lightVertex->source == smdl::TRANSPORT_MODE_IMPORTANCE);
  if (!cameraVertex->prevVertex && lightVertex->prevVertex &&
      !lightVertex->isAtInfinity) {
    SMDL_PRESERVE(lightVertex->prevVertex->pdfAdjoint);
    auto result{Camera_last_vertex_sample(scene.camera,
                                          smdl::float2(rngf(), rngf()),
                                          *lightVertex, *cameraVertex)};
    beta = cameraVertex->beta;
    misWeight = multiple_importance_weight(cameraVertex, lightVertex);
    pixelCoord = cameraVertex->pixelCoord;
    return result && scene.test_visibility(allocator, rngf, wavelengthBase,
                                           *cameraVertex, *lightVertex, beta);
  }
  if (cameraVertex->prevVertex && !lightVertex->prevVertex &&
      !cameraVertex->isAtInfinity) {
    SMDL_PRESERVE(cameraVertex->prevVertex->pdfAdjoint);
    auto result{Light_last_vertex_sample(scene, rngf(),
                                         smdl::float2(rngf(), rngf()),
                                         *cameraVertex, *lightVertex)};
    beta = lightVertex->beta;
    misWeight = multiple_importance_weight(cameraVertex, lightVertex);
    return result && scene.test_visibility(allocator, rngf, wavelengthBase,
                                           *cameraVertex, *lightVertex, beta);
  }
  if (!cameraVertex->prevVertex || cameraVertex->isAtInfinity ||
      !lightVertex->prevVertex || lightVertex->isAtInfinity) {
    return false;
  }
  SMDL_PRESERVE(cameraVertex->prevVertex->pdfAdjoint,
                lightVertex->prevVertex->pdfAdjoint);
  smdl::float3 w{smdl::normalize(lightVertex->point - cameraVertex->point)};
  float cameraDirPdf{};
  float cameraDirPdfAdjoint{};
  Color cameraf{};
  if (!cameraVertex->scatter(w, cameraDirPdf, cameraDirPdfAdjoint, cameraf)) {
    return false;
  }
  lightVertex->pdfAdjoint = cameraVertex->convert_direction_pdf_to_point_pdf(
      cameraDirPdf, *lightVertex);
  if (cameraVertex->prevVertex && cameraVertex->prevVertex->prevVertex) {
    cameraVertex->prevVertex->pdfAdjoint =
        cameraVertex->convert_direction_pdf_to_point_pdf(
            cameraDirPdfAdjoint, *cameraVertex->prevVertex);
  }
  float lightDirPdf{};
  float lightDirPdfAdjoint{};
  Color lightf{};
  if (!lightVertex->scatter(-w, lightDirPdf, lightDirPdfAdjoint, lightf)) {
    return false;
  }
  cameraVertex->pdfAdjoint = lightVertex->convert_direction_pdf_to_point_pdf(
      lightDirPdf, *cameraVertex);
  if (lightVertex->prevVertex && lightVertex->prevVertex->prevVertex) {
    lightVertex->prevVertex->pdfAdjoint =
        lightVertex->convert_direction_pdf_to_point_pdf(
            lightDirPdfAdjoint, *lightVertex->prevVertex);
  }
  beta =
      cameraVertex->beta * cameraf * lightf * lightVertex->beta *
      (1.0f / smdl::length_squared(cameraVertex->point - lightVertex->point));
  misWeight = multiple_importance_weight(cameraVertex, lightVertex);
  return scene.test_visibility(allocator, rngf, wavelengthBase, *cameraVertex,
                               *lightVertex, beta);
}
#endif
