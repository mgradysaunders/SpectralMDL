#pragma once

#include "Scene.h"

class BidirConnector final {
public:
  explicit BidirConnector(RNG &rng, Scene &scene) : rng(rng), scene(scene) {}

  void connect(std::vector<Vertex> &cameraPath,
               std::vector<Vertex> &lightPath) const;

private:
  void truncate_camera_path(Vertex &lastCameraVertex) const;

  void truncate_light_path(Vertex &lastLightVertex) const;

  void complete_camera_path_by_resampling_light(Vertex &lastCameraVertex) const;

  void complete_light_path_by_resampling_camera(Vertex &lastLightVertex) const;

  void connect_camera_path_to_light_path(Vertex &lastCameraVertex,
                                         Vertex &lastLightVertex) const;

  /*
  [[nodiscard]] float mis_weight(smdl::Span<Vertex> cameraPath,
                                 smdl::Span<Vertex> lightPath) const; */

private:
  RNG &rng;

  Scene &scene;
};
