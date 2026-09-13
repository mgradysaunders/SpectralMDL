#include "RenderFilm.h"

#include "Sensor/Response.h"

RenderFilm::RenderFilm(const Response *response, size_t numBands,
                       size_t numPixelsX, size_t numPixelsY)
    : mFilm(response ? response->filmBandCount() : numBands, numPixelsX,
            numPixelsY),
      mResponse(response) {}
