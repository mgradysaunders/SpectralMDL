#define DOCTEST_CONFIG_IMPLEMENT 1
#include "doctest.h"

int main(int argc, char **argv) {
  return doctest::Context(argc, argv).run();
}
