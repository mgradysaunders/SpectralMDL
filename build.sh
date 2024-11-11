rm -rf build
cmake -S. -Bbuild -GNinja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=build/install
