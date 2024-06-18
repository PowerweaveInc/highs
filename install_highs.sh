set -e

cd HiGHS

mkdir -p build
cd build

cmake \
    -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_SHARED_LIBS=ON \
    -DFAST_BUILD=ON \
    -DIPX=ON \
    ..
cmake --build . --config Release -j 3 
cmake --install .
# running ldconfig is sometimes required in CI
# to re-build the library cache
ldconfig
