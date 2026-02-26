if [[ $1 == "fmt" ]]; then
  clang-format tests/*.cpp src/**/*.{cpp,h} -i
  exit
fi

if [[ $1 == "lint" ]]; then
  exit
fi

if [[ $1 == "help" || $1 == "-h" || $1 == "--help" ]]; then
  echo "Usage: ./build.sh [task] args"
  echo "tasks:
  run [filename]
  test
  clean
  fmt
  lint
  "
  echo "if no args are provided, the project just compiles"
  exit
fi

if [[ $1 == "clean" ]]; then
  rm -rf build
fi

if [[ $1 == "test" ]]; then
  cmake -S . -B build -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DBUILD_TESTS=ON >/dev/null 2>&1
  cmake --build build --target test_runner >/dev/null 2>&1
  ./build/test_runner
  exit
fi

cmake -S . -B build -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON && cmake --build build

if [[ $? -ne 0 ]]; then
  echo "Build failed"
  exit 1
fi

if [[ $1 == "run" ]]; then
  ./build/shikimori $2
fi
