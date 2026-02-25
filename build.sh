if [[ $1 == "fmt" ]]; then
  clang-format src/* -i
  exit
fi

if [[ $1 == "help" || $1 == "-h" || $1 == "--help" ]]; then
  echo "Usage: ./build.sh [task] args"
  echo "tasks:
  run [filename]
  clean
  fmt
  "
  echo "if no args are provided, the project just compiles"
  exit
fi

if [[ $1 == "clean" ]]; then
  rm -rf build
fi

cmake -S . -B build -G Ninja && cmake --build build

if [[ $1 == "run" ]]; then
  ./build/shikimori $2
fi
