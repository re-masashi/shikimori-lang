RED='\033[0;31m'
NC='\033[0m'

# Files to skip (WIP)
skip=("all.shiki" "ffi.shiki" "comptime.shiki")

files=($(ls examples/*.shiki))
total=${#files[@]}
count=0
passed=0
failed=()

for file in "${files[@]}"; do
    filename=$(basename "$file")
    
    skip_file=false
    for s in "${skip[@]}"; do
        if [ "$filename" == "$s" ]; then
            skip_file=true
            break
        fi
    done
    
    if [ "$skip_file" = true ]; then
        echo "$filename - skipped"
        total=$((total - 1))
        continue
    fi
    count=$((count + 1))
    filename=$(basename "$file")
    if ./build.sh run "$file" > /dev/null 2>&1; then
        echo "$filename - ok"
        passed=$((passed + 1))
    else
        error=$(./build.sh run "$file" 2>&1)
        echo -e "${RED}[error] $filename ${NC}"
        echo "$error"
        failed+=("$filename")
    fi
done

echo ""
echo "$passed/$total succeeded"

if [ ${#failed[@]} -gt 0 ]; then
    echo "Failed:"
    for f in "${failed[@]}"; do
        echo "  - $f"
    done
fi
