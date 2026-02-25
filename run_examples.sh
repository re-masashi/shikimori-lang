RED='\033[0;31m'
NC='\033[0m'

files=($(ls examples/*.shiki))
total=${#files[@]}
count=0
passed=0
failed=()

for file in "${files[@]}"; do
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
