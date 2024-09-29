#!/bin/sh

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

poly_file="$script_dir/poly.fsx"

if [ ! -f "$poly_file" ]; then
    echo "File not found: $poly_file"
    exit 1
fi

temp_script=$(mktemp --suffix .fsx)

cat << EOF > "$temp_script"
#load "$poly_file"
open Poly
printfn "Module Poly loaded successfully."
EOF

dotnet fsi --use:"$temp_script"

rm "$temp_script"

