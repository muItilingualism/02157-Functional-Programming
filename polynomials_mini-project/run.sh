#!/bin/sh

generate_main_script() {
    echo "printfn \"Loading all .fsx files...\""
    find . -type f -name "*.fsx" | while read -r file; do
        absolute_path=$(realpath "$file")
        filename=$(basename "$absolute_path" .fsx)
        module_name="$(tr '[:lower:]' '[:upper:]' <<< ${filename:0:1})${filename:1}"
        echo "#load \"$absolute_path\""
        echo "open $module_name"
    done
    echo "printfn \"All .fsx files have been processed.\""
}

main_script=$(mktemp --suffix .fsx)

generate_main_script > "$main_script"

dotnet fsi --use:"$main_script"

rm "$main_script"

