#!/bin/sh

# declarations
output_dir="dist"
entry="src/Main.elm"
js_out="$output_dir/main.js"
debug=false
dist=false

for arg in $@; do
	if [[ $arg =~ "debug" ]]; then
		debug=true
	elif [[ $arg =~ "dist" ]]; then
		dist=true
	else
		echo "Unexpected argument $arg"
	fi
done

# copy resources to dist
# markup and design
cp src/index.html $output_dir/index.html
cp src/index.css $output_dir/index.css
cp src/audio.js $output_dir/audio.js

# assets
cp -rf assets/ $output_dir/assets/

# compile elm code
if $debug; then
	echo "Building for debug"
	elm make $entry --output=$output_dir/debug-main.js --debug
	cp src/index_debug.html $output_dir/index_debug.html
else
	echo "Building for prod"
	elm make $entry --output=$js_out
fi

if $dist; then
	echo "Compressing client for distribution"
	zip -r client.zip $js_out
fi
