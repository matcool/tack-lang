#!/bin/sh

total=0
failed=0

for folder in */; do
	set +e
	echo "\e[36m- Running test: $folder\e[m"
	./test.sh $folder
	if [ $? -ne 0 ]; then 
		failed=$(( failed + 1 ))
	fi
	total=$(( total + 1 ))
done

echo "Done! $failed/$total failed"