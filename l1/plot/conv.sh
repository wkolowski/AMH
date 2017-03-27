#!/bin/bash

for file in *svg
do
	convert "$file" "$file.jpg"
	rm "$file"
done

