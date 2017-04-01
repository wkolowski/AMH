#!/bin/bash

for file in img/*svg
do
	convert "$file" "$file.jpg"
	rm "$file"
done

