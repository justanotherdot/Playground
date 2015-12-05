#!/bin/bash

while inotifywait -qq -e modify `pwd`; do
    ./main.ml
done
