#!/bin/bash
#
# !!!Requires inotify-tools!!!
# Checks a directory for changes and runs actions based on, e.g. saves.
#

while inotifywait -qq -e modify `pwd`; do
    $1
done
