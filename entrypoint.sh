#!/bin/bash -l
set -e
echo "Looking for rebuilds in ${1}"
Rscript -e "rebuilds::trigger_rebuilds('${1}')"
#Rscript -e "rebuilds::rebuild_missing_binaries(basename('${1}'))"
echo "Action complete!"
