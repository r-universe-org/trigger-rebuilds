#!/bin/bash -l
set -e
echo "Looking for rebuilds in ${1}"
Rscript -e "rebuilds::trigger_rebuilds('${1}')"
echo "Action complete!"
