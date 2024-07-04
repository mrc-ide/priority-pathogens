#!/usr/bin/env bash

set -eux

PACKAGE_ORG=mrc-ide
PACKAGE_NAME=priority-pathogen
GIT_SHA=${BUILDKITE_COMMIT:0:7}
TAG_SHA=$PACKAGE_ORG/$PACKAGE_NAME:$GIT_SHA

docker build -t $TAG_SHA --progress=plain -f .buildkite/Dockerfile .
docker run -e GITHUB_TOKEN --rm -w /src $TAG_SHA Rscript /src/.buildkite/pipeline.R
