#!/usr/bin/env bash
echo "fastopt:"
sbt clean test

echo "fullopt:"
sbt clean "set scalaJSStage in Global := FullOptStage" test
