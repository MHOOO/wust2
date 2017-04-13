#!/bin/bash
echo "fastopt:"
sbt run test

echo "fullopt:"
sbt "set scalaJSStage in Global := FullOptStage" run test
