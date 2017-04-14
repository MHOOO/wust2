#!/bin/bash
echo "fullopt:"
sbt "set scalaJSStage in Global := FullOptStage" run test
