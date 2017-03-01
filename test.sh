#!/bin/bash
echo "fastopt works:"
sbt run

echo "fullopt fails:"
sbt "set scalaJSStage in Global := FullOptStage" run
