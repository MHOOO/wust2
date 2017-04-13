#!/bin/bash
echo "fastopt:"
sbt run test | grep production

echo "fullopt:"
sbt "set scalaJSStage in Global := FullOptStage" run test | grep production
