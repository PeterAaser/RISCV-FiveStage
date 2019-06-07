#!/bin/bash

read -p "Enter your student username (the one you use on badboard): "  username
echo "Cleaning your project"

./sbt.sh clean

echo "Creating archive"
mkdir wrap
cp -r src ./wrap/
cp build.sbt ./wrap
cp project/Dependencies.scala ./wrap/project/Dependencies.scala
cp project/build.properties ./wrap/project/build.properties
cp sbt.sh ./wrap
tar czfv $username.gz wrap

rm -rf ./wrap

echo "Unwrapping and testing your wrapped package"
mkdir wrapTest
tar -C ./wrapTest -xvf $username.gz
./wrapTest/wrap/sbt.sh test
rm -rf ./wrapTest

echo "If the test output looked good then you're good to go!"
