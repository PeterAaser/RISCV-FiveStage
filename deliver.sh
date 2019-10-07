#!/bin/bash

read -p "Enter your student username (the one you use on badboard): "  username
echo "Cleaning your project"

# ./sbt.sh clean

echo "Creating archive"
mkdir wrap
mkdir wrap/project
cp -r src ./wrap/
cp build.sbt ./wrap
cp project/Dependencies.scala ./wrap/project/Dependencies.scala
cp project/build.properties ./wrap/project/build.properties
cp sbt.sh ./wrap
(cd ./wrap/; tar czfv $username.tar.gz .)
mv ./wrap/$username.tar.gz .

rm -rf ./wrap

echo "Unwrapping and testing your wrapped package"
mkdir wrapTest
tar -C ./wrapTest -xvf $username.tar.gz
./wrapTest/sbt.sh test
rm -rf ./wrapTest

echo "If the test output looked good then you're good to go!"
