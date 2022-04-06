#!/bin/bash

path="info/kgeorgiy/ja/kalinichev/implementor"

javac -d jar \
  --module-path ../../java-advanced-2021/lib:../../java-advanced-2021/artifacts \
  module-info.java \
  $path/Implementor.java

cd jar

jar -cfm ../Implementor.jar \
  ../MANIFEST.MF \
  $path/*.class \
  module-info.class

cd ../
rm -rf jar
