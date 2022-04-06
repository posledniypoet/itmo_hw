#!/bin/bash
link="https://docs.oracle.com/en/java/javase/11/docs/api/"
prefix="info/kgeorgiy/ja/kalinichev/implementor/"
implementor=$prefix"Implementor.java"
javadoc -private -link $link -cp ../../java-advanced-2021/artifacts/info.kgeorgiy.java.advanced.implementor.jar \
 -d ../javadoc $implementor
