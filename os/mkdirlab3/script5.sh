#!/bin/bash

mkfifo pipe0

./script5obr.sh&
./script5gen.sh

rm pipe0
