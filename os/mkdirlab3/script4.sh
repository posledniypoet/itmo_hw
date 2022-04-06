#!/bin/bash
./script4mult.sh &
pid1=$!
./script4mult.sh &
pid2=$!
./script4mult.sh &
pid3=$!

cpulimit -p $pid1 -l 10 &

kill $pid3
