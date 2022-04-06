#!/bin/bash

if [[ "$HOME" == "$PWD" ]];
 then 
     echo "$HOME"
     exit 0;
 else 
     echo ERROR:not home directory
     exit 1;
 fi
