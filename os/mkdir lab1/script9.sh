#!/bin/bash

first=$(wc -l /var/log/*.log 2>/dev/null | tail -n1 | awk '{print $1}')
sec=$(wc -l /var/log/**/*.log 2>/dev/null | tail -n1 | awk '{print $1}')

echo $(($first + $sec))
