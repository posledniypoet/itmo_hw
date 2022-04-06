#!/bin/bash

awk  '$6 == "<info>"'  /var/log/syslog > info.log
