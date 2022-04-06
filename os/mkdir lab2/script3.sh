#!/bin/bash

ps aux | sort -nrk9 | head -1 | awk '{print $2 ": " $9}'
