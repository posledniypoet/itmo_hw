#!/bin/bash

flag="0"
grep -sraohE "[a-zA-Z0-9._-]+@([a-zA-Z0-9-]+\.)+[a-zA-Z]{2,}" /etc |
while read line; do
  if [[ "$flag" == "1" ]]; then
    echo -n ","
  fi

  flag="1"
  echo -n "$line"
done >emails.lst
