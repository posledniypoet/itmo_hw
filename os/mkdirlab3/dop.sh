#!/bin/bash

echo "i like linux-faq.ru" | at 12:15 tomorrow

at -l|awk ' {print $8,$2,$3,$4,$5,$6} '
