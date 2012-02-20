#!/bin/bash

while read line
do
  if [ $line = "q" ]
  then
    break
  fi
  cat $line | ./hskonf
done
