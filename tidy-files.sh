#!/bin/bash

for i in {1..25}
do
  if ls day$i.* day$i-* 1> /dev/null 2>&1; then
    # 若不存在，创建文件夹
    mkdir -p "day$i"

    # 移动文件到文件夹
    mv day$i.* day$i-* "day$i/" 2>/dev/null
  fi
done
