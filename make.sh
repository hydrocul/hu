#!/bin/bash

# ./make.sh
#   一部のテストケースを省略したビルド
# ./make.sh testall
#   すべてのテストケースを実行したビルド

echo cd `dirname $0`
cd `dirname $0`

. ./submake.sh

{

  buildlib_todo
  if [ $? -ne 0 ] ; then
    exit 1
  fi

  buildlib_compile
  if [ $? -ne 0 ] ; then
    exit 1
  fi

  if [ "$1" = "testall" ] ; then
    buildlib_testall
    if [ $? -ne 0 ] ; then
      exit 1
    fi
  else
    buildlib_test
    if [ $? -ne 0 ] ; then
      exit 1
    fi
  fi


  buildlib_build
  if [ $? -ne 0 ] ; then
    exit 1
  fi

  if [ "$1" = "all" ] ; then
    buildlib_scaladoc
    if [ $? -ne 0 ] ; then
      exit 1
    fi
  fi

} 2>&1 | sed -u -e 's%\([A-Za-z0-9]\+\.\(java\|scala\)\):\([0-9]\+\)%\x1b[31m\1\x1b[m:\x1b[31m\3\x1b[m%g'
