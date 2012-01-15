#!/bin/bash

JARNAME=hu.jar
JARNAME2=hu-min.jar
#MAINCLASSNAME=hydrocul.hu.IO
MAINCLASSNAME=hydrocul.hv.Test

buildlib_todo()
{
  grep -rn TODO src > todo.tmp 2>&1
  cat todo.tmp
  echo -n "count of TODO: "
  echo `wc -l < todo.tmp`
  rm todo.tmp
}

_buildlib_todo_sub1()
{
  if [ ! -d $1 ] ; then
  return 1
  fi

  pushd $1/src >/dev/null
  find . -name "*.java" -exec echo _buildlib_todo_sub2 $1 {} \;
  find . -name "*.scala" -exec echo _buildlib_todo_sub2 $1 {} \;
  popd >/dev/null
}

_buildlib_todo_sub2()
{
  grep TODO $1/src/$2 >/dev/null
  if [ $? -eq 0 ] ; then
    echo TODO $1/src/$2
    grep TODO $1/src/$2
  fi
}

buildlib_compile()
{
  _buildlib_compile_sub1 .
  if [ $? -ne 0 ] ; then
    return 1
  fi

  echo "compile process finished"
}

_buildlib_compile_sub1()
{
  if [ ! -d $1 ] ; then
    return 1
  fi

  if [ ! -d $1/class ] ; then
    echo mkdir $1/class
    mkdir $1/class
  fi
  if [ ! -d $1/touch ] ; then
    echo mkdir $1/touch
    mkdir $1/touch
  fi

  if [ -d $1/resource ] ; then
    cp -r $1/resource/* $1/class
  fi

  pushd $1/src >/dev/null
  {
    find . -name "*.java" -exec echo _buildlib_mkjava2 $1 {} \;
    find . -name "*.scala" -exec echo _buildlib_mkscala2 $1 {} \;
  } > build2.tmp
  popd >/dev/null
  mv $1/src/build2.tmp .
  touch build_java2.tmp
  touch build_scala2.tmp
  touch build_java3.tmp
  touch build_scala3.tmp
  . build2.tmp
  rm build2.tmp

  if [ -s build_java2.tmp ] ; then
    echo "javac -classpath ./class:./lib/* -sourcepath $1/src -d $1/class"
    cat build_java2.tmp
    javac -classpath ./class:./lib/* -sourcepath $1/src -d $1/class @build_java2.tmp

    if [ $? -ne 0 ] ; then
      rm build_java2.tmp
      rm build_scala2.tmp
      rm build_java3.tmp
      rm build_scala3.tmp
      return 1
    fi

    . build_java3.tmp
  fi

  if [ -s build_scala2.tmp ] ; then
    echo "scalac"
    cat build_scala2.tmp
    scalac -unchecked -deprecation -P:continuations:enable -classpath ./class:./lib/* -sourcepath $1/src -d $1/class @build_scala2.tmp

    if [ $? -ne 0 ] ; then
      rm build_java2.tmp
      rm build_scala2.tmp
      rm build_java3.tmp
      rm build_scala3.tmp
      return 1
    fi

    . build_scala3.tmp
  fi

  rm build_java2.tmp
  rm build_scala2.tmp
  rm build_java3.tmp
  rm build_scala3.tmp

  return 0

} # _buildlib_compile_sub1()

_buildlib_mkjava2()
{
  if [ $1/src/$2 -nt $1/touch/$2.touch ] ; then
    echo $1/src/$2 >> build_java2.tmp
    echo mkdir -p `dirname $1/touch/$2.touch` >> build_java3.tmp
    echo touch $1/touch/$2.touch >> build_java3.tmp
  fi
}

_buildlib_mkscala2()
{
  if [ $1/src/$2 -nt $1/touch/$2.touch ] ; then
    echo $1/src/$2 >> build_scala2.tmp
    echo mkdir -p `dirname $1/touch/$2.touch` >> build_scala3.tmp
    echo touch $1/touch/$2.touch >> build_scala3.tmp
  fi
}

buildlib_test()
{
  echo "testing..."

  mkdir -p ./tmp

  echo "java -classpath ./class:./lib/*:$SCALA_HOME/lib/* $MAINCLASSNAME test"
  java -classpath ./class:./lib/*:$SCALA_HOME/lib/* $MAINCLASSNAME test
  if [ $? -ne 0 ] ; then
    return 1
  fi

  echo "Test Success!"
}

buildlib_testall()
{
  echo "testing all..."

  echo "java -classpath ./class:./lib/*:$SCALA_HOME/lib/* $MAINCLASSNAME test all"
  java -classpath ./class:./lib/*:$SCALA_HOME/lib/* $MAINCLASSNAME test all
  if [ $? -ne 0 ] ; then
    return 1
  fi

  echo "Test Success!"
}

buildlib_build()
{
  if [ ! -d ./dist ] ; then
    mkdir ./dist
  fi
  if [ ! -d ./dist-tmp ] ; then
    mkdir ./dist-tmp
  fi
  if [ ! -d ./tmp ] ; then
    mkdir ./tmp
  fi

  if [ ./lib -nt ./dist-tmp/META-INF/MANIFEST.MF ] ; then
    pushd ./dist-tmp >/dev/null
      jar xvf $SCALA_HOME/lib/scala-library.jar
      jar xvf $SCALA_HOME/lib/scala-compiler.jar
      jar xvf $SCALA_HOME/lib/jline.jar
      find ../lib -name "*.jar" -exec jar xvf {} \;
      {
        echo "Manifest-Version: 1.0"
        echo -n "Main-Class: "
        echo $MAINCLASSNAME
      } > META-INF/MANIFEST.MF
      rm META-INF/*.SF
      rm META-INF/*.DSA
      rm META-INF/*.RSA
    popd >/dev/null
  fi
  cp -r ./class/* ./dist-tmp

  pushd ./dist-tmp >/dev/null
    echo "jar -cfm ../dist/$JARNAME META-INF/MANIFEST.MF *"
    jar -cfm ../dist/$JARNAME META-INF/MANIFEST.MF *
  popd >/dev/null

  pushd ./class >/dev/null
    echo "jar -cf ../dist/$JARNAME2 *"
    jar -cf ../dist/$JARNAME2 *
  popd >/dev/null

}

buildlib_scaladoc()
{
  if [ ! -d ./scaladoc ] ; then
    mkdir ./scaladoc
  fi

  pushd src >/dev/null
  find . -name "*.scala" > ../scaladoc.tmp
  echo "scaladoc -P:continuations:enable -d ../scaladoc -classpath ../class:../lib/* -encoding UTF-8 ..."
  scaladoc -P:continuations:enable -d ../scaladoc -classpath ../class:../lib/* -encoding UTF-8 @../scaladoc.tmp
  rm ../scaladoc.tmp
  popd >/dev/null
}

buildlib_clean()
{
  if [ -d ./class ] ; then
    echo rm -r ./class
    rm -r ./class
  fi
  if [ -d ./touch ] ; then
    echo rm -r ./touch
    rm -r ./touch
  fi
  if [ -d ./test/class ] ; then
    echo rm -r ./test/class
    rm -r ./test/class
  fi
  if [ -d ./test/touch ] ; then
    echo rm -r ./test/touch
    rm -r ./test/touch
  fi
  if [ -d ./tmp ] ; then
    echo rm -r ./tmp
    rm -r ./tmp
  fi
  if [ -d ./dist ] ; then
    echo rm -r ./dist
    rm -r ./dist
  fi
  if [ -d ./dist-tmp ] ; then
    echo rm -r ./dist-tmp
    rm -r ./dist-tmp
  fi
  if [ -d ./scaladoc ] ; then
    echo rm -r ./scaladoc
    rm -r ./scaladoc
  fi
}



