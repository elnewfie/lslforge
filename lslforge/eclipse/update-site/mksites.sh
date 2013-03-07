#!/bin/sh

if [ "$#" != "1" ]; then
    echo "Usage: mksites.hs <version>"
    exit 1
fi

unzip artifacts.jar
unzip content.jar

ln -s dist/$1 current_release

for site in `awk '{print $1;}' siteurls.txt`
do
    url=`grep "^$site " siteurls.txt | awk '{print $2;}'`
    desc=`grep "^$site " siteurls.txt | sed 's/^.*@//'`
    echo $desc
    if [ "$METADIST" = "" ]; then
        ant -DbaseUrl=$url -Dsite=$site -Ddesc="$desc" -Dversion=$1
    else
        ant -DbaseUrl=$url -Dsite=$site -Ddesc="$desc" -Dversion=$1 -Dmeta-dist="$METADIST"
    fi
done
