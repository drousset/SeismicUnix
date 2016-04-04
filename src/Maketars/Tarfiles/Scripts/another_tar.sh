#! /bin/sh
# tar up all the public domain codes listed in G-numbers 

TARDEV=/dev/rfd0b

NAMELIST="untar_me_first cwp plot su"
ASCIILIST="README_BEFORE_UNTARRING"

TARLIST=
for i in $NAMELIST
	do
		TMPLIST="$TARLIST $i.tar.Z" 
		TARLIST=$TMPLIST
	done

tar -cvf $TARDEV $ASCIILIST $TARLIST

exit 0
