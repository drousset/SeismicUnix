#! /bin/sh
# tar up all the public domain codes listed in G-numbers 

TARDEV=/dev/rfd0b

NAMELIST="Installer cwp plot su cshot rgps"
ASCIILIST=" README_DAVE README_FIRST README_TO_INSTALL README_X  SUN_NOTES SUN_NOTES_2 G-numbers"

TARLIST=
for i in $NAMELIST
	do
		TMPLIST="$TARLIST $i.tar.Z" 
		TARLIST=$TMPLIST
	done

tar -cvf $TARDEV make.2.1 $ASCIILIST $TARLIST

exit 0
