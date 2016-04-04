#! /bin/sh
# tar up all the proprietary codes listed in U-numbers 

TARDEV=/dev/rfd0b
NAMELIST="Rays Ccco Cz Cxz Elas Triso Bdmo cxzcs extrap cxzco cwell sumiggbzo"

TARLIST=
for i in $NAMELIST
	do
		TMPLIST="$TARLIST $i.tar.Z" 
		TARLIST=$TMPLIST
	done

tar -cvf $TARDEV U-numbers $TARLIST

exit 0


