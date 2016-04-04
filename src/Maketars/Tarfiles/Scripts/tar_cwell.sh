#! /bin/sh
set -x

FLOPPYDEV=/dev/rfd0b

i=0

while test "$i" != "28"
do
	/usr/etc/fdform $FLOPPYDEV
	tar -cvf $FLOPPYDEV cwell.tar.Z
	tar -tvf $FLOPPYDEV 
	/usr/etc/disk -e $FLOPPYDEV
	i=`expr $i + 1`
done
