#! /bin/sh

MAX=27
CODES="README_DMOVZ dmovz.tar.Z"

j=1

while test "$j" != "$MAX"
do
	echo
	echo
	echo "formatting diskette"
	echo
	echo
		/usr/etc/fdform /dev/rfd0b

	echo
	echo
	echo "tarring to diskette"
	echo
	echo
		tar -cvf /dev/rfd0b $CODES
	echo
	echo
	echo
	echo "ejecting diskette"
	echo
		/usr/etc/disk -e /dev/rfd0b
	echo
	echo
		j='expr $j + 1'
done

exit 0


