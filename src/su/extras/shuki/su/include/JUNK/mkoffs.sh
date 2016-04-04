#! /bin/sh
# mkoffs.sh - get offset.h for mkhdr.c from segy.h
# Usage: mkoffs.sh
# Caveat: assumes tracl is the FIRST field in Segy
#
# $Author: jkc $
# $Source: /src/segy/include/RCS/mkoffs.sh,v $
# $Revision: 1.5 $ ; $Date: 87/09/26 23:15:08 $

cmd=`/bin/basename $0`

/bin/sed '
	/^typedef struct segy/,/unass\[/!d
	/;/!d
	s/;.*//
	/tracl/d
	/unass\[/d
' |
/usr/bin/awk '
BEGIN {
	old = "tracl"
	i = 1
}
{
	new = $NF
	printf "\thdr[%d].offs = hdr[%d].offs +", i, i-1
	printf "\n\t\t(char *) &tr.%s - (char *) &tr.%s;\n", new, old
	old = new
	++i
}
'
