#! /bin/sh
# mkprehdr.sh - make hdr.h from segy.h
# Usage: mkprehdr.sh
# Caution: Don't move the unsigned substitutions below the signed ones!
#
# $Author: jkc $
# $Source: /src/segy/include/RCS/mkprehdr.sh,v $
# $Revision: 1.4 $ ; $Date: 87/09/26 23:09:07 $

cmd=`/bin/basename $0`

/bin/sed '
	/^typedef struct segy/,/unass\[/!d
	/;/!d
	s/;.*//
	s/unsigned short/u/
	s/unsigned long/v/
	s/short/h/
	s/long/l/
	s/float/f/
	s/double/z/
	s/int/i/
	s/char/s/
	/unass\[/d
' |
/usr/bin/awk '
BEGIN {
printf "static struct hdr_tag {\n\tchar *key;\tchar *type;\tint offs;\n"
printf "} hdr[] = {\n"
}
	{printf "\t\"%s\",\t\"%s\",\t0,\n", $2, $1}
END {
	printf "};\n"
} '
