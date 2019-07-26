#! /bin/sh
# /*********************** self documentation **********************/
# GENDOCS - generate complete list of selfdocs in latex form
#
# Usage: gendocs -o  output filename is:  selfdocs.tex
#
# /**************** end self doc ********************************/

# Author: John Stockwell -- 5 Jan 1992

PATH=/bin:/usr/bin:/usr/ucb

##################$############################################################
# test for CWPROOT
###############################################################################
if test "${CWPROOT}" = ""
then
	/bin/echo "The environment variable \"CWPROOT\" "
	/bin/echo "is not set in the user's working shell environment."
	/bin/echo "To set this variable in C-shell, use the command: "
	/bin/echo "  setenv  CWPROOT  /your/cwp/root/path"
	/bin/echo "To set this variable in Bourne or Korn-shell, use the command:"
	/bin/echo "  export  CWPROOT=/your/cwp/root/path" ; exit 1

fi

################################################################################
# test for CWPSRC, use value if set, define as $CWPROOT if not set
# (CWPSRC allows one set of source code and documentation for multiple machines)
################################################################################
if test "${CWPSRC}" = ""
then
CWPSRC=$CWPROOT
fi

WORKINGDIR=`pwd`
ROOT=${CWPSRC}
OUTFILE=selfdocs.tex
FILE=$WORKINGDIR/$OUTFILE
DIR=${ROOT}/src
STRIP=$DIR/doc/Stripped
HEADALL=${ROOT}/src/doc/Headers/Headers.all

SHELLS="cwp.shell par.shell su.shell psplot.shell "

LIBS="psplot.lib xplot.lib Xtcwp.lib Xmcwp.lib su.lib cwp.lib tri.lib "

MAINS="par.main su.main.amplitudes \
su.main.attributes_parameter_estimation \
su.main.convolution_correlation \
su.main.data_compression \
su.main.data_conversion \
su.main.datuming \
su.main.decon_shaping \
su.main.dip_moveout \
su.main.filters \
su.main.gocad \
su.main.headers \
su.main.interp_extrap \
su.main.migration_inversion \
su.main.multicomponent \
su.main.noise \
su.main.operations \
su.main.picking \
su.main.stacking \
su.main.statics \
su.main.stretching_moveout_resamp \
su.main.supromax \
su.main.synthetics_waveforms_testpatterns \
su.main.tapering \
su.main.transforms \
su.main.velocity_analysis \
su.main.well_logs \
su.main.windowing_sorting_muting \
su.graphics.psplot su.graphics.xplot \
psplot.main xplot.main Xtcwp.main Xmcwp.main su.graphics.psplot \
su.graphics.xplot xtri tri.main tri.graphics.psplot "

CWPMAINS="cwp.main"


# check for arguments
for i
do
	case $i in
	-o)	# inform user that file is being built
		/bin/echo
		/bin/echo "Beginning generation of" 
		/bin/echo "$OUTFILE"
		/bin/echo "This will take about a minute"
		/bin/echo "             --- Please standby"
		/bin/echo

		rm $FILE

		# begin building selfdocs.tex
/bin/echo "%selfdocs.tex --- complete list of CWP Free program selfdocs" >> $FILE
		/bin/echo "% generated by --- GENDOCS" >> $FILE
		/bin/echo " " >> $FILE
		/bin/echo "\\documentclass[12pt]{article}" >> $FILE
		/bin/echo " " >> $FILE
		/bin/echo -n "\\" >> $FILE ; /bin/echo "textwidth 6.25in" >> $FILE
		/bin/echo -n "\\" >> $FILE ; /bin/echo "textheight 8.75in" >> $FILE
		/bin/echo "\\oddsidemargin .125in" >> $FILE
		/bin/echo "\\evensidemargin .125in" >> $FILE
		/bin/echo -n "%\\" >> $FILE; /bin/echo "topmargin -.5in" >> $FILE
		/bin/echo " " >> $FILE
#
#		/bin/echo -n "\\"; /bin/echo "title{Complete Listing of CWP Free" >> $FILE 
#		/bin/echo "Program Self-Documentations}" >> $FILE
#		/bin/echo "\\author{{\\em generated by GENDOCS}, \\\\\ " >> $FILE
#		/bin/echo "a shell script by John Stockwell \\\\\ " >> $FILE
#		/bin/echo "Center for Wave Phenomena \\\\\ " >> $FILE
#		/bin/echo "Colorado School of Mines}" >> $FILE
#		/bin/echo "\\date{\\""\\today}" >> $FILE


		/bin/echo -n "\\" >> $FILE ; /bin/echo "begin{document}" >> $FILE
#		/bin/echo "\\maketitle " >> $FILE

## if you do not have titlepagesd.tex, uncomment the lines commented out
## above and comment out the next line
		/bin/echo "\\input /usr/local/cwp/src/su/tutorial/titlepagesd.tex" >> $FILE
		/bin/echo "" >> $FILE
		cd $DIR/doc

		
		/bin/echo "\\pagebreak" >> $FILE
		/bin/echo "\\section*{Names and Short descriptions of the Codes}" \
			>> $FILE
		/bin/echo " CWPROOT = $CWPROOT" >> $FILE
		/bin/echo >> $FILE
		/bin/echo -n "\\" >> $FILE ; /bin/echo "begin{verbatim}" >> $FILE

		cat $HEADALL >> $FILE

		/bin/echo "\\end{verbatim}" >> $FILE

		/bin/echo -n "\\" >> $FILE ;  /bin/echo "begin{verbatim}" >> $FILE
		/bin/echo "" >> $FILE
		/bin/echo "To search on a program name fragment, type:" >> $FILE
		/bin/echo "      suname name_fragment <CR>" >> $FILE
		/bin/echo "" >> $FILE
		/bin/echo "For more information type: program_name <CR>" >> $FILE
		/bin/echo "" >> $FILE
		/bin/echo "  Items labeled with an asterisk (*) are C programs \
				that may">> $FILE
		/bin/echo "  or may not have this self documentation feature. " \
				>> $FILE
		/bin/echo "" >> $FILE
		/bin/echo "  Items labeled with a pound sign (#) are shell \
				scripts that may,">>$FILE
		/bin/echo "  or may not have the self documentation feature." \
				>> $FILE
		/bin/echo "" >> $FILE

		/bin/echo "\\end{verbatim}" >> $FILE

		/bin/echo "\\pagebreak" >> $FILE

		/bin/echo "\\section*{Self Documentations}" >> $FILE


		# use the Stripped versions
		cd $DIR/doc/Stripped

		/bin/echo "Mains: " >> $FILE

		for i in $CWPMAINS
		do
			for j in $STRIP/*.$i
			do
				/bin/echo -n "\\" >> $FILE ; /bin/echo "begin{verbatim}" >> $FILE
				/bin/echo "______" >> $FILE
				cat $j >> $FILE
				/bin/echo "\\end{verbatim}" >> $FILE
			done
			/bin/echo "\\pagebreak" >> $FILE
		done
		

		for i in $MAINS
		do
			for j in $STRIP/*.$i
			do
				/bin/echo -n "\\" >> $FILE ; /bin/echo "begin{verbatim}" >> $FILE
				cat $j >> $FILE
				/bin/echo "\\end{verbatim}" >> $FILE
				/bin/echo "\\pagebreak" >> $FILE
			done
		done

		/bin/echo "Shells: " >> $FILE

		for i in $SHELLS
		do
			for j in $STRIP/*.$i
			do
				/bin/echo -n "\\" >> $FILE ; /bin/echo "begin{verbatim}" >> $FILE
				/bin/echo "______" >> $FILE
				cat $j >> $FILE
				/bin/echo "\\end{verbatim}" >> $FILE
			done
			/bin/echo "\\pagebreak" >> $FILE
		done

		/bin/echo "Libs: " >> $FILE

		for i in $LIBS
		do
			for j in $STRIP/*.$i
			do
				/bin/echo -n "\\" >> $FILE ; /bin/echo "begin{verbatim}" >> $FILE
				cat $j >> $FILE
				/bin/echo "\\end{verbatim}" >> $FILE
				/bin/echo "\\pagebreak" >> $FILE
			done
		done

		/bin/echo "\\end{document}" >> $FILE

		/bin/echo "The document $OUTFILE is now ready."
		exit 1
		;;
	esac
done

# else /bin/echo a usage message
/bin/echo
/bin/echo "GENDOCS - Shell to GENerate the complete listing of CWP selfDOCS"
/bin/echo
/bin/echo
/bin/echo "Output is the file in LaTeX format called:    selfdocs.tex" 
/bin/echo
/bin/echo
/bin/echo "Usage:  gendocs -o"  

exit 0
