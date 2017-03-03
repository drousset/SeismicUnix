#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define DOINITS
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
#include "../../inc/bom.h"
#include "../../inc/cnd.h"
#include "../../inc/cnv.h"
#include "../../inc/com.h"
#include "../../inc/comlists.h"
#include "../../inc/contouring.h"
#include "../../inc/cpf.h"
#include "../../inc/csf.h"
#include "../../inc/datafilelist.h"
#include "../../inc/dfir.h"
#include "../../inc/dfm.h"
#include "../../inc/dload.h"
#include "../../inc/eam.h"
#include "../../inc/exm.h"
#include "../../inc/fir.h"
#include "../../inc/fks.h"
#include "../../inc/gam.h"
#include "../../inc/gd2.h"
#include "../../inc/gdm.h"
#include "../../inc/gem.h"
#include "../../inc/hdr.h"
#include "../../inc/icm.h"
#include "../../inc/lhf.h"
#include "../../inc/mem.h"
#include "../../inc/msg.h"
#include "../../inc/nnm.h"
#include "../../inc/nvars.h"
#include "../../inc/sam.h"
#include "../../inc/scm.h"
#include "../../inc/sddhdr.h"
#include "../../inc/site.h"
#include "../../inc/smm.h"
#include "../../inc/snf.h"
#include "../../inc/spe.h"
#include "../../inc/specdata.h"
#include "../../inc/spectrogram.h"
#include "../../inc/tok.h"
#include "../../inc/tt.h"
#include "../../inc/uom.h"
#include "../../inc/usr.h"
#include "../../inc/vars.h"
#include "../../inc/wild.h"
#undef DOINITS


void /*FUNCTION*/ initsac()
{
	long int nerr;
	void zgetgd(), zinfo();
        char *dummy;
        int i;
        static long ifirst = 1;

	/*=====================================================================
	 * PURPOSE:  To initialize (or reinitialize) SAC.
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MUNOUT
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    exm:     ktime, kdate, kmach
	 *    gam:     kgddef
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  inimsg, initcommon, begingraphics, zinfo, flash, zgetgd,
	 *             setmsg, apcmsg, aplmsg
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920330:  Block data initialization implemented during proting to 
	 *             IBM RISC 6000.
	 *    890105:  Changed from terminal output to message subsystem.
	 *    881122:  Added initialization of default graphics device.
	 *    860327:  Moved call to DELIMS from here to INICM.
	 *    860218:  Added call to INIMSG.
	 *    830818:  Added call to ZGTERM.
	 *    821004:  Added initialization of graphics library.
	 *    810429:  Original version from top of MAINLP.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850222
	 *===================================================================== */
	/* PROCEDURE: */
/*
#ifndef POSIX
        i = ieee_flags("set","direction","nearest",&dummy);
#endif
*/
	/* -- BLOCK DATA initialization of variables in common blocks. */
	initblkdata();

	/* - Initialization that can't be handled in lower level modules. */

	/* -- SAC error message function. */
	inimsg();

	/* -- Common blocks. */
	initcommon();

	/* -- Graphics Library. */
	begingraphics( &nerr );

	/* -- Create time/date/machine stamp. */
	zinfo( kmexm.ktime,9, kmexm.kdate,9, kmexm.kmach,9 );

	/* -- Get name of default graphics device. */
	zgetgd( kmgam.kgddef,9 );

	/* Initialize Data Base Module */
	inissi () ;

        /* Read in the Resource Control file (RC) 
         *   to handle options and settings
         *   Status: Unknown as of 101.2
         *   rc();
         */

	/* - Say hello. */

        sac_history_file_set(NULL);

        if (ifirst){
            ifirst = 0;
	    xabout () ;
        }

L_8888:
	return;

} /* end of function */




void /*FUNCTION*/ initblkdata()
{
        long int _i, _r;
        static int _aini = 1;


        if( _aini ){ /* Do 1 TIME INITIALIZATIONS! */
                cmextcom.nfiles = 0;
                cmgdm.lginit = FALSE;
		cmdfm.ndsflcnt = 0;
                cmicnv.icnver = 0;
                cmvars.lvarsinit = FALSE;
                cmgetvlist.nlevelsgt = 0;
                cmcopyvlist.nlevelscp = 0;
                cmprintvlist.nlevelspr = 0;
                strcpy( kmvars.varsidcode, "VARS" );
                _aini = 0;
        }

        /* - inc/dload */
        /* - inc/gdm */
        /* - inc/dfm */
        /* - inc/cnv */
        /* - inc/vars */

        return ;
} /* end of function */

