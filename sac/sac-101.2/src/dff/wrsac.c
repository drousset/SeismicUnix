#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
void /*FUNCTION*/ wrsac(idfl, kname, kname_s, ldta, nerr)
long int idfl;
char *kname;   int kname_s;
long ldta;
long int *nerr;
{
	long int jcomp, ncerr, nlcdsk, nlcmem, nptwr, 
	 nun;
        float *bufout;
	void zwabs();
	int lswap;

	/* For Determining the byte-order of a file or endianness */
	long *hdrVer;
	char *header;
	int begin = 0;
	const int versionLocation = 76;
	
	/*=====================================================================
	 * PURPOSE:  To write a SAC data file from memory to disk.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list index number. [i]
	 *    KNAME:   Name of disk file to write. [c]
	 *    LDTA:    Set to .TRUE. if header and data are to be written. [l]
	 *             Set to .FALSE. if only header is to be written.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NCOMP, NLNDTA, NDXHDR, NDXDTA
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZGTFUN, ZDEST, ZNFILE, ZOPEN, ZWABS, ZCLOSE
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NUN:     Fortran file unit used to write file. [i]
	 *    NLNFIL:  Length of disk file to open or create. [i] {UNUSED}
	 *    NLCDSK:  Location in disk file to write to. [i]
	 *    NLCMEM:  Location in SACMEM array to write from. [i]
	 *    NPTWR:   Number of words to write. [i]
	 *    NCERR:   Error flag returned by ZCLOSE. [i] {UNUSED}
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870730:  Added logic to check file permissions before writing.
	 *    850731:  Changes due to new memory manager.
	 *    840118:  Deleted call to ZTRUNC.
	 *    800510:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870730
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	lswap = FALSE;
	
	/* - If header and data is to be written, a new file is created.
	 *   If header only is to be written, the old file is opened. */
	if( ldta ){
	    znfile( &nun, kname,kname_s, "DATA",5, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}
	else{
	    zopen_sac( &nun, kname,kname_s, "DATA",5, nerr );
	    if( *nerr != 0 ) {
		goto L_8888;
	    }
	    /* Handle writeheader with different byte-order or endianness */
	    
	    if((header = (char *) malloc(sizeof(char) * MHDRFILE * 4)) == NULL) {
	      *nerr = 301;
	      setmsg("ERROR", *nerr);
	      goto L_8888;
	    }
	    zrabs((int *)&nun, header, MHDRFILE, &begin, (int *)nerr);
	    if(*nerr != 0) goto L_8888;
	    hdrVer = (long *) (header + versionLocation * 4);
	    if(*hdrVer < 1 || *hdrVer > cmhdr.nvhdrc) {
	      byteswap( (void *) hdrVer, 4);
	      if(*hdrVer < 1 || *hdrVer > cmhdr.nvhdrc) {
		*nerr = 1317;
		setmsg("ERROR", *nerr);
		/* apcmsg2(&kmdfm.kdflrq[ic1 - 1], ic2-ic1+1); */
		aplmsg("not is sac format, nor byteswapped sac format.", 62);
		free(header);
		goto L_8888;
	      } else {
		free(header);
		lswap = TRUE;
	      }
	    }
	      
	}

	/* - Write the header to disk. */

	nlcdsk = 0;
	nlcmem = Ndxhdr[idfl];
        nptwr = MHDRFILE;

	if((bufout=(float *)malloc(MHDRFILE*4)) == NULL) {
	    printf("error allocating buffer-wrsac\n");
	    *nerr = 115;
	    goto L_8888;
	}

        map_hdr_out(cmmem.sacmem[nlcmem],bufout, lswap);

	zwabs( (int *)&nun, (char *)bufout, nptwr, (int *)&nlcdsk, (int *)nerr );

        free(bufout);

	/* - Write each data component, if requested. */

	if( ldta ){
	    for( jcomp = 0; jcomp < Ncomp[idfl]; jcomp++ ){
		nlcdsk = nlcdsk + nptwr;
		nlcmem = cmdfm.ndxdta[idfl - 1][jcomp];
		nptwr = Nlndta[idfl];
		zwabs( (int *)&nun, (char *)(cmmem.sacmem[nlcmem]), nptwr, (int *)&nlcdsk, (int *)nerr );
	    }
	}

	/* - Close disk file. */

L_8888:
	zclose( &nun, &ncerr );

	return;

} /* end of function */

