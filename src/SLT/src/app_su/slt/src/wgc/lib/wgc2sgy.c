/* wgc code-4 (ibm) to segy (ieee) trace conversion 
*
* 
* void wgc2sgy(char *wtrace, char *strace, int sort, int sr, int ns,
*	       int itracl, itracr); 
* wgc2sgy converts a wgc code4 trace (400 bytes of header and ns samples of
* floating point data) into a segy trace (240 bytes of header and ns samples of
* floating point data). IBM floating point to IEEE floating point conversion
* of both header floating point values and data is done in this subroutine. 
*
* input: 
*	wtrace	--	wgc code4 trace (ibm) 
*	sort	--	data sort code 
*				1=shot
*				2=cdp
*				4=stack
*	sr	--	sampling rate in micro-second 
*	ns	--	number of samples per trace
*	tracl	--	trace number within the line
*	tracr	--	trace number within the reel
* output:
*	strace	--	segy trace (ieee)
* 
* author: zhiming li	      		8/13/92
*/

#include "wgc.h"
#include "wgc4.h"
#include "segy.h"
#include "header.h"
#include "par.h"


void wgc2sgy(char *wtrace, char *strace, int sort, int sr, int ns,
		int tracl, int tracr) {
	
	static int iccdp = 0, icount = 0;
	float tmp, tmp2, tmp3;
	segytrace st;
	wgc4trace wt; 

	/* zero output st header*/
	bzero(&st,240);
	bcopy(wtrace,(char *)&wt,400+ns*4);

	/* trace header conversion */
	st.tracl = tracl;
	st.tracr = tracr;
	st.fldr = wt.idnti4;  	
	st.tracf = wt.itrci2;  	
	st.ep = wt.shpti2;  	
	st.cdp = wt.cdnmi4;
	if(sort == 4) {
		st.cdpt = 1;	
	} else {
		if (iccdp != st.cdp) {
			icount = 0;
			iccdp = st.cdp;
		}
		icount = icount +1;
		st.cdpt = icount;	
	}
	st.trid = 1;
	if(wt.stkwi2 < 1) st.trid = 2;
	if(sort==4) {
		st.nvs = 1;
		st.nhs = wt.stkwi2;
	} else { 
		st.nvs = wt.stkwi2;
		st.nhs = 1;
	}
	st.duse = 1;
	tmp2 = wt.ssddr4;
	conv_float( &tmp2, &tmp, 4, 1); 
	st.offset = (int) tmp;
	st.gelev = wt.eledi2;
	st.selev = wt.elesi2;
	st.sdepth = wt.dpsri2;
	st.gdel = wt.eddli2;
	st.sdel = wt.edsli2;
	tmp2 = wt.wtdsr4;
	conv_float(&tmp2, &tmp, 4, 1); 
	st.swdep = (int) tmp; 
	tmp2 = wt.wtddr4;
	conv_float(&tmp2, &tmp, 4, 1); 
	st.gwdep = (int) tmp; 
	st.scalel = 1;
	st.scalco = 1;
	st.sx = wt.xcdsi4;
	st.sy = wt.ycdsi4;
	st.gx = wt.xcddi4;
	st.gy = wt.ycddi4;
	st.counit = 1;
	if(sort==1) {
		st.wevel = wt.vwlsi2;
	} else {
		st.wevel = wt.vwlmi2;
	}
	if(sort==1) {
		st.swevel = wt.vslsi2;
	} else {
		st.swevel = wt.vslmi2;
	}
	st.sut = wt.utsli2;
	st.gut = wt.utdli2;
	st.sstat = wt.stcsi2 + wt.cdsci2 / 2; 
	st.gstat = wt.stcdi2 + wt.cdsci2 / 2; 
	if(sort==4) {
		st.tstat = 0;
	} else {
		tmp2 = wt.fscsr4;
		conv_float(&tmp2, &tmp3, 4, 1); 
		tmp = tmp3;
		tmp2 = wt.fscdr4;
		conv_float(&tmp2, &tmp3, 4, 1); 
		tmp = tmp + tmp3;
		tmp2 = wt.rscsr4;
		conv_float(&tmp2, &tmp3, 4, 1); 
		tmp = tmp + tmp3;
		tmp2 = wt.rscdr4; 
		conv_float(&tmp2, &tmp3, 4, 1); 
		tmp = tmp + tmp3;
		tmp = tmp - wt.stcsi2 - wt.stcdi2 - wt.cdsci2;
		st.tstat = (short) tmp;
	}
	if (wt.tmfsi2!=0) {
		st.delrt = wt.tmfsi2;
	} else {
		st.delrt = sr / 1000;
	}
	st.muts = st.delrt;
	st.mute = wt.strti2;
	st.ns = ns;
	st.dt = sr;

	/* floating point data conversion */
	conv_float((char *)wt.data, (char *)st.data, ns, 1);
	bcopy((char *)&st,strace,240+ns*4);
} 
