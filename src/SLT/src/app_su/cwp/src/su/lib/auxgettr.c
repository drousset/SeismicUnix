#include "su.h"
#include "segy.h"
#include "header.h"

/* auxgettr - get a segy trace from an auxiliary file by file pointer
 *
 * Note: 
 *      For an auxiliary segy file input (file pointer auxinfp), either
 *    	auxgethdr(auxinfp,&ch,&bh) must be called before first call 
 *	auxgettr(auxinfp,&tp), or efseek(auxinfp,3600,0) must be called,
 * 	to position auxinfp to 3600 bytes from begining of the file. 
 *
 * Returns:
 *	int: number of bytes read on current trace (0 after last trace)
 *
 * Synopsis:
 *	int auxgettr(FILE *fp, segy *tp)
 *
 * Example:
 *	segytrace tr;
 *      FILE *auxinfp, *auxoutfp;
 *      segybhdr bh;
 *      segychdr ch;
 *	...
 *
 *      auxgethdr(auxinfp,&ch,&bh);
 *      auxputhdr(auxoutfp,&ch,&bh);
 *	while (auxgettr(auxinfp,&tr)) {
 *		tr.offset = abs(tr.offset);
 *		auxputtr(auxoutfp,&tr);
 *	}
 *	...
 *       : Zhiming Li 
 */

int auxgettr(FILE *fp, segy *tp)
{
	int ns;		/* number of samples from header	*/
	int nsegy; 	/* segy bytes read		 	*/
	int nread;	/* bytes read				*/
	int databytes;  /* bytes of data per trace 		*/

	/* Get trace header*/
	nread = efread(tp, 1, HDRBYTES, fp);
	if(nread==0) return 0;

	/* find number of samples per trace */
	ns = tp->ns;
	databytes = ns *sizeof(float);
	nsegy = HDRBYTES + databytes;

	/* read in data */
	nread = efread((char*)tp+HDRBYTES, 1, databytes, fp);
	if (nread != databytes) err("auxgettr: error read trace \n");
	return nsegy;
}
