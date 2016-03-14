
#include "su.h"
#include "segy.h"
#include "header.h"

/* auxputtr - put a segy trace to an auxiliary file by file pointer
 *
 * Note:
 *      For an auxiliary segy file output (file pointer auxoutfp), either
 *      auxputhdr(auxoutfp,&ch,&bh) must be called before first call
 *      auxputtr(auxoutfp,&tp), or efseek(auxoutfp,3600,0) must be called,
 *      to position auxoutfp to 3600 bytes from begining of the file.
 *
 * Synopsis:
 *      int auxputtr(FILE *fp, segy *tp)
 *
 * Example:
 *      segytrace tr;
 *      FILE *auxinfp, *auxoutfp;
 *      segybhdr bh;
 *      segychdr ch;
 *      ...
 *
 *      auxgethdr(auxinfp,&ch,&bh);
 *      auxputhdr(auxoutfp,&ch,&bh);
 *      while (auxgettr(auxinfp,&tr)) {
 *              tr.offset = abs(tr.offset);
 *              auxputtr(auxoutfp,&tr);
 *      }
 *      ...
 *       : Zhiming Li
 */
 


void auxputtr(FILE *fp, segy *tp)
{
	int ns;         /* number of samples from header        */
        int nsegy;      /* segy bytes to write                  */

	ns = tp->ns;
	nsegy = HDRBYTES + ns * sizeof(float);
	efwrite(tp, 1, nsegy, fp);
	return;
}
