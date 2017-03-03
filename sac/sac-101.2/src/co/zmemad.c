/** 
 * @file   zmemad.c
 * 
 * @brief  Get a pointer to a 16 bit value
 * 
 */

#include "stdu.h"
 
/** 
 *  This function returns a pointer to the address of the
 *    specified variable in 16 bit increments.
 *
 * \param pvar
 *    a pointer to a short integer
 * \param pvarloc 
 *    a pointer to a location which will contain the address of pvar
 *    ADDRESS_TYPE is defined in stdu.h as the variable type of addresses 
 *
 * \return Nothing
 *
 * \author D. Trimmer
 *
 * \note  This function is to be called by a FORTRAN routine.  The
 *	    '_' is appended to the name for compatibility with FORTRAN.
 *
 * \note The way the address is returned is for compatibility with FORTRAN
 *
 * \date 07/24/84  Under development--D. Trimmer
 * \date 07/24/84  Tested--D. Trimmer
 *
 */
void
zmemad(short        *pvar,
       ADDRESS_TYPE *pvarloc)
{
	*pvarloc = (ADDRESS_TYPE) pvar;
	*pvarloc = *pvarloc/2;	/* return word address--not byte address */
	return;
}
 
