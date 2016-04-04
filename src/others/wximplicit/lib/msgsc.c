#include "par.h"
  	void msgsc(message,ff)
	float *ff;
        /* string message; */
	char message[];
	{
	int i,j;
	char s;
	i=0;
	while ((s=message[i]) != '\0') 
	  {
/*	  printf("%1s \n",&s);		*/
	  i++;
	  }
	  j=0;
	  for(j=0;j<i;j++) 
	     {
	     s = message[j];
	     fprintf(stderr,"%c",s);
	     }
	  fprintf(stderr," %f \n",*ff); 
	}
