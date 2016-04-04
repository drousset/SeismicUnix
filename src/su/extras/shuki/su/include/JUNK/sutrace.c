#include <stdio.h>
#define L 80
main(ac,av)
int ac;
char **av;
{
	int i=0;
	char buff[L],*pbuff;
	char *type,*name;

	while(fgets(buff,L,stdin)!=NULL) {

/* 		printf("%s\t",buff); */

		if(strlen(buff)>=L) {
			fprintf(stderr,"%s: input too long\n",av[0]);
			exit(-1);
		}

		/* PARSE THE LINE */
		for(pbuff=buff;*pbuff;pbuff++)
			if(*pbuff=='\t'||*pbuff==' '||*pbuff=='\n')
				*pbuff=(char)0;
		for(type=buff;*type==0;type++);
		for(name=type;*name!=0;name++);
		for(;*name==0;name++);

/* 		printf("(%d) type=%s name=%s\n",i,type,name); */

		printf("\toffset[%d] = (char*)&tr.%s - (char*)&tr;\n",i,name);

		i++;
	}
}
