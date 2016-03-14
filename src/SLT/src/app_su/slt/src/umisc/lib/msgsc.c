 	#include "subc.h" 

void msgsc (char *message, float ff) {
	int i,j;
	char s;
	i=0;
	while ((s=message[i]) != '\0') i++;
	 
	j=0;
	for(j=0;j<i;j++) {
	     	s = message[j];
	     	fprintf(stderr,"%c",s);
	}
	fprintf(stderr,"%f \n",ff); 
	fflush(stderr);
}

void msgscf (char *message, float ff) {
	int i,j;
	char s;
	i=0;
	while ((s=message[i]) != '\0') i++;
	 
	j=0;
	for(j=0;j<i;j++) {
	     	s = message[j];
	     	fprintf(stderr,"%c",s);
	}
	fprintf(stderr,"%f \n",ff); 
	fflush(stderr);
}

void msgsci (char *message, int ff) {
	int i,j;
	char s;
	i=0;
	while ((s=message[i]) != '\0') i++;
	 
	j=0;
	for(j=0;j<i;j++) {
	     	s = message[j];
	     	fprintf(stderr,"%c",s);
	}
	fprintf(stderr,"%d \n",ff); 
	fflush(stderr);
}

void msgscs (char *message, char *str) {
	int i,j;
	char s;
	i=0;
	while ((s=message[i]) != '\0') i++;
	 
	j=0;
	for(j=0;j<i;j++) {
	     	s = message[j];
	     	fprintf(stderr,"%c",s);
	}
	fprintf(stderr,"%s \n",str); 
	fflush(stderr);
}

/* fortran interface */
void msgsc_(char *message, float *ff) {
        msgsc (message,*ff);
}

void msgscs_(char *message, char *str) {
        msgscs (message,str);
}

void msgscf_(char *message, float *ff) {
        msgscf (message,*ff);
}

void msgsci_(char *message, int *ff) {
        msgsci (message,*ff);
}
