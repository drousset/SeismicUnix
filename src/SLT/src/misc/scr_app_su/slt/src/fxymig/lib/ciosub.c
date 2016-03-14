#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <strings.h>

/* fortran callable c subroutine of read */
void freadc(char *fname,int lfname,int irec,int lrec,float *a);

void freadc(char *fname,int lfname,int irec,int lrec,float *a) {
	long long lpos;
	off_t offset;
	FILE *fp;
	int ierr;
	char* s;

	lpos = (irec-1);
	lpos = lpos*lrec; 

	bcopy(&lpos,&offset,8);

	s = malloc((lfname+1)*sizeof(char));
	bcopy(fname,s,lfname);
	s[lfname] = '\0';

	if((fp = fopen(s,"r"))==NULL) fprintf(stderr,"fopen failed");

	ierr = fseeko(fp,offset,0);

	ierr=fread(a,sizeof(char),lrec,fp);

	free(s);
	fclose(fp);
}

/* fortran interface */
void freadc_(char *fname, int *lfname, int *irec, int *lrec, float *a) {
	freadc(fname,*lfname,*irec,*lrec,a);
}

/* fortran callable c subroutine of write */
void fwritec(char *fname,int lfname,int irec,int lrec,float *a);

void fwritec(char *fname,int lfname,int irec,int lrec,float *a) {
	long long lpos;
	off_t offset;
	FILE *fp;
	int ierr;
	char* s;

	lpos = (irec-1);
	lpos = lpos*lrec; 

	bcopy(&lpos,&offset,8);

	s = malloc((lfname+1)*sizeof(char));
	bcopy(fname,s,lfname);
	s[lfname] = '\0';

	if((fp = fopen(s,"r+"))==NULL) fprintf(stderr,"fopen failed");

	ierr = fseeko(fp,offset,0);

	ierr=fwrite(a,sizeof(char),lrec,fp);

	free(s);
	fclose(fp);
}

/* fortran interface */
void fwritec_(char *fname, int *lfname, int *irec, int *lrec, float *a) {
	fwritec(fname,*lfname,*irec,*lrec,a);
}
