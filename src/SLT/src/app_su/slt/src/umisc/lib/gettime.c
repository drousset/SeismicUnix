#include <stdio.h>
void gettime(char *cbuf) {
	FILE *fp;	
	char *cmd, *fname, *host;
	int len;

	cmd = (char*) malloc(1024);
	fname = (char*) malloc(80);
	host = (char*) malloc(80);

	bzero(cmd,1024);
	bzero(fname,80);
	bzero(host,80);

	gethostname(host,&len);
	sprintf(fname,"tmp.date.%s.%d\0",host,getpid());
	sprintf(cmd,"date > %s ",fname);
	system(cmd);

	fp = fopen(fname,"r");
	fread(cbuf,1,28,fp);
	fclose(fp);

	bzero(cmd,1024);
	sprintf(cmd,"/bin/rm %s ",fname);
	system(cmd);

	free(cmd);
	free(fname);
	free(host);
}
