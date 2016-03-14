/* backup 3 files to disk */
#include "usu.h"

void tar3to(char *dsn, char *file1, char *file2, char *file3) {

        char *cmd;

	cmd = (char*) malloc(1024*sizeof(char));
	bzero(cmd,1024);
        /* tar the files to tape */
	sprintf(cmd,"tar -cvKf %s %s %s %s >/dev/null ",
                dsn,file1,file2,file3);
	if(system(cmd)!=0) err("tar3to error");
	free(cmd);
}

void tar3fr(char *dsn, char *file1, char *file2, char *file3) {

        char *cmd;

	cmd = (char*) malloc(1024*sizeof(char));

	bzero(cmd,1024);
        /* tar the files to tape */
	sprintf(cmd,"tar -xvKf %s %s %s %s >/dev/null ",
                dsn,file1,file2,file3);
	if(system(cmd)!=0) err("tar3fr error");
	free(cmd);
}

