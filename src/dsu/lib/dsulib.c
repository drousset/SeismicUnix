/*
*	dsulib.c
*
*	10 Oct 1994  Murillo: c
*	Modified: 23 Feb 1995. A. Murillo
*	Modified: 02 Nov 1995. A. Murillo
*/

#include	"su.h"
#include	"dsulib.h"

char *copy_str1( str )
char *str;
{
        char *tmp;

        tmp = (char *) malloc( (unsigned) (strlen(str) + 1)
                * sizeof(char) );

        if ( tmp == NULL )
        {
          fprintf( stderr, "\nError Allocating Memory in copy_str1\n");
	  return(NULL);
  	}

        strcpy( tmp, str );

        return( tmp );
}

int InitLog(DsuTask *node, char * name)
{
	int	INFO;

	node -> fp_log   = 0;
	node -> DEBUG    = 0;
	node -> apl_name = name;
	node -> machine  = 0;
	INFO = StartLog(node);
/*
	Send back the log filename (set up in StartLog). 9/19/95
*/
	SendStr(node -> log_file, pvm_parent(), MsgFile);

	return(INFO);

} /* End of InitLog */

int CleanLog()
{
  char	command[512], *hd;
  char  *getenv();

/* Clean DSU log directory */
  if ( !(hd = (char *)getenv("DSULOG")) ) {
    hd = "/tmp"; /* Default value */
    fprintf(stderr, "Warning: Variable DSULOG has not been  set\n");
    fprintf(stderr, "Warning: Using /tmp as log directory !! \n");
  }
  sprintf(command, "rm -rf %s/DSU*\0", hd);
  system(command);

} /* end of CleanLog() */

int StartLog(DsuTask *node)
{
	char	log_file[256], myhost[256];
	char	*hd, *s1;
  	char  *getenv();

	if ((node -> NodeTid = pvm_mytid()) < 0) {
		pvm_perror("Error enrolling");
		return(-1);
	} 

/* Get host info */
	(void) gethostname(myhost, sizeof(myhost));
	node -> machine = (char *)malloc(strlen(myhost));
	strcpy(node -> machine, myhost);

/* Open the log file */
	if ( !(hd = (char *)getenv("DSULOG")) ) {
	    hd = "/tmp"; /* Default value */
            fprintf(stderr, "Warning: Variable DSULOG has not been  set\n");
      	}

	if ( (s1 = strrchr(node -> apl_name, '/') ) == NULL) 
	  s1 = node -> apl_name;
	else
	  s1 = s1++;

/*
	sprintf(log_file, "%s/DSU%s%d%s\0", hd, myhost, node -> NodeTid, s1);
*/
	sprintf(log_file, "%s/DSU%d%s\0", hd, node -> NodeTid, s1);
	node -> log_file = copy_str1(log_file);

/*
	fprintf(stderr, "Opening: %s\n", log_file); fflush(stderr);
*/
	if ( (node -> fp_log = freopen(log_file,"w", stderr)) == NULL) {
		fprintf(stderr, "Error opening the log file: %s\n", log_file);
		pvm_exit();
		return(-1);
	}

/* Redirect stderr to the above file */
	/* close(2);
	dup(node -> fd_log); */

/* Writea header in the log file */
	fprintf(stderr, "%s\n", node -> apl_name);
	fprintf(stderr, 
		"\tFile\t--> %s\n\tapl-tid\t--> %d\n\tMachine\t--> %s \n",
		log_file, node -> NodeTid, node -> machine);
	fflush(stderr);

	return(0);
} /* end of StartLog() */

void	MsgLog(DsuTask *node, char *msg)
{ 	
	fprintf(node -> fp_log, msg); 
	fflush(node -> fp_log); 
}

void 	PrintInfoLog(DsuTask *zz) { 
	fprintf(zz -> fp_log, "\nProcess -->  %s\n", zz -> apl_name);
	fprintf(zz -> fp_log, "\tMachine    %s\n", zz -> machine);
	fprintf(zz -> fp_log, "\tapl_id        <%d>\n", zz -> NodeTid);
	fprintf(zz -> fp_log, "\n");
	fflush(zz -> fp_log);
} /* End of PrintInfoLog */

/*
	PVM functions
*/

int DsuExit( DsuTask *ThisDsuTask, int stat)
{
  int tmp;
  int i;

  tmp = stat;

  /* Send End of Data message to childrem */

  for (i = 0; i < MAX_LINK; i++)
    if (ThisDsuTask -> branch_tid[i] > 0) 
      SendInt(&tmp, 1, ThisDsuTask -> branch_tid[i], MsgHdrSize);

  /* Notify completion to the GUI */

  SendInt(&tmp, 1, pvm_parent(), stat);

  fprintf(stderr, "End of application <%s>. Status (%d)\n",
		ThisDsuTask -> apl_name, stat);

  fclose (stderr);

  pvm_exit();
  return 0;
}


/*
int ParentAlive(int tid)
{
  if (pvm_pstat(tid) == PvmNoTask) return (-1);
  return 0;
}
*/

int  SendBytes(void * buff, int n,  int tid, int MsgType)
{
  int info;

  /* info = pvm_initsend(PvmDataDefault); */

  info = pvm_initsend(PvmDataInPlace);
  if ( info >= 0 ) {
    pvm_pkbyte((void *)buff, n, 1);
    pvm_send(tid, MsgType);
  }
  return(info);
}

int  SendBBytes(void * buff, int n,  int tid, int MsgType)
{
  int info;

  pvm_psend(tid, MsgType, buff, n, PVM_BYTE);
  return(info);

}
void *  RecvBytes(int tid, int MsgType)
{
  int   info, BufId, MsgLen, MsgTag, SenderTid;
  void *s1;

  BufId = pvm_recv(tid, MsgType);
  info  = pvm_bufinfo( BufId, &MsgLen, &MsgTag, &SenderTid);
  s1 = (void *)malloc(MsgLen);
  pvm_upkbyte(s1, MsgLen, 1);
  return(s1);
}

int RecvBBytes(void *buf, int n, int tid, int MsgType)
{
  int   info, BufId, MsgLen, MsgTag, SenderTid;

  pvm_precv(tid, MsgType, buf, n, PVM_BYTE, &SenderTid,
		&MsgTag, &MsgLen);

/*
  BufId = pvm_recv(tid, MsgType);
  info  = pvm_bufinfo( BufId, &NBytes, &MsgTag, &MasterTid);
  pvm_upkbyte((void *)buf, n, 1);
*/

}

int  BroadBytes(void * str, int size, int tid[], int ntid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataRaw);
  if ( info >= 0 ) {
    pvm_pkbyte((char *)str, size, 1);
    pvm_mcast(tid, ntid, MsgType);
  }
  return(info);
}

int  SendStr(char * str, int tid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  if ( info >= 0 ) {
    pvm_pkstr(str);
    pvm_send(tid, MsgType);
  }
  return(info);
}

char *  RecvStr(int tid, int MsgType)
{
  int   info, BufId, NBytes, MsgTag, MasterTid;
  char *s1, tmp_char[512];

  BufId = pvm_recv(tid, MsgType);
  /* info  = pvm_bufinfo( BufId, &NBytes, &MsgTag, &MasterTid); */
  pvm_upkstr(tmp_char);
  
  return(copy_str1(tmp_char));

/* AEM 9/19/95
  s1 = (char *)malloc(strlen(tmp_char)+1);
  strcpy(s1, tmp_char);
  return(s1);
*/
}

int  BroadInt(int IntBuf[], int n, int tid[], int ntid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  if ( info >= 0 ) {
    pvm_pkint(IntBuf, n, 1);
    pvm_mcast(tid, ntid, MsgType);
  }
  return(info);
}

int  SendInt(int *IntBuf, int n, int tid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  if ( info >= 0 ) {
    pvm_pkint(IntBuf, n, 1);
    pvm_send(tid, MsgType);
  }
  return(info);
}

int  RecvInt(int *IntBuf, int n, int tid, int MsgType)
{
  int   info, BufId, NBytes, MsgTag, SenderTid;

  BufId = pvm_recv(-1, MsgType);
  info  = pvm_bufinfo( BufId, &NBytes, &MsgTag, &SenderTid);
  pvm_upkint(IntBuf, n, 1);
  return(SenderTid);
}

/*
	Names switched to check if parent is alive (03/18/95)
*/

int  RecvOneInt(int tid, int MsgType)
{
  int   info, BufId, NBytes, MsgTag, SenderTid;
  int   IntBuf;

/*
  while ( 1) {
    if ( (BufId = pvm_nrecv(tid, MsgType)) > 0 ) break;
    if (pvm_pstat(tid) == PvmNoTask)
	if ( (BufId = pvm_nrecv(tid, MsgType)) > 0 ) 
	  break;
	else 
	  return(0);
  }
*/
    
  BufId = pvm_recv(tid, MsgType);
/*
  info  = pvm_bufinfo( BufId, &NBytes, &MsgTag, &SenderTid);
*/
  pvm_upkint(&IntBuf, 1, 1);

  return(IntBuf);
}
int  RecvOneIntNB(int tid, int MsgType)
{
  int   info, BufId, NBytes, MsgTag, SenderTid;
  int   IntBuf;

  BufId = pvm_recv(-1, MsgType);
  info  = pvm_bufinfo( BufId, &NBytes, &MsgTag, &SenderTid);
  pvm_upkint(&IntBuf, 1, 1);
  return(IntBuf);
}


int  BroadFI(float *FloatBuf, int nf, int *IntBuf, int ni, 
		int tid[], int ntid, int MsgType)
{
  int i, info;

  info = pvm_initsend(PvmDataDefault);
  pvm_pkint(IntBuf, ni, 1);
  pvm_pkbyte((char *)FloatBuf, nf*sizeof(float), 1);
  pvm_mcast(tid, ntid, MsgType);
  return(info);
}

int  BroadFloat(float *FloatBuf, int n, int tid[], int ntid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  pvm_pkbyte((char *)FloatBuf, n*sizeof(float), 1);
  pvm_mcast(tid, ntid, MsgType);
  return(info);
}

int  SendFloat(float *FloatBuf, int n, int tid, int MsgType)
{
  int info;

  pvm_psend(tid, MsgType, FloatBuf, n, PVM_FLOAT);

/* 3/15/96
  info = pvm_initsend(PvmDataDefault);
  pvm_pkbyte((char *)FloatBuf, n*sizeof(float), 1);
  pvm_send(tid, MsgType);
  return(info);
*/
  return(0);
}

int  RecvFloat(float *FloatBuf, int n, int tid, int MsgType)
{
  int   info, BufId, MsgLen, MsgTag, SenderTid;

  pvm_precv(tid, MsgType, FloatBuf, n, PVM_FLOAT, &SenderTid,
		&MsgTag, &MsgLen);

/* 3/15/96
  BufId = pvm_recv(tid, MsgType);
  info  = pvm_bufinfo( BufId, &MsgLen, &MsgTag, &SenderTid);
  pvm_upkbyte((char *)FloatBuf, n*sizeof(float), 1);
*/
  return(SenderTid);
}

int  SendCplx(complex *CplxBuf, int n, int tid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  pvm_pkbyte((char *)CplxBuf, n*sizeof(complex), 1);
  pvm_send(tid, MsgType);
  return(info);
}

int  RecvCplx(complex *CplxBuf, int n, int tid, int MsgType)
{
  int   info, BufId, NBytes, MsgTag, SenderTid;

  BufId = pvm_recv(-1, MsgType);
  info  = pvm_bufinfo( BufId, &NBytes, &MsgTag, &SenderTid);
  pvm_upkbyte((char *)CplxBuf, n*sizeof(complex), 1);
  return(SenderTid);
  /* return(info); ADDED FOR WENECES CODE 4/21/96 */
}

int  SendFI(float *FloatBuf, int nf, int *IntBuf, int ni, int tid, int MsgType)
{
  int info;

  info = pvm_initsend(PvmDataDefault);
  pvm_pkint(IntBuf, ni, 1);
  pvm_pkbyte((char *)FloatBuf, nf*sizeof(float), 1);
  pvm_send(tid, MsgType);
  return(info);
}

int  RecvFI(float *FloatBuf, int nf, int *IntBuf, int ni, int tid, int MsgType)
{
  int   info, BufId, NBytes, MsgTag, SenderTid;

  BufId = pvm_recv(-1, MsgType);
  info  = pvm_bufinfo( BufId, &NBytes, &MsgTag, &SenderTid);
  pvm_upkint(IntBuf, ni, 1);
  pvm_upkbyte((char *)FloatBuf, nf*sizeof(float), 1);
  return(SenderTid);
}

int CreateSlaves1(int **SeisSlaves, char *SlaveName, int px, int py)
{
  int info, i, j;

/*
  int **SeisSlaves;

  if ( (SeisSlaves = alloc2int(py,px) ) == NULL) {
    fprintf(stderr, "Out of memory (creating processes)\n");
    return(NULL);
  }

*/
/*
  for (i = 0; i < px; i++)
    for (j = 0; j < py; j++) {
*/
  info = pvm_spawn(SlaveName, (char **)0, 0,
                   (char *)0, px*py, SeisSlaves[0]);
  if (info != px*py) {
        fprintf(stderr, "Error starting pvm applications %s \n", SlaveName);
        return(-1);
  }

  return(0);

} /* End of CreateSlaves */

int CreatePipe(int *SeisSlaves, char *SlaveName, int n)
{
  int info, i;
  int	CntlInfo[2];

  info = pvm_spawn(SlaveName, (char **)0, 0,
                   (char *)0, n, SeisSlaves);

  if (info != n) {
        fprintf(stderr,"\n\tError starting pvm applications: %s \n", SlaveName);
        return(-1);
  }

  /* Setup the pipeline */

  for (i = 0; i < n; ++i) {
	CntlInfo[0] = i;
	if (i != (n - 1)) CntlInfo[1] = SeisSlaves[i+1];
        SendInt(CntlInfo, 2, SeisSlaves[i], MsgCntl);
  }

  return(0);
}

int CreateBin(int *SeisSlaves, char *SlaveName, int n)
{
  int info, i;
  int	CntlInfo[2];

  info = pvm_spawn(SlaveName, (char **)0, 0,
                   (char *)0, n, SeisSlaves);

  if (info != n) {
        fprintf(stderr,"\n\tError starting pvm applications: %s \n", SlaveName);
        return(-1);
  }

  /* Send virtual ID to all of them */

  for (i = 0; i < n; ++i) {

	CntlInfo[0] = i;
	CntlInfo[1] = n; /* Can be outside */
        SendInt(CntlInfo, 2, SeisSlaves[i], MsgCntl);
        SendInt(SeisSlaves, n, SeisSlaves[i], MsgCntl);

  }

  return(0);
}

int DsuSpawn(int *SeisSlaves, char *SlaveName, int pn)
{
  int info;

  info = pvm_spawn(SlaveName, (char **)0, 0,
                   (char *)0, pn, SeisSlaves);
  if (info != pn) {
        fprintf(stderr, "Error starting pvm applications %s \n", SlaveName);
        return(-1);
  }
  return(pn);
} /* End of CreateSlaves */

