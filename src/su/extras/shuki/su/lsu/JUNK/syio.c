/*
main()
{	char buff[200] ;
	printf("file type = %d \n",statfil(0) ) ;
	gname(0,buff);
	printf("name = %s\n",buff);
}
*/
/* a library of utilities for processing of segy data */
int xargc ;
char **xargv ;
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <segy.h>
#include <sys/dir.h>
#include <fcntl.h>
gname(fdes,buf)
int fdes ;
char *buf ;
{	struct stat sb, sc ;
	extern char *strcpy(), *strcat();
	char dev[255];
	DIR *opendir(), *dirp ;
	struct direct *readdir(),*dep ;
	int i,ino ;
	i = statfil(fdes) ;
	if( i < 0 ) {
/* 		fprintf(stderr, */
/* 		"this should not happen in gname: fdes=%d i=%d\n",fdes,i); */
/* 		err("this should not happen in gname"); */
		(void) strcpy(buf,"NEG_STATFIL");
	}
	else if ( i == 0 ) (void) strcpy(buf,"TTY");
	else if ( i == 2 ) (void) strcpy(buf,"PIPE");
	else if ( i == 1 )	/* TAPE */
	{
		(void) fstat(fdes,&sb);
		ino = (int)sb.st_ino ;
		(void) strcpy(buf,"TAPE");
		if(((DIR *) NULL) == (dirp = opendir("/dev"))) return;
		while( dep = readdir(dirp) )
		 if( dep->d_ino == ino ) 
		   {
			(void) strcpy(dev,"/dev/");
			(void) strcat(dev,dep->d_name);
			if( 0 == stat(dev,&sc))
			    if( sb.st_dev == sc.st_dev &&
			        sb.st_ino == sc.st_ino &&
			        sb.st_mode == sc.st_mode &&
			        sb.st_nlink == sc.st_nlink &&
				sb.st_rdev == sc.st_rdev) {
				(void) strcpy(buf,dev) ;
				}
		   }
		closedir(dirp);
	}
	else	/* DISK (?) */
	{
		(void)fstat(fdes,&sb) ;
		ino = (int)sb.st_ino ;
		(void)strcpy(buf,"DISK");
		if(((DIR *) NULL) == (dirp = opendir("."))) return;
		while( dep = readdir(dirp) )
		 if( dep->d_ino == ino ) 
		   {
			(void) strcpy(buf,dep->d_name) ;
			break ;
		   }
		closedir(dirp);
	}
}
statfil(fdes)
int fdes ;
/*	return value
	-1	error
	0	terminal (or none of the others)
	1	raw magtape
	2 	pipe
	3	disk
 */
{
	struct stat sb, sc ;
	if( 0 > fstat(fdes,&sb) ) return(-1);
	if( 0 == stat("/dev/null",&sc))
		if(sb.st_mode == sc.st_mode &&
		   sb.st_dev == sc.st_dev   &&
		   sb.st_rdev == sc.st_rdev  ) return(3);
	if(isatty(fdes)) return(0);
	if(isatape(fdes)) return(1);
	if(isapipe(fdes)) return(2);
	if((sb.st_mode&S_IFMT) == S_IFREG)
		if(0 <= lseek(fdes,0L,1)) return(3);
	return(0);
}
/*
 * 0 for synchronous tape I/O
 * 1 for asynchronous tape I/O
 */
static int inasync = -1;
setasync_(val)
int *val;
{ inasync = !(!(*val)) ; }
gettr_(tp)
struct segy *tp ;
{
	static nb = 0 , ftype = -2 ;
	static nss,itr = 0 ;
	int j ,nleft;
	char *bp ;
	static struct segy tape_buffer;
	static int inasync=0, nbnext;
	extern int asiostat(), fcntl();

	if(nb == 0)
	{
		ftype = statfil(0) ;
		if(ftype < 1 ) selfdoc() ;
		if(ftype == 1) /* tape: set up asynchronous I/O */
		{
			if(inasync == -1) {
			    inasync=0;
			    if(-1 == fcntl(0,F_SETFL,FASIO)) perror("syio async");
			    else inasync=1;
			    }
			if(inasync) nb=asiostat(0);
			nb = read(0,(char *)tp,SY_NDAT*4+240);
			if(inasync) nb=asiostat(0);
			nss = tp->ns ;
			if(nb != 240+nss*4 )
			err("gettr_: first tape record not segy trace");
			nbnext = read(0,(char *) &tape_buffer, nb);
		}
		else
		{
			j = read(0,(char*)tp,240);
			nss = tp->ns ;
			if(nss > SY_NDAT) 
err("gettr_: unable to handle %d > %d samples per trace\n",nss,SY_NDAT);
			if(j != 240)
			{  if(ftype ==2 )
err("gettr_: cannot read first trace header from pipe\n");
err("gettr_: cannot read first trace header from disk\n");
			}

			/* if( tp->cdp > 100000 || tp->cdp < -3 ||
			  tp->dt < 100 )
err("gettr_: dt=%d, input probably not segy",tp->dt); */

			if( ftype == 3)   /* this is for disk */
			{
				nb = nss*4 ;
				j = read(0,(char*)tp->data,nb);
				if(j != nb) 
err("gettr_: error reading first trace from disk");
				nb += 240 ;
			}
			else			/* this is just for pipes */
			{
				nleft = nss*4 ;
				bp = ( char * ) tp->data ;
				while(nleft)
				{	j = read(0,bp,nleft) ;
					if(j==0) err("out of data on pipe");
					bp += j ;
					nleft -= j ;
				}
				nb = nss*4+240 ;
			}
		}
	}
	else
	{  
	   bp = (char *)tp ;
	   nleft = nb ;
	   if( ftype == 2 )
	   {
		while(nleft)
		 {
			j = read(0,bp,nleft);
			nleft -= j ;
			if( nleft == nb ) return(0);
			if( j == 0 ) err("gettr_: out of data reading pipe");
			bp += j ;
		}
	    }
	    else if(ftype == 1) /* tape */
	    {
		if(inasync) j = asiostat(0); /* byte count of asynch read */
		else j = nbnext; /* otherwise byte count of stored read */
		bcopy((char *) &tape_buffer,bp,j);
		if( j == 0 ) { nb = 0 /* restart for next file or rewind */ ; return(0) ; }
		if( j != nleft ) err(" error reading tape ");
		nbnext = read(0,(char *) &tape_buffer, nb);
	    }
	    else
	    {
		j = read(0,bp,nleft);
		if( j == 0 ) return(0) ;
		if( j != nleft ) err("error reading data from disk ");
	    }
	}
 	if( tp->ns != nss) err("segy header error on input");
	itr++ ;
	return(nb);
}
/*
er(str)
char *str ;
{
	elog(str);
	exit(-1);
}
*/
selfdoc()
{
	extern char *sdoc ;
	fprintf(stderr,"%s\n",sdoc);
	exit(-1);
}
elog(str)
char *str ;
{	char *ctime();
	int tim ;
	FILE *logp,*fopen() ;
	logp = fopen("Errlog","a");
	tim = time(0);
	fprintf(stderr,"Error in %s         %s %s\n",*xargv,ctime(&tim),str);
	fprintf(logp,"Error in %s         %s %s\n",*xargv,ctime(&tim),str);
	fclose(logp);
}
hlog(str)
char *str ;
{
	FILE *hf , *fopen();
	int i ,tim;
	char *ctime(), st1[100],st2[100] ;
	hf = fopen("History","a");
	for( i = 0 ; i < xargc ; i++ ) fprintf(hf,"%s ",xargv[i]);
	gname(0,st1) ; gname(1,st2);
	fprintf(hf,"< %s > %s \n",st1,st2);
	tim = time(0);
	fprintf(hf,"# %s",ctime(&tim));
	if( *str ) fprintf(hf,"#%s\n",str);
	exit(0);
}
#include <sys/ioctl.h>
#include <sys/mtio.h>
syrew(fdes)
int fdes;
{
    int itype;
    static struct mtop rew = { MTREW, 1 };

    itype = statfil(fdes);
    switch(itype) {
    case 1:
	ioctl(fdes,MTIOCTOP,(char *) &rew);
	break;
    case 3:
        lseek(fdes,0L,0);
	break;
    default:
	err("syrew only allowed for tape or disk\n");
    }
}
