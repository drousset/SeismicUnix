/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*----------------------------------------------------------------------
 * Copyright (c) Colorado School of Mines, 1989.
 * All rights reserved.
 *
 * This code is part of SU.  SU stands for Seismic Unix, a processing line
 * developed at the Colorado School of Mines, partially based on Stanford
 * Exploration Project (SEP) software.  Inquiries should be addressed to:
 *
 *  Jack K. Cohen, Center for Wave Phenomena, Colorado School of Mines,
 *  Golden, CO 80401  (dix@mines.colorado.edu)
 *----------------------------------------------------------------------
 */

#include "par.h"
#define ERROR	-1

/* syscalls - routines for system calls with error checking
 *
 * ecreat  - creat with error check
 * efork   - fork with error check
 * eopen   - open with error check
 * eclose  - close with error check
 * eunlink - unlink with error check
 * elseek  - lseek with error check
 * epipe   - pipe with error check
 * eread   - read with error check
 * ewrite  - write with error check
 * pread   - read on pipe with error check
 *
 * Returns:
 *	ecreat returns a file descriptor
 *	efork returns child pid to parent, 0 to child or -1 on error
 *	eopen returns a file descriptor
 *	eclose returns 0 on success or -1 on error
 *	eunlink returns 0 on success or -1 on error
 *	elseek returns number of bytes actually read
 *	epipe returns 0 on success or -1 on error
 *	eread returns number of bytes actually read
 *	ewrite returns number of bytes actually written
 *	pread returns number of bytes actually read from pipe
 *
 * Synopsis:
 *	int ecreat(path, perms)
 *	char *path  - filename
 *	int perms   - mode
 *
 *	int efork()
 *
 *	int eclose(fd)
 *	int fd      - file descriptor of file to be read
 *
 *	int eunlink(path)
 *	char *path  - filename
 *
 *	int elseek(fd, offset, origin)
 *	int fd      - file descriptor of file to be read
 *	long offset - shift applied to "origin"
 *	int origin  - SEEK_SET, SEEK_CUR or SEEK_END
 *
 *	int epipe(fds)
 *	int fds[2] - fds[0] reading, fds[1] writing
 *
 *	int eread(fd, buf, nbytes)
 *	int fd      - file descriptor of file to be read
 *	char *buf   - buffer to store characters read
 *	int nbytes  - number of bytes to be read
 *
 *	int ewrite(fd, buf, nbytes)
 *	int fd      - file descriptor of file to be written
 *	char *buf   - buffer to store characters written
 *	int nbytes  - number of bytes to be written
 *
 *	int pread(fd, buf, nbytes)
 *	int fd;     - file descriptor of file to be read
 *	char *buf;  - pointer to buffer
 *	int nbytes; - number of bytes to be read
 *
 * Credits: 
 *	Rochkind, "Advanced UNIX Programming"
 *	Kernighan and Pike, "The UNIX Programming Environment"
 *	Kernighan and Ritchie, "The C Programming Language"
 *	Mark Williams Company, "ANSI C--A Lexical Guide"
 *	SEP: Rick, Ron, Jon, Stew
 *	CWP: Shuki, Jack
 *
 * Notes:
 *	Last arg to read/write is unsigned int on some ANSI systems,
 *	here we follow K&R, page 170.
 *
 *	Rochkind says creat is superfluous, K&R say its mandatory.
 *	I think Rochkind is right--see TEST program below.
 *
 *	Getting less than the number of bytes asked for on a read
 *	is *not* a system error--usually it just means end of file
 *	has been reached.  However, it *might* be an error in some
 *	application.  Similarly coming up empty is not a system error,
 *	but might be an application error.  It is left to the user to
 *	trap these instances.  Here is an example of the first situation
 *	after Rochkind, page 43:
 *
 *	#define SIZE sizeof(struct direct)
 *	...
 *		while ((nread=eread(fd, &dlink, SIZE)) == SIZE) {
 *			... (process dlink)
 *		}
 *		switch(nread) {
 *		case 0:
 *			return;
 *		default:
 *			err("partial read");
 *		}
 *
 *	In an application where end of file was an error, we could replace
 *	case 0 by :
 *		case 0:
 *			err("EOF");
 */

int ecreat(char *path, int perms)
{
	int fd;

	if (ERROR == (fd = creat(path, perms)))
		syserr("%s: ecreat: creat failed", __FILE__);
	
	return fd;
}

int efork()
{
	int fd;

	if (ERROR == (fd = fork()))
		syserr("%s: efork: fork failed", __FILE__);
	
	return fd;
}

int eopen(char *path, int flags, int perms)
{
	int fd;

	if (ERROR == (fd = open(path, flags, perms)))
		syserr("%s: eopen: open failed", __FILE__);
	
	return fd;
}

int eclose(int fd)
{
	int returnmode;

	if (ERROR == (returnmode = close(fd)))
		syserr("%s: eclose: close failed", __FILE__);
	
	return returnmode;
}

int eunlink(char *path)
{
	int returnmode;

	if (ERROR == (returnmode = unlink(path)))
		syserr("%s: eunlink: unlink failed", __FILE__);
	
	return returnmode;
}

long elseek(int fd, long offset, int origin)
{
	long position;

	if (ERROR == (position = lseek(fd, offset, origin)))
		syserr("%s: elseek: lseek failed", __FILE__);
	
	return position;
}


int epipe(int fds[2])
{
	int returnmode;

	if (ERROR == (returnmode = pipe(fds)))
		syserr("%s: epipe: pipe failed", __FILE__);
	
	return returnmode;
}


int eread(int fd, char *buf, int nbytes)
{
	int nread;	/* items actually read	*/

	if (ERROR == (nread = read(fd, buf, nbytes)))
		syserr("%s: eread: read failed", __FILE__);

	return nread;
}


int ewrite(int fd, char *buf, int nbytes)
{
	int nwrite;	/* bytes actually written	*/

	if (nbytes != (nwrite = write(fd, buf, nbytes))) {
		if (nbytes != -1) errno = EFBIG;
		syserr("%s: ewrite: write failed", __FILE__);
	}

	return nwrite;
}


int pread(int fd, char *buf, int nbytes)
{
	int nread, ntotal = 0;

	while (nbytes) {
		switch (nread = read(fd, buf, nbytes)) {
		case ERROR:
			syserr("%s: pread: read failed", __FILE__);
		case 0:	/* finished */
			return 0; /* must not default so return or break */
		default:
			ntotal += nread;
			nbytes -= nread;
			buf += nread;
		break;
		}
	}

	return ntotal;
}



#ifdef TEST
main(int argc, char **argv)
{
	int fdr, fdw;
	char msg[BUFSIZ];
	char erbuf[BUFSIZ], ewbuf[BUFSIZ], rbuf[BUFSIZ], wbuf[BUFSIZ];
	char pbuf[1];
	int mbytes, rbytes, wbytes;
	int ritems, witems;
	int (*readptr) ();	/* pointer to eread() or pread()	*/
	int eread();		/* must be declared to use ptr		*/
	int pread();		/* must be declared to use ptr		*/

	initargs(argc, argv);

	/* Exercise eread and ewrite using other functions in the package */
  	fdw = eopen("junk.wr", O_RDWR | O_CREAT | O_TRUNC, 0666);
 	strcpy(ewbuf, "   Writing with ewrite\n");
	wbytes = strlen(ewbuf);
	ewrite(fdw, ewbuf, wbytes);

	elseek(fdw, 0, SEEK_SET);
	read(fdw, rbuf, wbytes);
	strcpy(msg, "***ewrite from file to buffer ...");
	mbytes = strlen(msg);
	write(STDOUT, msg, mbytes);
	write(STDOUT, rbuf, wbytes);

  	fdr = eopen("junk.rd", O_RDWR | O_CREAT | O_TRUNC, 0666);
 	strcpy(wbuf, "   Reading with eread\n");
	rbytes = strlen(wbuf);
	write(fdr, wbuf, rbytes);
	strcpy(wbuf, "eread saw zero bytes\n");
	wbytes = strlen(wbuf);

	strcpy(msg, "***eread from file to buffer ...");
	mbytes = strlen(msg);
	write(STDOUT, msg, mbytes);
	elseek(fdr, 0, SEEK_SET);
	if (!eread(fdr, erbuf, rbytes)) {
		write(STDOUT, wbuf, wbytes);
	} else {
		write(STDOUT, erbuf, rbytes);
	}

 	strcpy(wbuf, "");
	rbytes = strlen(wbuf);
	write(fdr, wbuf, rbytes);
	strcpy(wbuf, "   eread saw zero bytes\n");
	wbytes = strlen(wbuf);

	strcpy(msg, "***eread from EMPTY file to buffer ...");
	mbytes = strlen(msg);
	write(STDOUT, msg, mbytes);
	elseek(fdr, 0, SEEK_SET);
	if (!eread(fdr, erbuf, rbytes)) {
		write(STDOUT, wbuf, wbytes);
	} else {
		write(STDOUT, erbuf, rbytes);
	}

	eclose(fdw);
	eclose(fdr);
	eunlink("junk.rd");
	eunlink("junk.wr");


	/* Exercise pread and eread--use < and | for input */
	/* Set appropriate read function for input filetype */
	switch(filestat(STDIN)) {
	case TTY:
		err("input can't be tty");
	break;
	case DISK:
	case TAPE:
		readptr = eread;
		strcpy(ewbuf, "***Disk stdin: use eread ...   ");
		wbytes = strlen(ewbuf);
		ewrite(STDOUT, ewbuf, wbytes);
	break;
	case PIPE:
		readptr = pread;
		strcpy(ewbuf, "***Pipe stdin: use pread ...   ");
		wbytes = strlen(ewbuf);
		ewrite(STDOUT, ewbuf, wbytes);
	break;
	default:
		err("undefined input filetype %s", printstat(STDIN));
	break;
	}

	while ((*readptr)(STDIN, pbuf, 1)) {
		ewrite(STDOUT, pbuf, 1);
	}

	return EXIT_SUCCESS;
}
#endif
