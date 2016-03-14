/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/* filestat - determine type of file from file descriptor
 * printstat - print the filetype as a string
 *
 * Returns: (see below for typedefs "filetype", "string")
 *	filestat:   filetype
 *	printstat:  string
 *
 * Synopsis:
 *	filetype filestat(fd)
 *	int fd;	  file descriptor
 *
 *	string printstat(fd)
 *	int fd;	  file descriptor
 *
 * Credits:
 *	SEP: Einar, Stew
 *	CWP: Jack
 *
 * Example:
 *	filetype ftype;
 *	...
 *	ftype = filestat(STDOUT)
 *	if (ftype = TTY) {
 *		...
 *
 * Notes:
 *
 * 	BADFILETYPE is the error return and it is up to the calling
 *	program to check for it.
 *
 *	filetype is defined in cwp.h as:
 *	typedef enum {BADFILETYPE = -1,
 *	   TTY, DISK, DIRECTORY, TAPE, PIPE, FIFO, SOCKET, SYMLINK} filetype;
 *
 *	string is defined in cwp.h as:
 *	typedef char *string;
 *
 *	Rochkind's Advanced Unix Programming assures us that the link count
 *	for a pipe is 0.  But it's an old book.  It seems that PIPES are
 *	sometimes implemented as FIFOs.  In most cases, the number of links for
 *	a pipe is 0, even so.  But on NeXT (at least) the link count is 1.
 *	In the code, I test for PIPE first and FIFO second, so for a PIPE you'll
 *	actually get FIFO on machines that do it NeXT's way.
 *
 *	Portability: the code assumes that /dev/rmt0 and /dev/mt0
 *	are tape devices on your system.  If not, make the obvious
 *      changes.
 *
 *	/dev/null is handled as a special case and classified
 *	as a disk file.
 *
 *	The check on tapes allows either the raw or buffered version.
 *	This is moot and easily changed.
 *
 *	If new types are added, the typedef "filetype" in cwp.h must
 *	 be extended.
 *
 */

#include "cwp.h"
#include <sys/stat.h>

#ifndef major	/* major() is a macro for getting the major device number */
#include <sys/sysmacros.h>
#endif

/* determine type of file (DISK, PIPE, ...) */
filetype filestat(int fd)
{
	struct stat sfd;	/* for passed fd	*/
	struct stat sdn;	/* for /dev/null	*/
	struct stat smt;	/* for tape devices	*/


	if (-1 == fstat(fd, &sfd)) return BADFILETYPE;

	/* UNIX subroutine */
	if (isatty(fd))  return TTY;

	/* Standard stat test for regular file */
	if ((sfd.st_mode & S_IFMT) == S_IFREG) return DISK;

	/* Standard stat test for directory */
	if ((sfd.st_mode & S_IFMT) == S_IFDIR) return DIRECTORY;

	/* Only pipes have 0 links (might be FIFO too, so this test first) */
	if (!sfd.st_nlink) return PIPE;

	/* Standard stat test for FIFO */
	if ((sfd.st_mode & S_IFMT) == S_IFIFO) return FIFO;

	/* Standard stat test for socket */
	if ((sfd.st_mode & S_IFMT) == S_IFSOCK) return SOCKET;

	/* Standard stat test for symbolic link */
	if ((sfd.st_mode & S_IFMT) == S_IFLNK) return SYMLINK;

	/* Detect tape by comparing its major device number to	*/
	/* /dev/rmt0 (this is not quite portable).  		*/
	if (0 == stat("/dev/rmt0", &smt) &&
	     major(sfd.st_rdev) == major(smt.st_rdev)) return TAPE;
	if (0 == stat("/dev/mt0", &smt) &&
	     major(sfd.st_rdev) == major(smt.st_rdev)) return TAPE;

	/* Detect file as /dev/null by its device number and	*/
	/* classify it as a disk file.				*/
	if (0 == stat("/dev/null", &sdn) &&
		sfd.st_rdev == sdn.st_rdev) return DISK;

	/* error return */
	return BADFILETYPE;
}


/* Supply ascii string describing type of file */
string printstat(int fd)
{
	switch (filestat(fd)) {
		case TTY:	return "TTY";
		case DISK:	return "DISK";
		case DIRECTORY:	return "DIRECTORY";
		case TAPE:	return "TAPE";
		case PIPE:	return "PIPE";
		case FIFO:	return "FIFO";
		case SOCKET:	return "SOCKET";
		case SYMLINK:	return "SYMLINK";
		default:	return "BADFILETYPE";
	}
}


#ifdef TEST

/* Test driver for function filestat
 *
 * Here are some tests using filestat to analyse STDIN, STDOUT and
 * the first command line argument along with the expected results:
 *	filestat filestat.c
 *		expect: TTY, TTY, DISK
 *	filestat <filestat.c /usr/local | cat
 *		expect: DISK, PIPE, DIRECTORY
 *	cat filestat.c | filestat filestat.c >/usr/tmp/junkxxx
 *		expect: PIPE, DISK, DISK
 *	filestat /dev/null
 *		expect: TTY, TTY, DISK
 *	filestat
 *		expect: TTY, TTY, "no filename given"
 */

main(int argc, char **argv)
{
	static filetype ftype;
	int fd;


	fprintf(stderr, "Checking filestat ...\n");
	/* Get filetype of stdin	*/
	switch (ftype = filestat(STDIN)) {
	case TTY:
		fprintf(stderr, "filetype(STDIN) = TTY\n");
	break;
	case DISK:
		fprintf(stderr, "filetype(STDIN) = DISK\n");
	break;
	case DIRECTORY:
		fprintf(stderr, "filetype(STDIN) = DIRECTORY\n");
	break;
	case TAPE:
		fprintf(stderr, "filetype(STDIN) = TAPE\n");
	break;
	case PIPE:
		fprintf(stderr, "filetype(STDIN) = PIPE\n");
	break;
	case FIFO:
		fprintf(stderr, "filetype(STDIN) = FIFO\n");
	break;
	case SOCKET:
		fprintf(stderr, "filetype(STDIN) = SOCKET\n");
	break;
	case SYMLINK:
		fprintf(stderr, "filetype(STDIN) = SYMLINK\n");
	break;
	case BADFILETYPE:
		fprintf(stderr, "filetype(STDIN) = BADFILETYPE\n");
	break;
	default:
	    fprintf(stderr, "filestat(stdin) failed: ftype = %d\n", ftype);
	}


	/* Get filetype of stdout	*/
	switch (ftype = filestat(STDOUT)) {
	case TTY:
		fprintf(stderr, "filetype(STDOUT) = TTY\n");
	break;
	case DISK:
		fprintf(stderr, "filetype(STDOUT) = DISK\n");
	break;
	case DIRECTORY:
		fprintf(stderr, "filetype(STDOUT) = DIRECTORY\n");
	break;
	case TAPE:
		fprintf(stderr, "filetype(STDOUT) = TAPE\n");
	break;
	case PIPE:
		fprintf(stderr, "filetype(STDOUT) = PIPE\n");
	break;
	case FIFO:
		fprintf(stderr, "filetype(STDIN) = FIFO\n");
	break;
	case SOCKET:
		fprintf(stderr, "filetype(STDIN) = SOCKET\n");
	break;
	case BADFILETYPE:
		fprintf(stderr, "filetype(STDOUT) = BADFILETYPE\n");
	break;
	default:
	    fprintf(stderr, "filestat(stdout) failed: ftype = %d\n", ftype);
	}

	/* Get filetype of argv[1]	*/
	if (argc == 1) {
		fprintf(stderr, "no filename given\n");
		exit(1);
	}
	if (-1 == (fd = open(argv[1], O_RDONLY))) {
		fprintf(stderr, "can't open %s", argv[1]);
		exit(2);
	}

	switch (ftype = filestat(fd)) {
	case TTY:
		fprintf(stderr, "filetype(fd) = TTY\n");
	break;
	case DISK:
		fprintf(stderr, "filetype(fd) = DISK\n");
	break;
	case DIRECTORY:
		fprintf(stderr, "filetype(fd) = DIRECTORY\n");
	break;
	case TAPE:
		fprintf(stderr, "filetype(fd) = TAPE\n");
	break;
	case PIPE:
		fprintf(stderr, "filetype(fd) = PIPE\n");
	break;
	case FIFO:
		fprintf(stderr, "filetype(STDIN) = FIFO\n");
	break;
	case SOCKET:
		fprintf(stderr, "filetype(STDIN) = SOCKET\n");
	break;
	case BADFILETYPE:
		fprintf(stderr, "filetype(argv[1]) = BADFILETYPE\n");
	break;
	default:
	    fprintf(stderr, "filestat(argv[1]) failed: ftype = %d\n", ftype);
	}


	fprintf(stderr, "Checking printstat ...\n");
	/* Print filetype of stdin	*/
	fprintf(stderr, "filetype(STDIN) = %s\n", printstat(STDIN));


	/* Print filetype of stdout	*/
	fprintf(stderr, "filetype(STDOUT) = %s\n", printstat(STDOUT));


	/* Print filetype of argv[1]	*/
	if (argc == 1) {
		fprintf(stderr, "no filename given\n");
		exit(1);
	}
	if (-1 == (fd = open(argv[1], O_RDONLY))) {
		fprintf(stderr, "can't open %s", argv[1]);
		exit(2);
	}

	fprintf(stderr, "filetype(fd) = %s\n", printstat(fd));

	exit(0);
}
#endif
