c a dummy fortran open subroutine used before closing standard i/o in C
c
c  Note:
c    At SUN workstation, you get "segmentation fault" if trying to open
c    a file within a fortran subroutine, after closing the standard input.
c    A solution is to issue a dummy fortran open before closing the
c    standard input.
c
c    calling example from c:
c	dummypopen_(&iunit);
c
c    input:
c	iunit 	int*4	unit number to use
ccc
c Author: Zhiming Li	      		7-2-92
ccc
c
	subroutine dummyopen(iunit)
     		open(unit=iunit,status="scratch")
		close(iunit)
	return
	end

