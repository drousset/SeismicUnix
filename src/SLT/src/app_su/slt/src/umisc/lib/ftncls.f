	subroutine ftncls(iunit,stats)
	integer iunit
	character*(*) stats
	if ( stats(1:1) .eq. 'd' .or. stats(1:1) .eq. 'd') then
	   close(unit=iunit,status='delete')
c	   fff = iunit
c	   call msgsc("disp is d at ftncls \0",fff)
	else
c	   fff = iunit
c	   call msgsc("disp is not d at ftncls \0",fff)
	   close(unit=iunit,status='keep')
	end if
	return
	end
