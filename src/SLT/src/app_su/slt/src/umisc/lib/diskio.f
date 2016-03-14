c       parameter(n=10,iunit=20,irecl=40,irec=1)
c       real f(n)
c       do 10 i=1,n
c         f(i) = i
c 10     continue 
c       call opdisk(iunit,irecl,ios,'junk.dat','keep','unknown')
c       call ridisk(iunit,f,irec,irecl,ios)
c       do 20 i=1,n
c 20        f(i) = 0.
c       call redisk(iunit,f,irec,irecl,ios)
c       write(*,*) (f(i),i=1,n)
c       close(unit=iunit,status='delete')
c       stop
c       end


       subroutine opdisk(iunit,lbyte,ios,fname,disps,stats)
       integer lbyte,iunit,ios
       character*(*) fname,disps,stats
       ld = lenstr(disps)
       lf = lenstr(fname)
       ls = lenstr(stats)
c       fff = ld
c       call msgsc("enter opdisk ldisps\0",fff)   
c       fff = lf
c       call msgsc("enter opdisk lfname\0",fff)   
c       fff = ls
c       call msgsc("enter opdisk lstats\0",fff)   
       if ( disps(1:1) .ne. 'd' .and. disps(1:1) .ne. 'd' ) then
c	  fff = iunit
c	  call msgsc("at opdisk disps is not d\0",fff)   
          open(unit=iunit,file=fname(1:lf),form='unformatted',
     &         status=stats(1:ls),access='direct',recl=lbyte,
     &         iostat=ios)
       else
c	  fff = iunit
c	  call msgsc("at opdisk disps is d\0",fff)   
          open(unit=iunit,form='unformatted',
     &         status='scratch',access='direct',recl=lbyte,
     &         iostat=ios)
       end if
       return
       end
       subroutine ridisk(iunit,f,irec,lbyte,ios)
       integer lbyte,iunit,irec,ios
       character f(lbyte)
       write(iunit,rec=irec,iostat=ios) f
       return
       end
       subroutine redisk(iunit,f,irec,lbyte,ios)
       integer lbyte,iunit,irec,ios
       character f(lbyte)
       read(iunit,rec=irec,iostat=ios) f
       return
       end
