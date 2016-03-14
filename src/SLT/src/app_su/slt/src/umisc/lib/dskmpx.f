c        parameter(n1=5,n2=4,lwork=10,idskun=10,ibypsm=8)
c        complex a(n1,n2), b(n2,n1), work(lwork)  
c 	 do 100 j=1,n2
c        do 100 i=1,n1
c100	a(i,j) = cmplx(i,j)
c        write(*,*) 'input a'
c        do 150 j=1,n2
c150     write(*,*) (a(i,j),i=1,n1)
c        call dskms(n1,n2,lwork,idskun,
c     &             'transp.dat','keep',ios,ibypsm)
c       if ( ios .ne. 0 ) call cancel(ios,'error dskms')
c       do 200 j=1,n2
c       call dskmi(a(1,j),b,work,n1,n2,lwork,ibypsm)
c200    continue
c       write(*,*) 'output b'
c       do 300 i=1,n1
c       call dskmo(b(1,i),n2,ibypsm)
c       write(*,*) (b(j,i),j=1,n2)
c300    continue
c       stop
c       end
cccccccccccccccccccccccccccccccccccccccccc
c
c    disk multiplex routine used to transpose a 2-d big matrix
c
	subroutine dskms(n1,n2,lwork,idskun,dfname,stats,ios,ibypsm)
	integer n1,n2,lwork,ibypsm
	integer idskun,is,ios
	integer in2w,n2c,irecl,n1c
	character*(*) dfname, stats 
	common /par/n2w,ns,jdskun,is,in2w,n1c,n2c,irecl
	save /par/
c initialization
	n2w = lwork/n1
	jdskun = idskun 
	if (n2w .le. 0 ) call cancel(lwork,'lwork too small in dskms')
	ns = (n2-1+n2w)/n2w
	irecl = n2 * ibypsm
c	if (stats .eq. 'scratch' .or. stats.eq.'scratch' ) then 
	if (stats(1:1).eq.'s' .or. stats(1:1).eq.'s' ) then 
	   call opdisk(jdskun,irecl,ios,dfname,'delete',stats)
	else
	   call opdisk(jdskun,irecl,ios,dfname,'keep',stats)
	end if
        is = 1	
	in2w = 0
	n2c = 0
	n1c = 0
	return
	end
        subroutine dskmi(pin,pout,work,n1,n2,lwork,ibypsm)
	character pin(ibypsm,n1),pout(ibypsm,n2),work(ibypsm,lwork)
	integer n1,n2,lwork
	common /par/n2w,ns,jdskun,is,in2w,n1c,n2c,irecl
	save /par/
	in2w = in2w + 1
	n2c = n2c + 1
	do 1000 i1=1,n1
	do 1000 ib=1,ibypsm 
1000	work(ib,(i1-1)*n2w+in2w) = pin(ib,i1)
	if ( in2w .eq. n2w .or. n2c .eq. n2 ) then
	   do 2000 i1=1,n1
	      if (is .gt. 1) call redisk(jdskun,pout,i1,irecl,ios)
	      do 1500 i2=1,in2w
	      do 1500 ib=1,ibypsm
1500          pout(ib,(is-1)*n2w+i2) = work(ib,(i1-1)*n2w+i2)
	      call ridisk(jdskun,pout,i1,irecl,ios)
2000       continue
           is = is + 1
	   in2w = 0
	end if 
	return
	end
        subroutine dskmo(pout,n2,ibypsm)
	character pout(ibypsm,n2)
	integer n2
	common /par/n2w,ns,jdskun,is,in2w,n1c,n2c,irecl
	save /par/
	n1c = n1c + 1
	call redisk(jdskun,pout,n1c,irecl,ios)
	return
	end
