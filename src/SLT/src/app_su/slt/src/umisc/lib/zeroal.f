c        integer*2 i2(10)
c	integer i4(10)
c	real r4(10)
c	complex c8(10)
c	do i=1,10
c	   i2(i) = i
c	   i4(i) = i
c	   r4(i) = i
c	   c8(i) = cmplx(i,i)
c	end do
c	call zeroal(i2,10,2)
c	call zeroal(i4,10,4)
c	call zeroal(r4,10,4)
c	call zeroal(c8,10,8)
c	write(*,*) i2
c	write(*,*) i4
c	write(*,*) r4
c	write(*,*) c8
c	stop
c	end
c
        subroutine zeroal(data,n,ibyps)
	integer n, ibyps
	integer*2 data(*)
	lend = n * ibyps / 2
	do i=1,lend
	   data(i) = 0
	end do
	return
	end
