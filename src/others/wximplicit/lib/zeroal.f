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
        SUBROUTINE ZEROAL(DATA,N,IBYPS)
	INTEGER N, IBYPS
	INTEGER*2 DATA(*)
	LEND = N * IBYPS / 2
	DO I=1,LEND
	   DATA(I) = 0
	END DO
	RETURN
	ENd
