cccc 2-d interpolation 
c	subroutine intp2d(x,y,f,m,n,xout,yout,fout,np,indx,work)
c	int m,n,np
c	real x(n),y(n),f(m,n),fout(m),xout,yout
c	real work(n)
ccc
c	input:
c		x(n)	---	x coordinates of data points
c		y(n)	---	y coordinates of data points
c               f(m,n)  ---     vector function values at (x,y)
c               m       ---     length of function f
c               n       ---     number of data points
c               xout    ---     x coordinate of output point
c               yout    ---     y coordinate of output point
c               np      ---     number of nearest data points used to
c                               interpolate output
c               indx(n) ---     work array
c               work(n) ---	work array
c	output:
c               fout(m) ---     vector function at (xout,yout) 
c
c	author:	
c		zhiming li	       	6/30/92
cccccccc   
	subroutine intp2d(x,y,f,m,n,xout,yout,fout,np,indx,work)
	integer m,n,np
	real x(n),y(n),f(m,n),fout(m),xout,yout
	real work(n)
	integer indx(n)
	
	integer i,j,ii 
	real tmp, norm, scale
	real tiny
	
c call c subroutine qkisort to find the indx such that
	do i=1,n
ccc		indx(i) = i - 1
		indx(i) = i
		work(i) = sqrt( (xout-x(i))**2 + (yout-y(i))**2 )
	end do


	nn = min0(np,n)

cccccc too slow
cccc	call qkisort(n,work,indx)
c	do i=1,n
c		indx(i) = indx(i) + 1
cccc	end do

	do i=1,nn
		tmp = work(i) 
		ii = i 
		do j=i+1,n
			if(tmp.gt.work(j)) then
				ii = j
				tmp = work(j)
			end if
		end do
		itmp = indx(ii)
		indx(ii) = indx(i)
		indx(i) = itmp
		work(ii) = work(i) 
		work(i) = tmp 
	end do   

cc	do i=1,nn
cc		write(6,*) "indx=",indx(i)," work=",work(i)
cc	end do
	

	tiny=1.e-6
	
cccc	if(work(indx(1)).le.tiny) then
	if(work(1).le.tiny) then
		ii = indx(1)
		do j=1,m
			fout(j) = f(j,ii)
		end do	
		return
	end if
	
	norm = 0.
	do i=1,nn
cccc		tmp = 1./work(indx(i))
cccc		work(indx(i)) = tmp
		tmp = 1./work(i)
		work(i) = tmp
		norm = norm + tmp
	end do
	do j=1,m
		fout(j) = 0.
	end do
	do i=1,nn
		ii = indx(i)
ccccc		scale = work(ii)/norm
		scale = work(i)/norm
		do j=1,m
			fout(j) = fout(j) + f(j,ii)*scale
		end do
	end do
	return
	end 
