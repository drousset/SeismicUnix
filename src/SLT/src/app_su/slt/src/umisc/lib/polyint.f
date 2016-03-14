
cccccc polynomial interpolation
cccc use 1/dis as weight
	subroutine plint(x,y,f,m,n,xout,yout,fout,work,dis,indx)
	integer m,n
	real x(n),y(n),f(m,n),fout(m),xout,yout
	real work(n)
	real dis
	integer indx(n)
	
	integer i,j,ii 
	real tmp, norm, scale
	real dismax,eps
	
	eps = 1.
	dismax = 0.
	do i=1,n
		work(i) = sqrt( (xout-x(i))**2 + (yout-y(i))**2 )
		if(dismax.lt.work(i)) dismax = work(i)
	end do

	tmp = dis
10000   continue
	ii = 0
	do i=1,n	
		if(work(i).le.tmp) then
			if(work(i).lt.eps) work(i) = eps
			ii = ii + 1
			indx(ii) = i
		end if 
	end do
	if(ii.eq.0) then
		tmp = tmp + (dismax-tmp)/2. 
		go to 10000 
	end if 

	norm = 0.
	do i=1,ii
		iii = indx(i)
		tmp = 1./work(iii)
		work(iii) = tmp
		norm = norm + tmp
	end do
	do j=1,m
		fout(j) = 0.
	end do
	do i=1,ii
		iii = indx(i)
		scale = work(iii)/norm
		do j=1,m
			fout(j) = fout(j) + f(j,iii)*scale
		end do
	end do
	return
	end 
