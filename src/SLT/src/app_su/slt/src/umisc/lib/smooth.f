ccccccc
c  2-d smoothing using triangular weighting functions 
c
c  input:
c	f(n1,n2)        --   input data
c       work(n1,n2)     --   work buffer
c       f1(nf1)         --   work buffer 
c       f2(nf2)         --   work buffer 
c       n1              --   number of rows in f
c       n2              --   number of columns in f
c       nf1             --   length of smoothing along 1st axis 
c       nf2             --   length of smoothing along 2nd axis 
c  output:
c	f(n1,n2)        --   smoothed output data
c
c  author:      Zhiming Li                     6/1/91
ccccccc
	subroutine smth2d(f,work,f1,f2,n1,n2,nf1,nf2)
	real f(n1,n2),work(n1,n2),f1(nf1),f2(nf2)
	integer n1,n2,nf1,nf2
cccc smooth along 1st axis 


	if (nf1.le.1 .and. nf2.le.1) return
	if (nf1 .gt. 1 .and. n1 .gt. 1) then
	   nf1h = (nf1+1)/2
	   s1 = 0.
ccccccccc  triangular smoothing function
	   do i1=1,nf1
	      f1(i1) = nf1h - abs(i1-nf1h) 
	      s1 = s1 + f1(i1)
	   end do
	   s1 = 1./s1 
	   do i1=1,nf1
	      f1(i1)= f1(i1)*s1
	   end do
ccccccccc  smoothing along 1st axis
	   do i2=1,n2
	      call smth1d(f(1,i2),f1,work(1,i2),n1,nf1,1)
	   end do 
           if(n2.le.1 .or. nf2.le.1) then	
	   	do i2=1,n2
			do i1=1,n1
				f(i1,i2) = work(i1,i2)
			end do
	   	end do
	        return
	   end if
	end if 
ccccc smoothing along 2nd axis
	if (nf2 .gt. 1 .and. n2 .gt. 1) then
	   if(nf1.le.1 .or. n1.le.1) then
	   	do i2=1,n2
	      		do i1=1,n1
		 		work(i1,i2) = f(i1,i2)
	      		end do
	   	end do
	   end if
	   nf2h = (nf2+1)/2
	   s2 = 0.
ccccccc triangular smoothing function
	   do i2=1,nf2
	      f2(i2) = nf2h - abs(i2-nf2h) 
	      s2 = s2 + f2(i2)
	   end do
	   s2 = 1./s2 
	   do i2=1,nf2
	      f2(i2)= f2(i2)*s2
	   end do
ccccccccc 2nd axis smoothing
	   call smth1d(work,f2,f,n2,nf2,n1)
	end if
	return
	end
ccccc
c    
c     vector 1-d smoothing 
c
c		   k
c      w(i,j)  =  sum  y(i,j-(k+1)/2+m) * f(m)
c	          m=1
c      1<=i<=l; 1<=j<=n
c
c input:
c	y(l,n)   --   input data
c       f(k)     --   smoothing function (coefficients)
c       n        --   number of columns of y
c       l        --   number of rows of y
c       k        --   smoothing length 
c output:
c       w(l,n)   --   smoothed output data
c
c author:      Zhiming Li                     6/1/91
cccccc
	subroutine smth1d(y,f,w,n,k,l)
	real y(l,n),f(k),w(l,n)
	integer n,k,l
	kk = (k+1)/2
	do i=1,n
	   do j=1,l
	      w(j,i) = 0.
	   end do
	end do
	do i=1,n
	   do j=1,k
	      m = i + j - kk
	      if (m.lt.1) then
	         m = 1
	      else if (m .gt. n) then 
		 m = n
	      end if
	      do ii=1,l
	         w(ii,i) = w(ii,i) + y(ii,m) * f(j)
	      end do
	   end do
	end do
	return
	end
