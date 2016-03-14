ccccc
c apply 2.5-D filter 
c       input: 
c          trace         input trace of length n
c          n             length of trace in samples
c       output:
c          trace         output trace with sqrt(-iw) applied 
c
	subroutine f2p5 (trace,n)
	parameter(nwsave=32768,nf=16384)
	real trace(n)
	real cos45
	real wsave(nwsave), sqrtf(nf) 
	real work(nwsave)
	integer n, ifirst, l, m
	real scale
	data ifirst/1/
c compute the filter
	if (ifirst.eq.1) then
	   ifirst = 999
	   m = (n+1)*3/2*2
	   call radix(m,l)
	   m = l
	   if ( 2*m+15 .gt. nwsave ) then
	      call msgscs("increase nwsave in f2p5")
	      stop
	   end if   
	   call rffti(m,wsave)
	   cos45 = 0.5 * sqrt(2.)
	   scale = (2./m)
	   do i=1,m/2+1
	      sqrtf(i) = sqrt( (i-1)*scale ) * cos45 / m
	   end do
	   l = (m+1)/2 
	end if	
c forward fft
	do i=1,n
		work(i) = trace(i) 
	end do
	do i=n+1,m
		work(i) = 0. 
	end do
	call rfftf(m,work,wsave)
c apply the filter
	work(1) = 0.
	do k=2,l
	   ir = 2 * k - 2 
           ii = 2 * k - 1
	   tr = work(ir)*sqrtf(k)
	   ti = work(ii)*sqrtf(k)
	   work(ir) = tr + ti 	
	   work(ii) = ti - tr 	
	end do  
c	work(m) = 0.
	work(m) = work(m)*sqrtf(m/2+1)
c inverse fft
	call rfftb(m,work,wsave)
	do i=1,n
		trace(i) = work(i)
	end do
	return
	end

