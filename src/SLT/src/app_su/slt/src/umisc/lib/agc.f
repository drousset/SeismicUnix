c agc subroutine
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  test program for agc subroutine
c
c	parameter (nt=1000,lwin=125)
c	real trace(nt), work(nt)
c	do it=1,nt
c		trace(it) = it 
c	end do
c	rmso = 0.
c	call agc(trace,nt,lwin,work,rmso)
c	ldt = nt/100
c	do it=1,nt,ldt
c		write(*,*) trace(it),it,work(it)
c	end do
c	end	
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c	subroutine agc does agc gain of trace input
c
c	input:
c		trace(nt)	---	real*4 		input trace 
c		nt			---	int*4		length of trace
c		lwin		---	int*4		agc window length 
c							in sample
c		work(nt)	---	real*4		work array
c		rmso		---	real*4		desired output 
c							rms value
c							(if 0., 1.0 is used)
c	output:
c		trace(nt)	---	real*4		agc gained trace
c	NOTE:
c		When an input trace y(lt) has a mute time at mute (in
c		sample), then
c			call agc(y(mute),lt-mute+1,lwin,work,rte),
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine agc(trace,nt,lwin,work,rmso)
	real trace(nt),work(nt),rmso
	integer nt,lwin,hwin,nwin
	real rms,a2
ccc compute half window length
	hwin = lwin/2
ccc 
	sum = 0.
	do it=1,nt
		work(it) = 0.
	end do 
	do it=1,hwin+1
		a2 = trace(it)*trace(it) 
		sum = sum + a2
	end do
	nwin = hwin + 1
	rms = sum/nwin
	if(rms .gt. 0.) work(1) = trace(it)/sqrt(rms)  
	do it=2,hwin+1
	   	a2 = trace(it+hwin) * trace(it+hwin) 
	   	sum = sum + a2
	  	nwin = nwin + 1
	   	rms = sum/nwin
           	if(rms.gt.0.) work(it) = trace(it)/sqrt(rms)
	end do

	tmp = 1./nwin
	do it=hwin+2,nt-hwin+1
	   	a2 = trace(it+hwin) * trace(it+hwin) 
		sum = sum + a2
	   	a2 = trace(it-hwin-1) * trace(it-hwin-1) 
		sum = sum - a2
		rms = sum * tmp 
	        if(rms.gt.0.) work(it) = trace(it)/sqrt(rms)
	end do

	do it=nt-hwin+2,nt
	   	a2 = trace(it-hwin-1) * trace(it-hwin-1) 
		sum = sum - a2
		nwin = nwin - 1
		rms = sum / nwin 
		if(rms.gt.0.) work(it) = trace(it)/sqrt(rms)
	end do

	if ( rmso .gt. 0. ) then	
		do it=1,nt
			trace(it) = work(it) * rmso 
		end do
	else
		do it=1,nt
			trace(it) = work(it)
		end do
	end if

	return
	end
