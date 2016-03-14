c
c *********************************************************************
c Input:
c nslopes	number of amplitudes specified
c smin		minimum slope
c ds		slope increment
c amps		amplitudes corresponding to smin+(is-1)*ds 
c bias		linear moveout slope before and after filtering
c nt		number of time samples
c dt		time sampling interval
c nx		number of traces
c dx		trace space (spatial sampling interval)
c data		data to be filtered
c cpfft         work array  
c pfft          work array  
c ntfft         fft length of t
c nxfft         fft length of x
c
c Output:
c data		filtered data
c *********************************************************************
c Notes:
c Linear interpolation and constant extrapolation are used to
c determine amplitudes for slopes that are not specified.
c **********************************************************************
	subroutine dpf (nslopes,smin,ds,amps,bias,nt,dt,nx,dx,data,
     &		       cpfft,pfft,ntfft,nxfft)
	integer nslopes,nt,nx
	integer ntfft, nxfft, nw, nk
	integer it,ix,iw,ik
	real sfft, dw, fw, dk, fk, w, k
	real slope,amp,bias
	complex cpfft(ntfft/2+1,nxfft)
	real pfft(ntfft,nxfft),data(nt,nx)
	real amps(nslopes)
	real phase
	complex cshift


	sfft = 1.0/(ntfft*nxfft)
	
c	/* determine frequency and wavenumber sampling */
	nw = ntfft/2+1
	dw = 2.0*3.141592654/(ntfft*dt)
c /* non-zero to avoid divide by zero w */
	fw = 0.000001*dw
	nk = nxfft
	dk = 2.0*3.141592654/(nxfft*dx)
	fk = 0.0

c	/* copy data from input to FFT array and pad with zeros */
	do ix=1,nx
		do it=1,nt 
			pfft(it,ix) = data(it,ix)
		end do
		do it=nt+1,ntfft 
			pfft(it,ix) = 0.
		end do
	end do
	
	do ix=nx+1,nxfft
		do it=1,ntfft
			pfft(it,ix) = 0.0
		end do
	end do
	
c	/* Fourier transform t to w */
	call pfa2rc(1,1,ntfft,nx,pfft,cpfft)
	
c	/* do linear moveout bias via phase shift */
	if ( bias .ne. 0. ) then
	    do ix=1,nx
		do iw=1,nw
			w=(iw-1)*dw
			phase = -(ix-1)*dx*w*bias
			cshift = cmplx(cos(phase),sin(phase))
			cpfft(iw,ix) = cpfft(iw,ix)*cshift
		end do
	    end do
	end if
	
c	/* Fourier transform x to k */
	call pfa2cc(-1,2,nw,nxfft,cpfft)
	
c	/* loop over wavenumbers */
	nkq=nk/2+1
	do ik=1,nk
c		/* determine wavenumber */
		if(ik.lt.nkq) then
			k = (ik-1)*dk
		else
			k = (ik-nk-1)*dk
		end if

c		/* loop over frequencies */
		do iw=1,nw
			w=fw+(iw-1)*dw
c			/* determine biased slope */
			slope = k/w+bias
			tmp = slope
			slope = (slope-smin)/ds + 1
			islope = slope
			if ( islope .lt. 1 ) then
				islope = 1
			else if(islope.gt.nslopes ) then
				islope = nslopes
			end if 
			amp = amps(islope) 

c			/* include fft scaling */
			amp = amp * sfft
			
c			/* apply filter */
			cpfft(iw,ik) = cpfft(iw,ik) * amp
		end do
	end do

c	/* Fourier transform k to x */
	call pfa2cc(1,2,nw,nxfft,cpfft)

c	/* undo linear moveout bias via phase shift */
	if ( bias .ne. 0. ) then
	    do ix=1,nx
		do iw=1,nw
			w=(iw-1)*dw
			phase = (ix-1)*dx*w*bias
			cshift = cmplx(cos(phase),sin(phase))
			cpfft(iw,ix) = cpfft(iw,ix)*cshift
		end do
	    end do
	end if

c	/* Fourier transform w to t */
	call pfa2cr(-1,1,ntfft,nx,cpfft,pfft)
	
c	/* copy filtered data from FFT array to output */
	do ix=1,nx
		do it=1,nt
			data(it,ix) =  pfft(it,ix)
		end do
	end do
	return
	end
