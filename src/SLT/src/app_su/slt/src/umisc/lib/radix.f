cccccccccccccccccccccccc
c	integer n, nfft
c	do i=0,20
c		n = 1252 + i *10
c		call radix(n,nfft)
c		write(*,*) "n=",n," nfft=",nfft
c	end do
c	end 
cccc
cccc subroutine radix determines optimal fft length  
cccc basic index 4,2,3,5
cccc n must be less than 737280 (2**14 * 3**2 * 5)
cccc
cccc  input:
cccc		n 	---    	length of array to be ffted
cccc  output:
cccc		nfft 	---    	length of fft to be used
cccc
cccc author:	Zhiming Li	      		2-21-92
cccc
cccc
cccc
	subroutine radix(n,nfft)
	parameter(n2=14,n3=2,n5=1) 
	integer n, nfft 
cccc
	nfft = 2**n2 * 3**n3 * 5**n5
	do i2=0,n2
		do i3=0,n3
			do i5=0,n5
				m = 2**i2 * 3**i3 * 5**i5
				if(m.ge.n .and. nfft.gt.m) nfft = m
			end do
		end do
	end do
	return
	end 
	
	
