c      complex data(16),x(16),cdft
c      nn = 16 
c      isign = -1
c      do i=1,nn
c	data(i) = cmplx(i,i)
c	x(i) = data(i)
c      end do
c      call four1(nn,data,isign)
c      dk = 2.*3.14159265/nn
c      do k=1,nn
c	cdft = (0.,0.)
c	do i=1,nn
c	   cdft = cdft + x(i) * cexp(cmplx(0.,-(i-1)*(k-1)*dk))
c	end do
c	cdft = cdft * sc 
c	write(*,*) 'four1=',data(k), ' dft=',cdft, k  
c      end do 
c      end
C                                                                              
      SUBROUTINE FOUR1(NN,DATA,SIGN)
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA                                          
      DIMENSION DATA(*)
      REAL SIGN
      N=2*NN                                                                    
      J=1
c      sc = 1./sqrt(float(NN))
c      do i=1,N
c	 DATA(i) = DATA(i) * sc
c      end do
      DO 11 I=1,N,2                                                             
	IF(J.GT.I)THEN                                                          
	  TEMPR=DATA(J)                                                         
	  TEMPI=DATA(J+1)                                                       
	  DATA(J)=DATA(I)                                                       
	  DATA(J+1)=DATA(I+1)                                                   
	  DATA(I)=TEMPR                                                         
	  DATA(I+1)=TEMPI                                                       
	ENDIF                                                                   
	M=N/2                                                                   
 1      IF ((M.GE.2).AND.(J.GT.M)) THEN                                         
	  J=J-M                                                                 
	  M=M/2                                                                 
	GO TO 1                                                                 
	ENDIF                                                                   
	J=J+M                                                                   
 11   CONTINUE                                                                  
      MMAX=2                                                                    
 2    IF (N.GT.MMAX) THEN                                                       
	ISTEP=2*MMAX                                                            
	THETA=6.28318530717959D0/(SIGN*MMAX)                                  
	WPR=-2.D0*DSIN(0.5D0*THETA)**2                                          
	WPI=DSIN(THETA)                                                         
	WR=1.D0                                                                 
	WI=0.D0                                                                 
	DO 13 M=1,MMAX,2                                                        
	  DO 12 I=M,N,ISTEP                                                     
	    J=I+MMAX                                                            
	    TEMPR=SNGL(WR)*DATA(J)-SNGL(WI)*DATA(J+1)                           
	    TEMPI=SNGL(WR)*DATA(J+1)+SNGL(WI)*DATA(J)                           
	    DATA(J)=DATA(I)-TEMPR                                               
	    DATA(J+1)=DATA(I+1)-TEMPI                                           
	    DATA(I)=DATA(I)+TEMPR                                               
	    DATA(I+1)=DATA(I+1)+TEMPI                                           
 12       CONTINUE                                                              
	  WTEMP=WR                                                              
	  WR=WR*WPR-WI*WPI+WR                                                   
	  WI=WI*WPR+WTEMP*WPI+WI                                                
 13     CONTINUE                                                                
	MMAX=ISTEP                                                              
      GO TO 2                                                                   
      ENDIF                                                                     
      RETURN                                                                    
      END 
