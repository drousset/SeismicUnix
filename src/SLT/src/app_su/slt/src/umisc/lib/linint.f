c--------------------------------------------------------------------- 
c                                                                     
c	        	vector binary search
c                                                                               
c    sample call:                                                               
c                                                                               
c        call bisear(n,nnew,x,xnew,indx)    
c                                                                               
c    input:                                                                     
c             n - int*4    length of x                                          
c          nnew - int*4    length of xnew                                       
c          x(n) - real*4   abscissas of input data knots                        
c    xnew(nnew) - real*4   abscissas of desired data knots                      
c                                                                               
c    output:                                                                    
c                                                                               
c      indx(nnew) - index of x nearest (lefthand side) to xnew                  
c                                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
      subroutine bisear(n,nnew,x,xnew,indx) 
c                                                                               
      real x(n), xnew(nnew)                                                     
      integer n, nnew, indx(nnew)                                               
      do 10000 j=1,nnew                                                         
         indx(j) = 1                                                 
10000 continue                                              
      do 10003 j=1,nnew                                                         
         if ( indx(j) .ge. n ) indx(j) = 1                                      
         if ( xnew(j) .lt. x(indx(j)) ) goto 10001                              
         if ( xnew(j) .le. x(indx(j)+1) ) goto 10003 
10001 indx(j) = 1                                                            
      jj = n + 1        
10002 k = (indx(j)+jj)/2                                                     
      if ( xnew(j) .lt. x(k) ) jj = k                                        
      if ( xnew(j) .ge. x(k) ) indx(j) = k                                   
      if ( jj .gt. indx(j) + 1 ) goto 10002    
10003 continue                                                                  
      return                                      
      end                                                                       
c----------------------------------------------------------
c		vector linear interpolation                         
c                                                                               
c    sample call:                                                               
c                                                                               
c        call linin(n,nnew,x,xnew,indx,y,ynew)                   
c                                                                               
c    input:                                                                     
c             n - int*4    length of x                                          
c          nnew - int*4    length of xnew                                       
c          x(n) - real*4   abscissas of input data knots                        
c    xnew(nnew) - real*4   abscissas of desired data knots                      
c    indx(nnew) - int*4    index of x nearest (lefthand side) to xnew           
c          y(n) - real*4   data values at input data knots x                    
c                                                                               
c    output:                                                                    
c    ynew(nnew) - real*4   data values at desired data knots xnew               
c                                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
      subroutine linin(n,nnew,x,xnew,indx,y,ynew)                               
      real x(n), xnew(nnew), y(n), ynew(nnew)                                   
      integer n, nnew, indx(nnew)                                               
      do 10000 i=1,nnew                                                         
         ii = indx(i)                                                           
         resd = xnew(i) - x(ii)                                                 
         if ( ii .lt. 1 .or. xnew(i).lt. x(1) ) then   
            ynew(i) = y(1)                                                      
         else if ( ii .ge. n ) then                                             
            ynew(i) = y(n)                                                      
         else                                                                   
            ynew(i) = y(ii) + resd*(y(ii+1)-y(ii))/(x(ii+1)-x(ii))              
         end if       
10000 continue                                                                  
      return                                                                    
      end                                                                       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                     lin1d                                                     
c                                                                               
c            linear one-dimensional interpolation                      
c                                                                     
c                   abstract                                                    
c
c   given data yin(nin) at abscissa xin(nin), this subroutine linearly          
c interpolates data yout(nout) at abscissa xout(nout).                          
c                                                                               
c                                                                   
c------------------program history---------------------------------
c                                                                 
c    authors - zhiming li  (linin)    june 1987                  
c                                                               
c                                                              
c------------------specific technical features----------------
c                                                            
c    deck type - vs fortran subroutine                      
c    subroutine called - bisear, linin                                          
c                                                                               
c------------------------------------------------------------------
c                                                                 
c                                                                               
c    sample call:                                                               
c                                                                              
c          call lin1d(xin,yin,nin,xout,yout,nout,indx)                          
c                                                                               
c                                                                               
c    input:                                                                     
c        xin(nin) - real*4   abscissas of input data knots                      
c        yin(nin) - real*4   input data at abscissa xin(nin)                    
c             nin - int*4    length of xin                                      
c      xout(nout) - real*4   abscissas of desired data knots                    
c
c      indx(nout) - int*4    auxilary array of length nout                      
c                                                                               
c    output:                                                                    
c                                                                               
c      yout(nout) - real*4   desired data at abscissa xout(nout)                
c                                                                               
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
      subroutine lin1d(xin,yin,nin,xout,yout,nout,indx)                         
      real xin(nin), yin(nin), xout(nout), yout(nout)                           
      integer nin, nout                                                         
      integer indx(nout)                                                        

      call bisear(nin,nout,xin,xout,indx)                                       
      call linin(nin,nout,xin,xout,indx,yin,yout)                               
      return                                                                    
      end                                                                       
c----------------------------------------------------------
c		1d vector linear interpolation method 2
c                                                                               
c    sample call:                                                               
c                                                                              
c          call lin1d2(xin,yin,nin,xout,yout,nout,indx)                         
c                                                                               
c                                                                               
c    input:                                                                     
c        xin(nin) - real*4   abscissas of input data knots                      
c                            must be monotonically increasing  
c        yin(nin) - real*4   input data at abscissa xin(nin)                    
c             nin - int*4    length of xin                                      
c      xout(nout) - real*4   abscissas of desired data knots                    
c                            must be monotonically increasing  
c
c      indx(nout) - int*4    auxilary array of length nout                      
c
c    output:                                                                    
c                                                                               
c      yout(nout) - real*4   desired data at abscissa xout(nout)                
c                                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
      subroutine lin1d2(xin,yin,nin,xout,yout,nout,indx)                        
      real xin(nin), yin(nin), xout(nout), yout(nout)                           
      integer nin, nout                                                         
      integer indx(nout)                                                        
      integer i1, j, i, i0
      real x

      if(nin.eq.1) then
		do j=1,nout
			yout(j) = yin(1)
 		end do	   
		return
      end if
      i1 = 1
      do j=1,nout
	 x = xout(j)
         if(i1.eq.nin) then
		indx(j) = i1
         else if(x.lt.xin(i1)) then
		indx(j) = i1
	 else
		i0 = i1
		do i=i0,nin-1
			if(x.ge.xin(i) .and. x.le.xin(i+1)) then
                            	i1  = i
				indx(j) = i
				goto 1000
			end if
		end do
		i1 = nin
		indx(j) = nin
1000		continue
	end if
      end do
      call linin(nin,nout,xin,xout,indx,yin,yout)                               
      return                                                                    
      end 

      subroutine lin1dn(xin,yin,nin,xout,yout,nout,indx,dydx0,dydxn)
      real xin(nin), yin(nin), xout(nout), yout(nout),dydx0,dydxn
      integer nin, nout                                                         
      integer indx(nout)                                                        
      integer i1, j, i, i0
      real x

      if(nin.eq.1) then
		do j=1,nout
			yout(j) = yin(1)
 		end do	   
		return
      end if
      i1 = 1
      do j=1,nout
	 x = xout(j)
         if(i1.eq.nin) then
		indx(j) = i1
         else if(x.lt.xin(i1)) then
		indx(j) = i1
	 else
		i0 = i1
		do i=i0,nin-1
			if(x.ge.xin(i) .and. x.le.xin(i+1)) then
                            	i1  = i
				indx(j) = i
				goto 1000
			end if
		end do
		i1 = nin
		indx(j) = nin
1000		continue
	end if
      end do
      call lininn(nin,nout,xin,xout,indx,yin,yout,dydx0,dydxn)
      return	
      end 	

      subroutine lininn(n,nnew,x,xnew,indx,y,ynew,dydx0,dydxn)
      real x(n), xnew(nnew), y(n), ynew(nnew), dydx0, dydxn
      integer n, nnew, indx(nnew)                                               
      do 10000 i=1,nnew                                                         
         if (xnew(i).lt. x(1)) then   
            ynew(i) = y(1)+dydx0*(xnew(i)-x(1))
         else if (xnew(i).ge.x(n)) then
            ynew(i) = y(n)+dydxn*(xnew(i)-x(n))
         else                                                                   
            ii = indx(i)
            resd = xnew(i) - x(ii)
            ynew(i) = y(ii) + resd*(y(ii+1)-y(ii))/(x(ii+1)-x(ii))              
         end if       
10000 continue                                                                  
      return                                                                    
      end                                                                       
