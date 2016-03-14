c          complex a(10),b(10),c(10),d(10),e(10),f(10),x(10)
c	  complex aa(10,10) 
c	  do i=1,10
c	     a(i) = 1. 
c	     b(i) = -2.
c	     c(i) = 1.
c	     x(i) = i
c          end do
c	  write(*,*) x
c	  do j=1,10
c	     do i=1,10
c		aa(i,j) = 0.
c             end do
c          end do
c	  do ii=1,10
c	     aa(ii,ii) = b(ii)
c          end do
c	  do ii = 1, 9
c	     aa(ii,ii+1) = a(ii)
c	     aa(ii+1,ii) = c(ii+1)
c          end do
c	  do i=1,10
c	     d(i) = 0.
c	     do j=1,10
c	       d(i) = d(i) + aa(i,j) * x(j)
c             end do
c          end do
c	  do i=1,10
c	  x(i) = 0.
c	  end do
c	  call ctris(10,a,b,c,d,x,e,f)
c          write(*,*) x
c	  end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c complex tridiagonal matrix solver
c     author:    zhiming li
c the matrix equation to solve is,
c
c   |b(1) a(1)  0                  0     | |x(1)  |          |d(1)  |
c   |c(2) b(2) a(2)  0                   | |x(2)  |          |d(2)  |        
c   | 0   c(3) b(3) a(3)  0              | |x(3)  |          |d(3)  |         
c   |      0   ............   0          | | .    |   =      |  .   |      
c   |           0   ...........    0     | | .    |          |  .   |         
c   |                0 c(n-1)b(n-1)a(n-1)| |x(n-1)|          |d(n-1)|      
c   | 0                   0  c(n)  b(n)  | |x(n)  |          |d(n)  |     
c
c   input: 
c       n    ---  int*4     number of columns (or rows) of matrix  
c       a(n) ---  cplx*8    upper diagonal coefficients
c       b(n) ---  cplx*8    diagonal coefficients
c       c(n) ---  cplx*8    lower diagonal coefficients
c       d(n) ---  cplx*8    right-hand size vector
c       e(n) ---  cplx*8    working array  
c       f(n) ---  cplx*8    working array  
c   output:
c       x(n) ---  solution vector  
c                
c    
      subroutine ctris(n,a,b,c,d,x,e,f)                              
      complex x(n),d(n),f(n),e(n)
      complex a(n),b(n),c(n),den                           
      integer n                                                               
      e(1) = -a(1)/b(1)
      f(1) = d(1)/b(1)                                                         
      do 1000 i = 2,n-1                                                        
         den = b(i)+c(i)*e(i-1)
         e(i) = - a(i)/den
         f(i) = (d(i)-c(i)*f(i-1))/den
1000  continue                                                               
      x(n) = (d(n)-c(n)*f(n-1))/(b(n)+c(n)*e(n-1))
      do 2000 i = n-1,1,-1                                                     
         x(i) = e(i)*x(i+1)+f(i)                                                
2000  continue                                                               
      return                                                                    
      end
