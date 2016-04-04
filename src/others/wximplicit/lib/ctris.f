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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COMPLEX TRIDIAGONAL MATRIX SOLVER
C     AUTHOR:    ZHIMING LI
C THE MATRIX EQUATION TO SOLVE IS,
C
C   |B(1) A(1)  0                  0     | |X(1)  |          |D(1)  |
C   |C(2) B(2) A(2)  0                   | |X(2)  |          |D(2)  |        
C   | 0   C(3) B(3) A(3)  0              | |X(3)  |          |D(3)  |         
C   |      0   ............   0          | | .    |   =      |  .   |      
C   |           0   ...........    0     | | .    |          |  .   |         
C   |                0 C(N-1)B(N-1)A(N-1)| |X(N-1)|          |D(N-1)|      
C   | 0                   0  C(N)  B(N)  | |X(N)  |          |D(N)  |     
C
C   INPUT: 
C       N    ---  INT*4     NUMBER OF COLUMNS (OR ROWS) OF MATRIX  
C       A(N) ---  CPLX*8    UPPER DIAGONAL COEFFICIENTS
C       B(N) ---  CPLX*8    DIAGONAL COEFFICIENTS
C       C(N) ---  CPLX*8    LOWER DIAGONAL COEFFICIENTS
C       D(N) ---  CPLX*8    RIGHT-HAND SIZE VECTOR
C       E(N) ---  CPLX*8    WORKING ARRAY  
C       F(N) ---  CPLX*8    WORKING ARRAY  
C   OUTPUT:
C       X(N) ---  SOLUTION VECTOR  
C                
C    
      SUBROUTINE CTRIS(N,A,B,C,D,X,E,F)                              
      COMPLEX X(N),D(N),F(N),E(N)
      COMPLEX A(N),B(N),C(N),DEN                           
      INTEGER N                                                               
      E(1) = -A(1)/B(1)
      F(1) = D(1)/B(1)                                                         
      DO 1000 I = 2,N-1                                                        
         DEN = B(I)+C(I)*E(I-1)
         E(I) = - A(I)/DEN
         F(I) = (D(I)-C(I)*F(I-1))/DEN
1000  CONTINUE                                                               
      X(N) = (D(N)-C(N)*F(N-1))/(B(N)+C(N)*E(N-1))
      DO 2000 I = N-1,1,-1                                                     
         X(I) = E(I)*X(I+1)+F(I)                                                
2000  CONTINUE                                                               
      RETURN                                                                    
      END
