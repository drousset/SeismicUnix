CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         
C                       VCTRIS                                                  
C          VECTOR COMPLEX TRIDIAGONAL MATRIX SOLVER                             
C                                                                               
C                                                                               
C   THERE ARE M (N BY N) COMPLEX TRIDIAGONAL MATRIX EQUATIONS TO SOLVE,         
C EACH ONE HAS THE FOLLOWING FORMAT,                                            
C                                                                               
C | D(1) U(1)  0                  0      | | X(1) |         |Y(1)  |            
C |  L(2) D(2) U(2)  0                   | | X(2) |         |Y(2)  |            
C |   0   L(3) D(3) U(3)  0              | | X(3) |         |Y(3)  |            
C |        0   ............   0          | |  .   |    =    | .    |           
C |             0   ...........    0     | |  .   |         | .    |            
C |                  0 L(N-1)D(N-1)U(N-1)| |X(N-1)|         |Y(N-1)|            
C |   0                   0  L(N)  D(N)  | |X(N)  |         |Y(N)  |       
C                                                                               
C   INPUT:                                                                      
C       M       ---  INT*4     NUMBER OF EQUATIONS                              
C       N       ---  INT*4     NUMBER OF UNKNOWNS IN EACH EQUATION              
C       U(M,N)  ---  CPLX*8    UPPER DIAGONAL COEFFICIENTS                      
C       D(M,N)  ---  CPLX*8    DIAGONAL COEFFICIENTS                            
C       L(M,N)  ---  CPLX*8    LOWER DIAGONAL COEFFICIENTS                      
C       X(M,N)  ---  CPLX*8    RIGHT-HAND SIZE VECTORS (Y)                      
C   OUTPUT:                                                                     
C       X(M,N)  ---  SOLUTION VECTORS                                           
C                                                                               
C   NOTE: AFTER VCTRIS, CONTENTS OF D IS DESTROYED                              
C                                                                               
C     AUTHOR:    ZHIMING LI    3/91                                             
C                                                                               
      SUBROUTINE VCTRIS(M,N,U,D,L,X)                                            
      COMPLEX U(M,N),D(M,N),L(M,N),X(M,N),C                                     
      INTEGER M,N                                                               
C FORWARD ELIMINATION                                                           
      DO J=1,M                                                                  
         D(J,1) = 1./D(J,1)                                                     
      END DO                                                                    
      DO I = 2,N                                                                
         DO J = 1,M                                                             
            C = -L(J,I)*D(J,I-1)                                                
            D(J,I) = 1./(D(J,I)+C*U(J,I-1))                                     
            X(J,I) = X(J,I)+X(J,I-1)*C                                          
         END DO                                                                 
      END DO                                                                    
C BACK SUBSTITUTION                                                             
      DO J = 1, M                                                               
         X(J,N) = X(J,N)*D(J,N)                                                 
      END DO                                                                    
      DO I = N-1,1,-1                                                           
         DO J = 1, M                                                            
            X(J,I) = (X(J,I)-X(J,I+1)*U(J,I))*D(J,I)                            
         END DO                                                                 
      END DO                                                                    
      RETURN                                                                    
      END                                                                       
