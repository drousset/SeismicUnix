
	subroutine recipr(v,n1,n2,n3)

	integer i1,i2,i3
	real v(n1,n2,n3)

	do 10 i3=1,n3
	  do 10 i2=1,n2
	    do 10 i1=1,n1
		v(i1,i2,i3) = 1.0/v(i1,i2,i3)
10	end do

	return
	end
  
 	subroutine tripd2(d, e, b, n, m)
******************************************************************************
* Given m n-by-n symmetric, tridiagonal, positive definite matri2 A's and m
* n-vector b's, the following algorithm overwrites b with the solution to Ax = b.
* The first dimension of arrays is independent of the algorithm. 

*  input 
*  	d() the diagonal of A 
*  	e() the superdiagonal of A
*****************************************************************************/
 
	integer k, i  
	real 	temp, e(m,n),b(m,n),d(m,n) 
	
c	decomposition 	
	do 10 k=2,n 
	do 10 i=1,m   
          	temp = e(i,k-1)  
           	e(i,k-1) = temp/d(i,k-1)
           	d(i,k) = d(i,k)-temp*e(i,k-1) 
10	end do

c	substitution	 
        do 20 k=2,n 
	do 20 i=1,m	 
 		b(i,k) = b(i,k)-e(i,k-1)*b(i,k-1)
20	end do
	
	do i=1,m
        	b(i,n) = b(i,n)/d(i,n)
	end do

        do 30 k=n,2,-1  
	do 30 i=1,m 
		b(i,k-1) = b(i,k-1)/d(i,k-1)-e(i,k-1)*b(i,k)
30	end do 
	
 	return
	end
	
	
