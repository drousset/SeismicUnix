ccccc test program
cc		real c(3), x(3), y(3)
cc		real tmp
c
c		x(1) = -1
c		x(2) = 0.
c		x(3) = 1.
c		y(1) = 0.2
c		y(2) = 1.0
c		y(3) = 0.4
c
c		call pol3coe(x(1),x(2),x(3),y(1),y(2),y(3),c(1),c(2),c(3))
c
c		do i=1,3
c				write(*,*) "c(",i,")=",c(i)
c		end do
c		do i=1,3
c			tmp = c(1) + c(2)*x(i) + c(3)*x(i)*x(i)
c			write(*,*) "i=",i," xi=",x(i), " yi=",y(i), " YI=",tmp
c		end do
c
c		xi = -c(2)/(2.*c(3))
c		yi = c(1) + c(2)*xi+c(3)*xi**2
c		write(*,*) xi, yi
c
c		call pol3max(x(1),x(2),x(3),y(1),y(2),y(3),xi,yi)
c		write(*,*) xi, yi
c
c
c		end

cccc
cccc given:     y1<y2>y3, (or y1>y2<y3) for x1<x2<n3
cccc find x, such that y is maximum (or minimun)
cccc using 2nd-order polynomial fit of data points
cccc
		subroutine pol3max(x1,x2,x3,y1,y2,y3,x,y)
		real x1,x2,x3,y1,y2,y3,x,y
		real c1,c2,c3

		call pol3coe(x1,x2,x3,y1,y2,y3,c1,c2,c3)

		x = -c2/(2.*c3)
		y = c1 + c2*x + c3*x*x

		return
		end



cccc compute coefficients for y = c1 + c2*x + c3*x**2
cccc given input xi,yi,i=1,2,3
ccc
       	subroutine pol3coe(x1,x2,x3,y1,y2,y3,c1,c2,c3)
		real x1,x2,x3,y1,y2,y3,c1,c2,c3

	   	real*8 x1mx3,x1mx2,xx1mxx3, xx1mxx2
	   	real*8 tmp1,tmp2,tmp3

		x1mx3 = x1 - x3
		x1mx2 = x1 - x2
		xx1mxx3 = x1*x1 - x3*x3
		xx1mxx2 = x1*x1 - x2*x2

	   	tmp1 = x1mx3*xx1mxx2 - x1mx2*xx1mxx3
	   	tmp2 = (y1-y2)*x1mx3 - (y1-y3)*x1mx2 

	   	tmp3 = tmp2/tmp1
		c3 = tmp3
		tmp2 = (y1-y2-tmp3*xx1mxx2)/x1mx2
	   	c2 = tmp2
		tmp1 = y1 - tmp2*x1 - tmp3*x1*x1
		c1 = tmp1
	   	return
	   	end
 
