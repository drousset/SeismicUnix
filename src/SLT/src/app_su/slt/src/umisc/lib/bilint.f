c
c vector bilinar interpolation 
c constant extrapolation used at edges
c
c input:
c     f(n1,nx,ny)
c     x0,y0,dx,dy
c     x,y
c output:
c     g(n1)
c
c author:	z. li
c
	subroutine bilint(n1,nx,ny,x0,y0,dx,dy,x,y,f,g)
	real f(n1,nx,ny),g(n1)
	integer n1,nx,ny
	real x0, y0, dx, dy, x, y, tmp
	integer ix, iy, ix1, iy1
	real rx, ry, rx1, ry1
	real r00,r10,r01,r11
	integer i1
c x indice and scales	
	tmp = (x - x0)/dx + 1.0
	ix = tmp
	if(ix.lt.1) then
		ix = 1
		ix1 = 1
		rx = 0.5
		rx1 = 0.5
	else if (ix.ge.nx) then
		ix = nx
		ix1 = nx
		rx = 0.5
		rx1 = 0.5
	else 
		ix = tmp
		ix1 = ix + 1
		rx1 = tmp-ix
		rx = 1.-rx1
	end if
c y indice and scales
	tmp = (y - y0)/dy + 1.0
	iy = tmp
	if(iy.lt.1) then
		iy = 1
		iy1 = 1
		ry = 0.5
		ry1 = 0.5
	else if (iy.ge.ny) then
		iy = ny
		iy1 = ny
		ry = 0.5
		ry1 = 0.5
	else 
		iy = tmp
		iy1 = iy + 1
		ry1 = tmp-iy
		ry = 1.-ry1
	end if
c scales at four corners
	r00 = rx * ry 
	r10 = rx1 * ry
	r01 = rx * ry1
	r11 = rx1 * ry1
c interpolation 
	do i1=1,n1
		g(i1)=r00*f(i1,ix,iy)+r10*f(i1,ix1,iy)
     1		     +r01*f(i1,ix,iy1)+r11*f(i1,ix1,iy1)
	end do
	return
	end
	
