  
 
**  residual traveltime calculation based on reference time **
	subroutine resit(t,nx,ny,nz,nr,dx,dy,dr,fx,fy,x0,y0,tb)
c
c Input:
c   t(nz,nx,ny) traveltime before implementation of residual
c   fx,fy,fz,nx,ny,nz,dx,dy,dz define t grid 
c   tb(nz,nr) reference traveltime
c   nr,dr define tb as function of lateral distance r (fr=0.)
c Output:
c   t(nz,nx,ny) residual traveltime  

	integer ix,iy,iz,jr  
	real xi,yi,ar,sr,sr1 
	real t(nz,nx,ny),tb(nz,nr)  		
  
   
 	do 10 iy = 1,ny
  	    do 20 ix = 1,nx 
		yi = fy+(iy-1)*dy-y0
		xi = fx+(ix-1)*dx-x0
     		ar = sqrt(xi*xi+yi*yi)/dr 
		jr = 1+int(ar) 
 		jr = min0(nr-1,jr)
		sr = 1.0+ar-float(jr) 
 		sr1 = 1.0-sr 
c$DIR FORCE_VECTOR
    	    	do 30 iz = 1,nz
30    		  t(iz,ix,iy) = t(iz,ix,iy)-sr1*tb(iz,jr)-sr*tb(iz,jr+1)  
20	    continue
10 	continue
 
	return
	end 	
