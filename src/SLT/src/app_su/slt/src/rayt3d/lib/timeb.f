	subroutine timeb(nr,nz,dr,dz,fz,a,v0,t)
 
 	integer i, ir, iz 
 	real   r,z,zc,v,rc,oa,ov0,rou,t(*)  
   
   	ov0 = 1.0/v0

	if(a .eq. 0.0) then
c       when r=0
	    do 10 iz=1,nz
	    	z = (iz-1)*dz+fz
	    	t(iz) = z*ov0
 10	    end do

c   	when r>0
	    do 30 ir=1,nr-1
		r = ir*dr
		i = ir*nz    
  		do 40 iz=1,nz
			z = (iz-1)*dz+fz  
 			rou = sqrt(r*r+z*z) 
			t(i+iz) = rou*ov0 
40	 	end do
30	    end do
	    return
	end if

c   a is not 0
	oa = 1.0/a
	zc = v0*oa
c       when r=0
	do 110 iz=1,nz
	    	z = (iz-1)*dz+fz
		v = v0+a*z
	    	t(iz) = log(v*ov0)*oa
 110	    end do
 
c   when r>0
	do 130 ir=1,nr-1
		r = ir*dr
		i = ir*nz    
  		do 140 iz=1,nz
			z = zc+(iz-1)*dz+fz  
  			v = v0+a*(z-zc) 
  			rc = (r*r+z*z-zc*zc)/(2.0*r) 
 			rou = sqrt(zc*zc+rc*rc) 
 			t(i+iz) = log((v*(rou+rc))/(v0*(rou+rc-r)))*oa 
 140		end do
130	end do
 
	return
	end 

 
 
 
 
  	
