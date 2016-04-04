cccccccc   3-d implicit f.d. error compensation by phase shift
	subroutine errcom(cp,nx,ny,dx,dy,w,dz,v,wk,
     *          	  n,alpha,beta,trick)
	complex cp(nx,ny),wk(ny),cerr
	real alpha(n),beta(n),trick
	real dx,dy,dz,w,v
	real ax,bx,ay,by
	integer nx,ny,n
ccccccc
	if ( nx*dx .gt. 0. ) then
	   dkx = 2.*3.141592654/(nx*dx) 
	   dx2 = dx*dx
	   odx2=1./dx2
	else
	   dkx = 0.
	   odx2= 0.
	end if
ccccccc
	if ( ny*dy .gt. 0. ) then
	   dky = 2.*3.141592654/(ny*dy) 
	   dy2 = dy*dy
	   ody2=1./dy2
	else
	   dky = 0.
	   ody2= 0.
	end if
ccccc
	nxq = nx/2+1
	nyq = ny/2+1
	wv2 = (w/v)**2
	wv = w/v
	vw2 = (v/w)**2
	vwh = .5  * v/w
	scale = nx*ny
	scale = 1./scale
	odz = 1./dz
	do 1000 iy=1,ny
	   call four1(nx,cp(1,iy),1.)
 1000   continue
	if ( ny .gt. 1 ) then
	  do 2000 ix=1,nx
	     do 1500 iy=1,ny
 1500        wk(iy) = cp(ix,iy)
	     call four1(ny,wk,1.)
	     do 1800 iy=1,ny
 1800        cp(ix,iy) = wk(iy)
 2000      continue
	end if
	do 3000 iy=1,nyq
           fky = (iy-1)*dky	 
cccccccc   if ( iy .gt. nyq ) fky = (iy-ny-1)*dky
	   fky2 = fky*fky
	   fky2n = 2.*(1.-cos(fky*dy))*ody2 
	   iys = 0
	   if ( iy .gt. 1 .and. iy .lt. nyq ) iys = ny-iy+2
	   do 2500 ix=1,nxq
              fkx = (ix-1)*dkx	 
cccccccc      if ( ix .gt. nxq ) fkx = (ix-nx-1)*dkx
	      fkx2 = fkx*fkx
	      tmp = wv2 - fkx2 - fky2 
	      ixs = 0
	      if ( ix .gt. 1 .and. ix .lt. nxq ) ixs = nx-ix+2
	      if ( tmp .ge. 0. ) then
	         fkx2n = 2.*(1.-cos(fkx*dx))*odx2 
	         err = sqrt(tmp) - wv
		 err = err * dz
		 cerr = cmplx(1.,0.)
	         do i=1,n
		    ax = (1. - (trick*dx2+beta(i)*vw2)*fkx2n)*odz
		    bx = alpha(i)*vwh*fkx2n
		    ay = (1. - (trick*dy2+beta(i)*vw2)*fky2n)*odz
		    by = alpha(i)*vwh*fky2n
		    cerr = cerr*cmplx(ax,bx)/cmplx(ax,-bx)
		    cerr = cerr*cmplx(ay,by)/cmplx(ay,-by)
		 end do
		 cerr = cmplx(cos(err),sin(err))*cerr
		 cp(ix,iy) = cp(ix,iy)*cerr
		 if (iys.ne.0 .and. ixs.eq.0) then 
                   cp(ix,iys)=cp(ix,iys)*cerr
		 else if (iys.eq.0 .and. ixs.ne.0) then 
     	           cp(ixs,iy)=cp(ixs,iy)*cerr
		 else if (iys.ne.0 .and. ixs.ne.0) then 
     	           cp(ixs,iys)=cp(ixs,iys)*cerr
     	           cp(ix,iys)=cp(ix,iys)*cerr
     	           cp(ixs,iy)=cp(ixs,iy)*cerr
		 end if
	      else
	         cp(ix,iy) = cmplx(0.,0.) 
		 if (iys.ne.0 .and. ixs.eq.0) then 
                   cp(ix,iys)=cmplx(0.,0.)
		 else if (iys.eq.0 .and. ixs.ne.0) then 
                   cp(ixs,iy)=cmplx(0.,0.)
		 else if (iys.ne.0 .and. ixs.ne.0) then 
                   cp(ixs,iys)=cmplx(0.,0.)
                   cp(ix,iys)=cmplx(0.,0.)
                   cp(ixs,iy)=cmplx(0.,0.)
		 end if
	      end if
 2500      continue
 3000   continue
	do 5000 iy=1,ny
	   call four1(nx,cp(1,iy),-1.)
 5000   continue
	do 6000 ix=1,nx
	   if (ny .gt. 1 ) then
	      do 5500 iy=1,ny
 5500         wk(iy) = cp(ix,iy)
	      call four1(ny,wk,-1.)
	      do 5800 iy=1,ny
 5800         cp(ix,iy) = wk(iy) * scale
	   else
	      do 5900 iy=1,ny
 5900         cp(ix,iy) = cp(ix,iy) * scale
	   end if
 6000   continue
	return
	end
c compute alpha and beta used in migration
      subroutine alpbe(iorder,alpha,beta,n)
      real alpha(n),beta(n)
      integer iorder,n
      if ( iorder .eq. 0 ) then
cccc 45-degrees
         alpha(1) = 0.5
         beta(1) = 0.25
      else if ( iorder .eq. 1 ) then
cccc 65-degrees
         alpha(1) = 0.47824060
         beta(1) =  0.376369527
      else if ( iorder .eq. 2 ) then
cccc 80-degrees
         alpha(1) = 0.040315157
         alpha(2) = 0.457289566
         beta(1) =  0.873981642
         beta(2) =  0.222691983
      else if ( iorder .eq. 3 ) then
         NSPLIT = 3        
         alpha(1) = 0.004210420
         alpha(2) = 0.081312882
         alpha(3) = 0.414236605
         beta(1) =  0.972926132
         beta(2) =  0.744418059
         beta(3) =  0.150843924
      else if ( iorder .eq. 4 ) then
cccc 89-degrees
         alpha(1) = 0.000523275
         alpha(2) = 0.014853510
         alpha(3) = 0.117592008
         alpha(4) = 0.367013245
         beta(1) =  0.994065088
         beta(2) =  0.919432661
         beta(3) =  0.614520676
         beta(4) =  0.105756624
      else if ( iorder .eq. 5 ) then
cccc 90-degrees
         alpha(1) = 0.000153427
         alpha(2) = 0.004172967
         alpha(3) = 0.033860918
         alpha(4) = 0.143798076
         alpha(5) = 0.318013812
         beta(1) =  0.997370236
         beta(2) =  0.964827992
         beta(3) =  0.824918565
         beta(4) =  0.483340757
         beta(5) =  0.073588213
      end if
      return
      end
