ccccc dip scan routine

        subroutine sstack(data,pxo,pyo,stack,work,
     1                    iyy,ixx,jy,nycore,nx,
     2                    d1,o1,n1,
     3                    dt,t0,nt,
     4                    dto,to0,nto,
     4                    ipow,hy,hx,ht,ty,tx,tt)

        real data(nt,nx,nycore),pxo(n1),pyo(n1)
	real stack(nto),work(nt)
	real d1,o1,dt,t0,dto,to0
        integer iyy,ixx,jy,nycore,nx,nt,nto,n1,ipow,hy,hx,ht
	real ty(-hy:hy),tx(-hx:hx),tt(-ht:ht)

cccc dip stack 
	ky0 = iyy - jy - hy + 1
	kyn = iyy - jy + hy + 1 
	if(ky0.lt.1) ky0 = 1
	if(ky0.gt.nycore) ky0 = nycore
	if(kyn.lt.1) kyn = 1
	if(kyn.gt.nycore) kyn = nycore
	nyy = kyn - ky0 + 1

	kx0 = ixx - hy
	kxn = ixx + hy
	if(kx0.lt.1) kx0 = 1
	if(kx0.gt.nx) kx0 = nx
	if(kxn.lt.1) kxn = 1
	if(kxn.gt.nx) kxn = nx
	nxx = kxn - kx0 + 1

	scale = 1.
        if(nyy.gt.1.) scale = scale/nyy
        if(nxx.gt.1.) scale = scale/nxx
	

	od1 = 1./d1
	odt = 1./dt
	odto = 1./dto

	do it=1,nt
		work(it) = 0.
	end do

	iy0 = iyy-jy+1
	ix0 = ixx 

	call dump2xplot(data,nt,nx,1,"data")
	call dump2xplot(pxo,n1,1,1,"pxo")

	do it=1,nt
		t = t0+(it-1)*dt 
		tmp = (t-o1)*od1 + 1.5
		itmp = tmp
		if(itmp.lt.1) then
			i1 = 1
			i2 = 1
			w1 = 0.5
			w2 = 0.5
		else if(itmp.ge.n1) then
			i1 = n1
			i2 = n1
			w1 = 0.5
			w2 = 0.5
		else 
			i1 = itmp
			i2 = i1 + 1
			w2 = tmp - itmp
			w1 = 1. - w2
		end if
		py = pyo(i1)*w1 + pyo(i2)*w2
		px = pxo(i1)*w1 + pxo(i2)*w2

		call msgscf("px=",px)
		call msgscf("py=",py)

		do iy=ky0,kyn
			y = iy - iy0
			sy = ty(iy-iy0)
			do ix=kx0,kxn
				x = ix - ix0
				sxy = tx(ix-ix0)*sy
				tp = t + py*y + px*x
				tmp = (tp - t0)	* odt + 1.5
				itmp = tmp
				if(itmp.lt.1) then
					i1 = 1
					i2 = 1
					w1 = 0.5
					w2 = 0.5
				else if(itmp.ge.nt) then
					i1 = n1
					i2 = n1
					w1 = 0.5
					w2 = 0.5
				else 
					i1 = itmp
					i2 = i1 + 1
					w2 = tmp - itmp
					w1 = 1. - w2
				end if
				
				work(it) = work(it)+(w1*data(i1,ix,iy)	
     1                                     + w2*data(i2,ix,iy))*sxy
			end do
		end do
	end do
	call dump2xplot(work,nt,1,0,"work")
ccc compute power
	if (ipow.eq.1) then
		do it=1,nt
                       work(it) = abs(work(it))
                end do
        else if(ipow.eq.2) then
                do it=1,nt
			work(it) = work(it)*work(it)
                end do
        else
                tmp = ipow
                do it=1,nt
                      	work(it) = work(it)**tmp
                end do
        end if
ccccc sum over time window
	scale = scale/(2.*ht+1.)
        do it=1,nto
              t = to0 + (it-1)*dto
	      t = (t - t0)*odt
              itt = t		
              it1 = itt - ht
              itn = itt + ht

              if(it1.lt.1) it1 = 1
              if(itn.gt.nt) itn = nt
              n1 = itn - it1 + 1
	      if(n1.lt.1) n1 = 1
		call msgsci("it1=",it1)
		call msgsci("itn=",itn)

              tmp = 0.
              do i1=it1,itn
			tmp = tmp + work(i1)*tt(i1-itt)
	      end do		
              stack(it) = tmp*scale
	end do
	return
	end
