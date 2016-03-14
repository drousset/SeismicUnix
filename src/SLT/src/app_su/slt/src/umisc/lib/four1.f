c      complex data(16),x(16),cdft
c      nn = 16
c      isign = -1
c      do i=1,nn
c     data(i) = cmplx(i,i)
c     x(i) = data(i)
c      end do
c      call four1(nn,data,isign)
c      dk = 2.*3.14159265/nn
c      do k=1,nn
c     cdft = (0.,0.)
c     do i=1,nn
c        cdft = cdft + x(i) * cexp(cmplx(0.,-(i-1)*(k-1)*dk))
c     end do
c     cdft = cdft * sc
c     write(*,*) 'four1=',data(k), ' dft=',cdft, k
c      end do
c      end
c
      subroutine four1(nn,data,sign)
      real*8 wr,wi,wpr,wpi,wtemp,theta
      dimension data(*)
      real sign
      n=2*nn
      j=1
c      sc = 1./sqrt(float(nn))
c      do i=1,n
c      data(i) = data(i) * sc
c      end do
      if (nn.eq.1) return
      do 11 i=1,n,2
      if(j.gt.i)then
        tempr=data(j)
        tempi=data(j+1)
        data(j)=data(i)
        data(j+1)=data(i+1)
        data(i)=tempr
        data(i+1)=tempi
      endif
      m=n/2
 1      if ((m.ge.2).and.(j.gt.m)) then
        j=j-m
        m=m/2
      go to 1
      endif
      j=j+m
 11   continue
      mmax=2
 2    if (n.gt.mmax) then
      istep=2*mmax
      theta=6.28318530717959d0/(sign*mmax)
      wpr=-2.d0*dsin(0.5d0*theta)**2
      wpi=dsin(theta)
      wr=1.d0
      wi=0.d0
      do 13 m=1,mmax,2
        do 12 i=m,n,istep
          j=i+mmax
          tempr=sngl(wr)*data(j)-sngl(wi)*data(j+1)
          tempi=sngl(wr)*data(j+1)+sngl(wi)*data(j)
          data(j)=data(i)-tempr
          data(j+1)=data(i+1)-tempi
          data(i)=data(i)+tempr
          data(i+1)=data(i+1)+tempi
 12       continue
        wtemp=wr
        wr=wr*wpr-wi*wpi+wr
        wi=wi*wpr+wtemp*wpi+wi
 13     continue
      mmax=istep
      go to 2
      endif
      return
      end
