c tpow subroutine
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c	subroutine tp does tpow gain of trace input
c
c		out(t) = in(t) * t**tpow
c
c
c	input:
c		trace(nt)	---	real*4 		input trace 
c		nt		---	int*4		length of trace
c		tpow		---	real*4		power of t
c		tmin		---	real*4		minimum time
c		dt		---	real*4		sampling interval
c	output:
c		trace(nt)	---	real*4		tpow gained trace
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine tp(trace,nt,tpow,tmin,dt)
	parameter (ntmax=8192,tiny=0.0001)
	real trace(nt),tfct(ntmax),t,tpow,tmax,scale
	integer nt, first
	real tpsave
	data first/0/,tpsave/0./
ccc 
	if (abs(tpow).le.tiny) return 
	if (nt.gt.ntmax) then
		call msgsc("ntmax too small in tp program\0",1.0)
		stop
	end if

	if (first.eq.0 .or. tpow.ne.tpsave) then
		first = 1
		tpsave = tpow
		tmax = tmin + (nt-1)*dt
		scale = 1./(0.5*tmax)
		do it=1,nt
			t = (tmin + (it-1)*dt)*scale 
			if (t.eq.0) then
				tfct(it) = 0.
			else 
				tfct(it) = (abs(t))**tpow
			end if
		end do
	end if

	do it=1,nt
		trace(it) = trace(it) * tfct(it)
	end do

	return
	end
