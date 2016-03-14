      subroutine vrfftf (m,n,r,rt,mdimr,wsave)
c***begin prologue  vrfftf
c***date written   850801   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a1
c***keywords  fast fourier transform, real periodic transform,
c             fourier analysis, forward transform, multiple sequences
c***author  sweet, r.a. (nist) and lindgren, l.l. (nist)
c***purpose  forward real periodic transform, m sequences.
c***description
c
c  subroutine vrfftf computes the fourier coefficients (forward
c  transform) of a number of real periodic sequences.  specifically,
c  for each sequence the subroutine claculates the independent
c  fourier coefficients described below at output parameter r.
c
c  the array wsave which is used by subroutine vrfftf must be
c  initialized by calling subroutine vrffti(n,wsave).
c
c
c  input parameters
c
c  m       the number of sequences to be transformed.
c
c  n       the length of the sequences to be transformed.  the method
c          is most efficient when n is a product of small primes,
c          however n may be any positive integer.
c
c  r       a real two-dimensional array of size mdimx x n containing the
c          the sequences to be transformed.  the sequences are stored
c          in the rows of r.  thus, the i-th sequence to be transformed,
c          x(i,j), j=0,1,...,n-1, is stored as
c
c               r(i,j) = x(i,j-1) , j=1, 2, . . . , n.
c
c  rt      a real two-dimensional work array of size mdimx x n.
c
c  mdimr   the row (or first) dimension of the arrays r and rt exactly
c          as they appear in the calling program.  this parameter is
c          used to specify the variable dimension of these arrays.
c
c  wsave   a real one-dimensional work array which must be dimensioned
c          at least n+15.  the wsave array must be initialized by
c          calling subroutine vrffti.  a different wsave array must be
c          used for each different value of n.  this initialization does
c          not have to be repeated so long as n remains unchanged.  the
c          same wsave array may be used by vrfftf and vrfftb.
c
c  output parameters
c
c  r       contains the fourier coefficients f(k) for each of the m
c          input sequences.  specifically, row i of r, r(i,j),
c          j=1,2,..,n, contains the independent fourier coefficients
c          f(i,k), for the i-th input sequence stored as
c
c             r(i,1) = real( f(i,0) ),
c                    = sqrt(1/n)*sum(j=0,n-1)[ x(i,j) ],
c
c             r(i,2*k) = real( f(i,k) )
c                      = sqrt(1/n)*sum(j=0,n-1)[x(i,j)*cos(2j*k*pi/n)]
c
c             r(i,2*k+1) = imag( f(i,k) )
c                        =-sqrt(1/n)*sum(j=0,n-1)[x(i,j)*sin(2j*k*pi/n)]
c
c                   for k = 1, 2, . . . , m-1,
c
c              and, when n is even,
c
c              r(i,n) = real( f(i,n/2) ).
c                     = sqrt(1/n)*sum(j=0,n-1)[ (-1)**j*x(i,j) ].
c
c  wsave   contains results which must not be destroyed between calls
c          to vrfftf or vrfftb.
c
c  -----------------------------------------------------------------
c
c  note  -  a call of vrfftf followed immediately by a call of
c           of vrfftb will return the original sequences r.  thus,
c           vrfftb is the correctly normalized inverse of vrfftf.
c
c  -----------------------------------------------------------------
c
c  vrfftf is a straightforward extension of the subprogram rfftf to
c  handle m simultaneous sequences.  rfftf was originally developed
c  by p. n. swarztrauber of ncar.
c
c
c              * * * * * * * * * * * * * * * * * * * * *
c              *                                       *
c              *         program specifications        *
c              *                                       *
c              * * * * * * * * * * * * * * * * * * * * *
c
c
c     dimension of    r(mdimr,n), rt(mdimr,n), wsave(n+15)
c     arguments
c
c     latest          august 1, 1985
c     revision
c
c     subprograms     vrffti, vrfti1, vrfftf, vrftf1, vradf2, vradf3,
c     required        vradf4, vradf5, vradfg, vrfftb, vrftb1, vradb2,
c                     vradb3, vradb4, vradb5, vradbg, pimach
c
c     special         none
c     conditions
c
c     common          none
c     blocks
c
c     i/o             none
c
c     precision       single
c
c     specialist      roland sweet
c
c     language        fortran
c
c     history         written by linda lindgren and roland sweet at the
c                     national bureau of standards (boulder).
c
c     algorithm       a real variant of the stockham autosort version
c                     of the cooley-tukey fast fourier transform.
c
c     portability     american national standards institute fortran 77.
c                     the only machine dependent constant is located in
c                     the function pimach.
c
c     required        cos,sin
c     resident
c     routines
c
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  vrftf1
c***end prologue  vrfftf
c
c     vrfftpk, version 1, august 1985
c
      dimension       r(mdimr,n)  ,rt(mdimr,n)    ,wsave(n+15)
c***first executable statement  vrfftf
      if (n .eq. 1) return
      call vrftf1 (m,n,r,rt,mdimr,wsave(1),wsave(n+1))
      return
      end
      subroutine vrfftb(m,n,r,rt,mdimr,wsave)
c***begin prologue  vrfftb
c***date written   850801   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a1
c***keywords  fast fourier transform, real periodic transform,
c             fourier synthesis, backward transform, multiple sequences
c***author  sweet, r.a. (nist) and lindgren, l.l. (nist)
c***purpose  backward real periodic transform, m sequences.
c***description
c
c  subroutine vrfftb computes the synthesis (backward transform) of a
c  number of real periodic sequences from their fourier coefficients.
c  specifically, for each set of independent fourier coefficients
c  f(k), the corresponding real periodic sequence is computed.c
c  the array wsave which is used by subroutine vrfftb must be
c  initialized by calling subroutine vrffti(n,wsave).
c
c
c  input parameters
c
c  m       the number of sets of coefficients.
c
c  n       the length of the sequences of coefficients to be
c          transformed.  the method is most efficient when n is a
c          product of small primes, however n may be any positive
c          integer.
c
c  r       areal two-dimensional array of size mdimx x n containing the
c          coefficients to be transformed.  each set of coefficients
c          f(k), k\0,1,..,n-1, is stored as a row of r.  specifically,
c          the i-th set of independent fourier coefficients is stored
c
c                r(i,1) = real( f(i,0) ),
c
c                r(i,2*k) = real( f(i,k) )
c
c                r(i,2*k+1) = imag( f(i,k) )
c
c                   for k = 1, 2, . . . , m-1,
c
c                and, when n is even,
c
c                r(i,n) = real( f(i,n/2) ).
c
c  rt      a real two-dimensional work array of size mdimx x n.
c
c  mdimr   the row (or first) dimension of the arrays r and rt exactly
c          as they appear in the calling program.  this parameter is
c          used to specify the variable dimension of these arrays.
c
c  wsave   a real one-dimensional work array which must be dimensioned
c          at least n+15.  the wsave array must be initialized by
c          calling subroutine vrffti.  a different wsave array must be
c          used for each different value of n.  this initialization does
c          not have to be repeated so long as n remains unchanged.  the
c          same wsave array may be used by vrfftb and vrfftb.
c
c  output parameters
c
c  r       contains m real periodic sequences corresponding to the given
c          coefficients.  specifically, the i-th row of r contains the
c          real periodic sequence corresponding to the i-th set of
c          independent fourier coefficients f(i,k) stored as
c
c               r(i,j) = x(i,j-1) ,   j = 1, 2, . . . , n, where
c
c               x(i,j) = sqrt(1/n)* f(i,0) + (-1)**j*f(i,n/2)
c                        + 2*sum(k=1,m)[ real(f(i,2k))*cos(2k*j*pi/n)
c                        - imag(f(i,2k+1))*sin(2k*j*pi/n) ]  ,
c
c                 when n is even, and
c
c               x(i,j) = sqrt(1/n)* f(i,0) +
c                        2*sum(k=1,m)[ real(f(i,2k))*cos(2k*j*pi/n)
c                        - imag(f(i,2k+1))*sin(2k*j*pi/n) ]  ,
c
c                 when n is odd.
c
c  wsave   contains results which must not be destroyed between calls
c          to vrfftf or vrfftb.
c
c  -----------------------------------------------------------------
c
c  note  -  a call of vrfftf followed immediately by a call of
c           of vrfftb will return the original sequences r.  thus,
c           vrfftb is the correctly normalized inverse of vrfftf.
c
c  -----------------------------------------------------------------
c
c  vrfftb is a straightforward extension of the subprogram rfftb to
c  handle m simultaneous sequences.  rfftb was originally developed
c  by p. n. swarztrauber of ncar.
c
c
c              * * * * * * * * * * * * * * * * * * * * *
c              *                                       *
c              *         program specifications        *
c              *                                       *
c              * * * * * * * * * * * * * * * * * * * * *
c
c
c     dimension of    r(mdimr,n), rt(mdimr,n), wsave(n+15)
c     arguments
c
c     latest          august 1, 1985
c     revision
c
c     subprograms     vrffti, vrfti1, vrfftf, vrftf1, vradf2, vradf3,
c     required        vradf4, vradf5, vradfg, vrfftb, vrftb1, vradb2,
c                     vradb3, vradb4, vradb5, vradbg, pimach
c
c     special         none
c     conditions
c
c     common          none
c     blocks
c
c     i/o             none
c
c     precision       single
c
c     specialist      roland sweet
c
c     language        fortran
c
c     history         written by linda lindgren and roland sweet at the
c                     national bureau of standards (boulder).
c
c     algorithm       a real variant of the stockham autosort version
c                     of the cooley-tukey fast fourier transform.
c
c     portability     american national standards institute fortran 77.
c                     the only machine dependent constant is located in
c                     the function pimach.
c
c     required        cos,sin
c     resident
c     routines
c
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  vrftb1
c***end prologue  vrfftb
c
c     vrfftpk, version 1, august 1985
c
      dimension     r(mdimr,n),rt(mdimr,n),wsave(n+15)
      if (n .eq. 1) return
      call vrftb1 (m,n,r,rt,mdimr,wsave(1),wsave(n+1))
      return
      end
      subroutine vrffti (n,wsave)
c***begin prologue  vrffti
c***date written   860701   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a1
c***keywords  fast fourier transform, real periodic transform,
c             multiple sequences
c***author  sweet, r.a. (nist) and lindgren, l.l. (nist)
c***purpose  initialization for vrfftf and vrfftb.
c***description
c
c  subroutine vrffti initializes the array wsave which is used in
c  both vrfftf and vrfftb.  the prime factorization of n together with
c  a tabulation of certain trigonometric functions are computed and
c  stored in the array wsave.
c
c  input parameter
c
c  n       the length of the sequence to be transformed.  there is no
c          restriction on n.
c
c  output parameter
c
c  wsave   a work array which must be dimensioned at least n+15.
c          the same work array can be used for both vrfftf and vrfftb
c          as long as n remains unchanged.  different wsave arrays
c          are required for different values of n.  the contents of
c          wsave must not be changed between calls of vrfftf or vrfftb.
c
c
c              * * * * * * * * * * * * * * * * * * * * *
c              *                                       *
c              *         program specifications        *
c              *                                       *
c              * * * * * * * * * * * * * * * * * * * * *
c
c
c     dimension of    r(mdimr,n), rt(mdimr,n), wsave(n+15)
c     arguments
c
c     latest          august 1, 1985
c     revision
c
c     subprograms     vrffti, vrfti1, vrfftf, vrftf1, vradf2, vradf3,
c     required        vradf4, vradf5, vradfg, vrfftb, vrftb1, vradb2,
c                     vradb3, vradb4, vradb5, vradbg, pimach
c
c     special         none
c     conditions
c
c     common          none
c     blocks
c
c     i/o             none
c
c     precision       single
c
c     specialist      roland sweet
c
c     language        fortran
c
c     history         written by linda lindgren and roland sweet at the
c                     national bureau of standards (boulder).
c
c     algorithm       a real variant of the stockham autosort version
c                     of the cooley-tukey fast fourier transform.
c
c     portability     american national standards institute fortran 77.
c                     the only machine dependent constant is located in
c                     the function pimach.
c
c     required        cos,sin
c     resident
c     routines
c
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  vrfti1
c***end prologue  vrffti
c
c     vrfftpk, version 1, august 1985
c
      dimension       wsave(n+15)
c***first executable statement  vrffti
      if (n .eq. 1) return
      call vrfti1 (n,wsave(1),wsave(n+1))
      return
      end
      subroutine vcost(m,n,x,xt,mdimx,wsave)
c***begin prologue  vcost
c***date written   860701   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a3
c***keywords  fast fourier transform, cosine transform, multiple
c             sequences
c***author  boisvert, r. f. (nist)
c***purpose  cosine transform of one or more real, even sequences.
c***description
c
c  subroutine vcost computes the discrete fourier cosine transform
c  of m even sequences x(j,i), j=1,...,m.  the transform is defined
c  below at output parameter x.
c
c  the array wsave which is used by subroutine vcost must be
c  initialized by calling subroutine vcosti(n,wsave).
c
c  input parameters
c
c  m       the number of sequences to be transformed.
c
c  n       the length of the sequence to be transformed.  n must be
c          greater than 1.  the method is most efficient when n-1 is
c          is a product of small primes.
c
c  x       an array of size at least x(mdimx,n) which contains the
c          the sequences to be transformed.  the sequences are stored
c          in the rows of x.  thus, the jth sequence is stored in
c          x(j,i), i=1,..,n.
c
c  xt      a work array of size at least xt(mdimx,n-1).
c
c  mdimx   the first dimension of the array x exactly as it appears in
c          the calling program.
c
c  wsave   a work array which must be dimensioned at least 3*n+15
c          in the program that calls vcost.  the wsave array must be
c          initialized by calling subroutine vcosti(n,wsave), and a
c          different wsave array must be used for each different
c          value of n.  this initialization does not have to be
c          repeated so long as n remains unchanged.  thus subsequent
c          transforms can be obtained faster than the first.
c
c  output parameters
c
c  x       for i=1,...,n and j=1,...,m
c
c             x(j,i) = ( x(j,1)+(-1)**(i-1)*x(j,n)
c
c               + the sum from k=2 to k=n-1
c
c                 2*x(j,k)*cos((k-1)*(i-1)*pi/(n-1)) )/sqrt(2*(n-1))
c
c  wsave   contains initialization calculations which must not be
c          destroyed between calls of vcost.
c
c  -----------------------------------------------------------------
c
c  note  -  a call of vcost followed immediately by another call
c           of vcost will return the original sequences x.  thus,
c           vcost is the correctly normalized inverse of itself.
c
c  -----------------------------------------------------------------
c
c  vcost is a straightforward extension of the subprogram cost to
c  handle m simultaneous sequences.  the scaling of the sequences
c  computed by vcost is different than that of cost.  cost was
c  originally developed by p. n. swarztrauber of ncar.
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  vrfftf
c***end prologue  vcost
      dimension       x(mdimx,*), xt(mdimx,*), wsave(*)
c***first executable statement  vcost
      if (m .le. 0)  go to 900
      if (n .le. 1)  go to 900
      if (n .gt. 3)  go to 400
      if (n .eq. 3)  go to 300
c
c  case  n = 2
c
      scale = sqrt(0.50e0)
      do 210 j=1,m
         x1h = scale*(x(j,1)+x(j,2))
         x(j,2) = scale*(x(j,1)-x(j,2))
         x(j,1) = x1h
  210 continue
      go to 900
c
c  case  n = 3
c
  300 continue
      scale = 0.50e0
      do 310 j=1,m
         x1p3 = x(j,1)+x(j,3)
         tx2 = x(j,2)+x(j,2)
         x(j,2) = scale*(x(j,1)-x(j,3))
         x(j,1) = scale*(x1p3+tx2)
         x(j,3) = scale*(x1p3-tx2)
  310 continue
      go to 900
c
c  case  n .gt. 3
c
c     ... preprocessing
c
  400 continue
      nm1 = n-1
      np1 = n+1
      ns2 = n/2
      do 410 j=1,m
         xt(j,1) = x(j,1)-x(j,n)
         x(j,1) = x(j,1)+x(j,n)
  410 continue
      do 420 k=2,ns2
         kc = np1-k
         do 420 j=1,m
            t1 = x(j,k)+x(j,kc)
            t2 = x(j,k)-x(j,kc)
            xt(j,1) = xt(j,1)+wsave(kc)*t2
            t2 = wsave(k)*t2
            x(j,k) = t1-t2
            x(j,kc) = t1+t2
  420 continue
      modn = mod(n,2)
      if (modn .ne. 0) then
         do 430 j=1,m
            x(j,ns2+1) = x(j,ns2+1)+x(j,ns2+1)
  430    continue
      endif
      do 435 j=1,m
         x(j,n) = xt(j,1)
  435 continue
c
c     ... real periodic transform
c
      call vrfftf (m,nm1,x,xt,mdimx,wsave(np1))
c
c     ... postprocessing
c
      factor = 1.0/sqrt(real(nm1))
      do 440 j=1,m
         xt(j,1) = x(j,2)
         x(j,2) = factor*x(j,n)
  440 continue
      do 450 i=4,n,2
         do 450 j=1,m
            xi = x(j,i)
            x(j,i) = x(j,i-2)-x(j,i-1)
            x(j,i-1) = xt(j,1)
            xt(j,1) = xi
  450 continue
      if (modn .ne. 0) then
         do 460 j=1,m
            x(j,n) = xt(j,1)
  460    continue
      endif
c
c     ... normalization
c
      scale = sqrt(0.5)
      do 490 i=1,n
         do 490 j=1,m
            x(j,i) = scale*x(j,i)
  490 continue
c
c  exit
c
  900 continue
      return
      end
      subroutine vcosti(n,wsave)
c***begin prologue  vcosti
c***date written   860701   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a3
c***keywords  fast fourier transform, cosine transform, multiple
c             sequences
c***author  boisvert, r. f. (nist)
c***purpose  initialize for vcost.
c***description
c
c  subroutine vcosti initializes the array wsave which is used in
c  subroutine vcost.  the prime factorization of n together with
c  a tabulation of the trigonometric functions are computed and
c  stored in wsave.
c
c  input parameter
c
c  n       the length of the sequence to be transformed.  the method
c          is most efficient when n-1 is a product of small primes.
c
c  output parameter
c
c  wsave   a work array which must be dimensioned at least 3*n+15.
c          different wsave arrays are required for different values
c          of n.  the contents of wsave must not be changed between
c          calls of vcost.
c
c  -----------------------------------------------------------------
c
c  vcosti is a straightforward extension of the subprogram costi to
c  handle m simultaneous sequences.  costi was originally developed
c  by p. n. swarztrauber of ncar.
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  vrffti
c***end prologue  vcosti
      dimension       wsave(*)
c***first executable statement  vcosti
      pi = pimach(1.0)
      if (n .le. 3) return
      nm1 = n-1
      np1 = n+1
      ns2 = n/2
      dt = pi/real(nm1)
      fk = 0.
      do 101 k=2,ns2
         fk = fk+1.
         wsave(k) = 2.*sin(fk*dt)
  101 continue
      fk = 0.
      do 102 k=2,ns2
         kc = np1-k
         fk = fk+1.
         wsave(kc) = 2.*cos(fk*dt)
  102 continue
      call vrffti (nm1,wsave(n+1))
      return
      end
      subroutine vsint(m,n,x,xt,mdimx,wsave)
c***begin prologue  vsint
c***date written   860701   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a3
c***keywords  fast fourier transform, sine transform, multiple
c             sequences
c***author  boisvert, r. f., (nist)
c***purpose  sine transform of one or more real, odd sequences.
c***description
c
c  subroutine vsint computes the discrete fourier sine transform
c  of m odd sequences x(j,i), j=1,...,m.  the transform is defined
c  below at output parameter x.
c
c  the array wsave which is used by subroutine vsint must be
c  initialized by calling subroutine vsinti(n,wsave).
c
c  input parameters
c
c  m       the number of sequences to be transformed.
c
c  n       the length of the sequence to be transformed.  the method
c          is most efficient when n+1 is the product of small primes.
c
c  x       an array of size at least x(mdimx,n+1) which contains the
c          the sequences to be transformed.  the sequences are stored
c          in the rows of x.  thus, the jth sequence is stored in
c          x(j,i), i=1,..,n.  the extra column of x is used as work
c          storage.
c
c  xt      a work array of size at least xt(mdimx,n+1).
c
c  mdimx   the first dimension of the array x exactly as it appears in
c          the calling program.
c
c  wsave   a work array with dimension at least int(2.5*n+15)
c          in the program that calls vsint.  the wsave array must be
c          initialized by calling subroutine vsinti(n,wsave), and a
c          different wsave array must be used for each different
c          value of n.  this initialization does not have to be
c          repeated so long as n remains unchanged.
c
c  output parameters
c
c  x       for i=1,...,n and j=1,...,m
c
c               x(j,i)= the sum from k=1 to k=n
c
c                    2*x(j,k)*sin(k*i*pi/(n+1))/sqrt(2*(n+1))
c
c  wsave   contains initialization calculations which must not be
c          destroyed between calls of vsint.
c
c  -----------------------------------------------------------------
c
c  note  -  a call of vsint followed immediately by another call
c           of vsint will return the original sequences x.  thus,
c           vsint is the correctly normalized inverse of itself.
c
c  -----------------------------------------------------------------
c
c  vsint is a straightforward extension of the subprogram sint to
c  handle m simultaneous sequences.  the scaling of the sequences
c  computed by vsint is different than that of sint.  sint was
c  originally developed by p. n. swarztrauber of ncar.
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  vrfftf
c***end prologue  vsint
      dimension       x(mdimx,*), xt(mdimx,*), wsave(*)
c***first executable statement  sint
      if (m .le. 0)  go to 900
      if (n .le. 1)  go to 900
      if (n .gt. 2)  go to 300
c
c  case   n = 2
c
      sqrth = sqrt(0.50e0)
      do 201 j=1,m
         xh = sqrth*(x(j,1)+x(j,2))
         x(j,2) = sqrth*(x(j,1)-x(j,2))
         x(j,1) = xh
  201 continue
      go to 900
c
c  case   n .gt. 2
c
c     ... preprocessing
c
  300 continue
      np1 = n+1
      ns2 = n/2
      do 301 j=1,m
         xt(j,1) = 0.0
  301 continue
      do 310 k=1,ns2
         kc = np1-k
         do 310 j=1,m
            t1 = x(j,k)-x(j,kc)
            t2 = wsave(k)*(x(j,k)+x(j,kc))
            xt(j,k+1) = t1+t2
            xt(j,kc+1) = t2-t1
  310 continue
      modn = mod(n,2)
      if (modn .ne. 0) then
         do 320 j=1,m
            xt(j,ns2+2) = 4.0*x(j,ns2+1)
  320    continue
      endif
c
c     ... real periodic transform
c
      nf = ns2+1
      call vrfftf(m,np1,xt,x,mdimx,wsave(nf))
c
c     ... postprocessing
c
      do 330 j=1,m
         x(j,1) = 0.5*xt(j,1)
  330 continue
      do 350 i=3,n,2
         do 340 j=1,m
            x(j,i-1) = -xt(j,i)
  340    continue
         do 345 j=1,m
            x(j,i) = x(j,i-2)+xt(j,i-1)
  345    continue
  350 continue
      if (modn .eq. 0) then
         do 360 j=1,m
            x(j,n) = -xt(j,n+1)
  360    continue
      endif
c
c     ... normalization
c
      scale = sqrt(0.5)
      do 370 i=1,n
         do 370 j=1,m
            x(j,i) = scale*x(j,i)
  370 continue
c
c  exit
c
  900 continue
      return
      end
      subroutine vsinti(n,wsave)
c***begin prologue  vsinti
c***date written   860701   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a3
c***keywords  fast fourier transform, sine transform, multiple
c             sequences
c***author  boisvert, r. f. (nist)
c***purpose  initialize for vsint.
c***description
c
c  subroutine vsinti initializes the array wsave which is used in
c  subroutine sint.  the prime factorization of n together with
c  a tabulation of the trigonometric functions are computed and
c  stored in wsave.
c
c  input parameter
c
c  n       the length of the sequence to be transformed.  the method
c          is most efficient when n+1 is a product of small primes.
c
c  output parameter
c
c  wsave   a work array with at least int(2.5*n+15) locations.
c          different wsave arrays are required for different values
c          of n.  the contents of wsave must not be changed between
c          calls of vsint.
c
c  -----------------------------------------------------------------
c
c  vsinti is a straightforward extension of the subprogram sinti to
c  handle m simultaneous sequences.  sinti was originally developed
c  p. n. swarztrauber of ncar.
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  vrffti
c***end prologue  vsinti
      dimension       wsave(*)
c***first executable statement  sinti
      pi = pimach(1.0)
      if (n .le. 1) return
      np1 = n+1
      ns2 = n/2
      dt = pi/real(np1)
      ks = 1
      kf = ks+ns2-1
      fk = 0.
      do 101 k=ks,kf
         fk = fk+1.
         wsave(k) = 2.*sin(fk*dt)
  101 continue
      call vrffti (np1,wsave(kf+1))
      return
      end
      subroutine vcosqf(m,n,x,xt,mdimx,wsave)
c***begin prologue  vcosqf
c***date written   860701   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a3
c***keywords  fast fourier transform, cosine transform, odd wave
c             numbers, multiple sequences
c***author  boisvert, r. f. (nist)
c***purpose  forward cosine transform, odd wave numbers, m sequences.
c***description
c
c  subroutine vcosqf computes the forward fast fourier cosine transform
c  of m quarter wave sequences.  that is, cosine series representations
c  with only odd wave numbers.  the transform is defined below at output
c  parameter x.
c
c  the array wsave which is used by subroutine vcosqf must be
c  initialized by calling subroutine vcosqi(n,wsave).
c
c
c  input parameters
c
c  m       the number of sequences to be transformed.
c
c  n       the length of the sequences to be transformed.  the method
c          is most efficient when n is a product of small primes.
c
c  x       an array of size at least x(mdimx,n) which contains the
c          the sequences to be transformed.  the sequences are stored
c          in the rows of x.  thus, the jth sequence is stored in
c          x(j,i), i=1,..,n.
c
c  xt      a work array of size at least xt(mdimx,n).
c
c  mdimx   the first dimension of the array x exactly as it appears in
c          the calling program.
c
c  wsave   a work array which must be dimensioned at least 2*n+15
c          in the program that calls vcosqf.  the wsave array must be
c          initialized by calling subroutine vcosqi(n,wsave), and a
c          different wsave array must be used for each different
c          value of n.  this initialization does not have to be
c          repeated so long as n remains unchanged.
c
c  output parameters
c
c  x       for i=1,...,n and j=1,...,m
c
c               x(i) = ( x(1) + the sum from k=2 to k=n of
c
c                  2*x(k)*cos((2*i-1)*(k-1)*pi/(2*n)) )/sqrt(4*n)
c
c  wsave   contains initialization calculations which must not
c          be destroyed between calls of vcosqf or vcosqb.
c
c  -----------------------------------------------------------------
c
c  note  -  a call of vcosqf followed immediately by a call of
c           of vcosqb will return the original sequences x.  thus,
c           vcosqb is the correctly normalized inverse vcosqf.
c
c  -----------------------------------------------------------------
c
c  vcosqf is a straightforward extension of the subprogram cosqf to
c  handle m simultaneous sequences.  cosqf was originally developed
c  by p. n. swarztrauber of ncar.
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  (none)
c***end prologue  vcosqf
      dimension       x(mdimx,*), xt(mdimx,*), wsave(*)
c***first executable statement  vcosqf
      if (m .le. 0)  go to 900
      if (n .gt. 2)  go to 300
      if (n .lt. 2)  go to 900
c
c  case  n = 2
c
      sqrt2 = sqrt(2.0)
      scale = 0.50e0/sqrt2
      do 210 j=1,m
         tsqx = sqrt2*x(j,2)
         x(j,2) = scale*(x(j,1)-tsqx)
         x(j,1) = scale*(x(j,1)+tsqx)
  210 continue
      go to 900
c
c  case n .gt. 2
c
  300 continue
c
c     ... preprocessing
c
      ns2 = (n+1)/2
      np2 = n+2
      do 310 k=2,ns2
         kc = np2-k
         do 310 j=1,m
            xt(j,k) = x(j,k)+x(j,kc)
            xt(j,kc) = x(j,k)-x(j,kc)
  310 continue
      modn = mod(n,2)
      if (modn .eq. 0) then
         do 320 j=1,m
            xt(j,ns2+1) = x(j,ns2+1)+x(j,ns2+1)
  320    continue
      endif
      do 330 k=2,ns2
         kc = np2-k
         do 330 j=1,m
            x(j,k) = wsave(k-1)*xt(j,kc)+wsave(kc-1)*xt(j,k)
            x(j,kc) = wsave(k-1)*xt(j,k)-wsave(kc-1)*xt(j,kc)
  330 continue
      if (modn .eq. 0) then
         do 340 j=1,m
            x(j,ns2+1) = wsave(ns2)*xt(j,ns2+1)
  340    continue
      endif
c
c     ... real, periodic transform
c
      call vrfftf (m,n,x,xt,mdimx,wsave(n+1))
c
c     ... postprocessing
c
      do 350 i=3,n,2
         do 350 j=1,m
            xim1 = x(j,i-1)-x(j,i)
            x(j,i) = x(j,i-1)+x(j,i)
            x(j,i-1) = xim1
  350 continue
c
c     ... normalization
c
      scale = 0.5
      do 360 i=1,n
         do 360 j=1,m
            x(j,i) = scale*x(j,i)
  360 continue
c
c  exit
c
  900 continue
      return
      end
      subroutine vcosqb(m,n,x,xt,mdimx,wsave)
c***begin prologue  vcosqb
c***date written   860701   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a3
c***keywords  fast fourier transform, cosine transform, odd wave
c             numbers, multiple sequences
c***author  boisvert, r. f. (nist)
c***purpose  normalized inverse of vcosqf.
c***description
c
c  subroutine vcosqb computes the backward fast fourier cosine transform
c  of m quarter wave sequences.  that is, cosine series representations
c  with only odd wave numbers.  the transform is defined below at output
c  parameter x.
c
c  the array wsave which is used by subroutine vcosqb must be
c  initialized by calling subroutine vcosqi(n,wsave).
c
c
c  input parameters
c
c  m       the number of sequences to be transformed.
c
c  n       the length of the sequences to be transformed.  the method
c          is most efficient when n is a product of small primes.
c
c  x       an array of size at least x(mdimx,n) which contains the
c          the sequences to be transformed.  the sequences are stored
c          in the rows of x.  thus, the jth sequence is stored in
c          x(j,i), i=1,..,n.
c
c  xt      a work array of size at least xt(mdimx,n).
c
c  mdimx   the first dimension of the array x exactly as it appears in
c          the calling program.
c
c  wsave   a work array which must be dimensioned at least 2*n+15
c          in the program that calls vcosqb.  the wsave array must be
c          initialized by calling subroutine vcosqi(n,wsave), and a
c          different wsave array must be used for each different
c          value of n.  this initialization does not have to be
c          repeated so long as n remains unchanged.
c
c  output parameters
c
c  x       for i=1,...,n and j=1,...,m
c
c               x(i)= the sum from k=1 to k=n of
c
c                 4*x(k)*cos((2*k-1)*(i-1)*pi/(2*n)) /sqrt(4*n)
c
c  wsave   contains initialization calculations which must not
c          be destroyed between calls of vcosqf or vcosqb.
c
c  -----------------------------------------------------------------
c
c  note  -  a call of vcosqf followed immediately by a call of
c           of vcosqb will return the original sequences x.  thus,
c           vcosqb is the correctly normalized inverse vcosqf.
c
c  -----------------------------------------------------------------
c
c  vcosqb is a straightforward extension of the subprogram cosqb to
c  handle m simultaneous sequences.  cosqb was originally developed
c  by p. n. swarztrauber of ncar.
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  (none)
c***end prologue  vcosqb
      dimension       x(mdimx,*), xt(mdimx,*), wsave(*)
c***first executable statement  vcosqb
      if (m .le. 0)  go to 900
      if (n .gt. 2)  go to 300
      if (n .eq. 2)  go to 200
      go to 900
c
c  case  n = 2
c
  200 continue
      scale = 2.0e0*sqrt(0.50e0)
      do 210 j=1,m
         x1 = scale*(x(j,1)+x(j,2))
         x(j,2) = x(j,1)-x(j,2)
         x(j,1) = x1
  210 continue
      go to 900
c
c  case n .gt. 2
c
c     ... preprocessing
c
  300 continue
      ns2 = (n+1)/2
      np2 = n+2
      do 310 i=3,n,2
         do 310 j=1,m
            xim1 = x(j,i-1)+x(j,i)
            x(j,i) = x(j,i)-x(j,i-1)
            x(j,i-1) = xim1
  310 continue
      do 320 j=1,m
         x(j,1) = x(j,1)+x(j,1)
  320 continue
      modn = mod(n,2)
      if (modn .eq. 0) then
         do 330 j=1,m
            x(j,n) = x(j,n)+x(j,n)
  330    continue
      endif
c
c     ... real, periodic transform
c
      call vrfftb (m,n,x,xt,mdimx,wsave(n+1))
c
c     ... postprocessing
c
      do 340 k=2,ns2
         kc = np2-k
         do 340 j=1,m
            xt(j,k) = wsave(k-1)*x(j,kc)+wsave(kc-1)*x(j,k)
            xt(j,kc) = wsave(k-1)*x(j,k)-wsave(kc-1)*x(j,kc)
  340 continue
      if (modn .eq. 0) then
         do 350 j=1,m
            x(j,ns2+1) = wsave(ns2)*(x(j,ns2+1)+x(j,ns2+1))
  350    continue
      endif
      do 360 k=2,ns2
         kc = np2-k
         do 360 j=1,m
            x(j,k) = xt(j,k)+xt(j,kc)
            x(j,kc) = xt(j,k)-xt(j,kc)
  360 continue
      do 370 j=1,m
         x(j,1) = x(j,1)+x(j,1)
  370 continue
c
c     ... normalization
c
      scale = 0.5
      do 380 i=1,n
         do 380 j=1,m
            x(j,i) = scale*x(j,i)
  380 continue
c
c  exit
c
  900 continue
      return
      end
      subroutine vcosqi(n,wsave)
c***begin prologue  vcosqi
c***date written   860701   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a3
c***keywords  fast fourier transform, cosine transform, odd wave
c             numbers, multiple sequences
c***author  boisvert, r. f. (nist)
c***purpose  initialize for vcosqf and vcosqb.
c***description
c
c  subroutine vcosqi initializes the array wsave which is used in
c  both vcosqf and vcosqb.  the prime factorization of n together with
c  a tabulation of the trigonometric functions are computed and
c  stored in wsave.
c
c  input parameter
c
c  n       the length of the array to be transformed.  the method
c          is most efficient when n is a product of small primes.
c
c  output parameter
c
c  wsave   a work array which must be dimensioned at least 2*n+15.
c          the same work array can be used for both vcosqf and vcosqb
c          as long as n remains unchanged.  different wsave arrays
c          are required for different values of n.  the contents of
c          wsave must not be changed between calls of vcosqf or vcosqb.
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  vrffti
c***end prologue  vcosqi
      dimension       wsave(*)
c***first executable statement  vcosqi
      pih = 0.5*pimach(1.0)
      dt = pih/real(n)
      fk = 0.
      do 101 k=1,n
         fk = fk+1.
         wsave(k) = cos(fk*dt)
  101 continue
      call vrffti (n,wsave(n+1))
      return
      end
      subroutine vsinqf(m,n,x,xt,mdimx,wsave)
c***begin prologue  vsinqf
c***date written   860701   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a3
c***keywords  fast fourier transform, sine transform, odd wave
c             numbers, multiple sequences
c***author  boisvert, r. f. (nist)
c***purpose  forward sine transform, odd wave numbers, m sequences.
c***description
c
c  subroutine vsinqf computes the forward fast fourier sine transform
c  of m quarter wave sequences.  that is, sine series representations
c  with only odd wave numbers.  the transform is defined below at output
c  parameter x.
c
c  the array wsave which is used by subroutine vsinqf must be
c  initialized by calling subroutine vsinqi(n,wsave).
c
c
c  input parameters
c
c  m       the number of sequences to be transformed.
c
c  n       the length of the sequences to be transformed.  the method
c          is most efficient when n is a product of small primes.
c
c  x       an array of size at least x(mdimx,n) which contains the
c          the sequences to be transformed.  the sequences are stored
c          in the rows of x.  thus, the jth sequence is stored in
c          x(j,i), i=1,..,n.
c
c  xt      a work array of size at least xt(mdimx,n).
c
c  mdimx   the first dimension of the array x exactly as it appears in
c          the calling program.
c
c  wsave   a work array which must be dimensioned at least 2*n+15
c          in the program that calls vsinqf.  the wsave array must be
c          initialized by calling subroutine vsinqi(n,wsave), and a
c          different wsave array must be used for each different
c          value of n.  this initialization does not have to be
c          repeated so long as n remains unchanged.
c
c  output parameters
c
c  x       for i=1,...,n and j=1,...,m
c
c               x(i) = ( (-1)**(i-1)*x(n)
c
c                        + the sum from k=1 to k=n-1 of
c
c                        2*x(k)*sin((2*i-1)*k*pi/(2*n)) )/sqrt(4*n)
c
c  wsave   contains initialization calculations which must not
c          be destroyed between calls of vsinqf or vsinqb.
c
c  -----------------------------------------------------------------
c
c  note  -  a call of vsinqf followed immediately by a call of
c           of vsinqb will return the original sequences x.  thus,
c           vsinqb is the correctly normalized inverse vsinqf.
c
c  -----------------------------------------------------------------
c
c  vsinqf is a straightforward extension of the subprogram sinqf to
c  handle m simultaneous sequences.  sinqf was originally developed
c  by p. n. swarztrauber of ncar.
c
c***routines called  vcosqf
c***end prologue  vsinqf
      dimension       x(mdimx,*), xt(mdimx,*), wsave(*)
c***first executable statement  vsinqf
      if (m .le. 0)  go to 900
      if (n .le. 1)  go to 900
c
c     ... preprocessing
c
      ns2 = n/2
      do 110 k=1,ns2
         kc = n-k
         do 110 j=1,m
            xhold = x(j,k)
            x(j,k) = x(j,kc+1)
            x(j,kc+1) = xhold
  110 continue
c
c     ... cosine quarter wave transform
c
      call vcosqf (m,n,x,xt,mdimx,wsave)
c
c     ... postprocessing
c
      do 120 k=2,n,2
         do 120 j=1,m
            x(j,k) = -x(j,k)
  120 continue
c
c  exit
c
  900 continue
      return
      end
      subroutine vsinqb(m,n,x,xt,mdimx,wsave)
c***begin prologue  vsinqb
c***date written   860701   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a3
c***keywords  fast fourier transform, sine transform, odd wave
c             numbers, multiple sequences
c***author  boisvert, r. f. (nist)
c***purpose  normalized inverse of vsinqf.
c***description
c
c  subroutine vsinqb computes the backward fast fourier sine transform
c  of m quarter wave sequences.  that is, sine series representations
c  with only odd wave numbers.  the transform is defined below at output
c  parameter x.
c
c  the array wsave which is used by subroutine vsinqb must be
c  initialized by calling subroutine vsinqi(n,wsave).
c
c
c  input parameters
c
c  m       the number of sequences to be transformed.
c
c  n       the length of the sequences to be transformed.  the method
c          is most efficient when n is a product of small primes.
c
c  x       an array of size at least x(mdimx,n) which contains the
c          the sequences to be transformed.  the sequences are stored
c          in the rows of x.  thus, the jth sequence is stored in
c          x(j,i), i=1,..,n.
c
c  xt      a work array of size at least xt(mdimx,n).
c
c  mdimx   the first dimension of the array x exactly as it appears in
c          the calling program.
c
c  wsave   a work array which must be dimensioned at least 2*n+15
c          in the program that calls vsinqb.  the wsave array must be
c          initialized by calling subroutine vsinqi(n,wsave), and a
c          different wsave array must be used for each different
c          value of n.  this initialization does not have to be
c          repeated so long as n remains unchanged.
c
c  output parameters
c
c  x       for i=1,...,n and j=1,...,m
c
c               x(i)= the sum from k=1 to k=n of
c
c                 4*x(k)*sin((2k-1)*i*pi/(2*n)) /sqrt(4*n)
c
c
c  wsave   contains initialization calculations which must not
c          be destroyed between calls of vsinqb or vsinqf.
c
c  -----------------------------------------------------------------
c
c  note  -  a call of vsinqf followed immediately by a call of
c           of vsinqb will return the original sequences x.  thus,
c           vsinqb is the correctly normalized inverse vsinqf.
c
c  -----------------------------------------------------------------
c
c  vsinqb is a straightforward extension of the subprogram sinqb to
c  handle m simultaneous sequences.  sinqb was originally developed
c  by p. n. swarztrauber of ncar.
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  vcosqb
c***end prologue  vsinqb
      dimension       x(mdimx,*), xt(mdimx,*), wsave(*)
c***first executable statement  vsinqb
      if (m .le. 0)  go to 900
      if (n .le. 1)  go to 900
c
c  case  n .gt. 1
c
c     ... preprocessing
c
      ns2 = n/2
      do 210 k=2,n,2
         do 210 j=1,m
            x(j,k) = -x(j,k)
  210 continue
c
c     ... cosine quarter wave transform
c
      call vcosqb (m,n,x,xt,mdimx,wsave)
c
c     ... postprocessing
c
      do 220 k=1,ns2
         kc = n-k
         do 220 j=1,m
            xhold = x(j,k)
            x(j,k) = x(j,kc+1)
            x(j,kc+1) = xhold
  220 continue
c
c  exit
c
  900 continue
      return
      end
      subroutine vsinqi(n,wsave)
c***begin prologue  vsinqi
c***date written   860701   (yymmdd)
c***revision date  900509   (yymmdd)
c***category no.  j1a3
c***keywords  fast fourier transform, sine transform, odd wave
c             numbers, multiple sequences
c***author  boisvert, r. f. (nist)
c***purpose  initialize for vsinqf and vsinqb.
c***description
c
c  subroutine vsinqi initializes the array wsave which is used in
c  both vsinqf and vsinqb.  the prime factorization of n together with
c  a tabulation of the trigonometric functions are computed and
c  stored in wsave.
c
c  input parameter
c
c  n       the length of the sequence to be transformed.  the method
c          is most efficient when n is a product of small primes.
c
c  output parameter
c
c  wsave   a work array which must be dimensioned at least 3*n+15.
c          the same work array can be used for both vsinqf and vsinqb
c          as long as n remains unchanged.  different wsave arrays
c          are required for different values of n.  the contents of
c
c          wsave must not be changed between calls of vsinqf or vsinqb.
c
c***references  p. n. swarztrauber, vectorizing the ffts, in parallel
c               computations, (g. rodrigue, ed.), academic press, 1982,
c               pp. 51-83.
c***routines called  vcosqi
c***end prologue  vsinqi
      dimension       wsave(*)
c***first executable statement  vsinqi
      call vcosqi (n,wsave)
      return
      end
      subroutine vrftb1 (m,n,c,ch,mdimc,wa,fac)
c
c     vrfftpk, version 1, august 1985
c
      dimension       ch(mdimc,n), c(mdimc,n), wa(n) ,fac(15)
      nf = fac(2)
      na = 0
      l1 = 1
      iw = 1
      do 116 k1=1,nf
         ip = fac(k1+2)
         l2 = ip*l1
         ido = n/l2
         idl1 = ido*l1
         if (ip .ne. 4) go to 103
         ix2 = iw+ido
         ix3 = ix2+ido
         if (na .ne. 0) go to 101
         call vradb4 (m,ido,l1,c,ch,mdimc,wa(iw),wa(ix2),wa(ix3))
         go to 102
  101    call vradb4 (m,ido,l1,ch,c,mdimc,wa(iw),wa(ix2),wa(ix3))
  102    na = 1-na
         go to 115
  103    if (ip .ne. 2) go to 106
         if (na .ne. 0) go to 104
         call vradb2 (m,ido,l1,c,ch,mdimc,wa(iw))
         go to 105
  104    call vradb2 (m,ido,l1,ch,c,mdimc,wa(iw))
  105    na = 1-na
         go to 115
  106    if (ip .ne. 3) go to 109
         ix2 = iw+ido
         if (na .ne. 0) go to 107
         call vradb3 (m,ido,l1,c,ch,mdimc,wa(iw),wa(ix2))
         go to 108
  107    call vradb3 (m,ido,l1,ch,c,mdimc,wa(iw),wa(ix2))
  108    na = 1-na
         go to 115
  109    if (ip .ne. 5) go to 112
         ix2 = iw+ido
         ix3 = ix2+ido
         ix4 = ix3+ido
         if (na .ne. 0) go to 110
      call vradb5 (m,ido,l1,c,ch,mdimc,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 111
  110 call vradb5 (m,ido,l1,ch,c,mdimc,wa(iw),wa(ix2),wa(ix3),wa(ix4))
  111    na = 1-na
         go to 115
  112    if (na .ne. 0) go to 113
         call vradbg (m,ido,ip,l1,idl1,c,c,c,ch,ch,mdimc,wa(iw))
         go to 114
  113    call vradbg (m,ido,ip,l1,idl1,ch,ch,ch,c,c,mdimc,wa(iw))
  114    if (ido .eq. 1) na = 1-na
  115    l1 = l2
         iw = iw+(ip-1)*ido
  116 continue
      scale=sqrt(1./n)
      if (na .eq. 0) go to 118
      do 117 j=1,n
      do 117 i=1,m
         c(i,j) = scale*ch(i,j)
  117 continue
      return
  118 do 119 j=1,n
      do 119 i=1,m
         c(i,j)=scale*c(i,j)
  119 continue
      return
      end
      subroutine vrftf1 (m,n,c,ch,mdimc,wa,fac)
c
c     vrfftpk, version 1, august 1985
c
      dimension       ch(mdimc,n) ,c(mdimc,n)  ,wa(n)   ,fac(15)
      nf = fac(2)
      na = 1
      l2 = n
      iw = n
      do 111 k1=1,nf
         kh = nf-k1
         ip = fac(kh+3)
         l1 = l2/ip
         ido = n/l2
         idl1 = ido*l1
         iw = iw-(ip-1)*ido
         na = 1-na
         if (ip .ne. 4) go to 102
         ix2 = iw+ido
         ix3 = ix2+ido
         if (na .ne. 0) go to 101
         call vradf4 (m,ido,l1,c,ch,mdimc,wa(iw),wa(ix2),wa(ix3))
         go to 110
  101    call vradf4 (m,ido,l1,ch,c,mdimc,wa(iw),wa(ix2),wa(ix3))
         go to 110
  102    if (ip .ne. 2) go to 104
         if (na .ne. 0) go to 103
         call vradf2 (m,ido,l1,c,ch,mdimc,wa(iw))
         go to 110
  103    call vradf2 (m,ido,l1,ch,c,mdimc,wa(iw))
         go to 110
  104    if (ip .ne. 3) go to 106
         ix2 = iw+ido
         if (na .ne. 0) go to 105
         call vradf3 (m,ido,l1,c,ch,mdimc,wa(iw),wa(ix2))
         go to 110
  105    call vradf3 (m,ido,l1,ch,c,mdimc,wa(iw),wa(ix2))
         go to 110
  106    if (ip .ne. 5) go to 108
         ix2 = iw+ido
         ix3 = ix2+ido
         ix4 = ix3+ido
         if (na .ne. 0) go to 107
      call vradf5(m,ido,l1,c,ch,mdimc,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 110
  107 call vradf5 (m,ido,l1,ch,c,mdimc,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 110
  108    if (ido .eq. 1) na = 1-na
         if (na .ne. 0) go to 109
         call vradfg (m,ido,ip,l1,idl1,c,c,c,ch,ch,mdimc,wa(iw))
         na = 1
         go to 110
  109    call vradfg (m,ido,ip,l1,idl1,ch,ch,ch,c,c,mdimc,wa(iw))
         na = 0
  110    l2 = l1
  111 continue
      scale=sqrt(1./n)
      if (na .eq. 1) go to 113
      do 112 j=1,n
      do 112 i=1,m
         c(i,j) = scale*ch(i,j)
  112 continue
      return
  113 do 114 j=1,n
      do 114 i=1,m
         c(i,j)=scale*c(i,j)
  114 continue
      return
      end
      subroutine vrfti1 (n,wa,fac)
c
c     vrfftpk, version 1, august 1985
c
      dimension       wa(n)      ,fac(15)    ,ntryh(4)
      data ntryh(1),ntryh(2),ntryh(3),ntryh(4)/4,2,3,5/
      nl = n
      nf = 0
      j = 0
  101 j = j+1
      if (j-4) 102,102,103
  102 ntry = ntryh(j)
      go to 104
  103 ntry = ntry+2
  104 nq = nl/ntry
      nr = nl-ntry*nq
      if (nr) 101,105,101
  105 nf = nf+1
      fac(nf+2) = ntry
      nl = nq
      if (ntry .ne. 2) go to 107
      if (nf .eq. 1) go to 107
      do 106 i=2,nf
         ib = nf-i+2
         fac(ib+2) = fac(ib+1)
  106 continue
      fac(3) = 2
  107 if (nl .ne. 1) go to 104
      fac(1) = n
      fac(2) = nf
      tpi = 2.*pimach(1.0)
      argh = tpi/float(n)
      is = 0
      nfm1 = nf-1
      l1 = 1
      if (nfm1 .eq. 0) return
      do 110 k1=1,nfm1
         ip = fac(k1+2)
         ld = 0
         l2 = l1*ip
         ido = n/l2
         ipm = ip-1
         do 109 j=1,ipm
            ld = ld+l1
            i = is
            argld = float(ld)*argh
            fi = 0.
            do 108 ii=3,ido,2
               i = i+2
               fi = fi+1.
               arg = fi*argld
               wa(i-1) = cos(arg)
               wa(i) = sin(arg)
  108       continue
            is = is+ido
  109    continue
         l1 = l2
  110 continue
      return
      end
      subroutine vradf2 (mp,ido,l1,cc,ch,mdimc,wa1)
c
c     vrfftpk, version 1, august 1985
c
      dimension   ch(mdimc,ido,2,l1)  ,cc(mdimc,ido,l1,2)     ,
     1                wa1(ido)
      do 101 k=1,l1
         do 1001 m=1,mp
         ch(m,1,1,k) = cc(m,1,k,1)+cc(m,1,k,2)
         ch(m,ido,2,k) = cc(m,1,k,1)-cc(m,1,k,2)
 1001    continue
  101 continue
      if (ido-2) 107,105,102
  102 idp2 = ido+2
      do 104 k=1,l1
         do 103 i=3,ido,2
            ic = idp2-i
            do 1003 m=1,mp
            ch(m,i,1,k) = cc(m,i,k,1)+(wa1(i-2)*cc(m,i,k,2)-
     1       wa1(i-1)*cc(m,i-1,k,2))
            ch(m,ic,2,k) = (wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*
     1       cc(m,i-1,k,2))-cc(m,i,k,1)
            ch(m,i-1,1,k) = cc(m,i-1,k,1)+(wa1(i-2)*cc(m,i-1,k,2)+
     1       wa1(i-1)*cc(m,i,k,2))
            ch(m,ic-1,2,k) = cc(m,i-1,k,1)-(wa1(i-2)*cc(m,i-1,k,2)+
     1       wa1(i-1)*cc(m,i,k,2))
 1003       continue
  103    continue
  104 continue
      if (mod(ido,2) .eq. 1) return
  105 do 106 k=1,l1
         do 1006 m=1,mp
         ch(m,1,2,k) = -cc(m,ido,k,2)
         ch(m,ido,1,k) = cc(m,ido,k,1)
 1006    continue
  106 continue
  107 return
      end
      subroutine vradf3 (mp,ido,l1,cc,ch,mdimc,wa1,wa2)
c
c     vrfftpk, version 1, august 1985
c
      dimension   ch(mdimc,ido,3,l1)  ,cc(mdimc,ido,l1,3)     ,
     1                wa1(ido)     ,wa2(ido)
      arg=2.*pimach(1.0)/3.
      taur=cos(arg)
      taui=sin(arg)
      do 101 k=1,l1
         do 1001 m=1,mp
         ch(m,1,1,k) = cc(m,1,k,1)+(cc(m,1,k,2)+cc(m,1,k,3))
         ch(m,1,3,k) = taui*(cc(m,1,k,3)-cc(m,1,k,2))
         ch(m,ido,2,k) = cc(m,1,k,1)+taur*
     1      (cc(m,1,k,2)+cc(m,1,k,3))
 1001    continue
  101 continue
      if (ido .eq. 1) return
      idp2 = ido+2
      do 103 k=1,l1
         do 102 i=3,ido,2
            ic = idp2-i
            do 1002 m=1,mp
            ch(m,i-1,1,k) = cc(m,i-1,k,1)+((wa1(i-2)*cc(m,i-1,k,2)+
     1       wa1(i-1)*cc(m,i,k,2))+(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*
     1       cc(m,i,k,3)))
            ch(m,i,1,k) = cc(m,i,k,1)+((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*
     1       cc(m,i-1,k,2))+(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*
     1       cc(m,i-1,k,3)))
            ch(m,i-1,3,k) = (cc(m,i-1,k,1)+taur*((wa1(i-2)*
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa2(i-2)*
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))))+(taui*((wa1(i-2)*
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa2(i-2)*
     1       cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3))))
            ch(m,ic-1,2,k) = (cc(m,i-1,k,1)+taur*((wa1(i-2)*
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa2(i-2)*
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))))-(taui*((wa1(i-2)*
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa2(i-2)*
     1       cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3))))
            ch(m,i,3,k) = (cc(m,i,k,1)+taur*((wa1(i-2)*cc(m,i,k,2)-
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*
     1       cc(m,i-1,k,3))))+(taui*((wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*
     1       cc(m,i,k,3))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*
     1       cc(m,i,k,2))))
            ch(m,ic,2,k) = (taui*((wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*
     1       cc(m,i,k,3))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*
     1       cc(m,i,k,2))))-(cc(m,i,k,1)+taur*((wa1(i-2)*cc(m,i,k,2)-
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*
     1       cc(m,i-1,k,3))))
 1002       continue
  102    continue
  103 continue
      return
      end
      subroutine vradf4 (mp,ido,l1,cc,ch,mdimc,wa1,wa2,wa3)
c
c     vrfftpk, version 1, august 1985
c
      dimension    cc(mdimc,ido,l1,4)   ,ch(mdimc,ido,4,l1)     ,
     1                wa1(ido)     ,wa2(ido)     ,wa3(ido)
      hsqt2=sqrt(2.)/2.
      do 101 k=1,l1
         do 1001 m=1,mp
         ch(m,1,1,k) = (cc(m,1,k,2)+cc(m,1,k,4))
     1      +(cc(m,1,k,1)+cc(m,1,k,3))
         ch(m,ido,4,k) = (cc(m,1,k,1)+cc(m,1,k,3))
     1      -(cc(m,1,k,2)+cc(m,1,k,4))
         ch(m,ido,2,k) = cc(m,1,k,1)-cc(m,1,k,3)
         ch(m,1,3,k) = cc(m,1,k,4)-cc(m,1,k,2)
 1001    continue
  101 continue
      if (ido-2) 107,105,102
  102 idp2 = ido+2
      do 104 k=1,l1
         do 103 i=3,ido,2
            ic = idp2-i
            do 1003 m=1,mp
            ch(m,i-1,1,k) = ((wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*
     1       cc(m,i,k,2))+(wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*
     1       cc(m,i,k,4)))+(cc(m,i-1,k,1)+(wa2(i-2)*cc(m,i-1,k,3)+
     1       wa2(i-1)*cc(m,i,k,3)))
            ch(m,ic-1,4,k) = (cc(m,i-1,k,1)+(wa2(i-2)*cc(m,i-1,k,3)+
     1       wa2(i-1)*cc(m,i,k,3)))-((wa1(i-2)*cc(m,i-1,k,2)+
     1       wa1(i-1)*cc(m,i,k,2))+(wa3(i-2)*cc(m,i-1,k,4)+
     1       wa3(i-1)*cc(m,i,k,4)))
            ch(m,i,1,k) = ((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*
     1       cc(m,i-1,k,2))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*
     1       cc(m,i-1,k,4)))+(cc(m,i,k,1)+(wa2(i-2)*cc(m,i,k,3)-
     1       wa2(i-1)*cc(m,i-1,k,3)))
            ch(m,ic,4,k) = ((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*
     1       cc(m,i-1,k,2))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*
     1       cc(m,i-1,k,4)))-(cc(m,i,k,1)+(wa2(i-2)*cc(m,i,k,3)-
     1       wa2(i-1)*cc(m,i-1,k,3)))
            ch(m,i-1,3,k) = ((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*
     1       cc(m,i-1,k,2))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*
     1       cc(m,i-1,k,4)))+(cc(m,i-1,k,1)-(wa2(i-2)*cc(m,i-1,k,3)+
     1       wa2(i-1)*cc(m,i,k,3)))
            ch(m,ic-1,2,k) = (cc(m,i-1,k,1)-(wa2(i-2)*cc(m,i-1,k,3)+
     1       wa2(i-1)*cc(m,i,k,3)))-((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*
     1       cc(m,i-1,k,2))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*
     1       cc(m,i-1,k,4)))
            ch(m,i,3,k) = ((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*
     1       cc(m,i,k,4))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*
     1       cc(m,i,k,2)))+(cc(m,i,k,1)-(wa2(i-2)*cc(m,i,k,3)-
     1       wa2(i-1)*cc(m,i-1,k,3)))
            ch(m,ic,2,k) = ((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*
     1       cc(m,i,k,4))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*
     1       cc(m,i,k,2)))-(cc(m,i,k,1)-(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*
     1       cc(m,i-1,k,3)))
 1003       continue
  103    continue
  104 continue
      if (mod(ido,2) .eq. 1) return
  105 continue
      do 106 k=1,l1
         do 1006 m=1,mp
            ch(m,ido,1,k) = (hsqt2*(cc(m,ido,k,2)-cc(m,ido,k,4)))+
     1       cc(m,ido,k,1)
            ch(m,ido,3,k) = cc(m,ido,k,1)-(hsqt2*(cc(m,ido,k,2)-
     1       cc(m,ido,k,4)))
            ch(m,1,2,k) = (-hsqt2*(cc(m,ido,k,2)+cc(m,ido,k,4)))-
     1       cc(m,ido,k,3)
            ch(m,1,4,k) = (-hsqt2*(cc(m,ido,k,2)+cc(m,ido,k,4)))+
     1       cc(m,ido,k,3)
 1006    continue
  106 continue
  107 return
      end
      subroutine vradf5 (mp,ido,l1,cc,ch,mdimc,wa1,wa2,wa3,wa4)
c
c     vrfftpk, version 1, august 1985
c
      dimension  cc(mdimc,ido,l1,5)    ,ch(mdimc,ido,5,l1)     ,
     1           wa1(ido)     ,wa2(ido)     ,wa3(ido)     ,wa4(ido)
      arg=2.*pimach(1.0)/5.
      tr11=cos(arg)
      ti11=sin(arg)
      tr12=cos(2.*arg)
      ti12=sin(2.*arg)
      do 101 k=1,l1
         do 1001 m=1,mp
         ch(m,1,1,k) = cc(m,1,k,1)+(cc(m,1,k,5)+cc(m,1,k,2))+
     1    (cc(m,1,k,4)+cc(m,1,k,3))
         ch(m,ido,2,k) = cc(m,1,k,1)+tr11*(cc(m,1,k,5)+cc(m,1,k,2))+
     1    tr12*(cc(m,1,k,4)+cc(m,1,k,3))
         ch(m,1,3,k) = ti11*(cc(m,1,k,5)-cc(m,1,k,2))+ti12*
     1    (cc(m,1,k,4)-cc(m,1,k,3))
         ch(m,ido,4,k) = cc(m,1,k,1)+tr12*(cc(m,1,k,5)+cc(m,1,k,2))+
     1    tr11*(cc(m,1,k,4)+cc(m,1,k,3))
         ch(m,1,5,k) = ti12*(cc(m,1,k,5)-cc(m,1,k,2))-ti11*
     1    (cc(m,1,k,4)-cc(m,1,k,3))
 1001    continue
  101 continue
      if (ido .eq. 1) return
      idp2 = ido+2
      do 103 k=1,l1
         do 102 i=3,ido,2
            ic = idp2-i
            do 1002 m=1,mp
            ch(m,i-1,1,k) = cc(m,i-1,k,1)+((wa1(i-2)*cc(m,i-1,k,2)+
     1       wa1(i-1)*cc(m,i,k,2))+(wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*
     1       cc(m,i,k,5)))+((wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*
     1       cc(m,i,k,3))+(wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4)))
            ch(m,i,1,k) = cc(m,i,k,1)+((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*
     1       cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*
     1       cc(m,i-1,k,5)))+((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*
     1       cc(m,i-1,k,4)))
            ch(m,i-1,3,k) = cc(m,i-1,k,1)+tr11*
     1      ( wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2)
     1       +wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5))+tr12*
     1      ( wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3)
     1       +wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))+ti11*
     1      ( wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2)
     1       -(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*cc(m,i-1,k,5)))+ti12*
     1      ( wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3)
     1       -(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*cc(m,i-1,k,4)))
            ch(m,ic-1,2,k) = cc(m,i-1,k,1)+tr11*
     1      ( wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2)
     1       +wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5))+tr12*
     1     ( wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3)
     1      +wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))-(ti11*
     1      ( wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2)
     1       -(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*cc(m,i-1,k,5)))+ti12*
     1      ( wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3)
     1       -(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*cc(m,i-1,k,4))))
            ch(m,i,3,k) = (cc(m,i,k,1)+tr11*((wa1(i-2)*cc(m,i,k,2)-
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*
     1       cc(m,i-1,k,5)))+tr12*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*
     1       cc(m,i-1,k,4))))+(ti11*((wa4(i-2)*cc(m,i-1,k,5)+
     1       wa4(i-1)*cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*
     1       cc(m,i,k,2)))+ti12*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*
     1       cc(m,i,k,3))))
            ch(m,ic,2,k) = (ti11*((wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*
     1       cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*
     1       cc(m,i,k,2)))+ti12*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*
     1       cc(m,i,k,3))))-(cc(m,i,k,1)+tr11*((wa1(i-2)*cc(m,i,k,2)-
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*
     1       cc(m,i-1,k,5)))+tr12*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*
     1       cc(m,i-1,k,4))))
            ch(m,i-1,5,k) = (cc(m,i-1,k,1)+tr12*((wa1(i-2)*
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa4(i-2)*
     1       cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5)))+tr11*((wa2(i-2)*
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))+(wa3(i-2)*
     1       cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))))+(ti12*((wa1(i-2)*
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa4(i-2)*cc(m,i,k,5)-
     1       wa4(i-1)*cc(m,i-1,k,5)))-ti11*((wa2(i-2)*cc(m,i,k,3)-
     1       wa2(i-1)*cc(m,i-1,k,3))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*
     1       cc(m,i-1,k,4))))
            ch(m,ic-1,4,k) = (cc(m,i-1,k,1)+tr12*((wa1(i-2)*
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa4(i-2)*
     1       cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5)))+tr11*((wa2(i-2)*
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))+(wa3(i-2)*
     1       cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))))-(ti12*((wa1(i-2)*
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa4(i-2)*cc(m,i,k,5)-
     1       wa4(i-1)*cc(m,i-1,k,5)))-ti11*((wa2(i-2)*cc(m,i,k,3)-
     1       wa2(i-1)*cc(m,i-1,k,3))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*
     1       cc(m,i-1,k,4))))
            ch(m,i,5,k) = (cc(m,i,k,1)+tr12*((wa1(i-2)*cc(m,i,k,2)-
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*
     1       cc(m,i-1,k,5)))+tr11*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*
     1       cc(m,i-1,k,4))))+(ti12*((wa4(i-2)*cc(m,i-1,k,5)+
     1       wa4(i-1)*cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*
     1       cc(m,i,k,2)))-ti11*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*
     1       cc(m,i,k,3))))
            ch(m,ic,4,k) = (ti12*((wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*
     1       cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*
     1       cc(m,i,k,2)))-ti11*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*
     1       cc(m,i,k,3))))-(cc(m,i,k,1)+tr12*((wa1(i-2)*cc(m,i,k,2)-
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*
     1       cc(m,i-1,k,5)))+tr11*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*
     1       cc(m,i-1,k,4))))
 1002       continue
  102    continue
  103 continue
      return
      end
      subroutine vradbg (mp,ido,ip,l1,idl1,cc,c1,c2,ch,ch2,
c
c     vrfftpk, version 1, august 1985
c
     *                 mdimc,wa)
      dimension    ch(mdimc,ido,l1,ip)    ,cc(mdimc,ido,ip,l1) ,
     1           c1(mdimc,ido,l1,ip)     ,c2(mdimc,idl1,ip),
     2                ch2(mdimc,idl1,ip)       ,wa(ido)
      tpi=2.*pimach(1.0)
      arg = tpi/float(ip)
      dcp = cos(arg)
      dsp = sin(arg)
      idp2 = ido+2
      nbd = (ido-1)/2
      ipp2 = ip+2
      ipph = (ip+1)/2
      if (ido .lt. l1) go to 103
      do 102 k=1,l1
         do 101 i=1,ido
            do 1001 m=1,mp
            ch(m,i,k,1) = cc(m,i,1,k)
 1001       continue
  101    continue
  102 continue
      go to 106
  103 do 105 i=1,ido
         do 104 k=1,l1
            do 1004 m=1,mp
            ch(m,i,k,1) = cc(m,i,1,k)
 1004       continue
  104    continue
  105 continue
  106 do 108 j=2,ipph
         jc = ipp2-j
         j2 = j+j
         do 107 k=1,l1
            do 1007 m=1,mp
            ch(m,1,k,j) = cc(m,ido,j2-2,k)+cc(m,ido,j2-2,k)
            ch(m,1,k,jc) = cc(m,1,j2-1,k)+cc(m,1,j2-1,k)
 1007       continue
  107    continue
  108 continue
      if (ido .eq. 1) go to 116
      if (nbd .lt. l1) go to 112
      do 111 j=2,ipph
         jc = ipp2-j
         do 110 k=1,l1
            do 109 i=3,ido,2
               ic = idp2-i
               do 1009 m=1,mp
               ch(m,i-1,k,j) = cc(m,i-1,2*j-1,k)+cc(m,ic-1,2*j-2,k)
               ch(m,i-1,k,jc) = cc(m,i-1,2*j-1,k)-cc(m,ic-1,2*j-2,k)
               ch(m,i,k,j) = cc(m,i,2*j-1,k)-cc(m,ic,2*j-2,k)
               ch(m,i,k,jc) = cc(m,i,2*j-1,k)+cc(m,ic,2*j-2,k)
 1009          continue
  109       continue
  110    continue
  111 continue
      go to 116
  112 do 115 j=2,ipph
         jc = ipp2-j
         do 114 i=3,ido,2
            ic = idp2-i
            do 113 k=1,l1
               do 1013 m=1,mp
               ch(m,i-1,k,j) = cc(m,i-1,2*j-1,k)+cc(m,ic-1,2*j-2,k)
               ch(m,i-1,k,jc) = cc(m,i-1,2*j-1,k)-cc(m,ic-1,2*j-2,k)
               ch(m,i,k,j) = cc(m,i,2*j-1,k)-cc(m,ic,2*j-2,k)
               ch(m,i,k,jc) = cc(m,i,2*j-1,k)+cc(m,ic,2*j-2,k)
 1013          continue
  113       continue
  114    continue
  115 continue
  116 ar1 = 1.
      ai1 = 0.
      do 120 l=2,ipph
         lc = ipp2-l
         ar1h = dcp*ar1-dsp*ai1
         ai1 = dcp*ai1+dsp*ar1
         ar1 = ar1h
         do 117 ik=1,idl1
            do 1017 m=1,mp
            c2(m,ik,l) = ch2(m,ik,1)+ar1*ch2(m,ik,2)
            c2(m,ik,lc) = ai1*ch2(m,ik,ip)
 1017       continue
  117    continue
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         do 119 j=3,ipph
            jc = ipp2-j
            ar2h = dc2*ar2-ds2*ai2
            ai2 = dc2*ai2+ds2*ar2
            ar2 = ar2h
            do 118 ik=1,idl1
               do 1018 m=1,mp
               c2(m,ik,l) = c2(m,ik,l)+ar2*ch2(m,ik,j)
               c2(m,ik,lc) = c2(m,ik,lc)+ai2*ch2(m,ik,jc)
 1018          continue
  118       continue
  119    continue
  120 continue
      do 122 j=2,ipph
         do 121 ik=1,idl1
            do 1021 m=1,mp
            ch2(m,ik,1) = ch2(m,ik,1)+ch2(m,ik,j)
 1021       continue
  121    continue
  122 continue
      do 124 j=2,ipph
         jc = ipp2-j
         do 123 k=1,l1
            do 1023 m=1,mp
            ch(m,1,k,j) = c1(m,1,k,j)-c1(m,1,k,jc)
            ch(m,1,k,jc) = c1(m,1,k,j)+c1(m,1,k,jc)
 1023       continue
  123    continue
  124 continue
      if (ido .eq. 1) go to 132
      if (nbd .lt. l1) go to 128
      do 127 j=2,ipph
         jc = ipp2-j
         do 126 k=1,l1
            do 125 i=3,ido,2
               do 1025 m=1,mp
               ch(m,i-1,k,j) = c1(m,i-1,k,j)-c1(m,i,k,jc)
               ch(m,i-1,k,jc) = c1(m,i-1,k,j)+c1(m,i,k,jc)
               ch(m,i,k,j) = c1(m,i,k,j)+c1(m,i-1,k,jc)
               ch(m,i,k,jc) = c1(m,i,k,j)-c1(m,i-1,k,jc)
 1025          continue
  125       continue
  126    continue
  127 continue
      go to 132
  128 do 131 j=2,ipph
         jc = ipp2-j
         do 130 i=3,ido,2
            do 129 k=1,l1
               do 1029 m=1,mp
               ch(m,i-1,k,j) = c1(m,i-1,k,j)-c1(m,i,k,jc)
               ch(m,i-1,k,jc) = c1(m,i-1,k,j)+c1(m,i,k,jc)
               ch(m,i,k,j) = c1(m,i,k,j)+c1(m,i-1,k,jc)
               ch(m,i,k,jc) = c1(m,i,k,j)-c1(m,i-1,k,jc)
 1029          continue
  129       continue
  130    continue
  131 continue
  132 continue
      if (ido .eq. 1) return
      do 133 ik=1,idl1
         do 1033 m=1,mp
         c2(m,ik,1) = ch2(m,ik,1)
 1033    continue
  133 continue
      do 135 j=2,ip
         do 134 k=1,l1
            do 1034 m=1,mp
            c1(m,1,k,j) = ch(m,1,k,j)
 1034       continue
  134    continue
  135 continue
      if (nbd .gt. l1) go to 139
      is = -ido
      do 138 j=2,ip
         is = is+ido
         idij = is
         do 137 i=3,ido,2
            idij = idij+2
            do 136 k=1,l1
               do 1036 m=1,mp
               c1(m,i-1,k,j) = wa(idij-1)*ch(m,i-1,k,j)-wa(idij)*
     1          ch(m,i,k,j)
               c1(m,i,k,j) = wa(idij-1)*ch(m,i,k,j)+wa(idij)*
     1          ch(m,i-1,k,j)
 1036          continue
  136       continue
  137    continue
  138 continue
      go to 143
  139 is = -ido
      do 142 j=2,ip
         is = is+ido
         do 141 k=1,l1
            idij = is
            do 140 i=3,ido,2
               idij = idij+2
               do 1040 m=1,mp
               c1(m,i-1,k,j) = wa(idij-1)*ch(m,i-1,k,j)-wa(idij)*
     1          ch(m,i,k,j)
               c1(m,i,k,j) = wa(idij-1)*ch(m,i,k,j)+wa(idij)*
     1          ch(m,i-1,k,j)
 1040          continue
  140       continue
  141    continue
  142 continue
  143 return
      end
      subroutine vradb2 (mp,ido,l1,cc,ch,mdimc,wa1)
c
c     vrfftpk, version 1, august 1985
c
      dimension  cc(mdimc,ido,2,l1)    ,ch(mdimc,ido,l1,2),
     1                wa1(ido)
      do 101 k=1,l1
          do 1001 m=1,mp
         ch(m,1,k,1) = cc(m,1,1,k)+cc(m,ido,2,k)
         ch(m,1,k,2) = cc(m,1,1,k)-cc(m,ido,2,k)
 1001     continue
  101 continue
      if (ido-2) 107,105,102
  102 idp2 = ido+2
      do 104 k=1,l1
         do 103 i=3,ido,2
            ic = idp2-i
               do 1002 m=1,mp
            ch(m,i-1,k,1) = cc(m,i-1,1,k)+cc(m,ic-1,2,k)
            ch(m,i,k,1) = cc(m,i,1,k)-cc(m,ic,2,k)
            ch(m,i-1,k,2) = wa1(i-2)*(cc(m,i-1,1,k)-cc(m,ic-1,2,k))
     1      -wa1(i-1)*(cc(m,i,1,k)+cc(m,ic,2,k))
            ch(m,i,k,2) = wa1(i-2)*(cc(m,i,1,k)+cc(m,ic,2,k))+wa1(i-1)
     1      *(cc(m,i-1,1,k)-cc(m,ic-1,2,k))
 1002          continue
  103    continue
  104 continue
      if (mod(ido,2) .eq. 1) return
  105 do 106 k=1,l1
          do 1003 m=1,mp
         ch(m,ido,k,1) = cc(m,ido,1,k)+cc(m,ido,1,k)
         ch(m,ido,k,2) = -(cc(m,1,2,k)+cc(m,1,2,k))
 1003     continue
  106 continue
  107 return
      end
      subroutine vradb3 (mp,ido,l1,cc,ch,mdimc,wa1,wa2)
c
c     vrfftpk, version 1, august 1985
c
      dimension  cc(mdimc,ido,3,l1)    ,ch(mdimc,ido,l1,3),
     1                wa1(ido)   ,wa2(ido)
      arg=2.*pimach(1.0)/3.
      taur=cos(arg)
      taui=sin(arg)
      do 101 k=1,l1
          do 1001 m=1,mp
         ch(m,1,k,1) = cc(m,1,1,k)+2.*cc(m,ido,2,k)
         ch(m,1,k,2) = cc(m,1,1,k)+(2.*taur)*cc(m,ido,2,k)
     1   -(2.*taui)*cc(m,1,3,k)
         ch(m,1,k,3) = cc(m,1,1,k)+(2.*taur)*cc(m,ido,2,k)
     1   +2.*taui*cc(m,1,3,k)
 1001     continue
  101 continue
      if (ido .eq. 1) return
      idp2 = ido+2
      do 103 k=1,l1
         do 102 i=3,ido,2
            ic = idp2-i
               do 1002 m=1,mp
            ch(m,i-1,k,1) = cc(m,i-1,1,k)+(cc(m,i-1,3,k)+cc(m,ic-1,2,k))
            ch(m,i,k,1) = cc(m,i,1,k)+(cc(m,i,3,k)-cc(m,ic,2,k))
            ch(m,i-1,k,2) = wa1(i-2)*
     1 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))-
     * (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))
     2                   -wa1(i-1)*
     3 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))+
     * (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))
            ch(m,i,k,2) = wa1(i-2)*
     4 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))+
     8 (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))
     5                  +wa1(i-1)*
     6 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))-
     8 (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))
              ch(m,i-1,k,3) = wa2(i-2)*
     7 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))+
     8 (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))
     8                      -wa2(i-1)*
     9 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))-
     8 (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))
            ch(m,i,k,3) = wa2(i-2)*
     1 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))-
     8 (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))
     2                 +wa2(i-1)*
     3 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))+
     8 (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))
 1002          continue
  102    continue
  103 continue
      return
      end
      subroutine vradb4 (mp,ido,l1,cc,ch,mdimc,wa1,wa2,wa3)
c
c     vrfftpk, version 1, august 1985
c
      dimension  cc(mdimc,ido,4,l1)  ,ch(mdimc,ido,l1,4)    ,
     1                wa1(ido)  ,wa2(ido)  ,wa3(ido)
      sqrt2=sqrt(2.)
      do 101 k=1,l1
          do 1001 m=1,mp
         ch(m,1,k,3) = (cc(m,1,1,k)+cc(m,ido,4,k))
     1   -(cc(m,ido,2,k)+cc(m,ido,2,k))
         ch(m,1,k,1) = (cc(m,1,1,k)+cc(m,ido,4,k))
     1   +(cc(m,ido,2,k)+cc(m,ido,2,k))
         ch(m,1,k,4) = (cc(m,1,1,k)-cc(m,ido,4,k))
     1   +(cc(m,1,3,k)+cc(m,1,3,k))
         ch(m,1,k,2) = (cc(m,1,1,k)-cc(m,ido,4,k))
     1   -(cc(m,1,3,k)+cc(m,1,3,k))
 1001     continue
  101 continue
      if (ido-2) 107,105,102
  102 idp2 = ido+2
      do 104 k=1,l1
         do 103 i=3,ido,2
            ic = idp2-i
               do 1002 m=1,mp
            ch(m,i-1,k,1) = (cc(m,i-1,1,k)+cc(m,ic-1,4,k))
     1      +(cc(m,i-1,3,k)+cc(m,ic-1,2,k))
            ch(m,i,k,1) = (cc(m,i,1,k)-cc(m,ic,4,k))
     1      +(cc(m,i,3,k)-cc(m,ic,2,k))
            ch(m,i-1,k,2)=wa1(i-2)*((cc(m,i-1,1,k)-cc(m,ic-1,4,k))
     1      -(cc(m,i,3,k)+cc(m,ic,2,k)))-wa1(i-1)
     1      *((cc(m,i,1,k)+cc(m,ic,4,k))+(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))
            ch(m,i,k,2)=wa1(i-2)*((cc(m,i,1,k)+cc(m,ic,4,k))
     1      +(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))+wa1(i-1)
     1      *((cc(m,i-1,1,k)-cc(m,ic-1,4,k))-(cc(m,i,3,k)+cc(m,ic,2,k)))
            ch(m,i-1,k,3)=wa2(i-2)*((cc(m,i-1,1,k)+cc(m,ic-1,4,k))
     1      -(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))-wa2(i-1)
     1      *((cc(m,i,1,k)-cc(m,ic,4,k))-(cc(m,i,3,k)-cc(m,ic,2,k)))
            ch(m,i,k,3)=wa2(i-2)*((cc(m,i,1,k)-cc(m,ic,4,k))
     1      -(cc(m,i,3,k)-cc(m,ic,2,k)))+wa2(i-1)
     1      *((cc(m,i-1,1,k)+cc(m,ic-1,4,k))-(cc(m,i-1,3,k)
     1      +cc(m,ic-1,2,k)))
            ch(m,i-1,k,4)=wa3(i-2)*((cc(m,i-1,1,k)-cc(m,ic-1,4,k))
     1      +(cc(m,i,3,k)+cc(m,ic,2,k)))-wa3(i-1)
     1     *((cc(m,i,1,k)+cc(m,ic,4,k))-(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))
            ch(m,i,k,4)=wa3(i-2)*((cc(m,i,1,k)+cc(m,ic,4,k))
     1      -(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))+wa3(i-1)
     1      *((cc(m,i-1,1,k)-cc(m,ic-1,4,k))+(cc(m,i,3,k)+cc(m,ic,2,k)))
 1002          continue
  103    continue
  104 continue
      if (mod(ido,2) .eq. 1) return
  105 continue
      do 106 k=1,l1
               do 1003 m=1,mp
         ch(m,ido,k,1) = (cc(m,ido,1,k)+cc(m,ido,3,k))
     1   +(cc(m,ido,1,k)+cc(m,ido,3,k))
         ch(m,ido,k,2) = sqrt2*((cc(m,ido,1,k)-cc(m,ido,3,k))
     1   -(cc(m,1,2,k)+cc(m,1,4,k)))
         ch(m,ido,k,3) = (cc(m,1,4,k)-cc(m,1,2,k))
     1   +(cc(m,1,4,k)-cc(m,1,2,k))
         ch(m,ido,k,4) = -sqrt2*((cc(m,ido,1,k)-cc(m,ido,3,k))
     1   +(cc(m,1,2,k)+cc(m,1,4,k)))
 1003          continue
  106 continue
  107 return
      end
      subroutine vradb5 (mp,ido,l1,cc,ch,mdimc,wa1,wa2,wa3,wa4)
c
c     vrfftpk, version 1, august 1985
c
      dimension  cc(mdimc,ido,5,l1)    ,ch(mdimc,ido,l1,5),
     1             wa1(ido)     ,wa2(ido)     ,wa3(ido)     ,wa4(ido)
      arg=2.*pimach(1.0)/5.
      tr11=cos(arg)
      ti11=sin(arg)
      tr12=cos(2.*arg)
      ti12=sin(2.*arg)
      do 101 k=1,l1
      do 1001 m=1,mp
         ch(m,1,k,1) = cc(m,1,1,k)+2.*cc(m,ido,2,k)+2.*cc(m,ido,4,k)
         ch(m,1,k,2) = (cc(m,1,1,k)+tr11*2.*cc(m,ido,2,k)
     1   +tr12*2.*cc(m,ido,4,k))-(ti11*2.*cc(m,1,3,k)
     1   +ti12*2.*cc(m,1,5,k))
         ch(m,1,k,3) = (cc(m,1,1,k)+tr12*2.*cc(m,ido,2,k)
     1   +tr11*2.*cc(m,ido,4,k))-(ti12*2.*cc(m,1,3,k)
     1   -ti11*2.*cc(m,1,5,k))
         ch(m,1,k,4) = (cc(m,1,1,k)+tr12*2.*cc(m,ido,2,k)
     1   +tr11*2.*cc(m,ido,4,k))+(ti12*2.*cc(m,1,3,k)
     1   -ti11*2.*cc(m,1,5,k))
         ch(m,1,k,5) = (cc(m,1,1,k)+tr11*2.*cc(m,ido,2,k)
     1   +tr12*2.*cc(m,ido,4,k))+(ti11*2.*cc(m,1,3,k)
     1   +ti12*2.*cc(m,1,5,k))
 1001          continue
  101 continue
      if (ido .eq. 1) return
      idp2 = ido+2
      do 103 k=1,l1
         do 102 i=3,ido,2
            ic = idp2-i
      do 1002 m=1,mp
            ch(m,i-1,k,1) = cc(m,i-1,1,k)+(cc(m,i-1,3,k)+cc(m,ic-1,2,k))
     1      +(cc(m,i-1,5,k)+cc(m,ic-1,4,k))
            ch(m,i,k,1) = cc(m,i,1,k)+(cc(m,i,3,k)-cc(m,ic,2,k))
     1      +(cc(m,i,5,k)-cc(m,ic,4,k))
            ch(m,i-1,k,2) = wa1(i-2)*((cc(m,i-1,1,k)+tr11*
     1      (cc(m,i-1,3,k)+cc(m,ic-1,2,k))+tr12
     1      *(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))-(ti11*(cc(m,i,3,k)
     1      +cc(m,ic,2,k))+ti12*(cc(m,i,5,k)+cc(m,ic,4,k))))
     1      -wa1(i-1)*((cc(m,i,1,k)+tr11*(cc(m,i,3,k)-cc(m,ic,2,k))
     1      +tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))+(ti11*(cc(m,i-1,3,k)
     1      -cc(m,ic-1,2,k))+ti12*(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))
            ch(m,i,k,2) = wa1(i-2)*((cc(m,i,1,k)+tr11*(cc(m,i,3,k)
     1      -cc(m,ic,2,k))+tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))
     1      +(ti11*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))+ti12
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))+wa1(i-1)
     1      *((cc(m,i-1,1,k)+tr11*(cc(m,i-1,3,k)
     1      +cc(m,ic-1,2,k))+tr12*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))
     1      -(ti11*(cc(m,i,3,k)+cc(m,ic,2,k))+ti12
     1      *(cc(m,i,5,k)+cc(m,ic,4,k))))
            ch(m,i-1,k,3) = wa2(i-2)
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))-(ti12*(cc(m,i,3,k)
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k))))
     1     -wa2(i-1)
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))
     1      +(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))
            ch(m,i,k,3) = wa2(i-2)
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))
     1      +(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))
     1      +wa2(i-1)
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))-(ti12*(cc(m,i,3,k)
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k))))
            ch(m,i-1,k,4) = wa3(i-2)
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti12*(cc(m,i,3,k)
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k))))
     1      -wa3(i-1)
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))
     1      -(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))
            ch(m,i,k,4) = wa3(i-2)
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))
     1      -(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))
     1      +wa3(i-1)
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti12*(cc(m,i,3,k)
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k))))
            ch(m,i-1,k,5) = wa4(i-2)
     1      *((cc(m,i-1,1,k)+tr11*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))
     1      +tr12*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti11*(cc(m,i,3,k)
     1      +cc(m,ic,2,k))+ti12*(cc(m,i,5,k)+cc(m,ic,4,k))))
     1      -wa4(i-1)
     1      *((cc(m,i,1,k)+tr11*(cc(m,i,3,k)-cc(m,ic,2,k))
     1      +tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))-(ti11*(cc(m,i-1,3,k)
     1      -cc(m,ic-1,2,k))+ti12*(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))
            ch(m,i,k,5) = wa4(i-2)
     1      *((cc(m,i,1,k)+tr11*(cc(m,i,3,k)-cc(m,ic,2,k))
     1      +tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))-(ti11*(cc(m,i-1,3,k)
     1      -cc(m,ic-1,2,k))+ti12*(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))
     1      +wa4(i-1)
     1      *((cc(m,i-1,1,k)+tr11*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))
     1      +tr12*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti11*(cc(m,i,3,k)
     1      +cc(m,ic,2,k))+ti12*(cc(m,i,5,k)+cc(m,ic,4,k))))
 1002          continue
  102    continue
  103 continue
      return
      end
      subroutine vradfg (mp,ido,ip,l1,idl1,cc,c1,c2,ch,ch2,mdimc,wa)
c
c     vrfftpk, version 1, august 1985
c
      dimension     ch(mdimc,ido,l1,ip)   ,cc(mdimc,ido,ip,l1)  ,
     1            c1(mdimc,ido,l1,ip)    ,c2(mdimc,idl1,ip),
     2                ch2(mdimc,idl1,ip)           ,wa(ido)
      tpi=2.*pimach(1.0)
      arg = tpi/float(ip)
      dcp = cos(arg)
      dsp = sin(arg)
      ipph = (ip+1)/2
      ipp2 = ip+2
      idp2 = ido+2
      nbd = (ido-1)/2
      if (ido .eq. 1) go to 119
      do 101 ik=1,idl1
         do 1001 m=1,mp
         ch2(m,ik,1) = c2(m,ik,1)
 1001    continue
  101 continue
      do 103 j=2,ip
         do 102 k=1,l1
            do 1002 m=1,mp
            ch(m,1,k,j) = c1(m,1,k,j)
 1002       continue
  102    continue
  103 continue
      if (nbd .gt. l1) go to 107
      is = -ido
      do 106 j=2,ip
         is = is+ido
         idij = is
         do 105 i=3,ido,2
            idij = idij+2
            do 104 k=1,l1
               do 1004 m=1,mp
               ch(m,i-1,k,j) = wa(idij-1)*c1(m,i-1,k,j)+wa(idij)
     1           *c1(m,i,k,j)
               ch(m,i,k,j) = wa(idij-1)*c1(m,i,k,j)-wa(idij)
     1           *c1(m,i-1,k,j)
 1004          continue
  104       continue
  105    continue
  106 continue
      go to 111
  107 is = -ido
      do 110 j=2,ip
         is = is+ido
         do 109 k=1,l1
            idij = is
            do 108 i=3,ido,2
               idij = idij+2
               do 1008 m=1,mp
               ch(m,i-1,k,j) = wa(idij-1)*c1(m,i-1,k,j)+wa(idij)
     1           *c1(m,i,k,j)
               ch(m,i,k,j) = wa(idij-1)*c1(m,i,k,j)-wa(idij)
     1           *c1(m,i-1,k,j)
 1008          continue
  108       continue
  109    continue
  110 continue
  111 if (nbd .lt. l1) go to 115
      do 114 j=2,ipph
         jc = ipp2-j
         do 113 k=1,l1
            do 112 i=3,ido,2
               do 1012 m=1,mp
               c1(m,i-1,k,j) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)
               c1(m,i-1,k,jc) = ch(m,i,k,j)-ch(m,i,k,jc)
               c1(m,i,k,j) = ch(m,i,k,j)+ch(m,i,k,jc)
               c1(m,i,k,jc) = ch(m,i-1,k,jc)-ch(m,i-1,k,j)
 1012          continue
  112       continue
  113    continue
  114 continue
      go to 121
  115 do 118 j=2,ipph
         jc = ipp2-j
         do 117 i=3,ido,2
            do 116 k=1,l1
               do 1016 m=1,mp
               c1(m,i-1,k,j) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)
               c1(m,i-1,k,jc) = ch(m,i,k,j)-ch(m,i,k,jc)
               c1(m,i,k,j) = ch(m,i,k,j)+ch(m,i,k,jc)
               c1(m,i,k,jc) = ch(m,i-1,k,jc)-ch(m,i-1,k,j)
 1016          continue
  116       continue
  117    continue
  118 continue
      go to 121
  119 do 120 ik=1,idl1
         do 1020 m=1,mp
         c2(m,ik,1) = ch2(m,ik,1)
 1020    continue
  120 continue
  121 do 123 j=2,ipph
         jc = ipp2-j
         do 122 k=1,l1
            do 1022 m=1,mp
            c1(m,1,k,j) = ch(m,1,k,j)+ch(m,1,k,jc)
            c1(m,1,k,jc) = ch(m,1,k,jc)-ch(m,1,k,j)
 1022       continue
  122    continue
  123 continue
c
      ar1 = 1.
      ai1 = 0.
      do 127 l=2,ipph
         lc = ipp2-l
         ar1h = dcp*ar1-dsp*ai1
         ai1 = dcp*ai1+dsp*ar1
         ar1 = ar1h
         do 124 ik=1,idl1
            do 1024 m=1,mp
            ch2(m,ik,l) = c2(m,ik,1)+ar1*c2(m,ik,2)
            ch2(m,ik,lc) = ai1*c2(m,ik,ip)
 1024       continue
  124    continue
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         do 126 j=3,ipph
            jc = ipp2-j
            ar2h = dc2*ar2-ds2*ai2
            ai2 = dc2*ai2+ds2*ar2
            ar2 = ar2h
            do 125 ik=1,idl1
               do 1025 m=1,mp
               ch2(m,ik,l) = ch2(m,ik,l)+ar2*c2(m,ik,j)
               ch2(m,ik,lc) = ch2(m,ik,lc)+ai2*c2(m,ik,jc)
 1025          continue
  125       continue
  126    continue
  127 continue
      do 129 j=2,ipph
         do 128 ik=1,idl1
            do 1028 m=1,mp
            ch2(m,ik,1) = ch2(m,ik,1)+c2(m,ik,j)
 1028       continue
  128    continue
  129 continue
c
      if (ido .lt. l1) go to 132
      do 131 k=1,l1
         do 130 i=1,ido
            do 1030 m=1,mp
            cc(m,i,1,k) = ch(m,i,k,1)
 1030       continue
  130    continue
  131 continue
      go to 135
  132 do 134 i=1,ido
         do 133 k=1,l1
            do 1033 m=1,mp
            cc(m,i,1,k) = ch(m,i,k,1)
 1033       continue
  133    continue
  134 continue
  135 do 137 j=2,ipph
         jc = ipp2-j
         j2 = j+j
         do 136 k=1,l1
            do 1036 m=1,mp
            cc(m,ido,j2-2,k) = ch(m,1,k,j)
            cc(m,1,j2-1,k) = ch(m,1,k,jc)
 1036       continue
  136    continue
  137 continue
      if (ido .eq. 1) return
      if (nbd .lt. l1) go to 141
      do 140 j=2,ipph
         jc = ipp2-j
         j2 = j+j
         do 139 k=1,l1
            do 138 i=3,ido,2
               ic = idp2-i
               do 1038 m=1,mp
               cc(m,i-1,j2-1,k) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)
               cc(m,ic-1,j2-2,k) = ch(m,i-1,k,j)-ch(m,i-1,k,jc)
               cc(m,i,j2-1,k) = ch(m,i,k,j)+ch(m,i,k,jc)
               cc(m,ic,j2-2,k) = ch(m,i,k,jc)-ch(m,i,k,j)
 1038          continue
  138       continue
  139    continue
  140 continue
      return
  141 do 144 j=2,ipph
         jc = ipp2-j
         j2 = j+j
         do 143 i=3,ido,2
            ic = idp2-i
            do 142 k=1,l1
               do 1042 m=1,mp
               cc(m,i-1,j2-1,k) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)
               cc(m,ic-1,j2-2,k) = ch(m,i-1,k,j)-ch(m,i-1,k,jc)
               cc(m,i,j2-1,k) = ch(m,i,k,j)+ch(m,i,k,jc)
               cc(m,ic,j2-2,k) = ch(m,i,k,jc)-ch(m,i,k,j)
 1042          continue
  142       continue
  143    continue
  144 continue
      return
      end
      function pimach(dum)
c***begin prologue  pimach
c
c     this subprogram supplies the value of the constant pi correct to
c     machine precision where
c
c     pi=3.1415926535897932384626433832795028841971693993751058209749446
c***routines called  (none)
c***end prologue  pimach
c
c***first executable statement  pimach
      pimach = 3.14159265358979
      return
      end




