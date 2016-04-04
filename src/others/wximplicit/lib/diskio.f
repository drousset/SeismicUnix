c       parameter(n=10,iunit=20,irecl=40,irec=1)
c       real f(n)
c       do 10 i=1,n
c         f(i) = i
c 10     continue 
c       call opdisk(iunit,irecl,ios,'junk.dat','DELETE','unknown')
c       call ridisk(iunit,f,irec,irecl,ios)
c       do 20 i=1,n
c 20        f(i) = 0.
c       call redisk(iunit,f,irec,irecl,ios)
c       write(*,*) (f(i),i=1,n)
c       stop
c       end
c
c
       SUBROUTINE OPDISK(IUNIT,LBYTE,IOS,FNAME,DISPS,STATS)
       INTEGER LBYTE,IUNIT,IOS
       CHARACTER*(*) FNAME,DISPS,STATS
       LD = lenstr(DISPS)
       LF = lenstr(FNAME)
       LS = lenstr(STATS)
       IF ( DISPS(1:1) .NE. 'D' .AND. DISPS(1:1) .NE. 'd' ) THEN
          OPEN(UNIT=IUNIT,FILE=FNAME(1:LF),FORM='UNFORMATTED',
     &         STATUS=STATS(1:LS),ACCESS='DIRECT',RECL=LBYTE,
     &         IOSTAT=IOS)
       ELSE
          OPEN(UNIT=IUNIT,FORM='UNFORMATTED',
     &         STATUS='SCRATCH',ACCESS='DIRECT',RECL=LBYTE,
     &         IOSTAT=IOS)
       END IF
       RETURN
       END
       SUBROUTINE RIDISK(IUNIT,F,IREC,LBYTE,IOS)
       INTEGER LBYTE,IUNIT,IREC,IOS
       CHARACTER F(LBYTE)
       WRITE(IUNIT,REC=IREC,IOSTAT=IOS) F
       RETURN
       END
       SUBROUTINE REDISK(IUNIT,F,IREC,LBYTE,IOS)
       INTEGER LBYTE,IUNIT,IREC,IOS
       CHARACTER F(LBYTE)
       READ(IUNIT,REC=IREC,IOSTAT=IOS) F
       RETURN
       END
