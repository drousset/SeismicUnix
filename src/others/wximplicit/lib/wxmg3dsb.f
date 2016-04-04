CCCCCCC compensating for f.d errors                      
C                                                                               
C      WX90MG ----   3-D W-X 90 DEGREE MIGRATION
C                                                                               
C   AUTHORS:    ZHIMING LI                                                      
C   CONTRIBUTORS:     CHUNLING LIU                                              
C   INPUT:                                                                      
C        Q(NT,NX,NY) ---   REAL*4   INPUT STACK SECTION
C              NT ---   INT*4       TRACE LENGTH IN SAMPLE                      
C              NX ---   INT*4       NUMBER OF TRACES                            
C              NY ---   INT*4       NUMBER OF LINES
C              NZ ---   INT*4       NUMBER OF DEPTH STEPS
C              DT ---   REAL*4      SAMPLING INTERVAL                           
C              DX ---   REAL*4      TRACE SPACING                               
C              DY ---   REAL*4      LINE SPACING                               
C              DZ ---   REAL*4      DEPTH SPACING                               
C          IVDISK ---   INT*4       VELOCITY DISK UNIT                          
C                                   0 = ARRAY V GIVEN AS (NX,NY,NZ) ARRAY
C                                   OTHERWISE = VELOCITY STORED AT DISK         
C                                   AS NZ*NY VECTORS OF NX LONG
C                                   (FILE NAME IS GIVEN BY VNAME)
C          VNAME  ---   CHAR*       VELOCITY DISK FILE NAME 
C        V(NX,NY,NZ) ---   REAL*4   VELOCITY (POSTSTACK VTRUE/2.)
C    CP(NX,NY,NFFTQ) ---   CPLX*8   WORKING ARRAY                  
C          IPDISK ---   INT*4       WORK DISK FORTRAN UNIT FOR ARRAY CP         
C                                   0 --- CP IS GIVEN                           
C                                   OTHERWISE --- DISK USED (CP NOT GIVEN)
C            PARA ---   REAL*4      DIP-FILTERING PARAMETER (0. TO 1.)          
C          IQDISK ---   INT*4       WORK DISK FORTRAN UNIT FOR ARRAY Q          
C                                   0 --- Q(NT,NX,NY) IS GIVEN
C                                   OTHERWISE --- DISK USED TO STORED           
C                                   INPUT SECTION (NX*NY VECTORS OF NT
C                                   LONG                               
C          QNAME  ---   CHAR*       DISK FILE NAME SPECIFIED BY IQDISK 
C          ITORD  ---   INT*4       DUMMY VARIABLE
C           WORK  ---   CPLX*8      WORK ARRAY OF LENGTH LWORK                  
C          LWORK  ---   INT*4       LENGTH OF WORK                              
C                                   LWORK=MAX0((NFFT*3/2+1+KXY),KXY*5))
C                                      WHERE
C                                           KXY=MAX0(NX,NY)  IF NO DISK USED
C                                           KXY=NX*NY        DISK USED
C           NFFT  ---   INT*4       LENGTH OF FFT OVER TIME                     
C           NFFTQ ---   INT*4       NFFT/2+1
C          ISTEP  ---   INT*4       DEPTH STEP OF EXTRAPOLATION                 
C    VX,QX,QX1,VOV --- REAL*4       WORKING ARRAYS OF LENGTH MXY (MAX0(NX,NY))
C    QT            --- REAL*4       WORKING ARRAYS OF LENGTH MAX0(NT,MXY)
C    A,B,AA,BB,CP ---   CMPX*8      WORKING ARRAYS OF LENGTH MXY
C         IORDER  ---   INT*4       ORDER OF MIGRATION                          
C                                   0=45 DEGREE                                 
C                                   1=65 DEGREE                                 
C                                   2=80 DEGREE                                 
C                                   3=87 DEGREE                                 
C                                   4=89.99999 DEGREE                           
C                                   5=90 DEGREE                                 
C    ICSTEP       ----  INT*4       DEPTH STEP TO DO F.D. ERROR COMPENSATION
C                                   (0=NO COMPENSATION TO BE DONE)
C    BCP(NXP,NYP) ----  CMPX*8      WORKING ARRAY WHEN ICSTEP >0 (NOT NEEDED
C				    WHEN ICSTEP=0)
C    NXP          ----  INT*4       SIZE OF 1ST DIMENSION OF BCP (MUST
C                                   BE LARGER OR EQUAL TO NX. 
C                                   NXP = NX PADDED WITH ZERO'S TO AVOID 
C                                   WRAPAROUND)
C    NYP          ----  INT*4       SIZE OF 2ND DIMENSION OF BCP (MUST
C                                   BE LARGER OR EQUAL TO NY. 
C                                   NYP = NY PADDED WITH ZERO'S TO AVOID 
C                                   WRAPAROUND)
C    CCP(NFFTQ,NXY) ---- CMPX*8     WORKING ARRAY IF IPDISK>0 (NOT NEEDED IF
C				    IPDISK=0)
C    NXY          ----  INT*4       SIZE OF 2ND DIMENSION OF CCP (MAX0(NX,NY))
C
C  OUTPUT:                                                                      
C       QQ(NX,NY,NZ) ---   REAL*4   MIGRATED SECTION TRANSPOSED                 
C                                   IF IQDISK IS NOT 0, DISK IQDISK IS          
C                                   USED TO STORE MIGRATED SECTION              
C                                   TRANSPOSED (NZ*NY VECTORS OF NX LONG)
C                                   QQ SHOULD SHARE THE SAME MEMORY
C                                   LOCATION AS Q TO REDUCE EXCESSIVE
C                                   USE OF MEMORY
C                                                                               
C                                                                               
      SUBROUTINE wxmg3d(Q,NT,NX,DT,DX,V,CP,WORK,PARA,LWORK,
     *        FMIN,FMAX,IPDISK,NFFT,ISTEP,IVDISK,IQDISK,VX,QT,QX,QX1,           
     *        VOV,AA,BB,ITORD,A,B,IORDER,QNAME,VNAME,NFFTQ,QQ,NZ,DZ,
     *        NY,DY,ICSTEP,BCP,NXP,NYP,CCP,NXY)
      REAL Q(NT,NX,NY),DW,DT,V(NX,NY,NZ),DX,PI,W,TRICK,PARA
      REAL QQ(NX,NY,NZ),DZ                     
      COMPLEX AA(*),BB(*),WORK(LWORK)                                         
      COMPLEX A1,A2,B1,B2,ENDL,ENDR                                             
      COMPLEX A(*),B(*),CSHIFT                                                
      INTEGER NT,NX,IT,IW,IX,KW,IZ,IPDISK,LWORK,ISTEP,NZ
      INTEGER IVDISK,IQDISK,ITORD                                               
      COMPLEX CP2,CP3,CPNM2,CPNM1,CA,CB,CCC1,CCC2,CTMP,BPA,BMA
      COMPLEX CSH1,CSH2,CCX,CCY
      COMPLEX DDX(5),DDY(5)
      COMPLEX CP(NX,NY,NFFTQ)
      COMPLEX BCP(NXP,NYP),CCP(NFFTQ,NXY)
      INTEGER NXP,NYP
      REAL VX(*),QT(*),QX(*),QX1(*),VOV(*)                                 
      REAL AAA,BBX(5),BBY(5)
      CHARACTER*(*) VNAME,QNAME
      real alpha(5), beta(5)
cccc
      KXY = MAX0(NX,NY)
      IIDISK = IVDISK + IPDISK + IQDISK 
      IF ( IIDISK .EQ. 0 ) THEN
         IF ( LWORK.LT.MAX0((NFFT*3/2+1+KXY),KXY*5)) THEN
            CALL CANCEL(LWORK,'LWORK TOO SHORT IN WX90MG')
         END IF                                           
      ELSE
        IF ( LWORK.LT.MAX0((NFFT*3/2+1+NX*NY),NX*NY*5)) THEN
            CALL CANCEL(LWORK,'LWORK TOO SHORT IN WX90MG')
         END IF                                           
      END IF                         
      DZN = -DZ 
      DZC = DZN * ICSTEP
      PI = 3.141592654                                                          
      TRICK = 0.14                                                              
      DW = 2.*PI/(NFFT*DT)
cccccc 
      if (ICSTEP.GT.0) PARA=0. 
      AAA = -(8.*PARA*DT)/PI                                                    
      CCX = - cmplx(0.,1.)*AAA/(2.*DX*DX)              
      CCY = - cmplx(0.,1.)*AAA/(2.*DY*DY)                    
CCCCC COMPUTE COEFFICIENTS FOR DIFFERENT ORDER OF MIGRATION                     
      CALL WXCOEF(DX,DZ,IORDER,ISTEP,NSPLIT,BBX,DDX)
      CALL WXCOEF(DY,DZ,IORDER,ISTEP,NSPLIT,BBY,DDY)
ccccc
      call alpbe(IORDER,alpha,beta,NSPLIT)
      CSH1 = CMPLX(0.,1.)*DZ*ISTEP
      CSH2 = CMPLX(0.,1.)*DZ 
      TMP = NT                                                                  
      SCALE = 2./TMP
      WMIN = FMIN*2.*PI/DW + 1.                                                 
      WMAX = FMAX*2.*PI/DW + 1.                                                 
      IW0 = WMIN                                                                
      IWN = WMAX                                                                
      IF ( IW0 .LT. 3 ) IW0 = 3                                                 
      IF ( IWN .LT. 3 ) IWN = 3                                                 
      IF ( IW0 .GE. NFFTQ) IW0 = NFFTQ - 1          
      IF ( IWN .GE. NFFTQ) IWN = NFFTQ - 1         
      IRECL = 2 * NFFTQ * 4
C POINTER POSITIONS
      IPCPP = 1                                                                 
      IPCE = IPCPP + KXY                                                        
      IPCX = IPCE + KXY                                                         
      IPCD = IPCX + KXY                                                         
      IPCF = IPCD + KXY
      IPW = 1                                                                   
      IPWK = IPW + NFFT                                                         
      IPCPP0 = IPCPP - 1                                                        
      IPCX0 = IPCX - 1                                                          
      IPCD0 = IPCD - 1
      LWK = LWORK - NFFT                                                        
CCCC  FFT OF INPUT SECTION                                        
C     
      IF ( IQDISK .NE. 0 ) THEN                                                 
         IRECLQ = NT * 4
C         CALL OPDISK(IQDISK,IRECLQ,IOS,QNAME,'DELETE','OLD')
C         IF (IOS.NE.0) CALL CANCEL(IOS,'ERROR OPDISK IQDISK')
      END IF                                                                    
      IF ( IPDISK .NE. 0 ) THEN                                                 
         IRECL = NFFTQ * 2 * 4
         CALL OPDISK(IPDISK,IRECL,IOS,'TEMP','DELETE','NEW')
         IF (IOS.NE.0) CALL CANCEL(IOS,'ERROR OPDISK IPDISK')
      END IF                                                                    
      DO 23016 IY = 1,NY                                                     
      DO 23016 IX = 1,NX                                                     
         IF ( IQDISK .EQ. 0 ) THEN                                           
            DO 23018 IT = 1,NT                                               
               WORK(IT) = CMPLX(Q(IT,IX,IY),0.)                                 
23018       CONTINUE                                                         
            DO 21018 IT = 1,NT                                               
               Q(IT,IX,IY) = 0.                                                 
21018       CONTINUE
         ELSE
            IREC = IX + (IY-1)*NX
            CALL REDISK(IQDISK,QT,IREC,IRECLQ,IOS)                             
            DO 53018 IT=1,NT                                                 
53018       WORK(IT) = CMPLX(QT(IT),0.)                                         
         END IF                                                              
         DO 23017 IT=NT+1,NFFT                                               
23017    WORK(IT) = (0.,0.)                                                  
         call four1(NFFT,WORK,1.)
         IF ( IPDISK .NE. 0 ) THEN         
	    IREC = IX + (IY-1)*NX
            CALL RIDISK(IPDISK,WORK,IREC,IRECL,IOS)         
         ELSE
            DO 33027 IW=1,NFFTQ                                              
33027       CP(IX,IY,IW) = WORK(IW)
         END IF                                                   
23016 CONTINUE                                                               
CCCCC VELOCITY DISK                                                             
      IF ( IVDISK .NE. 0 ) THEN                                                 
         IRECLV = NX * NY * 4
C         CALL OPDISK(IVDISK,IRECLV,IOS,VNAME,'KEEP','OLD')
C         IF (IOS.NE.0) CALL CANCEL(IOS,'ERROR OPDISK IVDISK')
      END IF                                                                    
CCCCC DISK TO STORE MIGRATION SECTION TRANSPOSED                                
      IF ( IQDISK .NE. 0 ) THEN                                                 
         CLOSE(IQDISK)
         IRECLQ = NX * NY * 4
         CALL OPDISK(IQDISK,IRECLQ,IOS,QNAME,'DELETE','NEW')
         IF ( IOS .NE. 0 ) CALL CANCEL(IOS,'ERROR OPDISK IQDISK')
         CALL ZEROAL(QQ,NX*NY,4)
         CALL RIDISK(IQDISK,QQ,1,IRECLQ,IOS)                                    
      END IF
CCCCC MIGRATION OVER DEPTH STEP                                                 
      DO 63022 IZ = 2,NZ                                                      
         ISEE = MOD((IZ-1),ISTEP)
	 ICOM = 99999
	 IF ( ICSTEP.GT.0 .AND. ISEE.EQ.0 ) ICOM = MOD((IZ-1),ICSTEP)
         N1 = NX
         N2 = NY
         IZV = IZ
         IZQ = IZ
         IZQ1= IZ
         IF ( IVDISK .NE. 0 ) IZV = 1
         IF ( IQDISK .NE. 0 ) IZQ = 2
         IF ( IQDISK .NE. 0 ) IZQ1 = 1
         IF ( IVDISK .NE. 0 ) CALL REDISK(IVDISK,V,IZ,IRECLV,IOS)
         IF ( IQDISK .NE. 0 ) CALL REDISK(IQDISK,QQ,IZ-1,IRECLQ,IOS)
	 dzcc = DZC
	 ncc = NSPLIT
	 igo = 1
	 if (ICSTEP.GT.0 .AND. IZ.EQ.2 .AND. ICOM.NE.0) then
cccc filter out
	    dzcc = 0.
	    ncc = 0
	    igo = 0
	 end if
ccccccc  error compensation
	 IF ( ICOM .NE. 0 .and. igo .eq. 1 ) GOTO 88888 
	 VA = 0.
	 DO 77770 IY=1,NY
	 DO 77770 IX=1,NX
77770    VA = VA + V(IX,IY,IZV)
	 VA = VA/NX/NY
C TRANSPOSE CP IF NEEDED
	 IF ( IPDISK .NE. 0 ) THEN
	    IPPDSK = 96
	    IPWK = 1
	    LWK = LWORK
	    M1 = NFFTQ
	    M2 = NX*NY
	    CALL DSKMS(M1,M2,LWK,IPPDSK,'TEMP2','SCRATCH',IOS,8)
	    DO 11110 IX=1,M2
	       CALL REDISK(IPDISK,CCP,IX,IRECL,IOS)
	       CALL DSKMI(CCP,CP,WORK(IPWK),M1,M2,LWK,8)
11110       CONTINUE
	    CLOSE(IPDISK)
         END IF
	 DO 77771 IW=IW0,IWN
	    W = (IW-1)*DW
            IF(IPDISK.NE.0) CALL REDISK(IPPDSK,CP,IW,M2*8,IOS)
	    IWP = IW
	    IF (IPDISK.NE.0) IWP=1
	    CALL ZEROAL(BCP,NXP*NYP,8)
	    DO 77789 IY=1,NY
	    DO 77789 IX=1,NX
77789       BCP(IX,IY) = CP(IX,IY,IWP)
	    CALL errcom(BCP,NXP,NYP,DX,DY,W,dzcc,VA,A,
     *            	ncc,alpha,beta,TRICK)
	    DO 77799 IY=1,NY
	    DO 77799 IX=1,NX
77799       CP(IX,IY,IWP) = BCP(IX,IY)
            IF(IPDISK.NE.0) CALL RIDISK(IPPDSK,CP,IW,M2*8,IOS)
77771    CONTINUE 
	 IF ( IPDISK .NE. 0 ) THEN
	    IPWK = 1
	    LWK = LWORK
	    M2 = NFFTQ
	    M1 = NX*NY
	    CALL DSKMS(M1,M2,LWK,IPDISK,'TEMP','SCRATCH',IOS,8)
	    DO 11120 IW=1,M2
	       CALL REDISK(IPPDSK,CP,IW,M1*8,IOS)
	       CALL DSKMI(CP,CCP,WORK(IPWK),M1,M2,LWK,8)
11120       CONTINUE
	    CLOSE(IPPDSK)
         END IF  
88888    CONTINUE
         IDIR = 0
30000    IDIR = IDIR + 1
         IF ( IDIR .EQ. 2 ) THEN
            N1 = NY
            N2 = NX 
         END IF
	 IMA = 0
	 IF ( IDIR .EQ. 2 .OR. (IDIR.EQ.1 .AND. NY.lE.2)
     &        .OR. ISEE .NE. 0 ) IMA=1
         I2 = 0
45000    I2 = I2 + 1
         IF (IPDISK .NE. 0 ) THEN
	    IF (IDIR.EQ.1) THEN
	       DO 22 IX=1,NX
		  IREC = (I2-1)*NX+IX
		  CALL REDISK(IPDISK,CCP(1,IX),IREC,IRECL,IOS)
22             CONTINUE
	    ELSE
	       DO 33 IY=1,NY
		  IREC = (IY-1)*NX+I2
		  CALL REDISK(IPDISK,CCP(1,IY),IREC,IRECL,IOS)
33             CONTINUE
            END IF
         END IF
         IF ( IDIR .EQ. 1 ) THEN
            DO 50010 I1=1,N1
50010       VX(I1) = V(I1,I2,IZV)
         ELSE
            DO 50011 I1=1,N1
50011       VX(I1) = V(I2,I1,IZV)
         END IF                                                 
         V1 = VX(1)
         VN = VX(N1)
         IF ( ISEE .EQ. 0 ) THEN
            DO 23011 I1 = 1,N1
               VOV(I1) = 1./VX(I1)
               QT(I1) = VX(I1)*VX(I1)
23011       CONTINUE
         END IF                                                            
	 ISUB = 0
	 IF ( ISEE .NE. 0 .OR. IPDISK .EQ. 0 ) GOTO 77077
         IF ( IDIR .EQ. 1 .AND. IMA .EQ. 1  ) THEN
	    ISUB = 1
            DO 50020 I1=1,N1     
50020       QX1(I1) = QQ(I1,I2,IZQ-1)
         ELSE IF (IDIR .EQ. 2 .AND. IMA .EQ. 1 ) THEN
            DO 50021 I1=1,N1     
50021       QX1(I1) = QQ(I2,I1,IZQ-1)
         END IF 
77077    CONTINUE
         CALL ZEROAL(QX,N1,4)
         DO 63024 IW = IW0,IWN
            KW = IW-1                                                           
            W=KW*DW
CCCCCCCCC   IF ( W.EQ.0. ) W = 0.001*DW                                         
            ISPLIT = 0
            IWP = IW
            IF ( IPDISK .NE. 0 ) IWP = 1
10000       ISPLIT = ISPLIT + 1                                                 
            IF ( ISPLIT .EQ. 1 ) THEN                                           
               IF ( IPDISK.NE.0) THEN
	          IF (IDIR.EQ.1) THEN
	             DO 44 I1=1,N1
44                   CP(I1,I2,IWP) = CCP(IW,I1)
	          ELSE
	             DO 55 I1=1,N1
55                   CP(I2,I1,IWP) = CCP(IW,I1)
                  END IF
	       END IF
               IF ( IDIR .EQ. 1 ) THEN
                  DO 23020 I1=1,N1           
23020             WORK(IPCPP0+I1) = CP(I1,I2,IWP)                       
               ELSE
                  DO 23021 I1=1,N1           
23021             WORK(IPCPP0+I1) = CP(I2,I1,IWP)                       
               END IF
               IF ( IPDISK .NE. 0 .AND. ISUB .EQ. 1 ) THEN
                  DO 33038 I1=1,N1                                              
                     WORK(IPCPP0+I1)=WORK(IPCPP0+I1)-QX1(I1)                    
33038             CONTINUE                                                      
               END IF                                                           
            END IF                                                              
            IF ( ISEE .NE. 0 ) GOTO 40000                                       
            CP2 = WORK(IPCPP0+2)                                                
            CP3 = WORK(IPCPP0+3)                                                
            CPNM1 = WORK(IPCPP0+N1-1)                                           
            CPNM2 = WORK(IPCPP0+N1-2)                                           
            A1=2.*CP2*CONJG(CP3)                                                
            B1= CP2*CONJG(CP2)+CP3*CONJG(CP3)                                   
            IF(B1.EQ.(0.,0.)) THEN                                              
CCCCC          A1=(0.,0.)                                                       
               A1=CEXP(CMPLX(0.,-W*DX*.5/V1))
            ELSE                                                                
               A1=A1/B1                                                         
            END IF                                                              
CCCCC       IF(AIMAG(A1).GT.0.) A1=CONJG(A1)                                    
            IF(AIMAG(A1).GT.0.) A1=CEXP(CMPLX(0.,-W*DX*.5/V1))
            A2=2.*CPNM1*CONJG(CPNM2)                                            
            B2= CPNM1*CONJG(CPNM1)+CPNM2*CONJG(CPNM2)                           
            IF(B2.EQ.(0.,0.)) THEN                                              
CCCCC          A2=(0.,0.)                                                       
               A2=CEXP(CMPLX(0.,-W*DX*.5/VN))
            ELSE                                                                
               A2=A2/B2                                                         
            END IF                                                              
CCCCC       IF (AIMAG(A2).GT.0.) A2=CONJG(A2)                                   
            IF (AIMAG(A2).GT.0.) A2=CEXP(CMPLX(0.,-W*DX*.5/VN))
CCCCCC ZERO SLOPE A1=A2=1           ZERO VALUE A1=A2=0                          
C           A1 = (0.,0.)                                                        
C           A2 = (0.,0.)                                                        
            RW = 1./W
            RW2 = RW * RW
            IF ( IDIR .EQ. 1 ) THEN
               CCC1 = RW*DDX(ISPLIT)
               CCC2 = RW2*BBX(ISPLIT) + RW*CCX
            ELSE
               CCC1 = RW*DDY(ISPLIT)
               CCC2 = RW2*BBY(ISPLIT) + RW*CCY
            END IF
            DO 23033 I1 = 1,N1                                                  
               CA = VX(I1)*CCC1
               CB =  QT(I1)*CCC2 + TRICK
               BMA = CB - CA
               A(I1) = BMA                                                    
               B(I1) = 1. - BMA - BMA
               BPA = CB + CA                                           
               AA(I1) = BPA                                                   
               BB(I1) = 1. - BPA - BPA
23033       CONTINUE                                                            
            DO 23034 I1 = 2,N1-1                                                
               WORK(IPCD0+I1) = A(I1+1)*WORK(IPCPP0+I1+1)                       
     &                         +B(I1)*WORK(IPCPP0+I1)+                          
     &                          A(I1-1)*WORK(IPCPP0+I1-1)                       
23034       CONTINUE                                                            
            WORK(IPCD0+1) = A(2)*WORK(IPCPP0+2)                                 
     &                     +(B(1)+A(1)*A1)*WORK(IPCPP0+1)                       
            WORK(IPCD0+N1) = A(N1-1)*WORK(IPCPP0+N1-1)                          
     &                     +(B(N1)+A(N1)*A2)*WORK(IPCPP0+N1)                    
            ENDL=AA(1)*A1+BB(1)                                                 
            ENDR=AA(N1)*A2+BB(N1)                                               
C STABLE EXTRAPOLATION                                                          
            DO 39999 I1=2,N1-1                                                  
               A(I1) = AA(I1+1)                                                 
               B(I1) = AA(I1-1)                                                 
39999       CONTINUE                                                            
            A(1) = AA(2)
            B(N1) = AA(N1-1)
            BB(1) = ENDL
            BB(N1) = ENDR                                                    
            CALL CTRIS(N1,A,BB,B,
     &                 WORK(IPCD),WORK(IPCX),WORK(IPCE),WORK(IPCF))             
            IF ( ISPLIT .LT. NSPLIT ) THEN                                      
               DO 53021 I1=1,N1                                                 
53021          WORK(IPCPP0+I1) = WORK(IPCX0+I1)                                 
               GOTO 10000                                                       
            END IF                                                              
40000       CONTINUE                                                            
            IF ( ISEE .EQ. 0 ) THEN
               IF ( IDIR .EQ. 1 ) THEN
                  CTMP = W * CSH1 
                  DO 23036 I1 = 1,N1
                     CSHIFT = CEXP(-CTMP*VOV(I1))                    
                     WORK(IPCPP0+I1) = WORK(IPCX0+I1)*CSHIFT
                     CP(I1,I2,IWP) = WORK(IPCPP0+I1)
23036             CONTINUE  
                  IF (IMA .EQ. 1 ) THEN
	             DO 77012 I1=1,N1
77012                QX(I1) = QX(I1)+REAL(WORK(IPCPP0+I1))
	          END IF		
               ELSE
                  DO 33021 I1=1,N1
                     WORK(IPCPP0+I1) = WORK(IPCX0+I1)
                     QX(I1) = QX(I1)+REAL(WORK(IPCPP0+I1))
33021             CP(I2,I1,IWP) = WORK(IPCPP0+I1)
               END IF                                    
               IF(IPDISK.NE.0) THEN
	          IF (IDIR.EQ.1) THEN
	             DO 66 I1=1,N1
66                   CCP(IW,I1) = CP(I1,I2,IWP)
	          ELSE
	             DO 77 I1=1,N1
77                   CCP(IW,I1) = CP(I2,I1,IWP)
                  END IF
	       END IF
            ELSE
               IF (IDIR.EQ.1) THEN
                  CTMP = W * CSH2 * ISEE 
                  DO 33036 I1 = 1,N1 
                     CSHIFT = CEXP(-CTMP*VOV(I1))                     
                     WORK(IPCPP0+I1) = WORK(IPCPP0+I1)*CSHIFT         
                     QX(I1) = QX(I1)+REAL(WORK(IPCPP0+I1))                    
33036             CONTINUE                                              
               END IF
            END IF 
CCCCCCCCCC  WRITE(*,*) 'MIGRATION DONE FOR FREQUENCY =',W*2.*PI,' HZ'           
63024    CONTINUE                                                               
         IF ( ISEE .EQ. 0 .AND. IPDISK .NE. 0 ) THEN
	    IF (IDIR.EQ.1) THEN
	       DO 88 IX=1,NX
		  IREC = (I2-1)*NX+IX
		  CALL RIDISK(IPDISK,CCP(1,IX),IREC,IRECL,IOS)
88             CONTINUE
	    ELSE
	       DO 99 IY=1,NY
		  IREC = (IY-1)*NX+I2
		  CALL RIDISK(IPDISK,CCP(1,IY),IREC,IRECL,IOS)
99             CONTINUE
            END IF
	 END IF
         IF ( IMA .EQ. 1 ) THEN
            DO 23025 I1=1,N1     
23025       QX(I1) = QX(I1)*SCALE                    
         ELSE
	    GOTO 77788
         END IF
	 IF ( ISEE .NE. 0 ) GOTO 77788
         IF ( IPDISK.EQ.0 .AND. ISEE.EQ.0 .AND. IMA.EQ.1  ) THEN
            DO 23040 IW=IW0,IWN
               IF ( IDIR .EQ. 1 ) THEN
                  DO 23038 I1=1,N1                  
                     CP(I1,I2,IW)=CP(I1,I2,IW)-CMPLX(QX(I1),0.)                 
c                     CP(I1,I2,IW)=CP(I1,I2,IW)                 
23038             CONTINUE                                             
               ELSE
                  DO 23039 I1=1,N1                  
                     CP(I2,I1,IW)=CP(I2,I1,IW)-CMPLX(QX(I1),0.)                 
c                     CP(I2,I1,IW)=CP(I2,I1,IW)                 
23039             CONTINUE                                             
               END IF
23040       CONTINUE                                                            
         END IF                                                                 
77788    CONTINUE
         IF ( IDIR.EQ.1 .AND. IMA .EQ. 1) THEN
            DO 50040 I1=1,N1  
50040       QQ(I1,I2,IZQ1) = QX(I1)
         ELSE IF (IDIR .EQ. 2 ) THEN
            DO 50041 I1=1,N1  
50041       QQ(I2,I1,IZQ1) = QX(I1)
         END IF
         IF(IQDISK.NE.0.AND.IMA.EQ.1)
     &   	 CALL RIDISK(IQDISK,QQ,IZ,IRECLQ,IOS)       
         IF ( I2.LT.N2) GOTO 45000
         IF ( IDIR.LT.2 .AND. NY.GT.2 .AND. ISEE.EQ.0 )GOTO 30000
C         WRITE(*,*) 'MIGRATION DONE FOR IZ=',IZ                             
         iwrite = mod(IZ,10)
	 if (iwrite .eq. 0 ) then
	    fiz = IZ
	    call msgsc('MIGRATION DONE FOR IZ= ',fiz)
         end if
63022 CONTINUE                                                                  
      IF (IPDISK.NE.0) CLOSE(IPDISK)
      IF (IVDISK.NE.0) CLOSE(IVDISK)
      RETURN                                                                    
      END 
