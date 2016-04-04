c SUBROUTINE WXCOEF COMPUTES THE COEFFICIENTS FOR OPERATOR
      SUBROUTINE WXCOEF(DX,DZ,IORDER,ISTEP,NSPLIT,BBB,DDD)
      REAL DX,DZ,BBB(5)
      INTEGER ISTEP,NSPLIT,IORDER
      COMPLEX DDD(5)
CCCCC COMPUTE COEFFICIENTS FOR DIFFERENT ORDER OF MIGRATION
      IF ( IORDER .EQ. 0 ) THEN
CCCC 45-DEGREES
         NSPLIT = 1
         AL1 = 0.5
         BL1 = 0.25
         BBB(1) = (1./DX)**2.*BL1
         DDD(1) = CMPLX(0.,1.)*DZ*ISTEP*AL1/(2.*DX**2.)
      ELSE IF ( IORDER .EQ. 1 ) THEN
CCCC 65-DEGREES
         NSPLIT = 1
         AL1 = 0.47824060
         BL1 = 0.376369527
         BBB(1) = (1./DX)**2.*BL1
         DDD(1) = CMPLX(0.,1.)*DZ*ISTEP*AL1/(2.*DX**2.)
      ELSE IF ( IORDER .EQ. 2 ) THEN
CCCC 80-DEGREES
         NSPLIT = 2
         AL1 = 0.040315157
         BL1 = 0.873981642
         AL2 = 0.457289566
         BL2 = 0.222691983
         BBB(1) = (1./DX)**2.*BL1
         BBB(2) = (1./DX)**2.*BL2
         DDD(1) = (0.,1.)*DZ*ISTEP*AL1/(2.*DX**2.)
         DDD(2) = (0.,1.)*DZ*ISTEP*AL2/(2.*DX**2.)
      ELSE IF ( IORDER .EQ. 3 ) THEN
CCCC 87-DEGREES             
         NSPLIT = 3        
         AL1 = 0.004210420
         BL1 = 0.972926132
         AL2 = 0.081312882
         BL2 = 0.744418059
         AL3 = 0.414236605
         BL3 = 0.150843924
         BBB(1) = (1./DX)**2.*BL1
         BBB(2) = (1./DX)**2.*BL2
         BBB(3) = (1./DX)**2.*BL3
         DDD(1) = CMPLX(0.,1.)*DZ*ISTEP*AL1/(2.*DX**2.)
         DDD(2) = CMPLX(0.,1.)*DZ*ISTEP*AL2/(2.*DX**2.)
         DDD(3) = CMPLX(0.,1.)*DZ*ISTEP*AL3/(2.*DX**2.)
      ELSE IF ( IORDER .EQ. 4 ) THEN
CCCC 90-DEGREES
         NSPLIT = 4
         AL1 = 0.000523275
         BL1 = 0.994065088
         AL2 = 0.014853510
         BL2 = 0.919432661
         AL3 = 0.117592008
         BL3 = 0.614520676
         AL4 = 0.367013245
         BL4 = 0.105756624
         BBB(1) = (1./DX)**2.*BL1
         BBB(2) = (1./DX)**2.*BL2
         BBB(3) = (1./DX)**2.*BL3
         BBB(4) = (1./DX)**2.*BL4
         DDD(1) = CMPLX(0.,1.)*DZ*ISTEP*AL1/(2.*DX**2.)
         DDD(2) = CMPLX(0.,1.)*DZ*ISTEP*AL2/(2.*DX**2.)
         DDD(3) = CMPLX(0.,1.)*DZ*ISTEP*AL3/(2.*DX**2.)
         DDD(4) = CMPLX(0.,1.)*DZ*ISTEP*AL4/(2.*DX**2.)
      ELSE IF ( IORDER .EQ. 5 ) THEN
CCCC 90-DEGREES
         NSPLIT = 5
         AL1 = 0.000153427
         BL1 = 0.997370236
         AL2 = 0.004172967
         BL2 = 0.964827992
         AL3 = 0.033860918
         BL3 = 0.824918565
         AL4 = 0.143798076
         BL4 = 0.483340757
         AL5 = 0.318013812
         BL5 = 0.073588213
         BBB(1) = (1./DX)**2.*BL1
         BBB(2) = (1./DX)**2.*BL2
         BBB(3) = (1./DX)**2.*BL3
         BBB(4) = (1./DX)**2.*BL4
         BBB(5) = (1./DX)**2.*BL5
         DDD(1) = CMPLX(0.,1.)*DZ*ISTEP*AL1/(2.*DX**2.)
         DDD(2) = CMPLX(0.,1.)*DZ*ISTEP*AL2/(2.*DX**2.)
         DDD(3) = CMPLX(0.,1.)*DZ*ISTEP*AL3/(2.*DX**2.)
         DDD(4) = CMPLX(0.,1.)*DZ*ISTEP*AL4/(2.*DX**2.)
         DDD(5) = CMPLX(0.,1.)*DZ*ISTEP*AL5/(2.*DX**2.)
      END IF
      RETURN
      END
