C       QUICKSORT FROM NUMERICAL RECIPIES
C       MODIFIED TO GENERATE INDICIES
C       TWICE AS FAST AS HEAP SORT AND TEN TIMES BUBBLE SORT
      SUBROUTINE QCKSTI(N,ARR,INDX)
      PARAMETER (M=7,NSTACK=500,FM=7875.,FA=211.,FC=1663.
     *    ,FMI=1.2698413E-4)
      DIMENSION ARR(N),INDX(N),ISTACK(NSTACK)
      DO IA=1,N
        INDX(IA)=IA-1
      ENDDO
      JSTACK=0
      L=1
      IR=N
      FX=0.
10    IF(IR-L.LT.M)THEN
        DO 13 J=L+1,IR
          A=ARR(J)
          IA=INDX(J)
          DO 11 I=J-1,1,-1
            IF(ARR(I).LE.A)GO TO 12
            ARR(I+1)=ARR(I)
            INDX(I+1)=INDX(I)
11        CONTINUE
          I=0
12        ARR(I+1)=A
          INDX(I+1)=IA
13      CONTINUE
        IF(JSTACK.EQ.0)RETURN
        IR=ISTACK(JSTACK)
        L=ISTACK(JSTACK-1)
        JSTACK=JSTACK-2
      ELSE
        I=L        
        J=IR
        FX=MOD(FX*FA+FC,FM)
        IQ=L+(IR-L+1)*(FX*FMI)
        A=ARR(IQ)  
        IA=INDX(IQ)
        ARR(IQ)=ARR(L)
        INDX(IQ)=INDX(L)
20      CONTINUE
21        IF(J.GT.0)THEN
            IF(A.LT.ARR(J))THEN
              J=J-1
              GO TO 21
            ENDIF
          ENDIF    
          IF(J.LE.I)THEN
            ARR(I)=A
            INDX(I)=IA
            GO TO 30
          ENDIF    
          ARR(I)=ARR(J)
          INDX(I)=INDX(J)
          I=I+1    
22        IF(I.LE.N)THEN
            IF(A.GT.ARR(I))THEN
              I=I+1
              GO TO 22
            ENDIF  
          ENDIF    
          IF(J.LE.I)THEN
            ARR(J)=A
            INDX(J)=IA
            I=J    
            GO TO 30
          ENDIF    
          ARR(J)=ARR(I)
          INDX(J)=INDX(I)
          J=J-1    
        GO TO 20   
30      JSTACK=JSTACK+2
        IF(JSTACK.GT.NSTACK)PAUSE 'NSTACK must be made larger.'
        IF(IR-I.GE.I-L)THEN
          ISTACK(JSTACK)=IR
          ISTACK(JSTACK-1)=I+1
          IR=I-1   
        ELSE       
          ISTACK(JSTACK)=I-1
          ISTACK(JSTACK-1)=L
          L=I+1    
        ENDIF      
      ENDIF        
      GO TO 10     
      END          
