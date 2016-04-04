c       i = nothing
c       CALL CANCEL(45,'TESTING')
c       write(*,*) 'nothing goes wrong'
c       stop
c       end
cccccccccc
c
c   job abend
c
c SAMPLE CALL:                                                                  
c     CALL CANCEL(INUM,MESAGE)                                                  
c                                                                              
c INPUT:                                                                        
c     INUM - INTEGER*4    ERROR INDICATOR                                       
c   MESAGE - STRING (LESS THAN 80 CHARACTERS)      ERROR MESSAGE                
c                                                                               
c                                                                               
      SUBROUTINE CANCEL(INUM,MESAGE)                                    
cccccccccccccccc                                                              
      INTEGER*4 INUM                                                         
      CHARACTER*(*) MESAGE
c                                                                               
      WRITE(*,*) ' !!! AN ERROR OCCURS, PROGRAM CANCELLED  !!! '
      WRITE(*,*) ' ERROR CODE    = ', INUM
      WRITE(*,*) ' ERROR MESSAGE = ', MESAGE
      STOP
      END

