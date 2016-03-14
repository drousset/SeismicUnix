c       i = nothing
c       call cancel(45,'testing')
c       write(*,*) 'nothing goes wrong'
c       stop
c       end
cccccccccc
c
c   job abend
c
c sample call:                                                                  
c     call cancel(inum,mesage)                                                  
c                                                                              
c input:                                                                        
c     inum - integer*4    error indicator                                       
c   mesage - string (less than 80 characters)      error message                
c                                                                               
c                                                                               
      subroutine cancel(inum,mesage)                                    
cccccccccccccccc                                                              
      integer*4 inum                                                         
      character*(*) mesage
c                                                                               
      write(*,*) ' !!! an error occurs, program cancelled  !!! '
      write(*,*) ' error code    = ', inum
      write(*,*) ' error message = ', mesage
      stop
      end

