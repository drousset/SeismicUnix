cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc     
c       character*12 name
c       integer lname
c       integer lenstr
c       name = 'zhiming-li'
c       lname = lenstr(name)
c       write(*,*) 'string=',name,'  stringlen=',lname
c       lname = lenstr(' zhiming-li   ')
c       write(*,*) 'string=',' zhiming-li   ','  stringlen=',lname
c       end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  function lenstr returns the length of a string (less than 81 chars).
c  blanks at right-hand side of the string are not counted for the length.
c  (this is the difference between len() and lenstring() )  
c  example:
c      length_of_string = lenstr(string)  
c
	integer function lenstr(string)
        character*(*) string
        ls = len(string)
        lenstr = ls
        inoch = 0
        do 10 i=1,ls
           if (string(i:i).ne.' ' .and. string(i:i).ne.char(0)) goto 9 
           if ( inoch .eq. 0 ) goto 10 
           lenstr = i - 1 
	   goto 20
9       inoch = 1  
10      continue
20      continue
        return
        end
