      program endfile
c-------------------------------------------------------------
	implicit none
 
c--------------------------------------------
c local variables
	integer*4 iun, iargc, narg,error, i,j, irec, n
    	
	logical search
 
	character*256  ch, file, string, line 
        character var*20
 
	integer*4 length,lstr, ll
        real*4 valget
c-----------------------------------------------------------
 
	narg = iargc()
        n=0
 
	  if (narg.eq.0) then
 
	print *,' '
	print *,'endfile help '
        print *,'put endfile n lines after string  '
      print *,' '
      print *,'-i input file'
      print *,' DEFAULT none'
	print *,' '
	print *,'-string string'
	print *,'   no DEFAULT '
	print *,' '
	print *,'-n n'
	print *,'   no DEFAULT '
	print *,' '
#if defined WIN32
	read *
#endif
	stop
	endif
c-----------------------------------------------------------------
c read arguments
	do i = 1,narg
	call getarg (i,ch)
c       ----------------------------------------------------
	  if (ch.eq.'-i') then
	  call getarg (i+1,file)
c       ----------------------------------------------------
	  else if (ch.eq.'-string') then
	  call getarg (i+1,string)
c       ----------------------------------------------------
	  else if (ch.eq.'-n') then
	  call getarg (i+1,ch)
          n = valget(ch)
          endif
c       ----------------------------------------------------
	enddo
c---------------------------------------------------------------
        call opnfree (iun,file)
        lstr= length(string)
        search = .true.
          mainloop: do while (search)
          read (iun,'(a)',end=200) line
             if (line.eq.' ') cycle
          ll = length(line)
            do i=1,ll-lstr+1
            j = i+lstr-1
              if (line(i:j).eq.string(1:lstr)) goto 100
            enddo            
          enddo mainloop

200     continue
        call exit

100     do i=1,n
        read (iun,*)   
        enddo

        endfile (iun)
c------------------------------------------------------------
	stop
 	end
