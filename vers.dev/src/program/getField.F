      program getField
c-------------------------------------------------------------
	implicit none
 
c--------------------------------------------
c local variables
	integer*4 iargc, narg,error, i,j
    	
	logical search, verify, yesno
 
	character*256  ch, line, file 
	integer*4 length, nmax, n, iun
        parameter (nmax=200)
        character names(nmax)*64,field*64, separator*1
 
        real*4 valget
c-----------------------------------------------------------
 
	narg = iargc()
        n=0
        line=' '
        file=' '
        field=' ' 
        separator='='
	  if (narg.eq.0) then
 
	print *,' '
	print *,'getField help '
        print *,'return value of field '
      print *,' '
      print *,'-i file '
      print *,' DEFAULT none'
	print *,' '
      print *,' '
      print *,'-l line '
      print *,' DEFAULT none'
	print *,' '
	print *,'-f field'
	print *,'   no DEFAULT '
	print *,' '
	print *,'-d separator'
	print *,'separator=',separator
	print *,' '
      print *,'-verify yes/no'
      print *,'    verify data processing'
      print *,'    DEFAULT is ',yesno(verify)
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
	  if (ch.eq.'-l') then
	  call getarg (i+1,line)
c       ----------------------------------------------------
	  else if (ch.eq.'-i') then
	  call getarg (i+1,file)
         line = file
c       ----------------------------------------------------
	  else if (ch.eq.'-f') then
	  call getarg (i+1,field)
c       ----------------------------------------------------
	  else if (ch.eq.'-d') then
	  call getarg (i+1,separator)
c------------------------------------------------------------
        else if ( ch .eq.'-verify') then
	  call getarg (i+1,ch)
		if (ch.eq.'yes') then
		verify = .true.
		else if (ch.eq.'no') then
		verify = .false.
		else
		verify = .true.
		endif
          endif
c       ----------------------------------------------------
	enddo
c----------------------------------------------------
        iun = -1
        if (file.ne.' ') call opnfree (iun,file)
        
        do while (.true.)
          if (iun.gt.0) read (iun,'(a)',end=100) line
c----------------------------------------------------
        do i = 1,length(line)
          if (line(i:i).eq.separator) line(i:i)=' '
        enddo
        call str2name (line,names,nmax,n,' ')
          if (verify) print *,(names(j)(1:20),j=1,nmax)
          do i = 1,n
            if (names(i).eq.field) then
            write (*,'(a)') names(i+1)(1:length(names(i+1)))
            stop
            endif
          enddo
c------------------------------------------------------------
           if (iun.le.0) exit
        enddo

100     stop
 	end
