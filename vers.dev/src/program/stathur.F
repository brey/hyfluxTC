	program provagrib
	
	implicit none
	
	character ch*256,inpname*256, outname*256, buff*256
     &           ,str(256)*1, strout(256)*1
       integer iargc, narg, i, j, k ,nval, length, nr, m, ln

       parameter (nval=13)
       character strnum(nval)*1 /"0","1","2","3","4","5","6","7","8","9",".","+","-"/
	 
	real hour, lat, lon, press, maxwind 

	integer iinp/10/, iout/11/

	logical exist, num, num0
    
  	inpname = ' '
	outname = ' '
     
	narg=iargc()
       if (narg.eq.0) then
		print *,'stathur program'
		print *,'This program extracts: long, lat, min pressure (hPa), max surf wind (knots)'
		print *,'from the file: ', inpname
       	print *,'-i input data file '
       	print *,'-o output kml file'
       	read *
       	stop
       endif
c-------------------------------------------------------------------------------
        do i = 1,narg
        	call getarg (i,ch)
          	if (ch.eq.'-i') then
          		call getarg (i+1,inpname)

          	else if (ch.eq.'-o') then
          	call getarg (i+1,outname)
	   	endif
	 enddo
    
c-------------------------------------------------------------------------------

        if (inpname.ne.' ') then
        	inquire (file=inpname,exist=exist)
          	if (.not.exist) then
          		print *,'the file:', inpname, 'not exist'
          		call exit(1)
          	endif
        	open(iinp,file=inpname,action='read')
        else
        	print *,'input file not defined'
        	call exit(1)
        endif


c-------------------------------------------------------------------------------
        if (outname.ne.' ') then
        	open(iout,file=outname,action='write')
        else
	        print *,'output file not defined'
       	 call exit(1)
        endif
c-------------------------------------------------------------------------------
       nr=0
10     continue
       read (iinp,'(a)',end=20) buff
	if (buff(1:4).eq.'    ') goto 10 
	nr = nr+1
	goto 10
20 	continue 
	rewind (iinp)
c-------------------------------------------------------------------------------

	do m=1, nr
	  read (iinp,'(256a1)') str
         j=0
         num0=.false.
         do i=1,256
         	num = .false. 
	  	do k=1,nval
 	     		num = str(i).eq.strnum(k)
           		if (num) exit 
		enddo
       	if (num) then
      	  		j=j+1
          		strout(j)=str(i)
          		num0 = .true.
       	else
          		if (num0) then
              		j = j+1
              		strout(j)=" "
          		endif
          		num0 = .false.
       	endif
	  enddo
		write (iout,'(256a1)') (strout(k),k=1,j) 
      enddo
c-------------------------------------------------------------------------------        	   
	stop
	end program







