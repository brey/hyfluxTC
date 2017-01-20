      program endtable
c-------------------------------------------------------------
	implicit none
        character versdate*20 /"Version - 5May2011"/
 
c--------------------------------------------
c local variables
	integer*4 iun, iargc, narg,error, i,j, irec

	real*4 numget, val
      	
	logical exist
 
	character*256  ch, file
        character msg*3, clog*2, var*20
 
	integer*4 length
c-----------------------------------------------------------
 
	narg = iargc()
        clog='gt'
 
	  if (narg.eq.0) then
 
	print *,' '
	print *,'endtable help '
        print *,'put EOF if value in column var > val '
      print *,' '
      print *,'-i input file'
      print *,' DEFAULT none'
	print *,' '
	print *,'-var variable'
	print *,'   no DEFAULT '
	print *,' '
	print *,'-log logical condition'
	print *,'   DEFAULT is ',clog
        print *,' can be: lt le eq ge gt '
	print *,' '
	print *,'-val value'
	print *,'   no DEFAULT '
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
	  else if (ch.eq.'-var') then
	  call getarg (i+1,var)
c       ----------------------------------------------------
	  else if (ch.eq.'-log') then
	  call getarg (i+1,clog)
c       ----------------------------------------------------
	  else if (ch.eq.'-val') then
          call getarg (i+1,ch)
          val = numget(ch,msg)
          endif
c       ----------------------------------------------------
	enddo
c---------------------------------------------------------------
        call opnfree (iun,file)
        call  findrec (iun,var,clog,val,error,irec)
          if (error.gt.0) call exit(error)
        print *,'found condition at record ',irec
        endfile (iun)

c------------------------------------------------------------
	stop
 	end
