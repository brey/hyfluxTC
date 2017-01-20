	subroutine opendb (unit, file,status, packed, typ1, head1, var1
     &                  , ptval, *)
	implicit none
#include <dbcom.h>
#include <reclen.h>
 
	integer*4 unit, blocksize, buffercount, jpack, ptval,ios
	character file*(*), status*(*)
	parameter (blocksize=2048,jpack=512000)
 
	integer*4  head1(50)
	character  var1(50)*4, typ1*8
	logical*4  packed
	logical*4 err
 
c----------------------------------------------------------
	err = .false.
 
        open (unit=unit,file=file,access='direct'
     &   ,status = status
#if defined WIN32
     &  ,buffered='yes'
#endif
#if defined lf95
     &       , recl=reclen*4
#elif defined WIN32
     &  ,recl=reclen
     &  ,blocksize=blocksize*4
     &  ,buffercount=nval/blocksize+1
     &  ,convert='little_endian'
#endif
     &  ,err=1000
     &  ,iostat=ios
     &  ,form='unformatted')
 
 
c------------------------------------------------------------------
 
	if (status.eq.'new') then
 
	pack = packed
	head = head1
	var  = var1
	typ  = typ1
 
	write (unit,rec=1) head, typ, var, pack
 
	if (pack) then
	nrec  = ((nval+1)/2+nfdat-1)/reclen+1
	else
	nrec  = (nval+nfdat-1)/reclen+1
	endif
 
	lirec = nvar*(tmax+1)*(nfor+1)
	ptxyz  = 1 + ((lirec-1)/reclen + 1 ) + 1
	ptval = ptxyz + nrec*3
 
c---------------------------------------------------------------------------
	else if (status.eq.'old') then
 
	read (unit,rec=1) head, typ, var, pack
	packed = pack
	head1  = head
	var1   = var
	typ1   = typ
 
	if (pack) then
	nrec  = ((nval+1)/2+nfdat-1)/reclen+1
	else
	nrec  = (nval+nfdat-1)/reclen+1
	endif
 
	lirec = nvar*(tmax+1)*(nfor+1)
	ptxyz = 1 + ((lirec-1)/reclen + 1 ) + 1
	ptval = ptxyz + nrec*3
 
*	print *,'ptval=',ptval
 
	endif
c-------------------------------------------------------------------------------
	return
 
1000    print *,'unit=',unit,' status=',status,' file=',file
        print *,'iostatus=',ios
        return 1
	end
c---------------------------------------------------------------------
	subroutine putptdb (iout,irecc)
	implicit none
#include <dbcom.h>
#include <reclen.h>
c     -----------------------------------------------------------
	integer*4 irec , iout
	integer*4  irecc(*)
c     ---------------------------------------------------------------------
	  irec = 2
	  call writefrec (iout,irec,irecc,lirec)
	return
	end
c---------------------------------------------------------------------
	subroutine putxyzdb (iout,x,y,z)
	implicit none
	
#include <dbcom.h>
#include <reclen.h>
 
c     -----------------------------------------------------------
	integer*4 irec , iout
	REAL*4  x(*), y(*), z(*)
c    ---------------------------------------------------------------------
	  irec = ptxyz
	  call putvaldb (iout,irec,x,nval)
	  call putvaldb (iout,irec,y,nval)
	  call putvaldb (iout,irec,z,nval)
	return
	end
c--------------------------------------------------------------------
	subroutine getptdb (iout,irecc)
	implicit none
#include <dbcom.h>
#include <reclen.h>
c     -----------------------------------------------------------
	integer*4 irec , iout
	integer*4  irecc(*)
c-------------------------------------------------------------------
	  irec = 2
	  call readfrec (iout,irec,irecc,lirec)
	return
	end	
c--------------------------------------------------------------------
	subroutine getxyzdb (iout,x,y,z)
	implicit none
#include <dbcom.h>
#include <reclen.h>
c     -----------------------------------------------------------
	integer*4 irec , iout
	REAL*4  x(*), y(*), z(*)
c    -------------------------------------------------------------------
	  irec = ptxyz
	  call getvaldb (iout,irec,x,nval)
	  irec = irec + nrec
	  call getvaldb (iout,irec,y,nval)
	  irec = irec + nrec
	  call getvaldb (iout,irec,z,nval)
	return
	end	
c---------------------------------------------------------
	subroutine putvaldb (iout1,irec,val,nval)
	implicit none
#include <reclen.h>
 
	integer*4 iout1,irec, nval, recl2
     &          , nrec,l1,l2, i, j, i2
 
	parameter (recl2=reclen*2)
	integer*2 ival2(2,recl2)
	integer*4 ival(recl2)
 
	equivalence (ival2,ival)
 
	real*4 val(nval), vmin, vmax, fac /0./, cost, ifac
	parameter (cost=2**16-1)
c--------------------------------------------------------------------
	integer*4 ip, pp
	integer*2 pp2(2)
	equivalence (pp,pp2)
	logical first /.true./
	save first, ip
		if (first) then
	    pp = 2
	      if (pp2(2).eq.2) then
	      ip = 2
	      else
	      ip = 1
	      endif
	    first = .false.
	    endif
c--------------------------------------------------------------------
	vmin = minval(val)
	vmax = maxval(val)
c------------------------------------------------------------
	if (pack) then
	nrec  = ((nval+1)/2+nfdat-1)/reclen+1
 
	fac = (vmax-vmin)/cost
	  if (fac.eq.0.) then
	  fac=1.
	  ifac = 1.
	  else
	  ifac = cost/(vmax-vmin)
	  endif
 
	l1 = 1
	l2 = min0((reclen-nfdat)*2,nval)
	i2 = l2-l1+1
	
	ival(1:i2) = (val(l1:l2)-vmin) * ifac + 0.5
	write (iout1,rec=irec) vmin,vmax,fac, ival2(ip,1:i2)
	irec = irec+1
 
	
	do i = 2,nrec
	l1 = l2 + 1
	l2 = min0(l2+recl2,nval)
	i2 = l2-l1+1
	ival(1:i2) = (val(l1:l2)-vmin) * ifac + 0.5
	write (iout1,rec=irec) ival2(ip,1:i2)
	irec = irec + 1
	enddo
 
c------------------------------------------------------------
	else
	nrec = (nval+nfdat-1)/reclen+1
 
	l1 = 1
	l2 = min0(reclen-nfdat,nval)
	write (iout1,rec=irec) vmin,vmax,fac, val(l1:l2)
	irec = irec+1
 
	do i = 2,nrec
	l1 = l2 + 1
	l2 = min0(l2+reclen,nval)
	write (iout1,rec=irec) val(l1:l2)
	irec = irec + 1
	enddo
	endif
 
	return
	end
c---------------------------------------------------------
	subroutine getvaldb (iout1,irec0,val,nval)
	implicit none
#include <reclen.h>
 
	integer*4 iout1,irec, nval, recl2, irec0
     &          , nrec,l1,l2,i2, i, j
 
	parameter (recl2=reclen*2)
	integer*2 ival2(2,recl2)
	integer*4 ival(recl2)
 
	equivalence (ival2,ival)
 
	real*4 val(nval), vmin, vmax, fac /0./, cost
	parameter (cost=2**16-1)
c--------------------------------------------------------------------
	integer*4 ip, pp
	integer*2 pp2(2)
	equivalence (pp,pp2)
	logical first /.true./
	save first, ip
 
		if (first) then
	    pp = 2
	      if (pp2(2).eq.2) then
	      ip = 2
	      else
	      ip = 1
	      endif
	    first = .false.
	    endif
c-------------------------------------------------------
	irec = irec0
c
	if (pack) then
	nrec  = ((nval+1)/2+nfdat-1)/reclen+1
 
	l1 = 1
	l2 = min0((reclen-nfdat)*2,nval)
	i2 = l2-l1+1
	
	read (iout1,rec=irec) vmin,vmax,fac, ival2(ip,1:i2)
	val(l1:l2) = vmin + ival(1:i2)*fac
	irec = irec+1
 
	do i = 2,nrec
	l1 = l2 + 1
	l2 = min0(l2+recl2,nval)
	i2 = l2-l1+1
	read (iout1,rec=irec) ival2(ip,1:i2)
	val(l1:l2) = vmin + ival(1:i2)*fac
	irec = irec + 1
	enddo
c-----------------------------------------------------------------------------
	else
	nrec = (nval+nfdat-1)/reclen+1
 
	l1 = 1
	l2 = min0(reclen-nfdat,nval)
	read (iout1,rec=irec) vmin,vmax, fac, val(l1:l2)
	irec = irec+1
 
	do i = 2,nrec
	l1 = l2 + 1
	l2 = min0(l2+reclen,nval)
	read (iout1,rec=irec) val(l1:l2)
	irec = irec + 1
	enddo
	endif
 
	return
	end
c-------------------------------------------------------------------------
	subroutine writefrec (iout1,irec,irec1,lirec)
	implicit none
#include <reclen.h>
	integer*4 iout1,irec, lirec, irec1(lirec)
     &          , nrec,l1,l2, i, j
c
	nrec = (lirec-1)/reclen+1
	
	l2 = 0
	do i = 1,nrec
	l1 = l2 + 1
	l2 = min0(l2+reclen,lirec)
	write (iout1,rec=irec) irec1(l1:l2)
	irec = irec + 1
	enddo
	return
	end
c-----------------------------------------------------------------------
	subroutine readfrec (iout1,irec,irec1,lirec)
	implicit none
#include <reclen.h>
	integer*4 iout1,irec,lirec, irec1(lirec)
     &          , nrec,l1,l2, i, j
c
	nrec = (lirec-1)/reclen+1
	
	l2 = 0
	do i = 1,nrec
	l1 = l2 + 1
	l2 = min0(l2+reclen,lirec)
	read (iout1,rec=irec) irec1(l1:l2)
	irec = irec + 1
	enddo
	return
	end
