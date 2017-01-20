      Subroutine opengrib (cinpfile)
	implicit none
	character cinpfile*(*)
	integer*4 kret, i, izlen
 
 
C     -----------------------------------------------------------------
 
C     Array for integer parameters from section 0 of GRIB message.
C
 
      integer*4 ISEC0(2), ISEC1(512), ISEC2(512), ISEC3(2), ISEC4(512)
     &          ,jpack, ilenb, lenout, iswap, ierr, ipunp, iword
      character*1  yoper
	parameter (jpack=512000)	
      real*4 ZSEC2(512), ZSEC3(2), ZSEC4(JPACK)
C
C     Array to read in packed data.
C
      real*4 anbuff(JPACK)
 
C
 
      INTEGER*4 FILE, jcont, ipar, forno, forind, tim1, tim2, timi
     &    , npar, nmer,ic, iy, im, id, ih
      integer*4  latf, longf, latl, longl, size1, size2, latPole,longPole
 
C     -----------------------------------------------------------------
 
C
	yoper = 'D'
      ILENB = JPACK * 4
      IPUNP = JPACK
 
 
	CALL PBOPEN( FILE, cinpfile, 'R', KRET)
      IF ( KRET .NE. 0 ) THEN
        WRITE (*, *) ' Return code from = ',KRET
        CALL PBCLOSE(FILE, KRET)
        STOP 'Fault in PBOPEN'
      ENDIF
 
      jcont = 0
	lenout = 0
 
	return
c---------------------------------------------------------------------
	entry readgrib (ipar, zsec4, izlen, forind, forno, tim1, tim2, timi
     &    , npar, nmer, latf, longf, latl, longl, latPole,longPole
     &    ,ic, iy, im, id, ih, *,*)
 
C     Read packed field into anbuff.
      CALL PBGRIB( FILE, anbuff, ILENB, LENOUT, KRET )
        IF ( KRET .eq. -1) THEN
        WRITE (*, *) ' Reached EOF after ', jcont,' attempt'
        CALL PBCLOSE(FILE, KRET)
        return 1
 
        else IF ( KRET .lt. -1) THEN
 	  write (*,*) ' Error from PBGRIB'
        WRITE (*, *) ' kret = ',KRET,' after ', jcont,' attempt'
        CALL PBCLOSE(FILE, KRET)
        return 2
	  endif
 
        ISWAP = 1 + LENOUT/4
#if defined WIN32
        CALL SWAP32 (anbuff,ISWAP)
#endif
        IERR = 1
        CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     &             ZSEC4,IPUNP,anbuff,ILENB,IWORD,YOPER,IERR)
		if (ierr.gt.0) then
		write (*,*) ' Error from GRIBEX'
          WRITE (*, *) ' ierr = ',ierr,' after ', jcont,' attempt'
          CALL PBCLOSE(FILE, KRET)
	    return 2
	    endif
 
c		print *,'ierr=',ierr
c	print *,'isec1=',isec1
	  jcont = jcont+1
c	  write (*,*) 'jcont=',jcont,' code=',isec1(6)
	  ipar = isec1(6)
	  izlen = isec4(1)	
	  forind = isec1(42)
	  forno  = isec1(43)
	
	ic = isec1(21)
	iy = isec1(10)
	im = isec1(11)
	id = isec1(12)
	ih = isec1(13)
 
	tim1 = isec1(16)
	tim2 = isec1(17)
	timi = isec1(18)
 
	nmer = isec2(2)
	npar = isec2(3)
	
	latf = isec2(4)
	longf = isec2(5)
	latl = isec2(7)
	longl = isec2(8)

        latPole = isec2(13)
        longPole = isec2(14)
 
c	size1 = isec2(9)
c	size2 = isec2(10)
 
	
 
	return
	end
