      Subroutine gribinfo (cinpfile,izlen,forno)
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
 
      INTEGER*4 FILE, forno
 
C     -----------------------------------------------------------------
 
C
	yoper = 'D'
      ILENB = JPACK * 4
      IPUNP = JPACK
 
 
      CALL PBOPEN( FILE, cinpfile, 'R', KRET)
        IF ( KRET .NE. 0 ) THEN
        WRITE (*, *) ' Return code from PBOPE, kret= ',KRET
        CALL PBCLOSE(FILE, KRET)
        print*, 'Fault in PBOPEN'
        print *
        call exit(1)
        ENDIF
 
	lenout = 0
 
C     Read packed field into anbuff.
      CALL PBGRIB( FILE, anbuff, ILENB, LENOUT, KRET )
c-------------------------------------
        if (kret.ne.0) then
        print*, 'kret=',kret,' file=',FILE
        print*, 'Fault in PBGRIB'
        print *
        call exit(1)
c-------------------------------------
        else
        ISWAP = 1 + LENOUT/4
#if defined WIN32
        CALL SWAP32 (anbuff,ISWAP)
#endif
        IERR = 1
c-------------------------------------
        CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     &             ZSEC4,IPUNP,anbuff,ILENB,IWORD,YOPER,IERR)
           if (ierr.gt.0) then
           WRITE (*, *) ' ierr = ',ierr,' file=',file
           CALL PBCLOSE(FILE, KRET)
           print*, 'Fault in GRIBEX'
           call exit(1)
	   endif
c-------------------------------------
        izlen = isec4(1)	
        forno  = isec1(43)
        ENDIF
c-------------------------------------
      CALL PBCLOSE(FILE, KRET)
	return
	end
