        module station_mod
        implicit none
        real*4 xf,yf,dx,dy, rainmax
        integer*4 ncol,nrow,nval, nmaskstat, nreq, nstations, ldir
        integer*4 mincode1,maxcode1
        integer*4,allocatable :: mincode(:),maxcode(:)
        real*4, allocatable  :: marsdem0(:), nstat(:), valcode(:), trueval(:)
        integer*4, allocatable :: pgrid(:), pstation(:),shiftcode(:)
        logical*1, allocatable :: maskreq(:)
        logical*4, allocatable :: maskstation(:)
        end module station_mod
c-------------------------------------------------------------------------
        subroutine readStationTable (stationDir,ndir)
        use station_mod
        implicit none
        integer ndir, idir, lm, length,itab /10/, code, i,j,L, k, nlines
     &         ,mi,ma,maold
        character stationDir(ndir)*(*), stationtab*256, table*132, tabname*256
        real*4 rcode, xval, yval, zval
        logical*4 eqmv, exist
c----------------------------------------------------------------
#if defined WIN32
      character bs*1 /'\'/
#else
      character bs*1 /'/'/
#endif
        real*4 mv 
        data table /'codexyz.txt'/
c----------------------------------------------------------------
        allocate  ( mincode(ndir),maxcode(ndir),shiftcode(ndir))
        call getmv(mv)

        maold = 0
        do idir = 1,ndir       
        stationtab = stationDir(idir)

c verify the station directory 
 
	if (stationtab.ne.'  ') then
	lm = length(stationtab)
	inquire (file=stationtab, exist=exist)
	  if (.not.exist) Then
	  print *,' '
	print *,'Error: the stationtab directory ',stationtab(1:lm)
     &       ,' does not exist'
	call exit(2)
	  endif
 
	else
	print *,'stationtab directory not defined'
	call exit(2)
	endif
c---------------------------------------------------------------
c open station table
 
	tabname = stationtab(1:lm)//bs//table(1:length(table))
 	inquire (file=tabname, exist=exist)
	  if (.not.exist) Then
	  print *,' '
	print *,'Error: the table ',tabname(1:length(tabname)),' does not exist'
	  call exit(2)
	  endif
 
        open (itab,file=tabname,action='read')
c-------------------------------------------------------------------	
	
c get mincode and maxcode for each station directory
 
c first get min and max code
 
        mi = 100 000 000
        ma = -mincode(idir)
        nlines = 0 
        do while (.true.)
        nlines = nlines +1
          read (itab,*,end=230,err=1000) rcode, xval, yval, zval
          code = rcode
          mi = min(mi,code)
          ma = max(ma,code)
          enddo
 
230     continue
        rewind (itab)
        close (itab)

        mincode(idir) = maold+1
        maxcode(idir) = mincode(idir) + ma-mi
        shiftcode(idir) = maxcode(idir) - ma
        maold = maxcode(idir)

        enddo
c-------------------------------------------------------
        print *
        print '(a40,4a13)','station directory','mincode','maxcode'
     &                   ,'mincode new','maxcode new'
        do idir = 1,ndir
        print '(a40,4i13)',stationDir(idir)
     &         ,mincode(idir)-shiftcode(idir)
     &         ,maxcode(idir)-shiftcode(idir)
     &         ,mincode(idir)
     &         ,maxcode(idir)
        enddo
        print *

        mincode1 = mincode(1)
        maxcode1 = maxcode(ndir)
c-------------------------------------------------------
 
c get dem and code from stationtab
	allocate (marsdem0(nval), nstat(nval)
     &           ,pgrid(mincode1:maxcode1)
     &           ,maskstation(mincode1:maxcode1)
     &           ,valcode(mincode1:maxcode1))
 
        maskstation = .false.
        marsdem0 = 0.
        nstat = 0.
 
c now read the values
        do idir = 1,ndir       
c---------------------------------------------------------------
c open station table (verification done previously)
 
        stationtab = stationDir(idir)
	lm = length(stationtab)
	tabname = stationtab(1:lm)//bs//table(1:length(table))
        open (itab,file=tabname,action='read')

        nlines = 0 
        do while (.true.)
        nlines = nlines +1
        read (itab,*,end=240,err=1000) rcode, xval, yval, zval
        code = rcode+shiftcode(idir)
 
	i = (xval-xf)/dx + 1.
	j = (yval-yf)/dy + 1.
        L = (j-1)*ncol + i

	  if ( i.ge.1 .and.i.le.ncol
     &      .and.j.ge.1 .and.j.le.nrow) then
*         print *,L,code,xval,yval,zval
          maskstation(code) = .true.
          pgrid(code)    = L
          marsdem0(L) = marsdem0(L) + zval
          nstat(L) = nstat(L) + 1.
          endif
        enddo
 
240     continue
        close (itab)

        enddo

c----------------------------------------------------------
        nmaskstat = count(maskstation)
        print *
        print *,'nval=',nval
        print *,'nmaskstat=',nmaskstat
c----------------------------------------------------------

        allocate (trueval(nmaskstat),pstation(nmaskstat))

        k = 0
          do code = mincode1,maxcode1
            if (maskstation(code)) then
            k = k+1
            pstation(k) = code
            endif
          enddo
 
        do i = 1,nval
          if (nstat(i).eq.0.) then
          nstat(i) = mv
          marsdem0(i) = mv
          else
          marsdem0(i) = marsdem0(i) / nstat(i)
          endif
        enddo

        return
c---------------------------------------------------------
1000    continue
        print *,'error reading table at line ',nlines,' station directory ',stationDir(idir)
        stop
        end
 
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine readStationValue (stationDir,ndir,vars,year,day
     &                              ,weightval,val,found)
        use station_mod
        implicit none
        integer linp,length, nlines, iun /10/, ndir, idir, j,k,L
        real*4 val(nval),weightval(nval)
        character stationDir(ndir)*(*)
        character year*4, day*8 
        character stationdata*256, vars*2, fname*256, buff*132
        logical found, exist, eqmv
c variables to verify file status
        integer*4 res,statb(20), stat

        real*4 val1, rcode
        integer*4 code

c-------------------------------------------------------
#if defined WIN32
      character bs*1 /'\'/
#else
      character bs*1 /'/'/
#endif
        real*4 mv 
c-------------------------------------------------------
        found = .false.
	valcode = mv

        do 100 idir = 1,ndir       
        stationdata = stationDir(idir)
        linp = length(stationdata)

c jump because ta is not in the mars data file
c       if (vars.eq.'ta') goto 110
 
c define input file name
	if (vars.eq.'td') then
	fname = stationdata(1:linp)//bs//year//bs//'pd'//day//'.dat'
	inquire (file=fname,exist=exist)
	  if (.not.exist) then
	  fname = stationdata(1:linp)//bs//year//bs//'pd'//day//'.datInt'
	  endif
 
	else
	fname = stationdata(1:linp)//bs//year//bs//vars//day//'.dat'
	inquire (file=fname,exist=exist)
	  if (.not.exist) then
	  fname = stationdata(1:linp)//bs//year//bs//vars//day//'.datInt'
	  endif
	endif
 
c Verify if the mars file exist
 
	inquire (file=fname,exist=exist)
          if (.not.exist) goto 100

c Get the status of the mars file
	
	res = stat(fname,statb)
	  if (res.ne.0) goto 100
 
c verify the size
 
	 if (statb(8).eq.0) goto 100

         found = .true.

c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
c------------------------------------------------
c open MARS file
c------------------------------------------------
	open (unit=iun,file=fname,action='read')
c------------------------------------------------
c read mars file
c j: code
c val1: value
c valcode: array with index from mincode to maxcode (see marscode)
c------------------------------------------------
 
	nlines = 0
        do while (.true.)
300	nlines = nlines + 1
#if defined WIN32
	read (iun,*,end=310,err=1000) rcode,val1
        code = rcode+shiftcode(idir)
#else
c remove cr in case of data in dos format
	read (iun,'(a)',end=310,err=1000) Buff
        call nocr (buff)
	read (buff,*) rcode,val1
        code = rcode+shiftcode(idir)
#endif
	  if (code.lt.mincode(idir).or.code.gt.maxcode(idir)) goto 300
          if (.not.maskstation(code)) goto 300

	  if (vars.eq.'pr'
     &       .and.(val1.lt.0.0.or.val1.gt.rainmax)) go to 300
 
	valcode(code) = val1
	enddo

310	close (iun)

100     continue
c------------------------------------------------
c  assign values to mars map
c------------------------------------------------
 
        do k = 1,nmaskstat
        code = pstation(k)
        L = pgrid(code)
          if (maskstation(code).and.weightval(L).gt.0.) then
          val1 = valcode(code)
            if (.not.eqmv(val1)) then 
            val(l) = val(l) + val1
            nstat(l) = nstat(l) + 1
            endif
          endif
        enddo
 
        l = 0
        nreq = 0
        do j = 1,nval
          if (nstat(j).eq.0.) then
          val(j) = mv
          else
          val(j) = val (j)/ nstat(j)
          l = l+1
            if (maskreq(j)) then
            nreq=nreq+1
            endif
          endif
        enddo
c----------------------------------------------------------------------
        nstations = l
	print *,'for ',vars,' found ',nstations,' true data'
     &         ,' with ',nreq,' in maskreq field'
c----------------------------------------------------------------------
        return
c-----------------------------------------------------------
1000    continue
        print *,'Error reading file',fname(1:length(fname))
     &       ,' at line=',nlines
        stop
        end
