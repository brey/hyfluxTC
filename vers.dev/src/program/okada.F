      program okada
      implicit none
c-------------------------------------------------------------
c interface with maps
      integer*4  nval, nvx, nvy, typ
      real*8 xor, yor, size
      real*4,    allocatable :: val(:)

	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)

      real*4     mv 
      integer*1  mv1
 
c--------------------------------------------
c local variables
      integer*4 iargc, narg,i,j,k
	
      real*4 numget, flength, fwidth, th, dip, rake, d, depth
     &        ,radius ,houtmin
 
        real*4 xmin,ymin, dx, dy, xepic, yepic
 
	character clonemap*132, outmap*132, ch*132, maps*132
     &          ,msg*3, inpdeck*254, confdeck*254, yesno*3
#if defined WIN32
     &          ,bs*1 /'\'/
#else
     &          ,bs*1 /'/'/
#endif
	integer*4 length, lm, lout, linp
     &           ,geog, addVal
c--------------------------------------------
* gdal additional variables
        integer*4 nband, iband,error, nbyte
        character driver*64, datatype*64
        real*8 sizey
c--------------------------------------------
      logical nocase, existFile
      real*4 getnumvar
c----------------------------------------------------------
	call getmv1(mv1)
	call getmv(mv)
 
	narg = iargc()
	flength = 50e3
        fwidth = 10e3
	clonemap = ' '
	outmap = ' '
        geog = 1
        addVal = 0
        radius = 6378388.00
        xepic=0
        yepic=0
        houtmin = 0.
        nocase = .true.
      inpdeck = 'Calc_input_deck.txt'
c-------------------------------------------------------------------------------------
      call getenv ('configHF',confdeck)
c-------------------------------------------------------------------------------------
	if (narg.eq.0) then
 	print *,' '
	print *,'okada help '
 	print *,' '
      print *,'generate the initial water suface elevation map (zsurf) '
      print *,' '
      print *,'-i input deck'
      print *,'   Tsunami input deck '
      print *,' DEFAULT is ',inpdeck(1:length(inpdeck))
      print *,' '
      print *,' '
      print *,'-conf configuration deck'
      print *,'   configuration deck '
      print *,' DEFAULT is ',confdeck(1:length(confdeck))
      print *,' '
      print *,'-nocase  yes/no'
      print *,'   names in input deck are no case sensitive'
      print *,'	DEFAULT ',yesno(nocase)	
      print *,' '
	print *,'-o outmap'
      print *,'   no DEFAULT'
	print *,' '
	print *,'-houtmin houtmin'
      print *,'   if abs(zsurf) < houtmin then zsurf=0'
	print *,'   DEFAULT is ',houtmin
 
	print *,' '
	print *,'-addVal addVal'
	print *,'   addVal = 0 the calculated values are stored in outmap'
	print *,'   addVal = 1 the calculated values are added to the previous one'
	print *,'   DEFAULT is ',addVal
	print *,' '
	print *,'-clone clonemap'
        print *,'  NOTE: if addVal = 1 clonemap is not necessary'
        print *,'   no DEFAULT'
	print *,' '
	print *,'-length length [m]'
	print *,'   fault length'
	print *,'   DEFAULT is ',flength
	print *,' '
	print *,'-width width [m]'
	print *,'   fault width'
	print *,'   DEFAULT is ',fwidth
	print *,' '
	print *,'-depth depth [m]'
	print *,'   top fault depth'
	print *,'   DEFAULT is ',depth
	print *,' '
	print *,'-th th'
	print *,'   azimuth or strike angle [as USGS, 0==north]'
	print *,'   DEFAULT is ',th
	print *,' '
	print *,'-dip dip'
	print *,'   dip angle'
	print *,'   DEFAULT is ',dip
	print *,' '
	print *,'-rake rake'
	print *,'   rake or slip angle'
	print *,'   DEFAULT is ',rake
	print *,' '
	print *,'-d d'
	print *,'   dislocation [m]'
	print *,'   DEFAULT is ',d
	print *,' '
	print *,'-radius radius'
	print *,'   earth radius (as in GISCO DB)'
	print *,'   DEFAULT is ',radius
	print *,' '
	print *,'-xepic xepic'
	print *,'   DEFAULT is ',xepic
	print *,' '
	print *,'-yepic yepic'
	print *,'   DEFAULT is ',yepic
	print *,' '
	print *,'-geog geog'
	print *,'   geog = 0 projected (x,y) coordinate'
	print *,'   geog = 1 geographic (lat,long) coordinate'
	print *,'   DEFAULT is ',geog
	print *,' '
#if defined WIN32
        read *
#endif
	stop
	endif
c-------------------------------------------------------------------------------------
c get input and configuration deck names
      do i = 1,narg
      call getarg(i,ch)
c       ----------------------------------------------------
        if (ch.eq.'-i') then
        call getarg(i+1,inpdeck)
 
        else if (ch.eq.'-conf') then
        call getarg(i+1,confdeck)
 
        else if (ch.eq.'-nocase') then
        call getarg(i+1,ch)
                if (ch.eq.'yes') then
                nocase = .true.
                else if (ch.eq.'no') then
                nocase = .false.
                else
                nocase = .true.
                endif
        endif
      enddo
c-------------------------------------------------------------------------------------
c process decks
      if (existFile(inpdeck))  call proc_deck (inpdeck,nocase)
      if (existFile(confdeck)) call proc_deck (confdeck,nocase)
c from inpdeck
      if (existFile(inpdeck)) then
      flength = getnumvar('flenght',mv)*1000
      fwidth  = getnumvar('fwidth',mv)*1000
      depth   = getnumvar('depth',mv)*1000
      th      = getnumvar('strike',mv)
      dip     = getnumvar('dip',mv)
      rake    = getnumvar('rake',mv)
      d       = getnumvar('slip',mv)
      xepic   = getnumvar('Lon',mv)
      yepic   = getnumvar('Lat',mv)
      endif
c from confdeck
      radius  = getnumvar('radius',mv)
      houtmin = getnumvar('houtmin',mv)
c-----------------------------------------------------------------
      print *
      print *,'Input deck data'
      print *
      call listvar(6)
 
c-----------------------------------------------------------------
c read arguments
 
	do i = 1,narg
	call getarg (i,ch)
	  if (ch.eq.'-clone') then
	  call getarg (i+1,clonemap)
 
	  else if (ch.eq.'-o') then
	  call getarg (i+1,outmap)
 
	  else if (ch.eq.'-length') then
	  call getarg (i+1,ch)
	  flength = numget(ch,msg)	
 
	  else if (ch.eq.'-width') then
	  call getarg (i+1,ch)
	  fwidth = numget(ch,msg)	
 
	  else if (ch.eq.'-depth') then
	  call getarg (i+1,ch)
	  depth = numget(ch,msg)	
 
	  else if (ch.eq.'-th') then
	  call getarg (i+1,ch)
	  th = numget(ch,msg)	
 
	  else if (ch.eq.'-dip') then
	  call getarg (i+1,ch)
	  dip = numget(ch,msg)	
 
	  else if (ch.eq.'-rake') then
	  call getarg (i+1,ch)
	  rake = numget(ch,msg)	
 
	  else if (ch.eq.'-d') then
	  call getarg (i+1,ch)
	  d = numget(ch,msg)	
 
	  else if (ch.eq.'-geog') then
	  call getarg (i+1,ch)
	  geog = numget(ch,msg)	
 
	  else if (ch.eq.'-xepic') then
	  call getarg (i+1,ch)
	  xepic = numget(ch,msg)	
 
	  else if (ch.eq.'-yepic') then
	  call getarg (i+1,ch)
	  yepic = numget(ch,msg)	
 
	  else if (ch.eq.'-addVal') then
	  call getarg (i+1,ch)
	  addVal = numget(ch,msg)	
 
	  else if (ch.eq.'-radius') then
	  call getarg (i+1,ch)
	  radius = numget(ch,msg)	
	  endif
	enddo
c---------------------------------------------------------------
c get outmap
c---------------------------------------------------------------
        if (addVal.eq.1) then
	  if (outmap.eq.' ') then
	  print *,'the output map has not been specified'
	  call exit(2)
	  endif
 
	maps = outmap(1:length(outmap))//null
	call OPENGDAL (maps,nvx,nvy,typ, xor, yor, size,sizey,driver,nband,error)
	nval = nvx*nvy
	allocate (val(nval))
        call GETGDALVAL (nband,val)
c---------------------------------------------------------------
c get clone maps
c---------------------------------------------------------------
        else
	  if (clonemap.eq.' ') then
	  print *,'the clone map has not been specified'
	  call exit(2)
	  endif
 
	maps = clonemap(1:length(clonemap))//null
	call OPENGDAL (maps,nvx,nvy,typ, xor, yor, size,sizey,driver,nband,error)
	nval = nvx*nvy
        call CLOSEGDAL()
 
	allocate (val(nval))
        endif
c---------------------------------------------------------------
        driver = driver(1:length(driver))//null
c---------------------------------------------------------------
        print *,'allocated nvx=',nvx,' nvy=',nvy,' nval=',nval
c---------------------------------------------------------------
c call okada
        xmin = xor + size/2
        ymin = yor - size/2
        dx = size
        dy = -size
 
       print *,'addVal=',addVal
       print *,'length=',flength
 
       call GETOKADA2 ( addVal, nvx, 0, 0, geog, radius
     &     ,flength , fwidth , depth,dip , th , rake , d
     &     ,xepic , yepic ,xmin , nvx ,ymin , nvy ,dx , dy,val)
 
        do i = 1,nval
          if (abs(val(i)).lt.houtmin) val(i)=0
        enddo
 
        print *,'done okada calculations'
 
	maps = outmap(1:length(outmap))//null
          call OUTGDAL (maps, nvx,nvy,4
     &               , xor, yor, size,val,nval
     &           , sizey, driver, nband,error)
 
 
c---------------------------------------------------------------
	stop
	end
