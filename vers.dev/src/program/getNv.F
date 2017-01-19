      program getNv
      use map_cover_data
      implicit none
c-------------------------------------------------------------
c interface with maps
      integer*4  nval, nvx, nvy, typ
      real*8 xor, yor, size
      real*4,    allocatable :: dem(:,:), height(:,:), maskCalc(:,:),lon(:,:),lat(:,:)
      logical*1, allocatable :: maskSea(:,:)


	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)

      real*4     mv 
      integer*1  mv1
 
c--------------------------------------------
c local variables
      integer*4 iargc, narg,i,j,n
	
      real*4 valget, lonmin, lonmax, latmin, latmax, minheight
      real*4 lonmin1, lonmax1, latmin1, latmax1
  
	character ch*132, maps*132, demmap*132, heightmap*132, maskmap*132

#if defined WIN32
     &          ,bs*1 /'\'/
#else
     &          ,bs*1 /'/'/
#endif
	integer*4 length, lm, lout, linp
c--------------------------------------------
* gdal additional variables
        integer*4 nband, iband,error, nbyte
        character driver*64, datatype*64, outfmt*64
        real*8 sizey
c--------------------------------------------
      character yesno*3
      logical eqmv, existFile, verify, inside, fullWindow
c----------------------------------------------------------
	call getmv1(mv1)
	call getmv(mv)
        lonmin = mv
        lonmax= mv
        latmin=mv
        latmax=mv  
        minheight=0.
	narg = iargc()
      demmap=' '
      heightmap=' '
      maskmap=' '
      verify = .false.
      fullWindow=.false.
      nradius_cover = 0
c-------------------------------------------------------------------------------------
	if (narg.eq.0) then
 	print *,' '
	print *,'getNv help '
 	print *,' '
      print *,'Get no. of calculation pixels in a specified window'
      print *,' '
      print *,'-dem demSea map'
      print *,' DEFAULT none'
      print *,' '
      print *,'-height Height map'
      print *,' DEFAULT none'
      print *,' '
      print *,'-mask mask map'
      print *,' DEFAULT none'
      print *,' '
      print *,'-nradius additional cells to put around true cell '
      print *,' nradius = maxDistLand[km] / (cellsize[deg] * deg2km [110] )'
      print *,' DEFAULT ',nradius_cover
      print *,' '
      print *,'-verify yes/no'
      print *,'    verify data processing'
      print *,'    DEFAULT is ',yesno(verify)
      print *,' '
      print *,'-fullWindow yes/no'
      print *,'    full window'
      print *,'    DEFAULT is ',yesno(fullWindow)
      print *,' '
      print *,'-minheight minheight'
      print *,' DEFAULT = ',minheight
      print *,' '
      print *,'-lonmin lonmin'
      print *,' DEFAULT none'
      print *,'-lonmax lonmax'
      print *,' DEFAULT none'
      print *,'-latmin latmin'
      print *,' DEFAULT none'
      print *,'-latmax latmax'
      print *,' DEFAULT none'
      print *

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
        if (ch.eq.'-dem') then
        call getarg(i+1,demmap)
 
        else if (ch.eq.'-height') then
        call getarg(i+1,heightmap)
 
        else if (ch.eq.'-minheight') then
        call getarg(i+1,ch)
        minheight = valget(ch)
c------------------------------------------------------------
      else if (ch.eq.'-nradius') then
      call getarg(i+1,ch)
      nradius_cover = valget(ch)
 
      else if (ch.eq.'-mask') then
      call getarg(i+1,maskmap)
 

      else if ( ch .eq.'-verify') then
      call getarg (i+1,ch)
        if (ch.eq.'yes') then
        verify = .true.
        else if (ch.eq.'no') then
        verify = .false.
        else
        verify = .true.
        endif
c------------------------------------------------------------
      else if ( ch .eq.'-fullWindow') then
      call getarg (i+1,ch)
        if (ch.eq.'yes') then
        fullWindow = .true.
        else if (ch.eq.'no') then
        fullWindow = .false.
        else
        fullWindow = .true.
        endif
c------------------------------------------------------------
        else if (ch.eq.'-lonmin') then
        call getarg(i+1,ch)
        lonmin = valget(ch)
 
        else if (ch.eq.'-lonmax') then
        call getarg(i+1,ch)
        lonmax = valget(ch)
 
        else if (ch.eq.'-latmin') then
        call getarg(i+1,ch)
        latmin = valget(ch)
 
        else if (ch.eq.'-latmax') then
        call getarg(i+1,ch)
        latmax = valget(ch)

        endif
      enddo
c---------------------------------------------------------------
	maps = demmap(1:length(demmap))//null
	call OPENGDAL (maps,nvx,nvy,typ, xor, yor, size,sizey,driver,nband,error)
          if (eqmv(lonmin)) lonmin = xor
          if (eqmv(latmax)) latmax = yor
          if (eqmv(lonmax)) lonmax = lonmin+nvx*size
          if (eqmv(latmin)) latmin = latmax+nvy*sizey

      if (verify) print *,'lonmin=',lonmin,' lonmax=',lonmax,' latmin=',latmin,' latmax=',latmax

      nval = nvx*nvy
      allocate (dem(nvx,nvy),maskSea(nvx,nvy),lon(nvx,nvy),lat(nvx,nvy))
      call GETGDALVAL (nband,dem)
          do i = 1,nvx
            do j = 1,nvy
            lon(i,j) = xor + size*(i-0.5)
            lat(i,j) = yor + sizey*(j-0.5)
            inside = (lon(i,j).ge.lonmin.and.lon(i,j).le.lonmax
     &           .and.lat(i,j).ge.latmin.and.lat(i,j).le.latmax)
            maskSea(i,j) = .not.eqmv(dem(i,j)).and.inside
            enddo
          enddo            

c---------------------------------------------------------------
	allocate (height(nvx,nvy))
        height = max(minheight*2,0.1)
          if (existFile(heightmap)) then
	  maps = heightmap(1:length(heightmap))//null
	  call OPENGDAL (maps,nvx,nvy,typ, xor, yor, size,sizey,driver,nband,error)
          call GETGDALVAL (nband,height)
            do i = 1,nvx
              do j = 1,nvy
              if (maskSea(i,j).and.dem(i,j).gt.0.) height(i,j)=mv
              enddo
            enddo              
          endif
c---------------------------------------------------------------
      allocate (maskCalc(nvx,nvy))
c---------------------------------------------------------------
      maskCalc=mv
      latmin1 = +1e37
      latmax1 = -1e37
      lonmin1 = +1e37
      lonmax1 = -1e37
      do i = 1,nvx
        do j = 1,nvy
          if (.not.maskSea(i,j)) cycle
          if (eqmv(height(i,j))) cycle
          if (height(i,j).lt.minheight) cycle
          maskCalc(i,j)=dem(i,j)
          latmin1 = min(latmin1,lat(i,j))
          latmax1 = max(latmax1,lat(i,j))
          lonmin1 = min(lonmin1,lon(i,j))
          lonmax1 = max(lonmax1,lon(i,j))
        enddo
      enddo

      latmin1 = max(latmin,latmin1)
      latmax1 = min(latmax,latmax1)
      lonmin1 = max(lonmin,lonmin1)
      lonmax1 = min(lonmax,lonmax1)
c---------------------------------------------------------------
      if (fullWindow) then
      if (verify) print '(4(a,f8.3))','lonmin1=',lonmin1,' lonmax1=',lonmax1,' latmin=',latmin,' latmax1=',latmax1
      maskCalc=mv
      do i = 1,nvx
        do j = 1,nvy
          if (.not.maskSea(i,j)) cycle
        inside = (lon(i,j).ge.lonmin1.and.lon(i,j).le.lonmax1
     &       .and.lat(i,j).ge.latmin1.and.lat(i,j).le.latmax1)
          if (inside) maskCalc(i,j) = 1
        enddo
      enddo

      else      
      if (nradius_cover.gt.0) then
        if (verify) then
        call coverMap (maskCalc,nvx,nvy,maskSea,6)
        else
        call coverMap (maskCalc,nvx,nvy,maskSea,0)
        endif
      endif
      endif

      n = 0
      do i = 1,nvx
        do j = 1,nvy
          if (.not.eqmv(maskCalc(i,j))) then
          n=n+1
          maskCalc(i,j) = dem(i,j)
          endif
        enddo
      enddo
c---------------------------------------------------------------
      if (maskmap.ne.' ') then
      maps = demmap(1:length(demmap))//null
      call OPENGDAL (maps,nvx,nvy,typ, xor, yor, size,sizey,driver,nband,error)
      outfmt = driver(1:length(driver))//null
      call GETGDALATTR (nband,nbyte,datatype)
      datatype = datatype(1:length(datatype))//null
      call CLOSEGDAL()

	maps = maskmap(1:length(maskmap))//null
	call OUTGDAL (maps,nvx,nvy,typ
     &               , xor, yor, size,maskCalc,nval, sizey, outfmt, nband,error)
      endif
c---------------------------------------------------------------
        print *,n
	stop
	end
