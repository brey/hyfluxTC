      program getMinHeight
      use map_cover_data
      implicit none
c-------------------------------------------------------------
c interface with maps
      integer*4  nval, nvx, nvy, typ
      real*8 xor, yor, size
      real*4,    allocatable :: dem(:,:), height(:,:), demCalc(:,:), lon(:,:),lat(:,:)
      logical*1, allocatable :: maskSea(:,:)

*      real*4 radius,pi,rad2deg, deg2rad,deg2m
*      parameter (radius=6378388.,pi=3.14159265)
*      parameter (rad2deg=180./pi, deg2rad=pi/180, deg2m=deg2rad*radius)

      character null*1 
      integer*1 inull /z'00'/
      equivalence (null,inull)

      real*4     mv 
      integer*1  mv1
c--------------------------------------------
c local variables
      integer*4 iargc, narg,i,j,nv, n, k, nlast
      
      real*4 valget, lonmin, lonmax, latmin, latmax, minheight, minHeightMin, h1, h2
  
      character*132 ch, maps, demmap, heightmap, maskmap

#if defined WIN32
     &        ,bs*1 /'\'/
#else
     &        ,bs*1 /'/'/
#endif
      integer*4 length, lm, lout, linp
c--------------------------------------------
* gdal additional variables
      integer*4 nband, iband,error, nbyte
      character driver*64, datatype*64, outfmt*64
      real*8 sizey
c--------------------------------------------
      character yesno*3
      logical eqmv, existFile, verify, inside
c----------------------------------------------------------
      call getmv1(mv1)
      call getmv(mv)
      lonmin = mv
      lonmax= mv
      latmin=mv
      latmax=mv  
      minHeightMin=0.
      narg = iargc()
      demmap=' '
      heightmap=' '
      maskmap=' '
      verify = .false.
      nradius_cover = 0
c-------------------------------------------------------------------------------------
      if (narg.eq.0) then
      print *,' '
      print *,'getMinHeight help '
      print *,' '
      print *,'Define a mask window and minHeight for which in nv pixels the height  is > minHeightMin '
      print *,' '
      print *,'-dem demSea map (input)'
      print *,' DEFAULT none'
      print *,' NOTE: can be bathymetry.tif or demSea.map'
      print *,' '
      print *,'-height Height map  (input)'
      print *,' DEFAULT none'
      print *,' '
      print *,'-minHeight minimum value of minHeight  (input/output)'
      print *,' DEFAULT ',minHeightMin
      print *,' NOTE: output minHeight can be > of input minHeight'
      print *,' '
      print *,'-mask mask map (output) '
      print *,' DEFAULT none'
      print *,' NOTE: the attribute of the map will be the same of demSea map'
      print *,' '
      print *,'-nradius additional cells (input) to put around true cell '
      print *,' nradius = maxDistLand[km] / (cellsize[deg] * deg2km [110] )'
      print *,' DEFAULT ',nradius_cover
      print *,' '
      print *,'-verify yes/no (input)'
      print *,'    verify data processing'
      print *,'    DEFAULT is ',yesno(verify)
      print *,' '
      print *,'-nv nv (input/output)'
      print *,' DEFAULT none '
      print *,' NOTE: output nv can be < of input nv'
      print *,' '
      print *,'-lonmin lonmin  (input)'
      print *,' DEFAULT from demSea map'
      print *,'-lonmax lonmax  (input)'
      print *,' DEFAULT  from demSea map  '
      print *,'-latmin latmin  (input)'
      print *,' DEFAULT  from demSea map  '
      print *,'-latmax latmax (input)'
      print *,' DEFAULT  from demSea map '
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
c       ----------------------------------------------------
      else if (ch.eq.'-height') then
      call getarg(i+1,heightmap)
c------------------------------------------------------------
      else if (ch.eq.'-nradius') then
      call getarg(i+1,ch)
      nradius_cover = valget(ch)
c------------------------------------------------------------
      else if (ch.eq.'-minHeight') then
      call getarg(i+1,ch)
      minHeightMin = valget(ch)
 
      else if (ch.eq.'-mask') then
      call getarg(i+1,maskmap)
 
      else if (ch.eq.'-nv') then
      call getarg(i+1,ch)
      nv = valget(ch)
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
c------------------------------------------------------------
      enddo
c---------------------------------------------------------------
      maps = demmap(1:length(demmap))//null
      call OPENGDAL (maps,nvx,nvy,typ, xor, yor, size,sizey,driver,nband,error)
      outfmt = driver(1:length(driver))//null
        if (eqmv(lonmin)) then
        lonmin = xor
        else
        lonmin = max(xor,lonmin)
        endif

        if (eqmv(latmax)) then
        latmax = yor
        else
        latmax = min(yor,latmax)
        endif

        if (eqmv(lonmax)) then
        lonmax = lonmin+nvx*size
        else
        lonmax = min(lonmax,lonmin+nvx*size)
        endif

        if (eqmv(latmin)) then
        latmin = latmax+nvy*sizey
        else
        latmin = max(latmin,latmax+nvy*sizey)
        endif

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
        if (existFile(heightmap)) then
        maps = heightmap(1:length(heightmap))//null
        call OPENGDAL (maps,nvx,nvy,typ, xor, yor, size,sizey,driver,nband,error)
        call GETGDALVAL (nband,height)
        h2 = 0
          do i = 1,nvx
            do j = 1,nvy
            if (maskSea(i,j).and.(.not.eqmv(height(i,j))) ) h2=max(height(i,j),h2)
            enddo
          enddo            
/*
          do i = 1,nvx
            do j = 1,nvy
            if (maskSea(i,j).and.dem(i,j).gt.0.) height(i,j)=mv
            enddo
          enddo            
*/
        else
        print *,'NaN'
        call exit(1)
        endif
      if (verify) print *,'maxheight=',h2
c---------------------------------------------------------------
      allocate (demCalc(nvx,nvy))
c---------------------------------------------------------------
      h1=minHeightMin
      k = 0
      minheight = (h1+h2)/2
      nlast=0
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      do while (abs(h1-h2).gt.0.0001.and.k.lt.100)
      k = k+1
c---------------------------------------------------------------
      demCalc=mv
      n=0
      do i = 1,nvx
        do j = 1,nvy
          if (.not.maskSea(i,j)) cycle
          if (eqmv(height(i,j))) cycle
          if (height(i,j).lt.minheight) cycle
          demCalc(i,j)=dem(i,j)
          n=n+1
        enddo
      enddo
c---------------------------------------------------------------
      if (nradius_cover.gt.0) then
        if (verify) then
        print *,'before cover: n=',n
        call coverMap (demCalc,nvx,nvy,maskSea,0)
        else
        call coverMap (demCalc,nvx,nvy,maskSea,0)
        endif
c repeat count of pixel after cover
      n = 0
      do i = 1,nvx
        do j = 1,nvy
          if (eqmv(demCalc(i,j))) cycle
        demCalc(i,j) = dem(i,j)
        n=n+1
        enddo
      enddo
      endif
c---------------------------------------------------------------
        if (verify)  print '(a,f6.3,a,i6)','minHeight=',minheight,' ; n=',n

        if (n.eq.nv.or.n.eq.nlast) exit
        if (n.le.nv) h2 = minheight
        if (n.ge.nv) h1 = minheight
      minheight = max(minHeightMin,(h1+h2)/2)
      nlast=n
      enddo
100   continue
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
     &               , xor, yor, size,demCalc,nval, sizey, outfmt, nband,error)
      endif
c---------------------------------------------------------------
      print '(a,f6.3,a,i6)','minHeight=',minheight,' ; nv=',n
        if (n.eq.0) call exit(1)
      stop
      end