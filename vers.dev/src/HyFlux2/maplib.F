c=========================================================================
      module index_mod
c=========================================================================
      use kind
      implicit none
      integer(i4), allocatable :: iP(:), jP(:)
      real(r4), allocatable :: distP(:)
      end module index_mod
*================================================================================
      module map_interp_data
*================================================================================
      use kind
      implicit none
c     ---------------------------
      real(r4), save, allocatable :: xx(:),yy(:)
      integer(i4), save, allocatable :: p0(:), p1(:), p2(:), p3(:)
      integer(i4), save :: interp0, nx0, ny0, iout0, nout0, ninp
      logical*1, allocatable :: mask0(:),maskout(:)
      end module map_interp_data
c============================================================================
        subroutine getsrtm  (verify,cover,intsrtm,iout,srtmpath,ext, workdir
     &           ,xstart,ystart,cellsize,ncol,nrow,dem
     &           , sizey, mapfmt, mapext, nband,error)
c============================================================================
      use kind
	implicit none
C---------------------------------------------------------------------|
        logical(i4) verify, cover !produce control maps
        integer(i4) ncol,nrow,npcr,iout, intsrtm, d_int
	real(rr8) xstart,ystart,cellsize
        real(r4) dem(ncol*nrow)
C---------------------------------------------------------------------|
        integer*4 nband , iband , error, nbyte
        character driver*64, datatype*64, mapfmt*64, outfmt*64, mapext*5
        real(rr8) sizey
C---------------------------------------------------------------------|
        character srtmpath*(*), ext*(*), workdir*(*), fullpath*254, fil*132
     &           , maps*254
C---------------------------------------------------------------------|
        real(r4), allocatable :: ndem(:,:), z1(:), xv(:),yv(:)
     &           , z0(:,:)
        integer*2, allocatable :: ival2(:,:), z02(:,:)
        logical*1, allocatable :: mask(:)
C---------------------------------------------------------------------|
	integer(i4) linp, lext, length
c-------------------------------------------------------------
C interface with srtm30 data
        real(r4) dcell
        parameter (dcell=30./3600.)  ! srtm30 cell size in deg
        integer(i4) range_lat, range_long, narc
        parameter (range_lat=10,range_long=10)
        parameter (narc=3600/30)  ! no of cell per deg
        integer(i4) longmin2,longmax2,latmin2,latmax2 ! requested srtm window
 
        integer(i4) nlong,nlat,nval
        parameter (nlong=range_long*narc,nlat=range_lat*narc,nval=nlong*nlat)
 
        integer(i4) nlong2, nlat2, nfile

	integer*2 mv2 /z'8f8f'/
 
	integer(i4) long_s /-180/, lat_s /90/
 
c       ----------------------------------------------------
*        data longTiles /-180.,-140.,-100.,-60.,-20.,20.,60.,100.,140.,180./
*        data latTiles /90.,40.,-10.,-60./
c--------------------------------------------
c local variables
	logical(i4) exist
	integer(i4) r_int, ii, i, j, k, l, i0, j0, i1, j1, ll, ilong, notfound
     &             , nval1,nlong1,nlat1, interp
        real(rr8) longmin1,longmax1,latmin1,latmax1,dcell1
        real(r4) x, y, lat1, long1, numget, sngl
      real(r4) pi, deg2rad, rad2deg
      parameter (pi=3.14159265)
      parameter (rad2deg=180/pi,deg2rad=pi/180.)

c-----------------------------------------------------------

	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)

        real(r4) mv /z'ffffffff'/
        integer(i4) imv /z'ffffffff'/, mv4
        integer*1 mv1
        logical*1 lmv1
        equivalence (mv1,lmv1)
#if defined WIN32
      character bs*1 /'\'/
#else
      character bs*1 /'/'/
#endif
      logical cover1
#if defined WIN32
      cover1 = .true.
#else
      cover1 = cover
#endif

c-----------------------------------------------------------
        npcr = ncol*nrow
	call getmv1(mv1)
	call getmv4(mv4)
        call getmv (mv)
c---------------------------------------------------------------
c define srtm30 windows
c---------------------------------------------------------------
       longmin1 = xstart-cellsize
       longmax1 = xstart+(ncol+1)*cellsize
       latmax1 = ystart+cellsize
       latmin1 = ystart-(nrow+1)*cellsize
c---------------------------------------------------------------
c find windows of range_long, range_lat  [deg]
       longmin2 = d_int(longmin1/range_long)*range_long
       longmax2 = d_int(longmax1/range_long+1.d0)*range_long
       latmin2 = d_int(latmin1/range_lat)*range_lat
       latmax2 = d_int(latmax1/range_lat+1.d0)*range_lat

       nlong2 = (longmax2-longmin2)*narc
       nlat2 = (latmax2-latmin2)*narc
       nfile = (nlong2*nlat2)/nval
c---------------------------------------------------------------
       write (iout,*) 'used srtm30 window '
       write (iout,*) 'longmin2=',longmin2,' longmax2=',longmax2
       write (iout,*) 'latmin2=',latmin2,' latmax2=',latmax2
       write (iout,*) 'nlong2=',nlong2,' nlat2=',nlat2,' n.tiles to read =',nfile
c---------------------------------------------------------------
c extract strm30 data (big window)
       linp = length(srtmpath)
       lext = length(ext)


*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       if (intsrtm.eq.0) then
	write (iout,*)
	write (iout,*) 'SRTM resample: suggested for grid size > 1 min'
	write (iout,*)
        if (cellsize.lt.dcell) write (iout,*) 'Caution:  requ. cellsize < srtm cell size : int>0 suggersted'
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
      allocate (ival2(nlong,nlat),ndem(ncol,nrow))
	dem = 0
	ndem = 0
	ii =0
c
	notfound = 0
       do j = latmax2, latmin2+range_lat, -range_lat
         do 100 i = longmin2,longmax2-range_long, range_long
	   ii = ii+1
 
	   ilong = i
*         write (iout,*) '1. ilong=',ilong,' ilat=',j
	     if (ilong.ge.180)  ilong = ilong - 360
	     if (ilong.lt.-180) ilong = ilong + 360
*        write (iout,*) '2. ilong=',ilong,' ilat=',j
         write (fil,'(i4,i3,a)') abs(ilong),abs(j),ext(1:lext)
         call fillzero(fil,7+lext)
           if (ilong.gt.0) then
           fil(1:1)='e'
           else
           fil(1:1)='w'
           endif
 
           if (j.gt.0) then
           fil(5:5)='n'
           else
           fil(5:5)='s'
           endif
 
         fullpath=srtmpath(1:linp)//bs//fil(1:7+lext)//null
         inquire (file=fullpath,exist=exist)
         write (iout,*) 'file ',fil(1:7+lext),' exist = ',exist, ' read ', float(ii)/nfile*100.,' %'
	
           if (.not.exist) then
		 notfound = notfound + 1
		 go to 100
	     endif
 
	   ival2 = mv2
         call getbin1(fullpath,nval,2,ival2)
*	   write (iout,*) '1) nzero=',count(ival2.eq.0),' nmv=',count(ival2.eq.mv2)
 
         call swap2f (ival2,nval)
*	   write (iout,*) '2) nzero=',count(ival2.eq.0),' nmv=',count(ival2.eq.mv2)
 
	   do l = 1,nlat
	   lat1 = j - (l-0.5)*dcell
	   do k = 1,nlong
	   long1 = i + (k-0.5)*dcell
 
	    if  (lat1.ge.latmin1.and.lat1.le.latmax1
     &    .and.long1.ge.longmin1.and.long1.le.longmax1 ) then
            x = long1
            y = lat1
	    i0 = (x-xstart)/cellsize + 1
	    j0 =-(y-ystart)/cellsize + 1
	      if (i0.ge.1.and.i0.le.ncol
     &    .and. j0.ge.1.and.j0.le.nrow
     &     .and. ival2(k,l).ne.mv2
     &       ) then
              ll = (j0-1)*ncol + i0
	      dem(ll) = dem(ll) + ival2(k,l)
	      ndem(i0,j0) = ndem(i0,j0) + 1
	      endif
	     endif

          enddo
        enddo
	
 
100      continue
       enddo
c---------------------------------------------------------------
	if (notfound.gt.0) then
	write (iout,*) '***************************************************'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) '***************************************************'
	endif
c---------------------------------------------------------------
        ll = 0
	do j = 1,nrow
	  do i = 1,ncol
          ll = ll+1
	    if (ndem(i,j).gt.0) then
	    dem(ll) = dem(ll)/ndem(i,j)
	    else
	    dem(ll) = mv
	    endif
	  enddo
	enddo
c---------------------------------------------------------------
          if (cover1) then
          allocate (mask(ncol*nrow))
          mask = .true.
          call coverMap (dem,ncol,nrow,mask,iout)
          deallocate (mask)
          endif
c---------------------------------------------------------------
       deallocate (ival2,ndem)
        write (iout,*) 'resampled srtm to demNode '


*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       else if (abs(intsrtm).ge.1) then
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       write(iout,*)
       write (iout,*) 'SRTM interpolation: suggested for grid size < 1 min'
       write(iout,*)
       if (cellsize.gt.dcell*2) 
     &         write(iout,*) 'Caution:  requ. cellsize > 2 * srtm cell size : int=0 suggested'

         if (intsrtm.ge.0) then
         interp = 0
         else if (intsrtm.lt.0) then
         interp = -1
         endif
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



       allocate (z02(nlong2,nlat2),ival2(nlong,nlat))
	z02 = mv2
	ival2 = mv
c
	ii=0
	notfound = 0
       do j = latmax2, latmin2+range_lat, -range_lat
         do 200 i = longmin2,longmax2-range_long, range_long
	   ii = ii+1
 
	   ilong = i
*          write (iout,*) '1. ilong=',ilong,' ilat=',j
	     if (ilong.ge.180)  ilong = ilong - 360
	     if (ilong.lt.-180) ilong = ilong + 360
         write (fil,'(i4,i3,a)') abs(ilong),abs(j),ext(1:lext)
         call fillzero(fil,7+lext)
           if (ilong.gt.0) then
           fil(1:1)='e'
           else
           fil(1:1)='w'
           endif
 
           if (j.gt.0) then
           fil(5:5)='n'
           else
           fil(5:5)='s'
           endif
 
         fullpath=srtmpath(1:linp)//bs//fil(1:7+lext)//null
         inquire (file=fullpath,exist=exist)
         write (iout,*) 'file ',fil(1:7+lext),' exist = ',exist, ' read ', float(ii)/nfile*100.,' %'
           if (.not.exist) then
		 notfound = notfound+1
		 go to 200
	     endif
 
	   ival2 = mv2
         call getbin1(fullpath,nval,2,ival2)
*	   write (iout,*) '1) nzero=',count(ival2.eq.0),' nmv=',count(ival2.eq.mv2)
 
         call swap2f (ival2,nval)
*	   write (iout,*) '2) nzero=',count(ival2.eq.0),' nmv=',count(ival2.eq.mv2)
 
 
         i0=(i-longmin2)*narc
         j0=(latmax2-j)*narc
 
         z02(i0+1:i0+nlong,j0+1:j0+nlat) = ival2
*	   if (verify) write (iout,*) 'i1=',i0+1,' i2=',i0+nlong,' j1=',j0+1,' j2=',j0+nlat,' nzero=',count(ival2.eq.0)
200      continue
       enddo
       write (iout,*)
       write (iout,*) 'min z02=',minval(z02),' max z02 = ',maxval(z02)
     &              ,' nzero=',count(z02.eq.0.),' nmv=',count(z02.eq.mv2)
c---------------------------------------------------------------
	if (notfound.gt.0) then
	write (iout,*) '***************************************************'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) 'CAUTION ! Not found ',notfound,' tiles'
	write (iout,*) '***************************************************'
	endif
c---------------------------------------------------------------
       deallocate (ival2)
c---------------------------------------------------------------
       write (iout,*)
        if (verify) then
	allocate (z0(nlong2,nlat2))
	z0 = z02

	maps = workdir//bs//'z0'//mapext
        call OUTGDAL (maps,nlong2,nlat2,4
     &                  ,dble(longmin2),dble(latmax2),dble(dcell)
     &                  ,z0, nlong2*nlat2
     &           , sizey, mapfmt, nband,error)

	write (iout,*) 'Created z0.map (extracted srtm tiles)'
	deallocate (z0)
        endif
       write (iout,*) 'min z0 = ',minval(z02),' max z0 = ',maxval(z02)
     &              ,' nzero=',count(z02.eq.0.),' nmv=',count(z02.eq.mv2)
c---------------------------------------------------------------
 
       i0 = (longmin1-longmin2)/dcell
       i1 = (longmax1-longmin2)/dcell+2
       i0 = max(i0,1)
       i1 = min(i1,nlong2)
       j0 = (latmax2-latmax1)/dcell
       j1 = (latmax2-latmin1)/dcell+2
       j0 = max(j0,1)
       j1 = min(j1,nlat2)
       nlong1 = i1-i0+1
       nlat1 = j1-j0+1
       nval1 =  nlong1*nlat1
       write (iout,*) 'i0=',i0,' i1=',i1,' j0=',j0,' j1=',j1
 
       allocate (z1(nval1))
	z1 = mv
       l = 0
        do j=j0,j1
          do i=i0,i1
          l=l+1
#if defined WIN32
	    if (z02(i,j).eq.mv2) then
	    z1(l)= mv
	    else
	    z1(l) = z02(i,j)
	    endif
#else
          z1(l) = z02(i,j)
#endif
          enddo
        enddo
        deallocate (z02)
c---------------------------------------------------------------
	dcell1 = dcell
        longmin1 = longmin2 + dcell1*(i0-1)
        longmax1 = longmin1 + dcell1*nlong1
        latmax1  = latmax2  - dcell1*(j0-1)
        latmin1  = latmax1  - dcell1*nlat1
c---------------------------------------------------------------
       write (iout,*) 'smaller srtm30 window dcell1=',dcell1
       write (iout,*) 'longmin1=',longmin1,' longmax1=',longmax1
       write (iout,*) 'latmin1=',latmin1,' latmax1=',latmax1
       write (iout,*) 'nlong1=',nlong1,' nlat1=',nlat1
c---------------------------------------------------------------
      if (cover1) then
      allocate (mask(nval1))
	mask = .true.
        call coverMap (z1,nlong1,nlat1,mask,iout)
	deallocate (mask)
       endif
c---------------------------------------------------------------
	write(iout,*)
	maps = workdir//bs//'z1'//mapext
        call OUTGDAL (maps,nlong1,nlat1,4
     &                  ,longmin1,latmax1,dcell1,z1, nval1
     &                  ,sizey,mapfmt,nband,error)

	write (iout,*) 'workdir ',workdir
        write (iout,*) 'Created ',maps(1:length(maps))

       write (iout,*) 'min z1=',minval(z1),' max z1 = ',maxval(z1),' nzero=',count(z1.eq.0.) 
       deallocate (z1)
c---------------------------------------------------------------
	allocate (xv(npcr),yv(npcr))
	l=0
	do j = 1,nrow
	  do i = 1,ncol
	  l = l+1
          xv(l) = i-0.5
          yv(l) = j-0.5
	  enddo
	enddo
c---------------------------------------------------------------------------------
c  cellratio = cellsize/1
        iband = 1
        call resmap (maps,iband,xstart,ystart,cellsize,xv,yv,dem,npcr,interp,iout)
        deallocate (xv,yv)
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       else 
       write (iout,*) 'error from getsrtm: not implemented intsrtm = ',intsrtm
       endif
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	return
	end

c==========================================================================================
        subroutine resmap (maps,iband,xstart,ystart,cellratio,x,y,val,nval,interp,iout)
c==========================================================================================
        use kind
        implicit none
c       -------------------------------------------------------------
        character maps*(*)
        real (rr8) xstart,ystart,cellratio
! interp = 0 => bilinear interpolation
! interp = 1 => estrapolation
! interp = -1 => no interpolation
        integer(i4) nval,interp,iout   
        real(r4) x(nval),y(nval),val(nval)
c       -------------------------------------------------------------
        real(rr8) xstart1, ystart1, cellsize1
        integer(i4) nvx, nvy, typemap, length
c--------------------------------------------
* gdal additional variables
        integer*4 nband, iband,error, nbyte
        character driver*64, datatype*64
        real(rr8) sizey
c       -------------------------------------------------------------
        real(r4), allocatable :: z(:)
        logical exist
c       -------------------------------------------------------------
       inquire (file=maps(1:length(maps)),exist=exist)
          if (.not.exist) return
       
       call OPENGDAL (maps,nvx,nvy,typemap, xstart1,ystart1,cellsize1,sizey,driver,nband,error)
       allocate (z(nvx*nvy))
       call GETGDALVAL (iband,z)
         

       xstart1=(xstart1-xstart)/cellratio
       ystart1=(ystart-ystart1)/cellratio
       cellsize1 = cellsize1/cellratio              
c----------------------------------------------------------------------------------
        if (iout.gt.0) then
        write (iout,*) 'resmap ',maps(1:length(maps)),' iband=',iband
        write (iout,*) 'xstart1=',xstart1,' ystart1=',ystart1,' cellsize1=',cellsize1 
        endif
c----------------------------------------------------------------------------------
        call initMapInterp (xstart1,ystart1, cellsize1,cellsize1, nvx, nvy
     &                     ,x,y,nval,interp,iout)
c
c create (inpterpolated) val
        call MapInterp (z,val)
        call endMapInterp
        deallocate (z)

 
        if (iout.gt.0) write(iout,*) 'resampled map=',maps(1:length(maps))

        return
        end

c==========================================================================================
        subroutine initMapInterp (xstart, ystart, dx,dy, nx, ny  ! input map
     &                           ,x,y,nout,interp,iout)  ! output map
c==========================================================================================
        use kind
        use map_interp_data
        implicit none
c       -------------------------------------------------------------
        real(rr8) xstart, ystart, dx,dy
 
! interp = 0 =>pointesr to nodes ( bilinear interpolation)
! interp = -1,1 => pointers to cell center (center value or estrapolation)

        integer(i4)  nx, ny, nout, interp, iout
 
        real(r4) x(nout),y(nout)
c--------------------------------------------------------------------
c local variables
        real(r4), allocatable :: ip(:), jp(:)
        integer(i4), allocatable :: p00(:)
        integer(i4) i,j,l
c--------------------------------------------------------------------
c  create pointers of output map to input map
	allocate (ip(nout),jp(nout),p00(nout))
c--------------------------------------------------------------------
        iout0 = iout
        interp0 = abs(interp)
        ninp = nx*ny
        nx0 = nx
        ny0 = ny
        nout0 = nout
	allocate (p0(nout),xx(nout),yy(nout),maskout(nout))
        if (iout.gt.0) then
	write (iout,*)
	write (iout,*) 'initMapInterp: interp=',interp,' interp0=',interp0
        endif
c--------------------------------------------------------------------
! interp = 0 =>pointers to nodes ( bilinear interpolation)
	if (interp0.eq.0) then
c---------------------------------------------------------------
	ip = (x-xstart)/dx + 0.5
	jp = (y-ystart)/dy + 0.5
        if (iout.gt.0) then
        write (iout,*) 'ip:  min=',minval(ip),' max=',maxval(ip)
        write (iout,*) 'jp:  min=',minval(jp),' max=',maxval(jp)
        endif
        maskout = ip.lt.0. .or. ip.gt.float(nx) .or. jp.lt.0. .or. jp.gt.float(ny)
c     ----------------------------------------------------------
	allocate (p1(nout),p2(nout),p3(nout))	
	xx = ip
	yy = jp
 
	ip = min(max(ip,1.),float(nx-1))
	jp = min(max(jp,1.),float(ny-1))
      p00 = (int(jp)-1)*nx + int(ip)

c note that ip, jp are always positive => int function works as expected

	xx = xx-int(ip)
	yy = yy-int(jp)
        if (iout.gt.0) then
        write (iout,*) 'xx:  min=',minval(xx),' max=',maxval(xx)
        write (iout,*) 'yy:  min=',minval(yy),' max=',maxval(yy)
        endif
c     ----------------------------------------------------------
	p0 = p00
	p1 = p00+1
	p2 = p00+nx
	p3 = p00+nx + 1
c     ----------------------------------------------------------
	deallocate (p00,ip,jp)
c---------------------------------------------------------------

! interp = -1,1 => pointers to cell center (center value or estrapolation)
	else if (interp0.eq.1) then
c---------------------------------------------------------------
	ip = (x-xstart)/dx + 1
	jp = (y-ystart)/dy + 1
c     ----------------------------------------------------------
	xx = ip
	yy = jp
 
	ip = min(max(ip,1.),float(nx))
	jp = min(max(jp,1.),float(ny))
        p00 = (int(jp)-1)*nx + int(ip)
 
c note that ip, jp are always positive => int function works as expected

	xx = xx-int(ip)
	yy = yy-int(jp)
        if (iout.gt.0) then
        write (iout,*) 'xx:  min=',minval(xx),' max=',maxval(xx)
        write (iout,*) 'yy:  min=',minval(yy),' max=',maxval(yy)
        endif
c     ----------------------------------------------------------
	p0 = p00
c     ----------------------------------------------------------
	deallocate (p00,ip,jp)
c----------------------------------------------------------------
	endif
c----------------------------------------------------------------
        return
        end        
c==========================================================================================
        subroutine MapInterp (val0,val)
c==========================================================================================
        use kind
        use map_interp_data
        implicit none
c       -------------------------------------------------------------
        real(r4) val0(*),val(*)
	integer(i4) i
        logical eqmv
c       -------------------------------------------------------------
       allocate (mask0(ninp))
         do i = 1,ninp
         mask0(i) = eqmv(val0(i))
         enddo
        if (iout0.gt.0) write (iout0,*) 'MapInterp: ninp=',ninp,' no. eqmv=',count(mask0)
c       -------------------------------------------------------------        
        if (interp0.eq.0) then
        do i = 1,nout0
          if (mask0(p0(i)).or.mask0(p1(i)).or.mask0(p2(i)).or.mask0(p3(i)).or.maskout(i)) goto 100
        val(i) = val0(p0(i)) 
     &             + xx(i)*(val0(p1(i))-val0(p0(i))) 
     &             + yy(i)*(val0(p2(i))-val0(p0(i)))
     &       + xx(i)*yy(i)*(val0(p0(i))+val0(p3(i))-val0(p1(i))-val0(p2(i)))
100       continue
	  enddo
c       -------------------------------------------------------------
        else
	do i = 1,nout0
          if (mask0(p0(i))) goto 200
        val(i) = val0(p0(i))
200     continue
	enddo
        endif

c       -------------------------------------------------------------
        return
        end

c==========================================================================================
        subroutine MapInterp1 (val0,dvaldx0,dvaldy0,val)
c==========================================================================================
c called if interp = 1  ? 
        use kind
        use map_interp_data
        implicit none
c       -------------------------------------------------------------
        real(r4) val0(*),dvaldx0(*),dvaldy0(*),val(*)
	  integer(i4) i
c       -------------------------------------------------------------
        do i = 1,nout0
        val(i) = val0(p0(i)) 
     &             + xx(i)*dvaldx0(p0(i)) 
     &             + yy(i)*dvaldy0(p0(i))
	  enddo
c       -------------------------------------------------------------
        return
        end
c==========================================================================================
        subroutine endMapInterp 
c==========================================================================================
        use kind
        use map_interp_data
        implicit none
c       -------------------------------------------------------------
       deallocate (p0,xx,yy,mask0,maskout)
         if (interp0.eq.0) deallocate (p1,p2,p3)
       return

       end

c=========================================================================
      subroutine distShore (mask,val, dist,nradius,npcr,ncol,nrow,iout)
c=========================================================================
      use index_mod
      implicit none
c------------------------------------------------------------
      integer(i4) npcr,nradius,ncol,nrow, iout
      logical*1 mask(npcr)
      real(r4) val(ncol,nrow),dist(npcr)
        real(r4) mv /z'ffffffff'/
c------------------------------------------------------------
c local variables
        integer(i4), save, allocatable :: jj(:), ii(:), LP(:,:)
      logical*1, save, allocatable :: notmv(:,:), mask1(:)
 
	integer(i4), save :: i, j, L, M, K, nv, np, i1,i2,j1,j2
 
        logical(i4) eqmv, missval
        logical(i4), save :: first 
        data first /.true./
c------------------------------------------------------------------
c construct indeces of neighboroud
c------------------------------------------------------------------
        call getmv (mv)
      if (first) then
	allocate (ii(npcr),jj(npcr),LP(ncol,nrow),notmv(ncol,nrow),mask1(npcr))
        L=0
          do j = 1,nrow
            do i = 1,ncol
            L = L+1
            ii(L) = i
            jj(L) = j
            LP(i,j) = L
            enddo
          enddo
 
      call makeindex (nradius,np,iout)
      first = .false.
      endif
c------------------------------------------------------------------
        L=0
          do j = 1,nrow
            do i = 1,ncol
            L = L+1
            missval = eqmv(val(i,j))
            notmv(i,j) = .not.missval
            mask1(l) = mask(l).and.missval
            enddo
          enddo
c------------------------------------------------------------------
c calculate distance
c------------------------------------------------------------------
        dist = mv
 
	do 100 L = 1,npcr
	  if (.not.mask1(L)) go to 100
        i1 = max(1,ii(L)-nradius); i2=min(ncol,ii(L)+nradius)
        j1 = max(1,jj(L)-nradius); j2=min(nrow,jj(L)+nradius)
          if (count(notmv(i1:i2,j1:j2)).eq.0) go to 100

	  do 200 K = 1,np
	  i = ii(L)+iP(K)
          j = jj(L)+jP(K)
	    if (i.lt.1.or.i.gt.ncol) go to 200
	    if (j.lt.1.or.j.gt.nrow) go to 200
            if (.not.notmv(i,j)) go to 200
            dist(L) = distP(K)
            go to 100
200       continue

100    continue
 
      return
      end
c=========================================================================
        subroutine makeindex (nradius,np,iout)
c=========================================================================
        use index_mod
	implicit none
c-------------------------------------------------------------
	integer(i4) nradius, np, iout
c-------------------------------------------------------------
c  local variables
	real(r4)     dist, radius
 
	integer(i4)  k_square, i, j, k
        real(r4), allocatable :: dist0(:)
	integer(i4), allocatable :: ii0(:), jj0(:), order(:)
c-------------------------------------------------------------
 
c k_square is the no of pixels in the square (minus the pixel to interpolate)
c (for nradius=1, k_square=9)
 
	k_square = (nradius*2+1)**2
 
	radius = float(nradius)
 
	allocate (order(k_square),ii0(k_square),jj0(k_square)
     &     ,dist0(k_square))
 
c now evaluate the relative index and dist
	
	k = 0
	do i = -nradius, nradius
	  do j = -nradius, nradius
	  dist = sqrt(float(i**2 + j**2))
          if (dist.le.radius) then
	    k = k+1
	    ii0(k) = i
	    jj0(k) = j
	    dist0(k) = dist
	    endif
	  enddo
	enddo
 
	np = k
 
	write (iout,*) 'k_square=',k_square,' np=',np
 
c put in the right order and evaluate weight
 
	call sort_list(dist0,order,np)
 
	allocate (iP(np),jp(np),distP(np))
 
	do i = 1,np
	iP(i) = ii0(order(i))
	jP(i) = jj0(order(i))
      distP(i) = dist0(order(i))
	enddo
 
	deallocate (ii0,jj0,dist0,order)
 
	return
        end
c==========================================================================================
	function s_int(x)
c==========================================================================================
        use kind
	implicit none
	real(r4) x
        integer*4 s_int
c-----------------------------------------
	s_int = int(x)
	if (x.lt.0..and.float(s_int).ne.x) s_int = s_int-1
	return
	end
c==========================================================================================
	function d_int(x)
c==========================================================================================
        use kind
	implicit none
	real(rr8) x
        integer*4 d_int
c-----------------------------------------
	d_int = int(x)
	if (x.lt.0.d0.and.dble(d_int).ne.x) d_int = d_int-1
	return
	end
