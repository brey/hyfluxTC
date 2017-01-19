
c============================================================================
      subroutine getBottom0
c============================================================================
      use geometry_data
      use parameter_data
      implicit none
c
      real(r8), allocatable :: demn(:,:)
      real(r4), allocatable :: dem(:),dzdx(:,:),dzdy(:,:),node(:,:)
c
      integer(i4) i,j,l,k,lm, length, nvx1, nvy1, nval1
      real(r8) lat, halfcell
      logical*4 existFile, batinput /.false./, eqmv
      character fileinp*256
      real(r4) facinp
C=============================================================================
      do while(.true.)
C=============================================================================
      if (allocated(val)) deallocate (val)
C=============================================================================
c get  demNode  and calculate dem by bilinear interpolation
C=============================================================================
        fileinp=' '
        if (existFile(demNodemap)) then 
        fileinp = demNodemap
        facinp = 1
        else if (existFile(batNode)) then
        fileinp = batNode
        facinp = -1
        endif

        if (existFile(fileinp)) then               
        lm = length(fileinp)
        maps = fileinp(1:lm)//null
        call OPENGDAL (maps,nvx,nvy,typemap, xstart, ystart, cellsize,sizey,driver,nband,error)

        nval = nvx*nvy

        allocate (demn(nvx,nvy),dem(nval))

        CALL GETGDALVAL (nband,dem)
        l = 0
	  do j = 1,nvy
	    do i = 1,nvx
            l = l+1
            demn(i,j) = max(-maxDepth,facinp*dble(dem(l)))
	    enddo
 	  enddo
c       -----------------------------------------
        write (iout, *)
        write (iout,*) 'got ',maps(1:lm),' error=',error
	write (iout,*) 'demn: min=',minval(demn),' max=',maxval(demn)

c-----------------------------------------------------------------
        halfcell = 0.5*cellsize
c-----------------------------------------------------------------
        nvx = nvx-1
        nvy = nvy-1
        xstart = xstart + 0.5*cellsize
        ystart = ystart - 0.5*cellsize
        nval = nvx*nvy

        deallocate (dem)
        allocate (dzdx(nvx,nvy),dzdy(nvx,nvy),dem(nval))
        dem = mv
        l = 0
        do j = 1,nvy
          do i = 1,nvx
          l = l+1
          dem(l) = (demn(i,j)+demn(i+1,j)+demn(i,j+1)+demn(i+1,j+1))/4.d0
          dzdx(i,j) = ((demn(i+1,j)-demn(i,j))+(demn(i+1,j+1)-demn(i,j+1)))/(4.d0*halfcell)
          dzdy(i,j) =-((demn(i,j+1)-demn(i,j))+(demn(i+1,j+1)-demn(i+1,j)))/(4.d0*halfcell)
          enddo
        enddo
c-----------------------------------------------------------------
      allocate (val(nval),mask(nvx,nvy))
        if (.not.existFile(batmap)) then
        maps = outdir(1:lout)//bs//'bathymetry.tif'//null
        val = -dem
        driver = 'GTiff'//null
        call OUTGDAL (maps, nvx,nvy,4
     &             , xstart, ystart, cellsize,val,nval, sizey, driver, nband,error)
        endif
c-----------------------------------------------------------------
      call nearShore (dem,dzdx,dzdy,mask,.true.)
      write (iout,*)' done nearShore'
c-----------------------------------------------------------------

        nv = count(mask)
        allocate (zv(nv),dzdk(2,nv))
 
        l = 0; k=0
        do j = 1,nvy
          do i = 1,nvx
          k = k+1
            if (mask(i,j)) then
            l = l+1
            zv(l) = (demn(i,j)+demn(i+1,j)+demn(i,j+1)+demn(i+1,j+1))/4.d0
            dzdk(1,l) = ((demn(i+1,j)-demn(i,j))+(demn(i+1,j+1)-demn(i,j+1)))/(4.d0*halfcell)
            dzdk(2,l) =-((demn(i,j+1)-demn(i,j))+(demn(i+1,j+1)-demn(i+1,j)))/(4.d0*halfcell)
            else
            dem(k) = mv
            endif
          enddo
        enddo
        
        dzdxmap = workdir(1:lw)//bs//'dzdx'//mapext
        dzdymap = workdir(1:lw)//bs//'dzdy'//mapext
c-------------------------------------------------
        if (.not.(existFile(dzdxmap).or.existFile(dzdymap))) then
        call OUTGDAL (dzdxmap, nvx,nvy,4
     &             , xstart, ystart, cellsize,dzdx,nval, sizey, mapfmt, nband,error)
c
        call OUTGDAL (dzdymap, nvx,nvy,4
     &             , xstart, ystart, cellsize,dzdy,nval, sizey, mapfmt, nband,error)        
        endif
c-------------------------------------------------
        write (iout, *)
	write (iout,*) 'zv: min=',minval(zv),' max=',maxval(zv)
        write (iout,*) 'Done dem, dzdx, dzdy in the cell center'
c       ------------------------------------------
        deallocate (demn)
        exit

C=============================================================================
        else  if (existFile(demmap).and.existFile(dzdxmap).and.existFile(dzdymap)) then
C=============================================================================
       write (iout,*)
       write (iout,*) '============================================================'
       write (iout,*) ' Get dem, dzdx, dzdy'
       write (iout,*) '============================================================'
        lm = length(demmap)
	maps = demmap(1:lm)//null
c	-------------------------------------------------------------------	
	call OPENGDAL (maps,nvx,nvy,typemap, xstart, ystart, cellsize,sizey,driver,nband,error)
        nval = nvx*nvy
c-----------------------------------------------------------------
        halfcell = 0.5*cellsize
c-----------------------------------------------------------------
	allocate (dem(nval),dzdx(nvx,nvy),dzdy(nvx,nvy))

	CALL GETGDALVAL (nband,dem)
        dem=max(dem,-maxDepth)

        call readmap ('dzdx',dzdxmap,dzdx,.true.)
        call readmap ('dzdy',dzdymap,dzdy,.true.)

c-----------------------------------------------------------------
      allocate (val(nval),mask(nvx,nvy))
      call nearShore (dem,dzdx,dzdy,mask,.true.)
      write (iout,*)' done nearShore'
c-----------------------------------------------------------------

        nv = count(mask)
        allocate (zv(nv),dzdk(2,nv))

        l = 0; k=0
        do j = 1,nvy
          do i = 1,nvx
          k = k+1
            if (mask(i,j)) then
            l = l+1
            zv(l) = dem(k)
            dzdk(1,l) = dzdx(i,j)
            dzdk(2,l) = dzdy(i,j)
            else
            dem(k) = mv
            endif
          enddo
        enddo
        write (iout, *)
        write (iout,*) 'got ',maps(1:lm)
	write (iout,*) 'dem: min=',minval(zv),' max=',maxval(zv)

        exit

C=============================================================================
        else  if (existFile(batmap).or.existFile(demmap)) then
C=============================================================================
        batinput = existFile(batmap)
        if (batinput) then
        lm = length(batmap)
        maps = batmap(1:lm)//null
        else
        lm = length(demmap)
        maps = demmap(1:lm)//null
        endif
          if (verify) error=-1
        call OPENGDAL (maps,nvx,nvy,typemap, xstart, ystart, cellsize,sizey,driver,nband,error)
        write (iout,*) 'get bathymetry map:',maps(1:lm)
        nval = nvx*nvy
        allocate (dem(nval))
        CALL GETGDALVAL (nband,dem)
          if (batinput) dem = -dem
        dem=max(dem,-maxDepth)

        nvx = nvx+1
        nvy = nvy+1
        xstart = xstart - 0.5*cellsize
        ystart = ystart + 0.5*cellsize
        nval = nvx*nvy

        allocate (demn(nvx,nvy),node(nvx,nvy))
        demn = 0; node=0

        l = 0
	  do j = 1,nvy-1
	    do i = 1,nvx-1
            l = l+1
            demn(i  ,j  ) = demn(i  ,j  ) + dem(l)
            demn(i+1,j  ) = demn(i+1,j  ) + dem(l)
            demn(i  ,j+1) = demn(i  ,j+1) + dem(l)
            demn(i+1,j+1) = demn(i+1,j+1) + dem(l)
            node(i  ,j  ) = node(i  ,j  ) + 1
            node(i+1,j  ) = node(i+1,j  ) + 1
            node(i  ,j+1) = node(i  ,j+1) + 1
            node(i+1,j+1) = node(i+1,j+1) + 1
	    enddo
 	  enddo

        deallocate (dem)
        allocate (dem(nvx*nvy))

        l = 0
	  do j = 1,nvy
	    do i = 1,nvx
            l = l+1
             if (node(i,j).gt.0) then
            dem(l) = demn(i,j)/node(i,j)
            else
            dem(l) = mv
            endif
	    enddo
 	  enddo       

        demNodemap = workdir(1:lw)//bs//'demNode'//mapext
        call OUTGDAL (demNodemap, nvx,nvy,4
     &             , xstart, ystart, cellsize,dem,nval, sizey, mapfmt, nband,error)

        write (iout, *)
        write (iout,*) 'done demNode using bathymetry'
	write (iout,*) 'demNode: min=',minval(dem),' max=',maxval(dem)
        deallocate (dem,demn,node)

c       do not exit

C=============================================================================
        else  
C=============================================================================
        write (iout,*) ' '
        write (iout,*) 'Getbottom0 - Error: not exist dem maps'
        call closeParam (2)
        exit
        endif

C=============================================================================
        enddo
C=============================================================================
*        if (code.eq.'pre_HyFlux2'.or.verify.or.batinput) then
        demmap = workdir(1:lw)//bs//'demSea'//mapext
        call OUTGDAL (demmap, nvx,nvy,4
     &             , xstart, ystart, cellsize,dem,nval, sizey, mapfmt, nband,error)

*        endif

C=============================================================================
       deallocate (dem,dzdx,dzdy)

      call parmap (.true.) 

      return
      end

c============================================================================
      subroutine parmap (outpar)
c============================================================================
      use geometry_data
      use parameter_data
      implicit none
      logical*4 outpar
      integer l
C=============================================================================
      if (mapdeg) then
      deg2m = deg2rad*radius0	! [ m/deg]
      else
      deg2m = 1
      endif
C=============================================================================
      cellsizeN = 2*baseS**maxlev			! coarse cell size [-]
      cellsizeD = cellsize		  		! coarse cell size [deg]
      cellratio = cellsize/cellsizeN
      cellsizeM = cellsize*deg2m 	 		! coarse cell size [m]

        do l = 0,maxlev
        Ms(l)  = baseS**l     			! no. of splits for each refinement level
        nsize(l) = (cellsizeN/2)/ms(l)		! half cell size [-]
        dsize(l) = (cellsizeD/2)/ms(l)		! half cell size [deg]
        msize(l) = (cellsizeM/2)/ms(l)		! half cell size [m]
        enddo
C=============================================================================
      if (.not.outpar) return
C=============================================================================
      write (iout,'(4(a,g13.6))') 'xstart=',xstart,' ystart=',ystart,' cellsize [m] =',cellsizeM,' cellsize [min] =',cellsizeD*60
      write (iout,'(4(a,i10))') ' nvx=', nvx,' nvy=',nvy,' nval=',nvx*nvy,' nv=',nv

      return
      end
c============================================================================
      subroutine resmapShort (map2res,z)
c============================================================================
      use geometry_data
      use parameter_data
      implicit none
c
      integer i,j, i1, j1
      character map2res*(*)
      real(r4) z(nvx,nvy)
      real(r4), allocatable :: z1(:,:)
      real(r4) x,y
      logical*4 eqmv
c-------------------------------------------------------------
c get map
        real(rr8) xstart1, ystart1, cellsize1
        integer(i4) nvx1, nvy1, typemap1
c       ----------------------------------------
* gdal additional variables
        integer*4 nband1, iband1,error1, nbyte1
        character driver1*64, datatype1*64
        real(rr8) sizey1
c============================================================================
c get map to resample       
       call OPENGDAL (map2res,nvx1,nvy1,typemap1, xstart1,ystart1,cellsize1,sizey1,driver1,nband1,error1)
       allocate (z1(nvx1,nvy1))
       iband1 = nband1
       call GETGDALVAL (iband1,z1)
c---------------------------------------------------------------------------------
c center of the requested cell
	do j = 1,nvy
        y = ystart + sizey*(j-0.5)
        j1 = int((y-ystart1)/sizey1) + 1
          if (j1.lt.1.or.j1.gt.nvy1) cycle 
	  do i = 1,nvx
          x = xstart + cellsize*(i-0.5)
          i1 = int((x-xstart1)/cellsize1) + 1
            if (i1.lt.1.or.i1.gt.nvx1) cycle 
          z(i,j) = z1(i1,j1)
	  enddo
	enddo
c---------------------------------------------------------------------------------
      deallocate (z1)
      return
      end
c============================================================================
      subroutine resmapSlope (map2res,val2,dval2)
c============================================================================
      use geometry_data
      use parameter_data
      implicit none
c
      integer i,j,l, lev0
      character map2res*(*)
      real(r4), allocatable :: xn(:,:,:), yn(:,:,:), zn(:,:,:)
      real(r4) val2(nv),dval2(2,nv)
      logical*4 eqmv
c
c============================================================================
      allocate (xn(2,2,nv),yn(2,2,nv),zn(2,2,nv))

      do l = 1,nv
      lev0 = lev(l)
        do j = 1,2
          do i = 1,2
          xn(i,j,l) =  xv(l) + (2*i-3)*nsize(lev0) 
          yn(i,j,l) =  yv(l) + (2*j-3)*nsize(lev0) 
          enddo
        enddo
      enddo
c----------------------------------------------------------------------------------------------------
      zn = mv
      call resmap (map2res,1,xstart,ystart,cellratio,xn,yn,zn,nv*4,0,iout)
c---------------------------------------------------------------------------------
      val2 = mv
      dval2 = mv
        do 100 l = 1,nv
        do j = 1,2
          do i = 1,2
          if (eqmv(zn(i,j,l))) go to 100
          enddo
        enddo

        val2(l) = (zn(1,1,l)+zn(2,1,l)+zn(1,2,l)+zn(2,2,l))/4.d0
        dval2(1,l) = ((zn(2,1,l)-zn(1,1,l))+(zn(2,2,l)-zn(1,2,l)))/(4.d0*dsize(lev(l)))
        dval2(2,l) =-((zn(1,2,l)-zn(1,1,l))+(zn(2,2,l)-zn(2,1,l)))/(4.d0*dsize(lev(l)))
100     continue
      deallocate (xn,yn,zn)
      return
      end

c============================================================================
      subroutine getBottom
c============================================================================
      use geometry_data
      use parameter_data
      implicit none
c
      real(r4), allocatable :: zv1(:), dzdk1(:,:)
      integer(i4) i,j,l,k,m,lm, length, lev0
      real(r8) lat, omega, halfcell
      logical*4 exist
      character chlev*1

      allocate (zv(nv),dzdk(2,nv),zv1(nv),dzdk1(2,nv))
c============================================================================
      call resmapSlope (workdir(1:lw)//bs//'z1'//mapext,zv1,dzdk1)
      zv = zv1;  dzdk = dzdk1
      deallocate (zv1,dzdk1)
c============================================================================
       write (iout,*) 'done zv, dzdk'

      if (allocated(val)) deallocate(val)
      allocate (val(qlev(maxlev)%nval))

      do lev0 = 0,maxlev
      ql => qlev(lev0)
      write (iout,*)'lev=',lev0,' nv=',ql%nv,' nval=',ql%nval
      val = mv
      val(ql%kl) = zv(ql%ll)
      write (chlev,'(i1)') lev0
      maps = workdir(1:lw)//bs//'demSea.'//chlev//mapext
        call OUTGDAL (maps, ql%nvx,ql%nvy,4
     &             , xstart, ystart, ql%cellsize, val, ql%nval, sizey, mapfmt, nband,error)
      enddo
      deallocate (val)

      return
      end

c============================================================================
      subroutine getDepth (outmap)
c============================================================================
      use parameter_data
      use geometry_data
      use hydro_data
      use slope_data
      use parameter_data
      implicit none
c
      integer(i4) l, length,lm, lev0
      logical(i4) eqmv, outmap, existFile, existFault
      logical(i4) sumslope1 
 
      real(r4), allocatable :: depth(:),zsurf(:)
      real(r4), allocatable :: fault(:),dfault(:,:)
      real(r8)  valmin, valmax
      real(r4) slope
      character chlev*1
c     ----------------------------------------------------
      if (allocated(val)) deallocate(val)
      if (allocated(ival4)) deallocate(ival4)
      allocate (val(nval),ival4(nval)) 
c------------------------------------------------------
c get pixshore
      maps = workdir(1:lw)//bs//'pixShore'//mapext
      allocate (pixShore(nval))
      call readmap ('pixShore',maps,pixShore,.true.)
c------------------------------
      allocate (hj(2,2,nv),dzwdk(2,nv))
      allocate (fault(nv),dfault(2,nv),depth(nv),zsurf(nv),zminv(nv)) ! local
c     -------------------------
      fault = mv
      dfault =mv
      existFault = existFile(faultmap) 
      if (existFault) call resmapSlope (faultmap(1:length(faultmap))//null,fault,dfault)
c------------------------------
      sumslope1 = .true.
        do l = 1,nv
        lev0 = lev(l)
          if (sumslope1) then
          slope =   (abs(dzdk(1,l))+abs(dzdk(2,l)))
          else
          slope = max(abs(dzdk(1,l)),abs(dzdk(2,l)))
          endif
        zminv(l) = zv(l)-dsize(lev0)*slope
        enddo

      write (iout,*) 'zmin: min=',minval(zminv),' max=',maxval(zminv)

c------------------------------
c water depth and surface level
      allocate (hv(nv),zwv(nv),wetfrac(nv),case(nv),hk(2,nv),wetfracIn(nv),zwvIn(nv),hvIn(nv))
c------------------------------
      zsurf = mv
        if (existFile(zsurfmap)) then
        call resmap (zsurfmap,1,xstart,ystart,cellratio,xv,yv,zsurf,nv,0,iout)
        else
        write (iout,*) 'zsurf map not defined: set to seaLevel = ',seaLevel
        endif
c------------------------------
C evaluate depth from zsurf/sea level
        do l = 1,nv
c zsurf is defined by seaLevel => 
            if (eqmv(zsurf(l))) then
              if (zminv(l).gt.runUpLim.and.pixShore(kl(l)).gt.minDistSlope) then
              zwv(l) = max(seaLevel,zv(l)+hmin)
              dzwdk(1:2,l) = dzdk(1:2,l)
              else
              zwv(l) = max(seaLevel,zminv(l)+hmin)
              dzwdk(1:2,l) = zero
              endif
c zsurf is defined by map => slope=0
            else
            zwv(l) = max(zsurf(l),zminv(l)+hmin)
            dzwdk(1:2,l) = zero
            endif
        enddo    
c-----------------------------------------------------------
c  add fault if faultDem == .true.
c-----------------------------------------------------------
      if (faultDem.and.existFault) then
        do l = 1,nv
          if (.not.(eqmv(fault(l)).or.eqmv(dfault(1,l)).or.eqmv(dfault(2,l)))) then
          zv(l) = zv(l) + fault(l)
          dzdk(1:2,l) = dzdk(1:2,l) + dfault(1:2,l)          
          endif
        enddo
      endif
c-----------------------------------------------------------
c  add fault if faultZsurf == .true.
c-----------------------------------------------------------
      if (faultZsurf.and.existFault) then
        do l = 1,nv
          if (.not.(eqmv(fault(l)).or.eqmv(dfault(1,l)).or.eqmv(dfault(2,l)))) then
          zwv(l) = zwv(l) + fault(l)
          dzwdk(1:2,l) = dzwdk(1:2,l) + dfault(1:2,l)          
          endif
        enddo
      endif
c------------------------------
c now re-evaluate depth for hydro calc
        do l = 1,nv
        call surf2depth (zv(l),zwv(l),dzdk(1:2,l),dzwdk(1:2,l),hv(l)
     &                  ,hj(1:2,1:2,l),hk(1:2,l),wetfrac(l),case(l),dsize(lev(l)))
          if (hv(l).le.hvel0) wetfrac(l) = zero
        enddo
c-----------------------------------------------------------
      wetfracIn = wetfrac
      zwvIn = zwv
      hvIn = hv
c-----------------------------------------------------------
      if (outmap) then
c-----------------------------------------------------------
      maps = workdir(1:lw)//bs//'depth'//mapext
        if (.not.existFile(maps)) then
        val = mv   
        val(kl) = hv
        call OUTGDAL (maps, nvx,nvy,4
     &           , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)
        endif
c     ----------------------------------------------------
      maps = workdir(1:lw)//bs//'fault'//mapext
        if (.not.existFile(maps)) then
        val = mv   
        val(kl) = fault
        call OUTGDAL (maps, nvx,nvy,4
     &            , xstart, ystart, cellsize,val,nval, sizey, mapfmt, nband,error)
        endif
c     ----------------------------------------------------
      maps = workdir(1:lw)//bs//'zsurf'//mapext
        if (.not.existFile(maps)) then
        val = mv   
        val(kl) = zwv
        call OUTGDAL (maps, nvx,nvy,4
     &           , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)
        endif
c     ----------------------------------------------------
      maps = workdir(1:lw)//bs//'wetfrac'//mapext
        if (.not.existFile(maps)) then
        val = mv   
        val(kl) = wetfrac
        call OUTGDAL (maps, nvx,nvy,4
     &           , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)
        endif
c     ----------------------------------------------------
      maps = workdir(1:lw)//bs//'case'//mapext
        if (.not.existFile(maps)) then
        ival4 = mv4
          do l = 1,nv
            if (case(l).ge.0) ival4(kl(l)) = case(l)
          enddo
        call OUTGDAL (maps, nvx,nvy,2
     &           , xstart, ystart, cellsize,ival4,nval
     &           , sizey, mapfmt, nband,error)
        endif
c     ------------------------------------------------------
        if (faultDem.and.existFault) then
        maps = workdir(1:lw)//bs//'demFault'//mapext
          if (.not.existFile(maps)) then
          val = mv   
          val(kl) = zv
          call OUTGDAL (maps, nvx,nvy,4
     &             , xstart, ystart, cellsize,val,nval
     &             , sizey, mapfmt, nband,error)
          endif
        endif
c     ----------------------------------------------------
      maps = workdir(1:lw)//bs//'zminv'//mapext
        if (.not.existFile(maps)) then
        val = mv   
        val(kl) = zminv
        call OUTGDAL (maps, nvx,nvy,4
     &           , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)
        endif
c-----------------------------------------------------------
      endif
c-----------------------------------------------------------
      if (maxlev.gt.0) then
      if (allocated(val)) deallocate(val)
      allocate (val(qlev(maxlev)%nval))
c     ------------------------------------------------------
      do lev0 = 0,maxlev
      ql => qlev(lev0)
      val = mv
      val(ql%kl) = courmax*msize(lev0)*2/(sqrt(g*hv(ql%ll))+vmaxShore)
      write(iout,'(a,i2,a,i10,4(a,f10.3))')'lev=',lev0,' nv=',ql%nv
     &  ,' min dtloc =',minval(courmax*msize(lev0)*2/(sqrt(g*hv(ql%ll))+vmaxShore))
     &  ,' max hv =',maxval(hv(ql%ll))
     &  ,' cellsize [m]=',msize(lev0)*2
     &  ,' cellsize [min] = ',dsize(lev0)*2*60
      write (chlev,'(i1)') lev0
      maps = workdir(1:lw)//bs//'dtloc.'//chlev//mapext
        call OUTGDAL (maps, ql%nvx,ql%nvy,4
     &             , xstart, ystart, ql%cellsize, val, ql%nval
     &           , sizey, mapfmt, nband,error)
      enddo

      deallocate (val)
      endif

c-----------------------------------------------------------
c evaluate maxdhmax if not defined
      if (maxdhmax.lt.0.) then
      valmin = 1d30
      valmax = -1d30
        do l = 1,nv
          if (zminv(l).gt.seaLevel) then
          valmin = min(valmin,hv(l))
          valmax = max(valmax,hv(l))
          else
          valmin = min(valmin,zwv(l))
          valmax = max(valmax,zwv(l))
          endif
        enddo
      maxdhmax = 1.5*(valmax-valmin)
      maxdhmax = min(max(maxdhmax,0.01),50.)
      write (iout,*) '----------------------------'
      write (iout,*) 'Set maxdhmax to ',maxdhmax
      write (iout,*) '----------------------------'
      endif

      deallocate (fault,depth,zsurf,dfault)
      return
      end

c============================================================================
      subroutine getJunction
c============================================================================
      use parameter_data
      use statistics_data
      use geometry_data
      implicit none
c
      integer(i4) l
      integer(i4) m,pl,pr, levj, lev0
      logical(i4) maskl,maskr
      real(r8) zL,zR, step    , lat, dxj, fac,coslat1, coslat2, dxL, dxR
c-------------------------------------------------
      allocate (cosLatv(nv),dzdkm(2,nv),tanLatv(nv))
      cosLatv = 1 ; dzdkm = dzdk; tanLatv=0
!      deg2m [ m/deg]
        if (mapdeg) then
          do l = 1,nv
          lat = ystart - yv(l)*cellsizeD/cellsizeN
          tanLatv(l) = tan(deg2rad*lat)/radius0
/*
          coslat1 = cos(deg2rad*(ystart - (yv(l)+0.5)*cellsizeD/cellsizeN))
          coslat2 = cos(deg2rad*(ystart - (yv(l)-0.5)*cellsizeD/cellsizeN))
          cosLatv(l) = 0.5d0*(coslat1+coslat2)
*/
          cosLatv(l) = cos(deg2rad*lat)
          dzdkm(1,l) = dzdk(1,l)/(deg2m*cosLatv(l))
          dzdkm(2,l) = dzdk(2,l)/deg2m  !  [m/deg] / [m/deg] = [m/m] = [-]
          enddo
        endif
c-------------------------------------------------
      allocate (areav(nv))
        do l = 1,nv
        areav(l) = (msize(lev(l))*2)**2
        enddo
c-------------------------------------------------
      write (iout,*) 'done cosLatv, areav'
c-------------------------------------------------
c
c junction

      nstep = 0
      maxstep = 0
      avgstep = 0

      allocate (zj(nj),facL(nj),facR(nj),deepJ(nj))
 
      do l = 1,nj
      m = jtype(l)
      pl = jtol(l)
      pr = jtor(l)
      maskl = pl.gt.0
      maskr = pr.gt.0
c     -------------------------------------------
      if (maskl.and.maskr) then
      levj = max(lev(pl),lev(pr))
      else if (maskl) then
      levj = lev(pl)
      else if (maskl) then
      levj = lev(pr)
      endif
      dxj = msize(levj)*2 
        if (m.eq.1.and.mapdeg) then
*        lat = ystart - yj(l)*cellsizeD/cellsizeN
*        dxL = dxj/cos(deg2rad*lat)
*        dxR = dxL
          if (pl.gt.0) dxL = dxj / cosLatv(pl) 
          if (pr.gt.0) dxR = dxj / cosLatv(pr) 
        else
        dxL = dxj
        dxR = dxj
        endif 
c     -------------------------------------------
          if (maskl.and.maskr) then
          zL = zv(pl)+dzdk(m,pl)*dsize(lev(pl))
          zR = zv(pr)-dzdk(m,pr)*dsize(lev(pr))
          zj(l) = max(zL,zR)
          facL(l) = dxL/areav(pl)
          facR(l) = dxR/areav(pr)

          else if (maskl) then
          zL = zv(pl)+dzdk(m,pl)*dsize(lev(pl))
          zR = zL
          zj(l) = zL
          facL(l) = dxL/areav(pl)

          else if (maskr) then
          zR = zv(pr)-dzdk(m,pr)*dsize(lev(pr))
          zL = zR
          zj(l) = zR
          facR(l) = dxR/areav(pr)
          endif

          deepJ(l) = solver.eq.2.and.varjun.eq.0

          if (.not.(maskl.and.maskr)) cycle

          step = max(abs(zL-zj(l)),abs(zR-zj(l)))
          maxstep = max(step,maxstep)
          deepJ(l) = step.lt.errstep
            if (step.lt.errstep) cycle
          nstep = nstep+1
          avgstep = avgstep+step        
c     -------------------------------------------
      enddo
c-----------------------------------------------------------
      write(iout,*) 'maxstep=',maxstep,' avgstep=',avgstep/max(nstep,1)
      write(iout,*) 'nstep=',nstep,' nstep/nj[%]=',nstep/float(nj)*100.
      write(iout,*) 'min facL=',minval(facL),' max facL=',maxval(facL)
      write(iout,*) 'min facR=',minval(facR),' max facR=',maxval(facR)
c-----------------------------------------------------------
      deallocate (areav)
c-----------------------------------------------------------
      return
      end

c========================================================================
      subroutine getOtherMaps
c========================================================================
      use parameter_data
      use boundary_data
      use geometry_data
      use hydro_data
      use slope_data
      implicit none
c----------------------------------------------------------
      real(r4), allocatable ::  vx(:),vy(:)

      integer(i4) i,j,l, length, stat
      logical(i4) eqmv, existFile
      real(r4) dum, cdAir/0.8e-3/
c------------------------------
      if (allocated(val))   deallocate (val)
      if (allocated(ival1)) deallocate (ival1)
      if (allocated(ival4)) deallocate (ival4)
      allocate (val(nval),ival1(nval),ival4(nval))
c-----------------------------------------------------
c Velocity
      allocate (vx(nv),vy(nv))
      allocate (vv(2,nv),vn(nv))
c------------------------------
      vx = zero
        if (vxmap.ne.' ') then
        call resmap (vxmap,1,xstart,ystart,cellratio,xv,yv,vx,nv,0,iout)
        else
        write (iout,*) 'vx map not defined: set to zero'
        endif
      vy = zero
        if (vymap.ne.' ') then
        call resmap (vymap,1,xstart,ystart,cellratio,xv,yv,vy,nv,0,iout)
        else
        write (iout,*) 'vy map not defined: set to zero'
        endif

      vn = zero
        do l =1,nv
          if (eqmv(vx(l))) vx(l) = 0
          if (eqmv(vy(l))) vy(l) = 0
          if (hv(l).le.hvel0) then
          vx(l) = 0
          vy(l) = 0
          endif
        vv(1,l) = vx(l)
        vv(2,l) = vy(l)
        vn(l) = sqrt(vv(1,l)*vv(1,l)+vv(2,l)*vv(2,l))
        enddo
/*
      vx = zero
      call readmap ('vx',vxmap,vx,.false.)
 
      vy = zero
      call readmap ('vy',vymap,vy,.false.)
 
      vn = zero
        do l =1,nv
        i = il(l)
        j = jl(l)
          if (eqmv(vx(i,j))) vx(i,j) = 0
          if (eqmv(vy(i,j))) vy(i,j) = 0
          if (hv(l).le.hvel0) then
          vx(i,j) = 0
          vy(i,j) = 0
          endif
        vv(1,l) = vx(i,j)
        vv(2,l) = vy(i,j)
        vn(l) = sqrt(vv(1,l)*vv(1,l)+vv(2,l)*vv(2,l))
        enddo
*/
c------------------------------
      deallocate (vx,vy)
c------------------------------
      write (iout,*) 'vx: min=',minval(vv(1,1:nv)),' max=',maxval(vv(1,1:nv))
      write (iout,*) 'vy: min=',minval(vv(2,1:nv)),' max=',maxval(vv(2,1:nv))
      write (iout,*) 'vn: min=',minval(vn),' max=',maxval(vn)
      write (iout,*) 'vn: min=',minval(vn),' max=',maxval(vn)
c-----------------------------------------------------
c rain
      if (existFile(rainmap)) then
      allocate (rain(nv),addrain(nv))
      val = zero
      call readmap ('rain',rainmap,val,.false.)
      rain = val(kl)
      addrain = zero
      endif

c------------------------------
c manning
      allocate (manslope(nv))
      allocate (man(nv))
      man = 0
c
      if (existFile(manmap)) then
      val = zero
      call readmap ('man',manmap,val,.false.)
      man = val(kl)
        do l = 1,nv
          if (eqmv(man(l))) man(l) = 0
        enddo

      else
        do l = 1,nv
          man(l)=manval
            if ((zv(l).gt.demReef.and.zv(l).lt.seaLevel).or.case(l).ge.0) man(i)=man(i)+manReef
        enddo
      endif
c------------------------------
c  manslope
      val = manslopeval
      call readmap ('manslope',manslopemap,val,.false.)
      manslope = val(kl)
 
      do l = 1,nv
        if (.not.eqmv(manslope(l)).and.man(l).gt.0) then
        dum = (dzdkm(1,l)**2+dzdkm(2,l)**2)**expslope
        man(l) = man(l) + manslope(l)*dum/(one+dum)
c botton friction must be higher than air friction
*        man(l)=max(man(l),sqrt(hv(l)**onethird*rhoa/rhow*cdAir/g))
        man(l)=max(man(l),1e-3)
        endif
      enddo
 
      deallocate (manslope)
 
      write (iout,*) 'manout: min=',minval(man),' max=',maxval(man)
c------------------------------
c out manning
      val = mv
      val(kl) = man
      maps = workdir(1:lw)//bs//'manout'//mapext
      call OUTGDAL (maps, nvx,nvy,4
     &           , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)
c------------------------------
      man = man*man
c------------------------------
c hydr. data
      allocate (manh(nv))
 
      manh = zero
 
      do l = 1,nv
c   n^2 = n0^2 + dzdx * h^(4/3) / v^2
      manh(l) =  man0*man0+sqrt(dzdkm(1,l)**2+dzdkm(2,l)**2)*hvel0**fourthird/velmin**2
      enddo
 
      write (iout,*) 'manh: min=',sqrt(minval(manh)),' max=',sqrt(maxval(manh))
c------------------------------
c out manh
      val = mv
      val(kl) = sqrt(manh)
      maps = workdir(1:lw)//bs//'manh'//mapext
      call OUTGDAL (maps, nvx,nvy,4
     &           , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)
c------------------------------
c noSlope
      allocate (noSlope(nv))
      noSlope = .false.
        if (existFile(noSlopemap)) then
        maps = noSlopemap(1:length(noSlopemap))//null
         call OPENGDAL (maps,nvx,nvy,typemap, xstart, ystart, cellsize,sizey,driver,nband,error)
         call GETGDALVAL1 (nband,ival1)
           do l = 1,nv
           i = kl(l)
           noSlope(l) = ival1(i).eq.1
           enddo

        else if (noslopemap.eq.'yes') then
          do l = 1,nv
          noSlope(l) =  zv(l).gt.seaLevel.or.case(l).gt.0
          enddo
        endif
c------------------------------
      write (iout,*)
      write (iout,*) 'done initial: nvol=',nv,' njun=',nj
c------------------------------
      deallocate (ival1)
c------------------------------
      return
      end

c=====================================================
      FUNCTION LIMITER (a,b,ilim,fac0,small0)
      use constants
      IMPLICIT NONE
      real(r8) LIMITER,a,b, fi,ab
     &               ,two,teta, fac1,small
      real(r4) fac0, small0
      integer(i4) ilim
c
      parameter (two=2.d0)
c=====================================================
      fac1 = dble(fac0)
      small = dble(small0)
c=====================================================
      if (ilim.eq.-1) then
        if (a*b.le.zero) then
        limiter = zero
        else
        limiter = (a+b)*half
        endif
c=====================================================
      else if (ilim.eq.0) then
      limiter = zero
c=====================================================
      else if (ilim.eq.1) then
        if(a*b.le.small) then
*       if(a*b.le.zero.or.abs(a).lt.small.or.abs(b).lt.small) then
        limiter = zero
 
        else  if(abs(a).lt.abs(b)) then
        limiter = a
 
        else
        limiter = b
        end if
c=====================================
      else if (ilim.eq.2) then
        if (a*b.le.small) then
*       if(a*b.le.zero.or.abs(a).lt.small.or.abs(b).lt.small) then
        fi  = zero
        else
          if (abs(a) .gt. zero ) then
          teta = b/(a+small)
 
          else
          teta = b/(a-small)
          end if
        fi = max(zero,min(one,two*teta),min(two,teta))
        endif
       limiter = fi*a
c=====================================
c if fac1=0  then equal to ilim = 1
      else if (ilim.eq.3) then
        if (a*b.le.small) then
*       if(a*b.le.zero.or.abs(a).lt.small.or.abs(b).lt.small) then
        limiter = zero
        else
          if (abs(a).lt.abs(b)) then
          limiter = a*(b/(fac1*a+b))

          else
          limiter = b*(a/(a+fac1*b))
          end if
        endif
c=====================================
c if fac1=0  then equal to ilim = 1
      else if (ilim.eq.4) then
        if (a*b.le.small) then
        limiter = zero

        else 
        ab = a*b
*         if (small.gt.zero) ab=ab*(one-small/(ab+small))

          if (fac1.ge.zero) then   ! same as for ilim=3
            if (abs(a).lt.abs(b)) then
            limiter = ab/(fac1*a+b)

            else 
            limiter = ab/(a+fac1*b)
            end if

          else   !   similar to superbe
          fac1 = abs(fac1)
            if (abs(a).lt.abs(b)) then
            limiter = (fac1+one)*ab/(fac1*a+b)
            else
            limiter = (fac1+one)*ab/(a+b*fac1)
            endif

          endif
        endif
c=====================================
c if fac1=0  then equal to ilim = 1
      else if (ilim.eq.6) then
        if (a*b.le.small) then
        limiter = zero

        else 
          if (a.lt.zero) then
          limiter = max(a*fac1,half*(a+b),b*fac1)
          else
          limiter = min(a*fac1,half*(a+b),b*fac1)
          endif
        endif
c=====================================
      else
      write (iout,*) 'ilim=',ilim,' not available'
      stop
      endif
c=====================================
      RETURN
      END
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c input zsurf, output depth & shore
c new shore model
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

        subroutine surf2depth (z0,zsurf,dzdk,dzwdk,depth,hj,hk,shore,case,halfcell)
        use parameter_data
        implicit none
        real(r8) zsurf,z0,dzdk(2),dzwdk(2),depth,hk(2),halfcell,cellsize, shore1
        real(r4) shore
        integer*1 case

        real(r8) hs, minhj, maxhj, eps/1d-12/
     &        ,dwdk(2), ae, an, hj(2,2)
     &        ,hne, hse, hnw, hsw, hn, he,dhk(2)

        integer(i4) i
        logical(i4) ok
C--------------------------------------------------------
c  case = 0 in case of completly wet or completly dry
c  case = 1-3 in case of partially wet
C--------------------------------------------------------
        hs = zsurf-z0
        cellsize = 2*halfcell
C--------------------------------------------------------
        dwdk = dzwdk-dzdk
        dhk = cellsize*abs(dwdk)
        dhk = max(dhk,eps)
C--------------------------------------------------------
        hj(1,1:2) = hs + halfcell*dwdk
        hj(2,1:2) = hs - halfcell*dwdk
C--------------------------------------------------------
        ae = maxval(abs(dwdk))
        an = minval(abs(dwdk))
        he = cellsize*ae
        hn = cellsize*an
C--------------------------------------------------------
        hsw = hs - half*(he+hn)
        hne = hs + half*(he+hn)
C--------------------------------------------------------
c completly wet
C--------------------------------------------------------
        if (hsw.ge.zero) then
        case = 0
        shore = 1.
        depth = max(hs,hmin)
        hk = depth
        hj = max(hj,zero)
        return
        endif
C--------------------------------------------------------
c dry or partially wet
C--------------------------------------------------------
        do i = 1,2
        minhj = minval(hj(1:2,i))
        maxhj = maxval(hj(1:2,i))
          if (minhj.ge.zero) then
          hk(i) = half*(hj(1,i)+hj(2,i))
          else if (maxhj.lt.hmin) then
          hk(i) = zero
          else
*          hk(i) = half*maxhj**2/dhk(i)
          hk(i) = half*maxhj*(hs/dhk(i)+half)
          endif
        enddo
        hj = max(hj,zero)
C--------------------------------------------------------
        hse = hne -  hn
        hnw = hne -  he
        ok = .true.
C--------------------------------------------------------
        if (hse.le.zero.and.hnw.le.zero) then
        case = 1
        shore1 = hne*hne/(2*max(hn,eps)*max(he,eps))
        depth = onethird*hne*shore1
c       -------------------------------------------
        else if (hse.ge.zero.and.hnw.le.zero) then
        case = 2
        depth = (3*hne*(hne-hn)+hn**2)/(6*max(he,eps))
        shore1 = (hne-half*hn)/max(he,eps)
c       -------------------------------------------
        else if (hse.ge.zero.and.hnw.ge.zero) then
        case = 3
        depth = (hne**3-hse**3-hnw**3)/(6*max(hn,eps)*max(he,eps))
        shore1 = (hne**2-hse**2-hnw**2)/(2*max(hn,eps)*max(he,eps))
c       -------------------------------------------
        else
        ok = .false.
        endif
c--------------------------------------------------
        if (.not.ok) then
        write (iout,*) 'surf2depth'
        write (iout,*) 'zv=',z0,' zsurf=',zsurf,' cellsize=',cellsize
        write (iout,*) 'dzdk=',dzdk
        write (iout,*) 'dzwdk=',dzwdk
        write (iout,*) 'case=',case,' shore1=',shore1
        write (iout,*) 'hs=',hs,' depth=',depth
        write (iout,*) 'hnw=',hnw,' hne=',hne
        write (iout,*) 'hsw=',hsw,' hse=',hse
        write (iout,*) 'ae=',ae,' an=',an
        depth = max(hs,hmin)
        hj = zero
        hk = zero
        shore = 0
        endif
C--------------------------------------------------------
c  completly dry
c--------------------------------------------------
          if (depth.le.hmin) then
          shore = 0.
          case = 0.
          hj = zero
          hk = zero
          endif
c-------------------------------------------------- 
        shore = shore1
        return 
        end
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c input depth, output zsurf & shore
c new shore model
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine depth2surf (depth,z0,dzdk,dzwdk,hj,hk
     &                       ,zsurf,shore,case,halfcell)
        use parameter_data
        use statistics_data
        implicit none
        real(r8) zsurf,z0,dzdk(2),dzwdk(2),hj(2,2),depth, hk(2),halfcell,cellsize
        real(r4) shore
        integer*1 case

        real(r8) hs,dwdk(2)
     &        , ae, an, err, eps/1d-12/
     &        ,HNE,HSE,HNW,hsw,he,hn,dhk(2), minhj, maxhj, shore1
     &        ,depth1, depth2, fach, B,C,D,root3, depthRec, hmin1/1d-8/

        integer(i4) i,iter,itermax/10/
        logical(i4) ok
C--------------------------------------------------------
c  case = 0 in case of completly wet or completly dry
c  case = 1-3 in case of partially wet
c--------------------------------------------------------
c completly dry
c--------------------------------------------------
          if (depth.le.hmin) then
          case = 0
          shore = 0.
          hj = zero
          hk = zero
          zsurf = z0
          return
          endif
c--------------------------------------------------
        cellsize = 2*halfcell
        dwdk = dzwdk-dzdk
        hk = depth
        hs = depth   ! guess value which is correct in case of completly wet
        hj(1,1:2) = hs + halfcell*dwdk
        hj(2,1:2) = hs - halfcell*dwdk
C--------------------------------------------------------
        dhk = cellsize*abs(dwdk)
        dhk = max(dhk,eps)
C--------------------------------------------------------
        ae = maxval(abs(dwdk))
        an = minval(abs(dwdk))
        he = cellsize*ae
        hn = cellsize*an
C--------------------------------------------------------
        hsw = hs - half*(he+hn)
        hne = hs + half*(he+hn)
C--------------------------------------------------------
c completly wet
C--------------------------------------------------------
        if (hsw.ge.zero) then  ! completly wet
        case = 0
        shore = 1.
        zsurf = z0 + hs
        return
        endif
C--------------------------------------------------------
c partially wet
C--------------------------------------------------------
*       depth = (3*hne*(hne-hn)+hn**2)/(6*he)
c   hse = 0 => hne=hn
        depth1 = hn*hn/(6*max(he,eps))
c   hnw = 0 => hne=he
*       depth2 = (3*he*(he-hn)+hn**2)/(6*max(he,eps))
        depth2 = (3*he*(he-hn))/(6*max(he,eps)) + depth1
C--------------------------------------------------------
        if (depth.le.depth1) then
        case = 1
        iter = 0
        hne = (6*depth*he*hn)**onethird
        shore = hne**2/(2*max(hn,eps)*max(he,eps))
        depthRec = hne**3/(6*max(hn,eps)*max(he,eps))
        ok = abs(depth-depthRec).le.hmin1
c       -------------------------------------------
        else if (depth.le.depth2.or.hn.lt.zero) then
        case = 2
        iter = 0
        hne = half*hn + sqrt(2*he*depth-hn*hn/12)
        hse = hne-hn
        shore = (hne-half*hn)/max(he,eps)
        depthRec = (3*hne*(hne-hn)+hn**2)/(6*max(he,eps))
        ok = abs(depth-depthRec).le.hmin1
c       -------------------------------------------
        else
        case = 3
!/*
!        B = -6*(hn+he)
!        C = 6*(hn**2+he**2)
!        D = 6*hn*he*depth-hn**3-he**3
!        hne = root3(B,C,D)
!*/
        hne = he+half*hn
          do iter=1,itermax
          hse = hne-hn
          hnw = hne-he
          depthRec = (hne**3-hse**3-hnw**3)/(6*max(hn,eps)*max(he,eps))
          err = depthRec-depth

          shore1 = (hne**2-hse**2-hnw**2)/(2*max(hn,eps)*max(he,eps))
          hne=hne-err/shore1
          ok = abs(err).le.hmin1
             if (ok) go to 10
          enddo
 
10      continue
        shore = shore1
        endif
c--------------------------------------------------
        hs = hne - half*(he+hn)
        hs = min(depth,hs)
        zsurf = z0 + hs
c--------------------------------------------------
        hj(1,1:2) = hs + halfcell*dwdk
        hj(2,1:2) = hs - halfcell*dwdk 
        do i = 1,2
        minhj = minval(hj(1:2,i))
        maxhj = maxval(hj(1:2,i))
          if (minhj.ge.zero) then
          hk(i) = half*(hj(1,i)+hj(2,i))
          else if (maxhj.le.hmin) then
          hk(i) = zero
          else
*          hk(i) = half*maxhj**2/dhk(i)
          hk(i) = half*maxhj*(hs/dhk(i)+half)
          endif
        enddo 
        hj = max(hj,zero)
c--------------------------------------------------
        if (.not.ok) then
        write (iout,*) 'depth2surf'
        write (iout,*) 'case=',case,' shore=',shore,' iter=',iter,' fach=',fach
        write (iout,*) 'depth1=',depth1,' depth2=',depth2
        write (iout,*) 'depth=',depth,' depthRec=',depthRec,' err=',abs(depth-depthRec)
        write (iout,*) 'cellsize=',cellsize,' he=',he,' hn=',hn,' hs=',hs,' hshore=',hshore
        hse = hne-hn
        hnw = hne-he
        write (iout,*) 'hne=',hne,' hse=',hse,' hnw=',hnw,' hsw=',hsw
        write (iout,*) 'ae=',ae,' an=',an
c set default (free surface // to bottom)
        hs = depth
        zsurf = z0 + hs
        hj = hs
        hk = depth
        endif
c--------------------------------------------------
        return
        end

      function root3orig (a,b,c,d)
      use kind
      implicit none
      real (r8) root3orig,a,b,c,d, q, r, s, t, ac, b2,a2,qr,onethird
      parameter (onethird=1./3.)
c from wikipedia
      ac=a*c
      b2=b*b
      a2=a*a
      q = (3*ac-b2)/(9*a2)
      r = (9*ac*b-27*a2*d-2*b2*b)/(54*a2*a)
      qr = sqrt(q*q*q+r*r)
      s = (r+qr)**onethird
      t = (r-qr)**onethird
      root3orig = s+t-b/(3*a)
      return
      end
c
      function root3 (b,c,d)
      use kind
      implicit none
      real (r8) root3,b,c,d, q, r, s, t, b2,qr,onethird
      parameter (onethird=1./3.)
c from wikipedia
      b2=b*b
      q = (3*c-b2)/9
      r = (9*c*b-27*d-2*b2*b)/54
      qr = sqrt(q*q*q+r*r)
      s = (r+qr)**onethird
      t = (r-qr)**onethird
      root3 = s+t-b/3
      return
      end

c===================================================================
      function footShore(dzwdk,dzdk,wetfrac,zwv,zv,halfcell)
c===================================================================
c zsurf is calculated in the projection of the cell center to the intersection 
c from the dem surface and water surface
      use parameter_data
      use map_data
      implicit none

      real(r4) wetfrac,zsurf,footShore
      real(r8) dzwdk(2),dzdk(2),zwv,zv,a,b,c,xx,yy, det,halfcell

        if (wetfrac.lt.one.and.wetfrac.gt.zero) then
        a=dzwdk(1)-dzdk(1)
        b=dzwdk(2)-dzdk(2)
        c=zwv-zv
        det= a*a + b*b
          if (det.gt.max(a,b)*c/halfcell.and.det.gt.zero) then
          yy = - b*c/det
          xx = - a*c/det
            if (abs(xx).gt.halfcell.or.abs(yy).gt.halfcell) then
            zsurf = zwv
            else
*           write (iout,*) 'form footShore, det=',det
*           write (iout,*) 'xx=',xx,' yy=',yy, ' > cellsize = ',cellsize
*           write (iout,*) 'zwv=',zwv,' zv=',zv,' zsurf=',zsurf
            zsurf = zwv+ xx*dzwdk(1) + yy*dzwdk(2)
            endif
          else
          zsurf = zwv
          endif
        else
        zsurf = zwv
        endif

        footShore = zsurf
        return
        end

c============================================================================
      subroutine nearShore (dem,dzdx,dzdy,mask1,outmap)
c============================================================================
      use parameter_data
      use map_data
      implicit none

      real(r4) maxDist1, fact, zmin, zmax, slope
      integer(i4) k, imaxdist, imaxdist0, sumdist, ndistmax
      logical(i4) eqmv
      integer ndist, ndist0, length

      real(r4) dem(nval),dzdx(nval),dzdy(nval)
      logical*1 mask1(nval)

      real(r4), allocatable :: dist(:),idist(:),maskVal(:)
      integer*1, allocatable :: ldd(:)
      logical*1, allocatable :: shore(:)
      logical*4 offshore, outmap, existFile
c-------------------------------------------------------------------------
      allocate (dist(nval),idist(nval),ldd(nval),shore(nval))
c-------------------------------------------------------------------------
        if (existFile(maskmap)) then  
        allocate (maskVal(nval))
        maps = maskMap(1:length(maskMap))//null
        call resmapShort (maps,maskVal)
	  do k = 1,nval
          mask1(k) = .not.eqmv(maskVal(k))
 	  enddo
        maps = workdir(1:lw)//bs//'mask'//mapext
        call OUTGDAL (maps, nvx,nvy,4
     &             , xstart, ystart, cellsize,maskVal,nval
     &           , sizey, mapfmt, nband,error)

        deallocate (maskVal)
        write (iout,*) 'got maskmap: count(mask1)=',count(mask1)
        else
        mask1 = .true.
        endif
c-------------------------------------------------------------------------
      write (iout,*)
      write (iout,*) 'nearShore start'
      write (iout,*) '1) count(mask1)=',count(mask1)
      ldd = mv1
      val = mv
      ndist = 0
      shore = .false.
        do k = 1,nval
        mask1(k) = .not.eqmv(dem(k)).and.mask1(k)
          if(.not.mask1(k)) cycle
*        slope =   (abs(dzdx(k))+abs(dzdy(k)))
        slope = max(abs(dzdx(k)),abs(dzdy(k)))
        zmin = dem(k)-cellsize*slope/2
        zmax = dem(k)+cellsize*slope/2
          if (zmin.gt.seaLevel.or.zmax.lt.seaLevel) cycle

        val(k)=0
        ndist = ndist+1
        ldd(k) = 5
        shore(k) = .true.
        enddo
c-------------------------------------------------------------------------
      write (iout,*) '2) count(mask1)=',count(mask1)
c-------------------------------------------------------------------------
      if (mapdeg) then
      fact = 1000/radius0*180/pi !   km-> deg
      else
      fact = 1000
      endif
 
      maxDist1 = maxDist*fact  ! deg
      imaxdist0 = maxDist1/cellsize ! no of pixels
      imaxdist = min(max(imaxdist0,3),10)

      if (minDistSea.lt.0.) minDistSea = cellsize/fact*2
      if (minDistLand.lt.0.) minDistLand = cellsize/fact*2

      maxDistLand=max(maxDistLand,minDistLand*2)

      write (iout,'(3(a,g13.7))') ' minDistSea[km]=',minDistSea,' minDistLand[km]=',minDistLand,' maxDistLand[km]=',maxDistLand

c-------------------------------------------------------------------------
       maps = workdir(1:lw)//bs//'pixShore'//mapext

      if (.not.existFile(maps)) then
      sumdist = 0
      ndistmax = count(mask1)
      write (iout,'(2(a,g13.7),2(a,i10))') ' maxDist[km]=',maxDist,' maxDist1[deg]=',maxDist1
     &        ,' imaxdist[ncell]=',imaxdist,' count(mask1)=',ndistmax

      ndist=1
      ndist0=0

      do while (ndist.lt.ndistmax.and.sumdist.lt.imaxdist0*2.and.ndist.gt.ndist0)
      ndist0 = ndist
      idist = mv
      call distShore (mask1,val, idist,imaxdist,nval,nvx,nvy,iout)
      ndist = 0
        do k = 1,nval
          if (.not.eqmv(idist(k)).and.idist(k).gt.0.) then
          val(k) = idist(k) + sumdist
          endif
        if (.not.eqmv(val(k))) ndist = ndist + 1
        enddo 
      write (iout,'(3(a,i10))') ' sumdist=',sumdist,' ndist0=',ndist0,' ndist=',ndist
      sumdist = sumdist + imaxdist
      enddo  

      idist = val

      else
      call readmap ('pixShore',maps,idist,.true.)
      endif
c-------------------------------------------------------------------------
       write (iout,*) '3) count(mask1)=',count(mask1)
       write (iout,*) 'fact=',fact,' cellsize=',cellsize
      dist = mv
        do k = 1,nval
          if (eqmv(dem(k))) goto 100

	  if (eqmv(idist(k))) then
          mask1(k) = mask1(k).and.dem(k).ge.minDem.and.dem(k).le.maxDem

          else
          dist(k) = idist(k)*cellsize/fact  !  now is km
          mask1(k) = mask1(k).and.((dem(k).lt.0.0.and.dem(k).ge.minDem.and.dist(k).le.maxDist)   ! sea
     &                         .or.(dem(k).lt.0.0.and.dist(k).le.minDistSea)  ! sea
     &                         .or.(dem(k).ge.0.0.and.dem(k).le.maxDem.and.dist(k).le.maxDistLand)  ! land
     &                         .or.(dem(k).ge.0.0.and.dist(k).le.minDistLand) ! land
     &                         .or.(idist(k).le.minDistShore) ! nearshore
     &    )
          endif
100     continue
        enddo
       write (iout,*) '4) count(mask1)=',count(mask1)
c-------------------------------------------------------------------------
       if (noSpot.and.ndist.gt.0) then
       call spreadSeed (mask1,shore,nvx,nvy)
       mask1 = shore
       endif
c-------------------------------------------------------------------------
       if (outmap) then
c put distShore.map [km]
       maps = workdir(1:lw)//bs//'distShore'//mapext
        call OUTGDAL (maps, nvx,nvy,4
     &             , xstart, ystart, cellsize,dist,nval
     &           , sizey, mapfmt, nband,error)

c put pixShore.map [km]
       maps = workdir(1:lw)//bs//'pixShore'//mapext
        call OUTGDAL (maps, nvx,nvy,4
     &             , xstart, ystart, cellsize,idist,nval
     &           , sizey, mapfmt, nband,error)


c put shoreline.map
       maps = workdir(1:lw)//bs//'shoreline'//mapext
        call OUTGDAL (maps, nvx,nvy,6
     &             , xstart, ystart, cellsize,ldd,nval
     &           , sizey, mapfmt, nband,error)
      endif
 
      deallocate (dist,val,ldd,shore)
      return
      end
c============================================================================
      subroutine spreadSeed (mask,seed,nvx,nvy)
c============================================================================
      use parameter_data
      implicit none

      integer*4 i,j, l, s
      integer*4 nvx, nvy

      logical*1 mask(nvx,nvy), seed(nvx,nvy), done
c-----------------------------------------------------
      done = .true.
      l = 0
      do while (done)
      done = .false. 
      s = 0
c spred forward
      loop1: do j = 1,nvy-1
       loop2: do i = 1,nvx-1
       if (.not.mask(i,j)) cycle loop2
       if (.not.seed(i,j)) cycle loop2
c vertical junction
          if (.not.seed(i+1,j).and.mask(i+1,j)) then
          seed(i+1,j)=.true.
          s = s+1
          done = .true. 
          endif
c horizontal junction
          if (.not.seed(i,j+1).and.mask(i,j+1)) then
          seed(i,j+1)=.true.
          s = s+1
          done = .true. 
          endif
       enddo loop2
      enddo loop1

c spread backward
      loop3: do j = nvy,2,-1
       loop4: do i = nvx,2,-1
       if (.not.mask(i,j)) cycle loop4
       if (.not.seed(i,j)) cycle loop4
c vertical junction
          if (.not.seed(i-1,j).and.mask(i-1,j)) then
          seed(i-1,j)=.true.
          s = s+1
          done = .true. 
          endif
c horizontal junction
          if (.not.seed(i,j-1).and.mask(i,j-1)) then
          seed(i,j-1)=.true.
          s = s+1
          done = .true. 
          endif
       enddo loop4
      enddo loop3
c-----------------------------------------------------
      l = l+1
      write (iout,*) 'spreadSeed: loop ',l,' spread ',s
      enddo
c-----------------------------------------------------
      return
      end