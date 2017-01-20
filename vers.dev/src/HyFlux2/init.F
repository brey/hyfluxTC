#define DEB c
#define DEC c  
#undef PUTBARRIER
*====================================================================================
      subroutine initialize
*====================================================================================
      use parameter_data
      use constants
      use hydro_data
      use geometry_data
      use boundary_data
      use slope_data
      use statistics_data
      use parallel_data
      use watch_data
      implicit none
c------------------------------
      real(r4) dh
      integer(i4) i,j,l,k, m,pl,pr, ind3,ind4, tag, length, r1, r2, lb, iB
      logical shore, large, existFile
      character ext*3
      integer(i4), allocatable :: vto(:,:)
      real(r8) lat, omega, slope, slopemax, cv
*      parameter (omega=2*pi/(24*3600))
c------------------------------
      omega=2*pi/(24*3600)
      slopemax = 0.01 
c------------------------------
#ifdef PARALLEL
c-------------------------------------------------
      tag = 1          
      call V_character_master_to_slave (boundaryFile,1000,tag)
c-------------------------------------------------
      write (iout,*) ' Initialize/getBottm: icore=',icore

c defined in pointers
      if (SLAVE) allocate (cosLatv(ivL1:ivL2),xv(ivL1:ivL2),yv(ivL1:ivL2))

      call real8_master_to_slave (coslatv,1,tag)
      call real4_master_to_slave (xv,1,tag)
      call real4_master_to_slave (yv,1,tag)

c defined in getBottom
      if (SLAVE) allocate (zv(nv),dzdk(2,ivL1:ivL2),dzdkm(2,nv),tanLatv(ivL1:ivL2),pixShore(nval))

      call V_real4_master_to_slave (pixShore,nval,tag)
      call V_real8_master_to_slave (zv,nv,tag)
      call real8_master_to_slave (dzdk,2,tag)
      call V_real8_master_to_slave (dzdkm,2*nv,tag)
      call real8_master_to_slave (tanLatv,1,tag)

      write (iout,*) ' Initialize/createPointVol: icore=',icore
c-------------------------------------------------
      write (iout,*) ' Initialize/getJunction: icore=',icore
      
c defined  in getJunction
      if (SLAVE) allocate (zj(ij1:ij2),facL(ij1:ij2),facR(ij1:ij2))
      call PJ_real8_master_to_slave    (zj,1,tag)
      call PJ_real8_master_to_slave    (facL,1,tag)
      call PJ_real8_master_to_slave    (facR,1,tag)


      if (SLAVE) allocate (deepJ(ij1:ij2))
      call PJ_logical1_master_to_slave    (deepJ,1,tag)

      write (iout,*) ' Initialize/getDepth: icore=',icore

c defined in getDepth
      if (SLAVE) allocate (hv(ivL1:ivL2),zwv(ivL1:ivL2),dzwdk(2,ivL1:ivL2),zminv(ivL1:ivL2))
      call real8_master_to_slave (hv,1,tag)
      call real8_master_to_slave (zwv,1,tag)
      call real8_master_to_slave (zminv,1,tag)
      call real8_master_to_slave (dzwdk,2,tag)

      if (SLAVE) allocate(wetfrac(ivL1:ivL2),case(ivL1:ivL2),hk(2,ivL1:ivL2),hj(2,2,ivL1:ivL2))
      call real4_master_to_slave (wetfrac,1,tag)
      call integer1_master_to_slave (case,1,tag)
      call real8_master_to_slave (hk,2,tag)
      call real8_master_to_slave (hj,4,tag)

      if (SLAVE) allocate (wetfracIn(ivL1:ivL2),zwvIn(ivL1:ivL2),hvIn(ivL1:ivL2))
      call real4_master_to_slave (zwvIn,1,tag)
      call real4_master_to_slave (wetfracIn,1,tag)
      call real4_master_to_slave (hvIn,1,tag)


c defined in getOtherMaps


      write (iout,*) ' Initialize/getMaps: icore=',icore
      if (SLAVE) allocate (vv(2,ivL1:ivL2),vn(ivL1:ivL2))
      call real8_master_to_slave (vv,2,tag)
      call real4_master_to_slave (vn,1,tag)
/*
      write (iout,*)'vx: min=',minval(vv(1,iv1:iv2)),' max=',maxval(vv(1,iv1:iv2))
      write (iout,*)'vy: min=',minval(vv(2,iv1:iv2)),' max=',maxval(vv(2,iv1:iv2))
      write (iout,*)'vn: min=',minval(vn),' max=',maxval(vn)
      write (iout,*)'hv: min=',minval(hv),' max=',maxval(hv)
      write (iout,*)'zv: min=',minval(zv),' max=',maxval(zv)
*/
      write (iout,*) ' Initialize/getOtherMaps: icore=',icore

        if (SLAVE) allocate (manh(ivL1:ivL2))
      call real4_master_to_slave (manh,1,tag)
        if (SLAVE) allocate (man(ivL1:ivL2))
      call real4_master_to_slave (man,1,tag)

      if (rainmap.ne.' ') then
        if (SLAVE) allocate (rain(ivL1:ivL2),addrain(ivL1:ivL2))
      call real4_master_to_slave (rain,1,tag)
      call real4_master_to_slave (addrain,1,tag)
      endif

      write (ext,'(i3)') icore
      call fillzero(ext,3)

#endif
c------------------------------
c used and updated in the border  (slave_to_master)
      if (MASTER) then
      ind3 = 1
      ind4 = nv
      else
      ind3 = ivL1
      ind4 = ivL2
      endif

      allocate (fhv(ind3:ind4),fvv(2,ind3:ind4),fcor(ind3:ind4))  ! NO output
      allocate (dzwdk0(2,ind3:ind4),upflux(ind3:ind4),cvv(2,ind3:ind4)
     &         ,dcvdk(2,2,ind3:ind4))  ! NO output
c------------------------------
      allocate (slopeV(0:nv),deepV(0:nv))
      slopeV = .true.
      deepV  = .false.
c------------------------------------------------------
        if (allocated(noSlope)) then
        slopeV = .not.noSlope.and.slopeV
        deallocate (noSlope)
        endif
c------------------------------
      if (SLAVE) allocate (outval(ivL1:ivL2))
c------------------------------      
      write (iout,*) 'exist boundaryFile ? ',existFile(boundaryFile)
      if (existFile(boundaryFile)) then
      write (iout,*)
      write (iout,*) '========================================================'
      write (iout,*) ' Start initBound'
      write (iout,*) '========================================================'
      call initBound
      write (iout,*) 'icore=',icore,' imapcurr=',imapcurr
      write (iout,*) 'Done initBound'
      write (iout,*)
      else
      nbound = 0
      endif
c------------------------------------------------------
      if (useMinDem) then
      cminDem=sqrt(-g*minDem)
      else
      cminDem=sqrt(-g*minval(zv))
      endif
c------------------------------------------------------      
       do l = 1,nv
c no slope if dem < minDem      
       k = kl(l)
         if (zv(l).lt.dble(minDem)) slopeV(l) = .false.
         if (sumslope) then
         slope =   (abs(dzdkm(1,l))+abs(dzdkm(2,l)))
         else
         slope = max(abs(dzdkm(1,l)),abs(dzdkm(2,l)))
         endif
        dh = slope*msize(lev(l))
        shore = (zv(l)+dh).gt.seaLevel.and.(zv(l)-dh).lt.seaLevel
c no slope if distance is lower than minDistSlope and land and near runup
          if ( (zminv(l).lt.runUpLim.and.(zv(l)+dh).gt.seaLevel) 
     &                .or.pixShore(k).lt.minDistSlope 
     &        ) slopeV(l) = .false.
c deep volume
       deepV(l) = .not.shore.and.(zv(l)+dh).lt.(-hlim).and.vtype(l).eq.1.and.slopeV(l)
       enddo
c------------------------------
        do l = ind3,ind4
          if (slopeV(l)) cycle
        dzwdk(1:2,l) = zero
        call depth2surf (hv(l),zv(l),dzdk(1:2,l),dzwdk(1:2,l)
     &       ,hj(1:2,1:2,l),hk(1:2,l),zwv(l),wetfrac(l),case(l),dsize(lev(l)))
          if (hv(l).le.hvel0) wetfrac(l) = zero
        enddo
c------------------------------
c deep junction
       do l = ij1,ij2     
       pl = jtol(l)
       pr = jtor(l)
       deepJ(l) = deepJ(l).and.(vtype(pl).eq.1).and.(vtype(pr).eq.1).and.deepV(pl).and.deepV(pr)
       enddo
c------------------------------
c used for output

      allocate (upvol(ind3:ind4),dvdk(2,2,ind3:ind4),dhjv(ind3:ind4))  ! output

      allocate (limvals(ind3:ind4),smalls(ind3:ind4),courn(ind3:ind4))   ! output
      allocate (zwmin(ind3:ind4),tzwmin(ind3:ind4),ztotmax(ind3:ind4),fhdmax(ind3:ind4))   ! output
      allocate (zwmax(ind3:ind4),wzwmax(ind3:ind4),hzwmax(ind3:ind4),tzwmax(ind3:ind4))   ! output
      allocate (zsurfmax(ind3:ind4),tzsurfmax(ind3:ind4),hzsurfmax(ind3:ind4))   ! output
      allocate (vnmax(ind3:ind4),tvnmax(ind3:ind4))   ! output
      allocate (tarr(ind3:ind4))   ! output

      write (iout,*) 'hollandFile=',hollandFile(1:length(hollandFile))
      write (iout,*) 'exist holland ? ',existHolland
        if (existHolland) then
        allocate (spu(ind3:ind4),u10(ind3:ind4),depr(ind3:ind4),spumax(ind3:ind4),u10max(ind3:ind4),deprmax(ind3:ind4))
        endif

      zwmin =mv 
      tzwmin =mv

      zsurfmax =mv
      tzsurfmax =mv
      hzsurfmax =mv

      zwmax =mv
      tzwmax =mv
      hzwmax =mv
      wzwmax =mv

      ztotmax =mv
      fhdmax = mv

      vnmax =mv
      tvnmax =mv
      tarr =mv
        if (existHolland) then
        u10max = mv
        deprmax = mv
        spumax = mv
        endif

      if (imap.gt.0.and.MASTER) then
      write (ext2,'(i8)') int(timestart+0.5)
      call fillzero (ext2,8)

      call map2outval ('zsurfmax'//ext2,' ',zsurfmax)
      call map2outval ('tzsurfmax'//ext2,' ',tzsurfmax)
      call map2outval ('hzsurfmax'//ext2,' ',hzsurfmax)

      call map2outval ('ztotmax'//ext2,' ',ztotmax)

      call map2outval ('fhdmax'//ext2,' ',fhdmax)

      call map2outval ('zwmin'//ext2,' ',zwmin)
      call map2outval ('tzwmin'//ext2,' ',tzwmin)

      call map2outval ('zwmax'//ext2,' ',zwmax)
      call map2outval ('tzwmax'//ext2,' ',tzwmax)
      call map2outval ('hzwmax'//ext2,' ',hzwmax)
      call map2outval ('wzwmax'//ext2,' ',wzwmax)

      call map2outval ('vnmax'//ext2,' ',vnmax)
      call map2outval ('tvnmax'//ext2,' ',tvnmax)

      call map2outval ('tarr'//ext2,' ',tarr)
        if (existHolland) then
        call map2outval ('u10max'//ext2,' ',u10max)
        call map2outval ('deprmax'//ext2,' ',deprmax)
        call map2outval ('spumax'//ext2,' ',spumax)
        endif
      endif
c------------------------------
c   f=2*Omega*sin(lat), where Omega is earth nd. rot. [rad/sec]

      if (MASTER) then       
        if (mapdeg) then 
          do l = ind3,ind4
          lat = ystart - yv(l)*cellratio
          fcor(l) = 2 * omega * sin(lat*deg2rad)
          enddo            
        endif
      write (iout,*) 'fcor: min=',minval(fcor),' max=',maxval(fcor)
      endif
c------------------------------------------------------
DEB      print *,'completed allocation: icore=',icore
c------------------------------------------------------
#ifdef PARALLEL
DEB       print *, 'icore=',icore,' imap=',imap,' time=',time,' timestart=',timestart,' nbound=',nbound
      call real4_master_to_slave (fcor,1,tag)
DEB       print *, 'icore = ',icore,' done fcor '

      if (imap.gt.0) then
      call real4_master_to_slave (zsurfmax,1,tag)
      call real4_master_to_slave (tzsurfmax,1,tag)
      call real4_master_to_slave (hzsurfmax,1,tag)
DEB       print *, 'icore = ',icore,' done zsurfmax '

      call real4_master_to_slave (ztotmax,1,tag)
      call real4_master_to_slave (fhdmax,1,tag)
DEB       print *, 'icore = ',icore,' done ztot '

      call real4_master_to_slave (zwmin,1,tag)
      call real4_master_to_slave (tzwmin,1,tag)
DEB       print *, 'icore = ',icore,' done zwmin '

      call real4_master_to_slave (zwmax,1,tag)
      call real4_master_to_slave (tzwmax,1,tag)
      call real4_master_to_slave (hzwmax,1,tag)
      call real4_master_to_slave (wzwmax,1,tag)
DEB       print *, 'icore = ',icore,' done zwmax '

      call real4_master_to_slave (vnmax,1,tag)
      call real4_master_to_slave (tvnmax,1,tag)
DEB       print *, 'icore = ',icore,' done vnmax '

      call real4_master_to_slave (tarr,1,tag)
        if (existHolland) then
        call real4_master_to_slave (u10max,1,tag)
        call real4_master_to_slave (deprmax,1,tag)
        call real4_master_to_slave (spumax,1,tag)
DEB         print *, 'icore = ',icore,' done holland '
        endif
      endif

      call S_real4_master_to_slave (maxdhmax, tag)
DEB       print *, 'icore=',icore,' maxdhmax = ',maxdhmax
#endif
c------------------------------
      allocate (sf(2,ind3:ind4))
      sf = zero
c------------------------------
      dhmax = maxdhmax
        do l = ind3,ind4
        dhjv(l)  = maxdhmax*fdhmin
        smalls(l) = (dhjv(l)/msize(lev(l)))**2
        enddo
c------------------------------
      call cputime (cpu0)
      call elapstime (elaps0)
c------------------------------------
      if (existHolland) call readHolland
c------------------------------------
      time = timestart
      istep = 0
      timeold = time
 
      stepdtmin = 0
      
      do itimeL = 1,ntimeL 
        if (time.lt.toutL(itimeL)) exit
      enddo

      dtout = dtoutL(itimeL)
      dtrest = dtrestL(itimeL)
      dtstat = dtstatL(itimeL)
      
      timeout =  time + dtout
      timerest = time + dtrest
      timestat = time + dtstat

      if (MASTER) then
      dtmax = dtout
        do l =  1,nv
        cv = min(max(sqrt(g*hv(l)),ccrit),cminDem)
          if (vtype(l).eq.1) dtmax = min(dtmax,2*msize(lev(l))/cv)
        enddo
      endif
c------------------------------------------------------
#ifdef PARALLEL
      call S_real8_master_to_slave (dtmax, tag)
DEB       print *, 'icore=',icore,' dtmax = ',dtmax
#endif
c------------------------------------------------------
      dt    = dtmax*courmax
      dtjun = dtmax

DEB      print *, 'icore=',icore,' dt = ',dt

      if (existHolland) then
      upvol = .true.
      else
      upvol = .false.
      endif

      dvdk = 0
      dcvdk = 0

      dzwdk0 = sf

      ldhmax=0
      maxzwmax = 0.
      maxvnmax = 0
c------------------------------
      if (MASTER) then
      val = mv
      val(kl) = vtype(1:nv) 
      maps = workdir(1:lw)//bs//'vtype'//mapext
        call OUTGDAL (maps, nvx,nvy,4
     &             , xstart, ystart, cellsize,val,nval, sizey, mapfmt, nband,error)

      val = mv
      val(kl) = dzwdk(1,1:nv) 
      maps = workdir(1:lw)//bs//'dzwdx'//mapext
        call OUTGDAL (maps, nvx,nvy,4
     &             , xstart, ystart, cellsize,val,nval, sizey, mapfmt, nband,error)

      val = mv
      val(kl) = dzwdk(2,1:nv) 
      maps = workdir(1:lw)//bs//'dzwdy'//mapext
        call OUTGDAL (maps, nvx,nvy,4
     &             , xstart, ystart, cellsize,val,nval, sizey, mapfmt, nband,error)

      val = mv
         do l=1,nv
         k = kl(l)
           if (slopeV(l)) then
           val(k) = 1
           else
           val(k) = 0
           endif
         enddo
      maps = workdir(1:lw)//bs//'slopev'//mapext
        call OUTGDAL (maps, nvx,nvy,4
     &             , xstart, ystart, cellsize,val,nval, sizey, mapfmt, nband,error)

      endif
DEB       print *,'icore=',icore,' done initialization' 
c------------------------------------------------------
      return
      end
