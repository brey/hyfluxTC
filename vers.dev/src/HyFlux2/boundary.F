#undef PUTBARRIER
c============================================================================
      subroutine createBoundary
c============================================================================
      use parameter_data
      use boundary_data
      use geometry_data
      use hydro_data
      use window_data
      implicit none
c----------------------------------------------------
      real(r4)  hlim1/1000./
      integer(i4) i,j,l, k, length, pl, pr, lev0
      logical(i4) eqmv,offshore, maskl, maskr
c----------------------------------------------------
c define boundary (only open sea cell)
c distance (pixShore, minDistBound) are in pixel
         do l =1,nv
	 k = kl(l)
           if (eqmv(pixShore(k))) then
           offshore = zv(l).lt.seaLevel
           else
           offshore = zv(l).lt.seaLevel.and.pixShore(k).gt.minDistBound
           endif
             if ( offshore 
     &           .and.vtol(1,l)*vtor(1,l)*vtol(2,l)*vtor(2,l).eq.0
     &           .and. vtype(l).eq.1
*     &           .and.hv(l).gt.hshore
*     &           .and.hv(l).gt.hlim0
     &          ) vtype(l) = boundaryCode0
         enddo
c-----------------------------------------------------
         write (iout,*) 'boundaryCode0 = ',boundaryCode0,' no of cell ',count(vtype.eq.boundaryCode0)
c-----------------------------------------------------
c write boundary
c----------------------------------------------------
       open (unit=iinp,file=boundaryFile)
       write (iinp,*) 'cellno code zsurf vx vy'
       val = mv
         do l =1,nv
           if (vtype(l).eq.1) cycle
           if (vtype(l).ne.boundaryCode0) cycle
	 k = kl(l)
	 val(k)   = vtype(l)
         write (iinp,*) k, vtype(l), 0, 0, 0
        enddo

      close (iinp)
      maps = workdir(1:lw)//bs//'cellBound'//mapext
        call OUTGDAL (maps, nvx,nvy,4
     &             , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)
      deallocate (val)
      return
      end
c============================================================================
        subroutine initBound
c============================================================================
        use parameter_data
        use boundary_data
        use geometry_data
        use hydro_data
        use slope_data
        use statistics_data
        use parallel_data
        implicit none
c------------------------------------------------------------------------
        logical(i4) exist, maskl, maskr, maskv, existFile
        integer(i4) k,l, m, length, pl, pr, idum, vtypel, vtyper, lb
        real(r4) code1, xb, yb,  numget

        real(rr8)  xstart1, ystart1, cellsize1
        integer(i4) nvx1,nvy1,nval1,typemap1,  nother
 
        integer(i4) nmax,nh,nvalue,itime, izsurf, ivx, ivy, m0, ib, lf,cell, jband
        parameter (nmax=30)
        character buff*132, header*132,names(nmax)*30, message*80, msg*3
        character ext*7
        logical(i4) noheader, eqmv
 
        integer(i4), allocatable :: codemap(:)
        real(r4), allocatable :: bat(:)

        integer tag/1/
c------------------------------------------------------------------------
c  read output header file
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      nbound = 0
c---------------------------------------------------------
      if (.not.existFile(boundaryFile)) return
c--------------------------------------------------------- 
	  open (unit=iinp,file=boundaryFile,action='read')
          read (iinp,'(a)') buff
          call nocr(buff)
*          write (iout,*) 'buff=',buff
 
c count the no of lines
 
	nbound = 0
          do while (.true.)
10        continue
          read (iinp,'(a)',end=100)  buff
          call nocr(buff)
          if (length(buff).le.1) go to 10
*         write(iout,*) 'buff=',buff
          nbound = nbound+1
          enddo
 
100     rewind (iinp)
 
      write (iout,*) 'found nbound=',nbound,' boundary cells'
c---------------------------------------------------------------
        allocate (zbound(nbound),vxbound(nbound),vybound(nbound)
     &       ,cellbound(nbound),codebound(nbound),lpB(nbound),kbound(nv))
       zbound =0.
       vxbound =0.
       vybound = 0.
       kbound=0

        read (iinp,'(a)') buff
 
      k = 0
      do while (.true.)
      read (iinp,'(a)',end=200) buff
      call nocr(buff)
        if (length(buff).le.1) cycle
 
      read (buff,*) cell,code1
        if (cell.gt.nval) cycle
      l = lk(cell)
	if (l.eq.0) cycle
      k = k+1
      read (buff,*) cell,code1
     &           ,zbound(k),vxbound(k),vybound(k)

      cellbound(k) = cell
      codebound(k) = code1
      lpB(k) = l
        kbound(l)  = k
      enddo 
200   continue

	nbound = k
 
	  close (iinp)

      write (iout,*) 'got nbound=',nbound,' boundary cells'
            
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       if (nbound.eq.0) return
       if (.not.nested) goto 2000
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
       maps = boundaryDir(1:lbdir)//bs//'listfiles.txt'
       inquire (file=maps,exist=exist)
         if (.not.exist) then
         write (iout,*) 'file ',maps(1:length(maps)),' not exist'
         call closeParam (2)
         endif
 
       open(iinp,file=maps)
       read (iinp,'(a)') header
       call nocr(header)
       call procheader (header,names,nmax,nh,nvalue)
         if (names(1).eq.'*'.or.names(1).eq.'#') then
         nh = nh-1
         names = names(2:nh)
         else if (names(1)(1:1).eq.'*') then
         names(1) = names(1)(2:)
         endif

*       write (iout,*) 'header=',(names(l),l=1,nh)

         itime=0; izsurf=0; ivx=0; ivy=0; jband=0
         noheader = .true.

         do l =1,nh
           if (names(l).eq.'time') then
           itime = l
           noheader = .false.
           else if (names(l).eq.'HA') then
           izsurf = l
           else if (names(l).eq.'U') then
           ivx = l
           else if (names(l).eq.'V') then
           ivy = l
           else if (names(l).eq.' IDTime') then
           jband = l
           endif
         enddo

        if (noheader) then
         itime=1; izsurf=2; ivx=3; ivy=4
        endif

        if (itime*izsurf*ivx*ivy.eq.0) then
        write (errmsg,*)'from listfiles.txt: itime=',itime,' izsurf=',izsurf,' ivx=',ivx,' ivy=',ivy,' jband=',jband
        call logout (2,errmsg)
        endif

        if (noheader) rewind(iinp) 

	imapMax = 0
          do while (.true.)
11        continue
          read (iinp,'(a)',end=101)  buff
          call nocr(buff)
          if (length(buff).le.1.or.buff(1:1).eq.'*') go to 11
*         write(iout,*) 'buff=',buff(1:length(buff))
          imapMax = imapMax+1
          enddo
 
101    continue
       write(iout,*) 'from ',maps(1:length(maps)),' imapMax=',imapMax
         if (imapMax.le.0) then
         message= 'imapMax must be > 1'
         call logout(2,message)
         endif
 
       allocate (timeBound(imapMax),zsurfBoundMap(imapMax),vxBoundMap(imapMax),vyBoundMap(imapMax),ibandBound(imapMax))

       rewind (iinp)
         if (.not.noheader) read (iinp,'(a)') header
       imapCurr = 1
         do l = 1,imapMax
12       continue
         read (iinp,'(a)')  buff
         call nocr(buff)
          if (length(buff).le.1.or.buff(1:1).eq.'*') go to 12
         call string_to_names (buff,names,nmax,nh)
         timeBound(l) = numget(names(itime),msg)
           if (timeBound(l).le.timestart) then
           imapCurr = l
           write(iout,*) 'timeBound=',timeBound(l),' imapCurr=',imapCurr
           endif
           if (jband.eq.0) then
           ibandBound(l)= 1
           else 
           ibandBound(l) = numget(names(jband),msg)+1
           endif
         lf = length(names(izsurf))
         zsurfBoundMap(l) = names(izsurf)(1:lf)//null
         lf = length(names(ivx))
         vxBoundMap(l) = names(ivx)(1:lf)//null
         lf = length(names(ivy))
         vyBoundMap(l) = names(ivy)(1:lf)//null
         enddo
 
       write (iout,*)
       write (iout,*) 'Card at timestart from listfiles.txt'
       write(iout,*) 'time=',timeBound(imapCurr),' zsurf=',zsurfBoundMap(imapCurr)(1:lf)
     &   ,' vx=',vxBoundMap(imapCurr)(1:lf),' vy=',vyBoundMap(imapCurr)(1:lf),' iband=',ibandBound(imapCurr)
       write (iout,*)
       write (iout,*) 'Last card from listfiles.txt'
       write(iout,*) 'time=',timeBound(imapmax),' zsurf=',zsurfBoundMap(imapmax)(1:lf)
     &   ,' vx=',vxBoundMap(imapmax)(1:lf),' vy=',vyBoundMap(imapmax)(1:lf),' iband=',ibandBound(imapmax)
 
       close (iinp)
       write (iout,*) 'Got informations from file ',maps(1:length(maps))

*--------------------------------------------------------------------------------------
* re-define cellbound and vtype
*--------------------------------------------------------------------------------------
c get window size from boundaryDir
       allocate (kB(3,nbound),lkB(3,nbound),nmapok(nbound))

       nmapok = 0

       allocate (ipB)
       ipB%nval = 0
       iband = 1
        call setBoundMap (zsurfBoundMap(imapCurr),1)
        call setBoundMap (vxBoundMap(imapCurr),2)
        call setBoundMap (vyBoundMap(imapCurr),3)
#ifdef PARALLEL
      call mpi_barrier (mpi_comm_world, mpi_erro)
#endif
       allocate (ipB%val(ipB%nval))
c--------------------------------
       allocate (bat(nkb(1)))
       iband = 1
       call getBoundMap ('bathymetry.tif'//null,1,bat)
          do m = 1,nkb(1)
          k = kb(1,m)
          l = cellbound(k)
             if (nmapok(k).eq.3.and.bat(m).gt.0.) then
             codeBound(k) = boundaryCode
             else
             codeBound(k) = 0
             endif
          enddo
        deallocate (bat,nmapok)

        do ib = 1,3
        m = 0
          do 300 m0 = 1,nkb(ib)
          k = kb(ib,m0)
          l = lpB(k)
#ifdef PARALLEL
        if (corev(l).ne.icore) goto 300
#endif
            if (codeBound(k).gt.1) then
            m = m+1
            kb(ib,m) = kb(ib,m0)
            lkb(ib,m) = lkb(ib,m0)
            endif
300         continue
        nkb(ib) = m
*        write (iout,*)' icore=',icore,' ib=',ib,' nkb=',nkb(ib)
*        print *,' icore=',icore,' ib=',ib,' nkb=',nkb(ib)
        enddo
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
2000   continue
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       outval = mv
        vtype(lpB) = codeBound
      if (MASTER) then
      val = mv
      val(kl(lpB)) = codeBound
      maps = workdir(1:lw)//bs//'cellBound1'//mapext
        call OUTGDAL (maps, nvx,nvy,4
     &             , xstart, ystart, cellsize,val,nval, sizey, mapfmt, nband,error)
      endif
c------------------------------------------------------
c slopes in hydroDynamic volumes
c------------------------------------------------------
c noSlope in the pixels close to boundaryCode ne 1
       do l = 1,nv
       if (vtype(l).ne.1) slopeV(l) = .false.
*       if (vtype(l).gt.2) slopeV(l) = .false.  ! do not uncomment because you'll have high waves in the border close to the shore
        do m=1,2
        pl = vtol(m,l)
        pr = vtor(m,l)
        maskl = vtype(pl).ne.1
        maskr = vtype(pr).ne.1
*        maskl = vtype(pl).gt.2
*        maskr = vtype(pr).gt.2
          if (maskl.or.maskr) slopeV(l) = .false.
        enddo
      enddo
c------------------------------------------------------
      return
      end

c============================================================================
        subroutine setTimeBound (err)
c============================================================================
        use parameter_data
        use boundary_data
        use geometry_data
        use parallel_data
        use hydro_data
        use slope_data
        use statistics_data
        implicit none
 
        character ext*7
        logical(i4) first/.true./, eqmv
 
        integer(i4) imap1, k, l, ix, iy, length, m, i, err

        real(r4) t0,t1, fact
 
        real(r4), save, allocatable :: z0(:),vx0(:),vy0(:),z1(:),vx1(:),vy1(:)
        save first, t0, t1
c------------------------------------------------------------------------
        if (nbound.eq.0.or.nkb(1).eq.0.or.nkb(2).eq.0.or.nkb(3).eq.0) return
c------------------------------------------------------------------------
c  get first and initialize
        if (first) then
c------------------------------------------------------------------------
        t1 = timeBound(imapCurr)
        t0 = t1-1
        iband = ibandBound(imapCurr)

        allocate (z0(nkb(1)),vx0(nkb(2)),vy0(nkb(3)))
        allocate (z1(nkb(1)),vx1(nkb(2)),vy1(nkb(3)))
        z0=0; vx0=0; vy0=0
        z1=0; vx1=0; vy1=0
c------------------------------------------------------------------------
      call cputime(cpu1)
        call getBoundMap (zsurfBoundMap(imapCurr),1,z1)
        call getBoundMap (vxBoundMap(imapCurr),2,vx1)
        call getBoundMap (vyBoundMap(imapCurr),3,vy1)
      call cputime(cpu2)
      cpuIoBound = cpuIoBound+ cpu2-cpu1
c------------------------------------------------------------------------
        do m = 1,nkb(1)
          if (eqmv(z1(m))) z1(m) = seaLevel
        enddo
        do m = 1,nkb(2)
          if (eqmv(vx1(m))) vx1(m) = zero
        enddo
        do m = 1,nkb(3)
          if (eqmv(vy1(m))) vy1(m) = zero
        enddo
c------------------------------------------------------------------------
        first = .false.
        endif
c------------------------------------------------------------------------
c get next
        do while (time.ge.t1.and.imapCurr.lt.imapMax)
        z0 = z1
        vx0 = vx1
        vy0 = vy1
        t0 = t1
        imapCurr = imapCurr+1
 
        t1 = timeBound(imapCurr)
        iband = ibandBound(imapCurr)
 
 
        write (ext,'(f7.3)') float(imap1)/1000.
        call fillzero (ext,7) 

c------------------------------------------------------------------------
      call cputime(cpu1)
        call getBoundMap (zsurfBoundMap(imapCurr),1,z1)
        call getBoundMap (vxBoundMap(imapCurr),2,vx1)
        call getBoundMap (vyBoundMap(imapCurr),3,vy1)
      call cputime(cpu2)
      cpuIoBound = cpuIoBound+ cpu2-cpu1
c------------------------------------------------------------------------
        do m = 1,nkb(1)
          if (eqmv(z1(m))) z1(m) = seaLevel
        enddo
        do m = 1,nkb(2)
          if (eqmv(vx1(m))) vx1(m) = zero
        enddo
        do m = 1,nkb(3)
          if (eqmv(vy1(m))) vy1(m) = zero
        enddo
c------------------------------------------------------------------------
*       write (iout,*)'t1=',t1,' imapCurr=',imapCurr,' z1: min=',minval(z1),' max=',maxval(z1)
c------------------------------------------------------------------------
        enddo
c------------------------------------------------------------------------
        if (time.gt.timeBound(imapmax)) err= 3
c------------------------------------------------------------------------
c interpolate
        if (time.le.t0) then
        fact = 0
        else if (time.ge.t1) then
        fact = 1
        else
        fact = (time-t0)/(t1-t0)
        endif
 
        do m = 1,nkb(1)
        k = kb(1,m)
        l = lpB(k)
        if (codebound(k).eq.2.or.codebound(k).eq.3) then
        zbound(k)  = z0(m)*(one-fact)+z1(m)*fact
        endif
        enddo
 
        do m = 1,nkb(2)
        k = kb(2,m)
        l = lpB(k)
        if (codebound(k).eq.2.or.codebound(k).eq.4) then
        vxbound(k) = vx0(m)*(one-fact)+vx1(m)*fact
        endif
        enddo
 
        do m = 1,nkb(3)
        k = kb(3,m)
        l = lpB(k)
        if (codebound(k).eq.2.or.codebound(k).eq.4) then
        vybound(k) = vy0(m)*(one-fact)+vy1(m)*fact
        endif
        enddo
/*
        write (iout,*) 'z0: min =',minval(z0(1:nkb(1))),' max=',maxval(z0(1:nkb(1)))
        write (iout,*) 'z1: min =',minval(z1(1:nkb(1))),' max=',maxval(z1(1:nkb(1)))
        write (iout,*) 'zbound: min =',minval(zbound),' max=',maxval(zbound)
       
        do m = 1,nkb(1)
        k = kb(1,m)
        write (iout,*)  m,k,cellbound(k),zbound(k),z0(m),z1(m)
        enddo
*/
c------------------------------------------------------------------------
        return
        end
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c codebound
c  1 water depth and velocity are calculated by the program (DEFAULT)'
c  2 user defined zsurf & velocity '
c  3 user defined zsurf: the velocity is taken from the upstream cell'
c  4 user defined velocity: the zsurf is taken from the upstream cell'
c  5 zsurf & velocity are taken from the upstream cell'
c============================================================================
        subroutine setBound
c============================================================================
        use parameter_data
        use statistics_data
        use boundary_data
        use geometry_data
        use parallel_data
        use hydro_data
        use slope_data
        implicit none
c------------------------------------------------------
        integer(i4) i,j,l,k,pl, pr, m, vtypeL, vtypeR, sum1, sum2
        logical(i4), save :: maskl, maskr, first, dry, moveh, movev
        data first /.true./
        real(r4) waveT /15./, waveL, fac, slope
        integer(i4), save, allocatable :: jbound(:)
        integer(i4), save :: njb, nups
c------------------------------------------------------
        if (nbound.eq.0) return
c------------------------------------------------------
       if (first) then
       i=0
        do k = 1,nbound
        l = lpB(k)
#ifdef PARALLEL
          if (corev(l).ne.icore) cycle
#endif
        i = i+1
        enddo

        nvb = i

       allocate (nupsh(ivL1:ivL2),nupsv(ivL1:ivL2))
       nupsh = 0
       nupsv = 0

       if (nvb.gt.0) allocate (ivbound(nvb))

       i=0
        do k = 1,nbound
        l = lpB(k)
#ifdef PARALLEL
          if (corev(l).ne.icore) cycle
#endif
        i = i+1
        ivbound(i) = k
        enddo


c evaluate njb
       j = 0
         do l = ij1,ij2
         pl = jtol(l)
         pr = jtor(l)
           if (pl.eq.0.or.pr.eq.0) cycle
         vtypeL = vtype(pl)
         vtypeR = vtype(pr)
           if (vtypeL.eq.1.and.vtypeR.eq.1) cycle
           if (vtypeL.eq.0.or.vtypeR.eq.0) cycle
         j = j + 1 
         enddo

       njb = j
       if (njb.gt.0) allocate (jbound(njb))

       j = 0
         do l = ij1,ij2
         pl = jtol(l)
         pr = jtor(l)
           if (pl.eq.0.or.pr.eq.0) cycle
         vtypeL = vtype(pl)
         vtypeR = vtype(pr)
           if (vtypeL.eq.1.and.vtypeR.eq.1) cycle
           if (vtypeL.eq.0.or.vtypeR.eq.0) cycle
         j = j + 1 
         jbound(j) = l

c update zwv

c left boundary,  right cell
           maskl = vtypeL.eq.4 .or. vtypeL.eq.5
           maskr = vtypeR.eq.1
             if (maskl.and.maskr) nupsh(pl) = nupsh(pl) + 1
 
c right boundary, left cell,
           maskl = vtypeL.eq.1
           maskr = vtypeR.eq.4 .or. vtypeR.eq.5
             if (maskl.and.maskr) nupsh(pr) = nupsh(pr) + 1

c update vv

c left boundary,  right cell
           maskl = vtypeL.eq.3 .or. vtypeL.eq.5
           maskr = vtypeR.eq.1
             if (maskl.and.maskr) nupsv(pl) = nupsv(pl) + 1
c right boundary, left cell,
           maskl = vtypeL.eq.1
           maskr = vtypeR.eq.3 .or. vtypeR.eq.5
             if (maskl.and.maskr) nupsv(pr) = nupsv(pr) + 1
         enddo
c------------------------------------------------------

#ifdef PARALLEL
      call cputime(cpu1)

      sum1 = sum(nupsh)
      call S_integer4_slave_to_master (sum1,  tagBound,Fsum)
      call S_integer4_master_to_slave (sum1,  tagBound)

      call integer1_putR    (nupsh, 1, tagBound,Fsum)

      sum2 = sum(nupsh)
      call S_integer4_slave_to_master (sum2,  tagBound,Fsum)
      call S_integer4_master_to_slave (sum2,  tagBound)

      moveh = sum2-sum1.ne.0


      sum1 = sum(nupsv)
      call S_integer4_slave_to_master (sum1,  tagBound,Fsum)
      call S_integer4_master_to_slave (sum1,  tagBound)

      call integer1_putR    (nupsv, 1, tagBound,Fsum)

      sum2 = sum(nupsv)
      call S_integer4_slave_to_master (sum2,  tagBound,Fsum)
      call S_integer4_master_to_slave (sum2,  tagBound)

      movev = sum2-sum1.ne.0

      call cputime(cpu2)
      cpuComBound = cpuComBound+ cpu2-cpu1
      write (iout,*) 'moveh=',moveh,' movev=',movev
#endif

c------------------------------------------------------
      write (iout,*) 'icore=',icore,' nbound=',nbound,' nvb=',nvb,' njb=',njb
      first = .false.
c-------------------------------------------------
        if (nvb.gt.0) write (iout,*) 'zwv_first: min =',minval(zwv(lpb(ivbound))),' max=',maxval(zwv(lpb(ivbound)))
      endif
c------------------------------------------------------
        if (nvb.eq.0.and.njb.eq.0) return
c------------------------------------------------------
         fhv = zero
         fvv = zero         
c------------------------------------------------------
c set user defined values
        do i = 1,nvb
        k = ivbound(i)
        l = lpB(k)

          if (codebound(k).eq.2) then
c user defined velocity and zsurf
          zwv(l) = zbound(k)*boundaryFac
          vv(1,l) = vxbound(k)
          vv(2,l) = vybound(k)
 
          else if (codebound(k).eq.3) then
c user defined zsurf, velocity from near cell
          zwv(l) = zbound(k)*boundaryFac
          fvv(1,l) = 0.
          fvv(2,l) = 0.
  
          else if (codebound(k).eq.4) then
c user defined velocity,  zsurf from near cell
          vv(1,l) = vxbound(k)
          vv(2,l) = vybound(k)
          fhv(l) = 0.
 
          else if (codebound(k).eq.5) then
c velocity and zsurf from near cell
          fhv(l) = 0.
          fvv(1,l) = 0.
          fvv(2,l) = 0.
          endif
        enddo
        if (time.le.0.) write (iout,*) 'Done loop 1'
c------------------------------------------------------
         do j = 1,njb
         l = jbound(j)

         pl = jtol(l)
         pr = jtor(l)
         vtypeL = vtype(pl)
         vtypeR = vtype(pr)
         m = jtype(l)

c update zwv
c left boundary,  right cell
           maskl = vtypeL.eq.4 .or. vtypeL.eq.5
           maskr = vtypeR.eq.1
             if (maskl.and.maskr) fhv(pl) = fhv(pl) + zwv(pr)
 
c right boundary, left cell,
           maskl = vtypeL.eq.1
           maskr = vtypeR.eq.4 .or. vtypeR.eq.5
             if (maskl.and.maskr) fhv(pr) = fhv(pr) + zwv(pl)
c update vv
c left boundary,  right cell
           maskl = vtypeL.eq.3 .or. vtypeL.eq.5
           maskr = vtypeR.eq.1
             if (maskl.and.maskr) fvv(1:2,pl) = fvv(1:2,pl) + vv(1:2,pr)
c right boundary, left cell,
           maskl = vtypeL.eq.1
           maskr = vtypeR.eq.3 .or. vtypeR.eq.5
             if (maskl.and.maskr) fvv(1:2,pr) = fvv(1:2,pr) + vv(1:2,pl)
         enddo
        if (time.le.0.) write (iout,*) 'Done loop 2'
c-------------------------------------------------
#ifdef PARALLEL
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      if (moveh.or.movev) then
#ifdef PUTBARRIER
      call cputime(cpu1)
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpu2)
        cpuBARBound = cpuBARbound+cpu2-cpu1
#endif
      call cputime(cpu1)
      if (moveh) call real8_putR    (fhv, 1, tagBound,Fsum)
      if (movev) call real8_putR    (fvv, 2, tagBound,Fsum)
      call cputime(cpu2)
      cpuComBound = cpuComBound+ cpu2-cpu1
      endif
#endif
c-------------------------------------------------
        do i = 1,nvb
        k = ivbound(i)
        l = lpB(k)
c       ---------------------
          if (vtype(l).le.1) cycle
c       ---------------------
        waveL = waveT * sqrt(max(hv(l),hlim0)*g)         
*        fac = min(max(waveL/(2*msize(lev(l))+waveL),0.1),0.99) ! in open sea fac is high
        fac = 1
          if (nupsh(l).gt.0) zwv(l) = fhv(l)/nupsh(l)*fac + zbound(k)*(1-fac)
          if (nupsv(l).gt.0) vv(1:2,l) = fvv(1:2,l)/nupsv(l)*fac + (/vxbound(k),vybound(k)/)*(1-fac)
c       ---------------------
        zwv(l) = max(zwv(l),zminv(l))
        dzwdk(1:2,l)=0
        call surf2depth (zv(l),zwv(l),dzdk(1:2,l),dzwdk(1:2,l),hv(l)
     &                ,hj(1:2,1:2,l),hk(1:2,l),wetfrac(l),case(l),dsize(lev(l)) )
        enddo
        if (time.le.0.) write (iout,*) 'Done loop 3'

c-------------------------------------------------
        if (nvb.gt.0.and.time.le.0.) then
        write (iout,*) 'zbound: min =',minval(zbound(ivbound)),' max=',maxval(zbound(ivbound))
        endif
c-------------------------------------------------        
        return
        end

c============================================================================
        subroutine setBoundMap (BoundMap,iB)
c============================================================================
        use parameter_data
        use boundary_data
        use geometry_data
        use window_data
        implicit none

        real(r4), allocatable :: valB(:)

        character BoundMap*(*)
        integer(i4)  m,k,l,ix,iy, ixb, iyb, lf, length, iB
        real(r4) xb, yb
        logical exist

c get window size from boundaryDir


      lf = length(boundMap)     

      write (iout,*) 
      write (iout,*) 'iB=',iB,' ',boundMap(1:lf)

      maps = boundaryDir(1:lbdir)//bs//BoundMap(1:lf)//null
        inquire (file=maps,exist=exist)
          if (.not.exist) then
          call logout(2,'In boundaryDir not exist file '//maps)
          endif
        call OPENGDAL (maps,ipB%nvx,ipB%nvy,ipB%typemap, ipB%xstart, ipB%ystart,ipB%cellsize
     &           ,ipB%cellsizey,ipB%driver,ipB%nband,error)
          if (error.ne.0) then
          call logout(2,'Error reading map '//maps)
          endif

      write (iout,*) 'xstart=',ipB%xstart,' ystart=',ipB%ystart,' cellsize=',ipB%cellsize

      ipB%nval = max(ipB%nval,ipB%nvx*ipB%nvy)
      write (iout,*) 'nvx=',ipB%nvx,' nvy=',ipB%nvy,' nval=',ipB%nval

        m = 0
           do k = 1,nbound
           l = cellbound(k)
           iy = int((l-1)/nvx)+1
           ix = l-(iy-1)*nvx
           xb = xstart+cellsize*(ix-0.5)
           yb = ystart-cellsize*(iy-0.5)
           ixb = int((xb-ipB%xstart)/ipB%cellsize)+1
           iyb = int((ipB%ystart-yb)/ipB%cellsize)+1
*             if (ixb.gt.2.and.ixb.lt.ipB%nvx-1.and.iyb.gt.2.and.iyb.lt.ipB%nvy-1) then
* no frame around the nested window, i.e., the window of the nested can be the same of the coarse
             if (ixb.ge.1.and.ixb.le.ipB%nvx.and.iyb.ge.1.and.iyb.le.ipB%nvy) then
             m=m+1
             kb(iB,m)=k
             lkb(iB,m)=(iyb-1)*ipB%nvx + ixb
             nmapok(k) = nmapok(k)+1
             endif
           enddo
      nkb(iB)=m
      write (iout,*) 'nbound=',nbound,' nkb=',nkb(iB)

      call CLOSEGDAL()

      return
      end
 
c============================================================================
        subroutine getBoundMap0 (BoundMap,iB,vbound)
c============================================================================
        use parameter_data
        use boundary_data
        use geometry_data
        implicit none

        character BoundMap*(*)
        integer(i4) nvbound,iB
        real(r4) vbound(*)
c------------------------------------------------------------------------
       nvbound=nkb(iB)
      maps = boundaryDir(1:lbdir)//bs//BoundMap
        call resmap (maps,iband,xstart,ystart,cellratio,xBound(iB,1:nvbound),yBound(iB,1:nvbound),vbound,nvbound,-1,-1)   
        return
        end
 
c============================================================================
        subroutine getBoundMap (BoundMap,iB,vbound)
c============================================================================
        use parameter_data
        use boundary_data
        use geometry_data
        use statistics_data
        use window_data
        implicit none

        character BoundMap*(*)

        logical exist

        integer(i4) m, l, iB, lf, length
        real(r4) vbound(*)
      
      lf = length(BoundMap)
      maps = boundaryDir(1:lbdir)//bs//BoundMap(1:lf)//null
        inquire (file=maps,exist=exist)
          if (.not.exist) then
          call logout(2,'In boundaryDir not exist file '//maps)
          endif

        call OPENGDAL (maps,ipB%nvx,ipB%nvy,ipB%typemap, ipB%xstart, ipB%ystart
     &        ,ipB%cellsize,ipB%cellsizey,ipB%driver,ipB%nband,error)
          if (error.ne.0) then
          call logout(2,'Error reading map '//maps)
          endif
        call GETGDALVAL (iband,ipB%val)

        do m = 1,nkb(iB)
        l = lkb(iB,m)
        vbound(m) = ipB%val(l)        
        enddo          

        return
        end
