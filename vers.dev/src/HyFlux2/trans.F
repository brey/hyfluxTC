#define DEB c
#define DEC c  
#undef PUTBARRIER
#undef hkCalc
#define hshoreFlux
*================================================================================
      subroutine TRANSIENT (err)
*================================================================================
      use Riemann_data
      use parameter_data
      use geometry_data
      use hydro_data
      use boundary_data
      use slope_data
      use statistics_data
      use parallel_data
      implicit none
c-------------------------------------------------
      integer(i4) i,j,k,l,n
     &         , pl, pr, m, lev0
     &         , tag, vtypeL, vtypeR
     &         ,ivb1(2),ivb2(2), err
c-------------------------------------------------
c flux calcualtion
      integer(i4) sgn(2), typeRiem,juncalc

      real(r8) PrimL(nvec),PrimR(nvec),PrimJ(nvec),VarJ(nvec)
      real(r8) FJ(nvec), hL, hR, hLF, hRF, vL(ndim), vR(ndim), vLB(ndim), vRB(ndim)
     &        ,vLF(ndim),vRF(ndim)
      real(r8) step, vL0(ndim), vR0(ndim), hvv(ndim), dwdk(ndim)
     &        ,dflux(2,nvec)

 
      real(r8) cv, frno, ratio, slope, facrain
     &      , sl, sr, MINMOD, SUPERBE,LIMITER
     &      ,fach,man2, dtg, faclim, small, facshore, hvol
     &      ,limfundh(0:maxmaxlev), fdh, mult, limZW0, dxj
     &      ,smallmin(0:maxmaxlev),smallmax(0:maxmaxlev), ldh, l05
     &      ,limdh, limdx(0:maxmaxlev), ldx, dx, invdx
     &      ,setdhmin,setdhmax, setdhmed, maxdhmax1, frnum /0.1d0/
     &      ,dzl, dzr, lam1, lam2, lmax, vinv(2), dfvv(2)
     &      ,zL,zR, zwL,zwR, zjun,  sl1, sl2, dh, hne
     &      ,fshore,fshore1 /0.5d0/, fshore2 /1.d0/
     &      ,cdx_1 /1.1667/, cdx_2 /-0.18096/  ! for dx=10000,1000,10 -> limdx=-0.5,0,0.75
     &      ,cdh_1 /-15.333/, cdh_2 /7.0935/  !   for dx=10000,1000,10 -> coef4=50,33.66,1

!     &      ,cdx_1 /1.125/, cdx_2 /-0.16286/  ! for dx=1000,10 -> limdx=0,0.75
!     &      ,cdh_1 /-8.5/, cdh_2 /4.1258/  !   for dx=1000,10 -> coef4=20,1

      real(r8) xcell, ycell, sx,sy, hlimsurge 
      parameter (hlimsurge=2.d0)
c------------------------------
      logical(i4) first /.true./, contLoop
c------------------------------
 
c  velocity calculation
      real(r8) Avel, facvol, facdry/0.25d0/
 
c limit flux
      real(r8) dhv, dtguess
 
c-------------------------------------------------------------
      logical maskl, maskr, eqmv,verjun
     &       ,upvols,reqmap
     &       , large
      logical*4     existFile
      integer(i4) length, lm

*================================================================================

      small=smallNew
      l05 = -log(0.5d0)
c---------------------------------------
c caution: this is called also at the end of iteration
c  get boundary data from boundaryDir

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      err = 0      
      cpuComBound = 0
      cpuIoBound = 0
      cpuBarBound = 0
      call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      if (nested) call setTimeBound (err)
c set boundary conditions according to boundary file/directory
      call setBound
*       if (nvb.gt.0) write (iout,*) 'zwv0: min =',minval(zwv(lpb(ivbound))),' max=',maxval(zwv(lpb(ivbound)))
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        tag = tagBound
        block(tag) = 'Boundary'
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       call cputime (cpuen)

       cpuCAL(tag) = cpuCAL(tag)+cpuen-cpust-cpuComBound-cpuIoBound-cpuBarBound
       cpuCOM(tag) = cpuCOM(tag)+cpuComBound
       cpuBAR(tag) = cpuBAR(tag)+cpuBarBound
       cpuIO(tag) = cpuIO(tag)+cpuIoBound
       call cputime(cpust)

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PARALLEL
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PUTBARRIER
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpuen)
        cpuBAR(tag) = cpuBAR(tag)+cpuen-cpust
        call cputime(cpust)
#endif
#endif


*================================================================================
      bigLoop:  do while (err.eq.0)
*================================================================================

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      call cputime (cpust)
      cpushore = 0

c---------------------------------------

c interpolate holland parameters
      if (existHolland) call setholland

*================================================================================

DEB       write(iout,*)'after loop1: icore=',icore,' allocated slopeV ? ',allocated(slopeV)

      do lev0 = 0,maxlev
      dx = 2*msize(lev0)
      ldx = log(dx)

      if (smallArg.ge.zero) then
      setdh = smallArg
      else
*      fdh   = limCoeff(4)*min(max(cdh_1 + cdh_2*ldx,0.1),50.)*fdhmed
      fdh   = limCoeff(4)*min(max(cdh_1 + cdh_2*ldx,0.5),50.)*fdhmed   ! fdhmed=1e-2
*      fdh   = min(max(limCoeff(4)*fdhmed*(cdh_1 + cdh_2*ldx),0.01),1.)
*      maxdhmax1 = min(max(dhmax,0.01),maxdhmax,max(dx*1e-2,10.),100.) ! 2011.01.31
      maxdhmax1 = min(max(dhmax,0.01),maxdhmax,1.)  ! 2012.01.11
      setdh = fdh*maxdhmax1
      endif

      setdhmax = fdhmax*maxdhmax1
      setdhmin = max(fdhmin*maxdhmax1,hmin)

      limdx(lev0)    = min(max(cdx_1 + cdx_2*ldx,-0.5),0.75)
      limfundh(lev0) = l05*setdh
      smallmin(lev0) = (small*setdhmin/(dsize(lev0)*2))**2
      smallmax(lev0) = (small*setdhmax/(dsize(lev0)*2))**2
        if (istep.le.1) then
        write (iout,*) 'fdh=',fdh,' limdx=',limdx(lev0)
        write (iout,*) 'dsize=',dsize(lev0),' limfundh=',limfundh(lev0)
        endif
      enddo

*================================================================================
      maxiter2 = 0
      maxiter3 = 0
      istep = istep + 1
      ncorr = 0
c------------------------------------------------
      dtmax = dtmax*1.1
      maxfr = 0
      maxvn = 0
      maxzw = 0
      maxu10 = 0
      maxdepr = 0
c       ------------------------------------------------
DEB       write(iout,*)'start loop transient=',icore

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        tag = 9
        block(tag) = 'StartTrans_S'
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	call cputime (cpuen)
        cpuCAL(tag) = cpuCAL(tag)+cpuen-cpust
        call cputime(cpust)
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#ifdef  PARALLEL
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PUTBARRIER
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpuen)
        cpuBAR(tag) = cpuBAR(tag)+cpuen-cpust
        call cputime(cpust)
#endif
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        call S_real8_master_to_slave (dt,tag)
        call S_real8_master_to_slave (dtjun,tag)
        call S_real8_master_to_slave (dtmax,tag)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 	call cputime (cpuen)
        cpuCOM(tag) = cpuCOM(tag)+cpuen-cpust
        call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#endif
        tag = 10
        block(tag) = 'StartTrans_V'
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef  PARALLEL
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#ifdef PUTBARRIER
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpuen)
        cpuBAR(tag) = cpuBAR(tag)+cpuen-cpust
        call cputime(cpust)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        call integer1_getLR (case,1,tag)
          if (.not.existHolland) call logical1_getLR (upvol,1,tag)
        call real8_getLR    (hv,1,tag)
        call real8_getLR    (zwv,1,tag)
        call real8_getLR    (vv,2,tag)       
#ifdef hkCalc
        call real8_getLR    (hk,2,tag)       
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 	call cputime (cpuen)
        cpuCOM(tag) = cpuCOM(tag)+cpuen-cpust
        call cputime(cpust)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c=====================================================================
c calculate conservative var in the cells 
        do k = 1,ndim
        cvv(k,ivL1:ivL2) = hv(ivL1:ivL2)*vv(k,ivL1:ivL2)
        enddo
c=====================================================================
c  loop 1 over volumes
c  evaluate slopes
c=====================================================================
c input for slaves:  upvol, hv, zwv, vv

DEB       write(iout,*)'before loop0: icore=',icore,' istep=',istep
DEB       write(iout,'(3(a,f6.2))')'dtmax=',dtmax,' dtjun=',dtjun,' dt=',dt
DEB      write (iout,*)'vx: min=',minval(vv(1,iv1:iv2)),' max=',maxval(vv(1,iv1:iv2))
DEB      write (iout,*)'vy: min=',minval(vv(2,iv1:iv2)),' max=',maxval(vv(2,iv1:iv2))
DEB      write (iout,*)'vn: min=',minval(vn),' max=',maxval(vn)
DEB      write (iout,*)'hv: min=',minval(hv),' max=',maxval(hv)
DEB      write (iout,*)'zv: min=',minval(zv),' max=',maxval(zv)
c--------------------------------------------------------------
        ivb1=(/ivL1,iv2+1/)
        ivb2=(/iv1-1,ivL2/)
        do n = 1,2
        do l=ivb1(n),ivb2(n)
        vn(l) = sqrt(vv(1,l)*vv(1,l)+vv(2,l)*vv(2,l))
        if (slopeV(l)) then
#ifdef hkCalc
          do m=1,2
          sf(m,l) = -sign(1.d0,vv(m,l))*min(g*hk(m,l)*frnum,abs(vv(m,l))*vn(l))*man(l)/(max(hk(m,l),hvel00)**fourthird)
          enddo
#else
          Avel = man2/(hv(l)**fourthird)
          sf(1:2,l) = -vv(1:2,l)*Avel*vn(l)
#endif
*    dzdkm(2,l) = dzdk(2,l)/deg2m  !  [m/deg] / [m/deg] = [m/m] = [-]
          if (mapdeg) then
          sf(1:2,l) = sf(1:2,l)*deg2m
          sf(1,l) = sf(1,l)*coslatv(l)
          endif
        else
        sf(1:2,l) = zero
        endif
        enddo
        enddo

c--------------------------------------------------------------
        loop1: do l = iv1, iv2
        lev0 = lev(l)
c vn
        vn(l) = sqrt(vv(1,l)*vv(1,l)+vv(2,l)*vv(2,l))
c lmax
        cv = min(max(sqrt(g*hv(l)),ccrit),cminDem)
        lmax = cv+vn(l)
c courn
        courn(l) = dt*lmax/(2*msize(lev(l)))
c dtmax
          dtv = 2*msize(lev(l))/lmax
            if (vtype(l).eq.1) dtmax = min(dtmax,dtv)
c------------------------------------------------------
           if ((abs(wetfrac(l)-wetfracIn(l)).gt.wout.or.wetfrac(l).ge.one)
     &     .and.maxval(hk(1:2,l)).gt.hout   !  pozzanghera, when case=1 & no water flux in the junctions
     &     ) then
c maxzw
        maxzw = max(maxzw,zwv(l))
c maxfr
        frno = vn(l)/cv
        frno = min(frno,10.)
        maxfr = max(maxfr,frno)
c maxvn
        maxvn = max(maxvn,vn(l))
        endif
c--------------------------------------------------------------
        limvals(l) = min(max(limval+limcoeff(1)+limcoeff(2)+limcoeff(3),-0.5),0.5)
        smalls(l)  = smallmin(lev(l))
c--------------------------------------------------------------
        dzwdk(1:2,l) = zero
        dvdk(1:2,1:2,l) = zero
        dcvdk(1:2,1:2,l) = zero
        fshore = one
c--------------------------------------------------------------
        if (vtype(l).ne.1) go to 100
        if (.not.slopeV(l).or.ilim.eq.0) goto 100
c--------------------------------------------------------------
c dry/wet  front (runup) ...... 
        hne =   (abs(dzdk(1,l))+abs(dzdk(2,l)))*dsize(lev(l)) 
*        if (.not.deepV(l).and.hv(l).lt.onethird*hne) then  !  depth=1/3*hne: shoreline and cell diagonal are the same
        if (.not.deepV(l).and.hne.gt.hvel00) then
          if (hv(l).lt.fshore1*hne) then  
          dzwdk(1:2,l) = zero
          dvdk(1:2,1:2,l) = zero
          fshore = zero
          goto 100

          else if (hv(l).lt.fshore2*hne) then  
          fshore = (hv(l)/hne-fshore1)/(fshore2-fshore1)          
          endif
        endif
c--------------------------------------------------------------
          if (ilim.eq.4) then
c         -----------------------------------------
c evaluate limZW
          limdh   = exp(-limfundh(lev(l))/max(dhjv(l),hmin))
            if ( hv(l).lt.hlim0) then
            faclim = 1
            else if (hv(l).lt.hlim) then
            faclim = 1-log(hv(l)/hlim0)*invhlim
            else
            faclim = 0
            endif

*          limZW = limval+limdx(lev0)+limdh*(limCoeff(3)+faclim*limCoeff(1))  
*          limZW = limval+limdx(lev(l))+limdh*limCoeff(3)*(1+faclim*limCoeff(1))  ! 2010.09.16 9:00
          limZW = limval+faclim*limCoeff(1)+limdx(lev0)*limcoeff(2)+limdh*limCoeff(3)  
            if (zv(l).gt.seaLevel) limZW = 0.5

          limZW=min(max(limZW,-0.5),0.5)
          limvals(l) = limZW
          endif
c--------------------------------------------------------------
        limV=limZW
c--------------------------------------------------------------
        invdx = 1.d0/(2*dsize(lev(l)))
        slope = dhjv(l)*invdx
        if (smallNew.ge.0.) then
        smallH = min(max((small*slope)**2,smallmin(lev(l))),smallmax(lev(l)))
        else if (smallArg.ge.zero) then
        smallH = (smallArg*invdx )**2
        else
        smallH = (fdhmed*invdx )**2
        endif
        smalls(l)=smallH
c--------------------------------------------------------------
*       smallV = smallH*sqrt(g/max(hv(l),hmin))
        mult = sqrt(g/max(hv(l),hmin))
*        mult = max(g/max(hv(l),hmin),5e-2)
        smallV = smallH*min(mult,1.)
c--------------------------------------------------------------
c evaluate slopes
c--------------------------------------------------------------
        loop11: do m=1,2
c--------------------------------------------------------------
        pl = vtol(m,l)
        pr = vtor(m,l)
c       --------------------------------------------------
        if (deepV(pl).and.deepV(pr)) then
        sl = (zwv(l)-zwv(pl))/(dsize(lev(l))+dsize(lev(pl)))
        sr =-(zwv(l)-zwv(pr))/(dsize(lev(l))+dsize(lev(pr)))
        dzwdk(m,l)=LIMITER(sl,sr,ilim,limZW,smallH)
            if (limnoise.and.dzwdk(m,l).eq.zero) then
            dvdk(m,1:2,l) = zero
            else
              do k = 1,2
              sl = (vv(k,l)-vv(k,pl))/(dsize(lev(l))+dsize(lev(pl)))
              sr =-(vv(k,l)-vv(k,pr))/(dsize(lev(l))+dsize(lev(pr)))
              dvdk(m,k,l) =LIMITER(sl,sr,ilim,limV,smallV)
              enddo
            endif
        goto 110
        endif
c         --------------------------------------------------
        upvols = upvol(l).or.first
          if (.not.upvols.and.pl.gt.0) upvols = upvol(pl)
          if (.not.upvols.and.pr.gt.0) upvols = upvol(pr)
          if (.not.upvols) goto 110
c         --------------------------------------------------
#ifdef hkCalc
          hvol = hk(m,pl)
#else
          hvol = hv(pl)
#endif
            if (pl.eq.0) then
            sl = zero
            else if (vtype(pl).eq.0) then
            sl = zero
            else if (.not.slopeV(pl)) then
            sl = zero
            else if (case(pl).gt.0) then
            sl = zero
            else if (hvol.le.hshore0) then
            sl = sf(m,pl)
            else
            sl = (zwv(l)-max(zwv(pl),zv(pl)))/(dsize(lev(l))+dsize(lev(pl)))
              if (hvol.lt.hshore) then   
              facshore = log(hvol/hshore0)*invhshore
              sl = (one-facshore)*sf(m,pl)  + facshore*sl
              endif
            endif
c         -------------------------------------------------
#ifdef hkCalc
          hvol = hk(m,pr)
#else
          hvol = hv(pr)
#endif
            if (pr.eq.0) then
            sr = zero
            else if (vtype(pr).eq.0) then
            sr = zero
            else if (.not.slopeV(pr)) then
            sr = zero
            else if (case(pr).gt.0) then
            sr = zero
            else if (hvol.le.hshore0) then
            sr = sf(m,pr)
            else
            sr =-(zwv(l)-max(zwv(pr),zv(pr)))/(dsize(lev(l))+dsize(lev(pr)))
              if (hvol.lt.hshore) then   
              facshore = log(hvol/hshore0)*invhshore
              sr = (one-facshore)*sf(m,pr)  + facshore*sr
              endif
            endif
c         -------------------------------------------------
          dzwdk0(m,l)=LIMITER(sl,sr,ilim,limZW,smallH)
c         ----------------------------------------------------------
c evaluate water slope 
c  in steady state: horizzontal if vn==0, parallel to bottom if friction+slope==0
c           ----------------------------------------------------
#ifdef hkCalc
          hvol = hk(m,l)
#else
          hvol = hv(l)
#endif
            if (hvol.lt.hshore0) then   
            dzwdk(m,l) = sf(m,l)

            else if (hvol.lt.hshore) then   
            facshore = log(hvol/hshore0)*invhshore
            dzwdk(m,l) = (one-facshore)*sf(m,l)  + facshore*dzwdk0(m,l)

            else
            dzwdk(m,l) = dzwdk0(m,l)
            endif
c           ----------------------------------------------------
            if ((limnoise.and.dzwdk(m,l).eq.zero).or.sl.eq.zero.or.sr.eq.zero) then
            dvdk(m,1:2,l) = zero
            else
              do k = 1,2
              sl = (vv(k,l)-vv(k,pl))/(dsize(lev(l))+dsize(lev(pl)))
              sr =-(vv(k,l)-vv(k,pr))/(dsize(lev(l))+dsize(lev(pr)))
              dvdk(m,k,l) =LIMITER(sl,sr,ilim,limV,smallV)
              enddo
            endif
c       -----------------------------------------
110     continue
        enddo loop11
c------------------------------------------------
        if (fshore.lt.one) then
        dzwdk(1:2,l) = dzwdk(1:2,l)*fshore
        dvdk(1:2,1:2,l) = dvdk(1:2,1:2,l)*fshore
        endif
c------------------------------------------------
100   continue
      enddo loop1

c=====================================================================

DEB       write(iout,*)'done loop1: icore=',icore
DEB       write(iout,'(3(a,f6.2))')'dtmax=',dtmax,' dtjun=',dtjun,' dt=',dt

c=====================================================================
c  end loop 1
c=====================================================================

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          tag = 12
        block(tag) = 'Slopes'
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	call cputime (cpuen)
        cpuCAL(tag) = cpuCAL(tag)+cpuen-cpust
        call cputime(cpust)
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#ifdef PARALLEL
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#ifdef PUTBARRIER
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpuen)
        cpuBAR(tag) = cpuBAR(tag)+cpuen-cpust
        call cputime(cpust)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          call real8_getR (dzwdk,2,tag)
          call real8_getR (dvdk,4,tag)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 	call cputime (cpuen)
        cpuCOM(tag) = cpuCOM(tag)+cpuen-cpust
        call cputime(cpust)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c--------------------------------------------------------------
c evaluate zsurf(new_slope,old_depth)
c--------------------------------------------------------------
        do l = iv1, ivL2
c       --------------------------------------------------
        if (vtype(l).ne.1) cycle
c       --------------------------------------------------
          if (deepV(l)) then
          zwv(l) = zv(l)+hv(l)
          dwdk = (dzwdk(1:2,l)-dzdk(1:2,l))*dsize(lev(l))
          hj(1,1:2,l) = hv(l) + dwdk
          hj(2,1:2,l) = hv(l) - dwdk
          hk(1:2,l) = hv(l)
          wetfrac(l) = 1
          case(l) = 0
          else 
          call depth2surf (hv(l),zv(l),dzdk(1:2,l),dzwdk(1:2,l)
     &           ,hj(1:2,1:2,l),hk(1:2,l),zwv(l),wetfrac(l),case(l),dsize(lev(l)))
          endif
        enddo

*        write (iout,*) 'zwv1: min =',minval(zwv(lpb(ivbound))),' max=',maxval(zwv(lpb(ivbound)))

c--------------------------------------------------------------
c evaluate momentum
c--------------------------------------------------------------
        loop12: do l = iv1, ivL2
c       --------------------------------------------------
        if (vtype(l).ne.1) cycle
c       --------------------------------------------------
        fshore = one
        hne =   (abs(dzdk(1,l))+abs(dzdk(2,l)))*dsize(lev(l)) 
        if (.not.deepV(l).and.hne.gt.hvel00) then
          if (hv(l).lt.fshore1*hne) then  
          fshore = zero
          else if (hv(l).lt.fshore2*hne) then
          fshore = (hv(l)/hne-fshore1)/(fshore2-fshore1)                   
          endif
        endif
          loop13: do m = 1,2
          dcvdk(m,1:2,l) = hv(l)*dvdk(m,1:2,l) + fshore*vv(1:2,l)*(dzwdk(m,l)-dzdk(m,l))
          enddo loop13
        enddo loop12
c=====================================================================
       dhmax = zero
       dtjun = dtjun*1.1
       ndry = 0
       verjun = .false.
 
C preliminary estimation used to limit flux
 
 
       dhjv = hmin

       fvv = zero
       fhv = zero
 
       upflux = .false.


c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       tag = 13
       block(tag) = 'ShoreTrack1'
       call cputime (cpuen)
       cpuCAL(tag) = cpuCAL(tag)+cpuen-cpust
       call cputime(cpust)
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#ifdef PARALLEL
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#ifdef PUTBARRIER
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpuen)
        cpuBAR(tag) = cpuBAR(tag)+cpuen-cpust
        call cputime(cpust)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      call S_real8_slave_to_master    (dtmax,  tag,Fmin)
      call S_real8_master_to_slave    (dtmax,  tag)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	call cputime (cpuen)
DEC        write (icore+icpudeb,1000) 'COM icore=',icore,' istep=',istep,' tag=',tag,' cpu=',cpuen-cpust
      cpuCOM(tag) = cpuCOM(tag)+cpuen-cpust
        call cputime(cpust)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 
c=====================================================================
c loop 2 over junctions
c evaluate fluxes: loop over all junctions
c=====================================================================
c input for slaves:  hv, vv,  hj, hk, dvdk, dcvdk,   if(debug) zwv, case, dzwdk

       dtguess=min(dt,dtmax*courmax,max(dtjun*fluxnum,dtmax*0.1))


DEB       write(iout,*)'Before loop2: icore=',icore,' dtguess=',dtguess
DEB       write(iout,'(3(a,f6.2))')'dtmax=',dtmax,' dtjun=',dtjun,' dt=',dt


c------------------------------------------------------
       loop2: do l = ij1,ij2
c------------------------------------------------------
c
       dflux = zero
c
       m = jtype(l)
       in = jn(m)
       it = jt(m)
     
       pl = jtol(l)
       pr = jtor(l)
       vtypeL = vtype(pl)
       vtypeR = vtype(pr)
       maskl = vtypeL.ge.1
       maskr = vtypeR.ge.1
c------------------------------------------------------
       if (deepJ(l)) then
       hL = hj(1,m,pl)
       vL = (cvv(1:2,pl)+dcvdk(m,1:2,pl)*dsize(lev(pl)))/hL
       hR = hj(2,m,pr)
       vR = (cvv(1:2,pr)-dcvdk(m,1:2,pr)*dsize(lev(pr)))/hR
       PrimL = (/vL(in),hL,vL(it)/)
       PrimR = (/vR(in),hR,vR(it)/)
       call riemannDeep (PrimL,PrimR,FJ,typeRiem)
       upflux(pl) = upflux(pl).or.typeRiem.ne.-4
       upflux(pr) = upflux(pr).or.typeRiem.ne.-4

       dflux(1,1:nvec) =  FJ
       dflux(2,1:nvec) =  FJ
*          if (vtypeL.eq.1.and.vtypeR.eq.1) then
          dh = abs(hL-hR)
          dhjv(pl)=max(dhjv(pl),dh)
          dhjv(pr)=max(dhjv(pr),dh)
            if (dh.gt.dhmax) then
            dhmax = dh
            ldhmax = l
            endif
*          endif
       goto 250
       endif
c------------------------------------------------------
c don't calculate numerical fluxes if both cells are boundary
         if (vtypeL.ne.1.and.vtypeR.ne.1) go to 200
c------------------------------------------------------
          if (maskl) then
          zL = zv(pl)+dzdk(m,pl)*dsize(lev(pl))
c         ----------------------------------------------
          hL = hj(1,m,pl)
            if (.not.slopeV(pl)) then
            vL0 = vv(1:2,pl)
            else
            vL0 = vv(1:2,pl)+dvdk(m,1:2,pl)*dsize(lev(pl))
            endif
#ifdef hshoreFlux
          hvol = hv(pl)
            if (hvol.le.hshore0) then
            vL0=zero
            else if (hvol.le.hshore) then
            facshore = log(hvol/hshore0)*invhshore
            vL0 = facshore*vL0
            endif
#endif             
            if (hL.lt.hlim0) then
            vL = vL0
            else if (hL.lt.hlim) then
            faclim = min(max(log( hL/hlim0)*invhlim,zero),one)
            vL = (cvv(1:2,pl)+dcvdk(m,1:2,pl)*dsize(lev(pl)))/max(hL,hlim0)
            vL = vL*faclim + vL0*(1-faclim)
            else
            vL = (cvv(1:2,pl)+dcvdk(m,1:2,pl)*dsize(lev(pl)))/hL
            endif
c         ----------------------------------------------
          endif
c------------------------------------------------------
          if (maskr) then
          zR = zv(pr)-dzdk(m,pr)*dsize(lev(pr))
c         ----------------------------------------------
          hR = hj(2,m,pr)
            if (.not.slopeV(pr)) then
            vR0 = vv(1:2,pr)
            else if (hR.le.hlim) then
            vR0 = vv(1:2,pr)-dvdk(m,1:2,pr)*dsize(lev(pr))
            endif
#ifdef hshoreFlux
          hvol = hv(pr)
            if (hvol.le.hshore0) then
            vR0=zero
            else if (hvol.le.hshore) then
            facshore = log(hvol/hshore0)*invhshore
            vR0 = facshore*vR0
            endif
#endif
            if (hR.le.hlim0) then
            vR = vR0
            else if (hR.le.hlim) then
            faclim = min(max(log( hR/hlim0)*invhlim,zero),one)
            vR = (cvv(1:2,pr)-dcvdk(m,1:2,pr)*dsize(lev(pr)))/max(hR,hlim0)
            vR = vR*faclim + vR0*(1-faclim) 
            else
            vR = (cvv(1:2,pr)-dcvdk(m,1:2,pr)*dsize(lev(pr)))/hR
            endif
c         ----------------------------------------------
          endif
c---------------------------------------------------------
          if (vtypeL.eq.1.and.vtypeR.eq.1) then
          zjun = max(zj(l),zL,zR)
          dxj = min(msize(lev(pl)),msize(lev(pr)))*2

          else if (vtypeL.eq.1) then
          zjun = zL
          zR = zL
          dxj = msize(lev(pl))*2

          else if (vtypeR.eq.1) then
          zjun = zR
          zL = zR
          dxj = msize(lev(pr))*2
          endif
 
          if (vtypeL.eq.4.or.vtypeL.eq.5) hL = hR
          if (vtypeL.eq.3.or.vtypeL.eq.5) vL = vR
 
          if (vtypeR.eq.4.or.vtypeR.eq.5) hR = hL
          if (vtypeR.eq.3.or.vtypeR.eq.5) vR = vL
c---------------------------------------------------------
c right cell is reflective boundary
          if (vtypeL.eq.1.and..not.maskr) then
          sgn = (/1,-1/)
          call fluxBound (hL,vL,sgn,FJ,VarJ)
          dflux(1,1:nvec) = FJ
          go to 250
c         ----------------------------------------------
c left cell is reflective boundary
          else if (vtypeR.eq.1.and..not.maskl) then
          sgn = (/-1,1/)
          call fluxBound (hR,vR,sgn,FJ,VarJ)
          dflux(2,1:nvec) = FJ
          go to 250
          endif
c---------------------------------------------------------
          step = max(abs(zL-zjun),abs(zR-zjun))
            if (step.lt.errstep) go to 150
c---------------------------------------------------------
c lower bottom at left side
          if (zL.lt.zjun) then
          step = zjun-zL
          sgn = (/1,-1/)
            if (hL.le.step) then
            call fluxBound (hL,vL,sgn,FJ,VarJ)
            dflux(1,1:nvec) = dflux(1,1:nvec) + FJ
            hL = hmin
            vL = zero
 
            else
            hLF = max(hL-step,hmin)
            ratio = hL/step
              if (facstep.eq.0.or.ratio.lt.2.) then
              vLB = vL
              vLF = vL
              else if (facstep.lt.100) then
              vLB = vL/(ratio-1)**facstep
              vLF = (vL*hL-step*vLB)/hLF
              else
              vLB = zero
              vLF = (vL*hL)/hLF
              endif
 
            call fluxBound (hLF,vLB,sgn,FJ,VarJ)
            dflux(1,1:nvec) = dflux(1,1:nvec) - FJ
 
            call fluxBound (hL,vLB,sgn,FJ,VarJ)
            dflux(1,1:nvec) = dflux(1,1:nvec) + FJ
 
            vL = vLF
            hL = hLF
            endif
          endif
c-------------------------------------------------------
c lower bottom at right side
          if (zR.lt.zjun) then
          step = zjun-zR
          sgn = (/-1,1/)
            if (hR.lt.step) then
            call fluxBound (hR,vR,sgn,FJ,VarJ)
            dflux(2,1:nvec) = dflux(2,1:nvec) + FJ
            hR = hmin
            vR = zero

            else
            hRF = max(hR-step,hmin)
            ratio = hR/step
              if (facstep.eq.0.or.ratio.lt.2.) then
              vRB = vR
              vRF = vR
              else if (facstep.lt.100) then
              vRB = vR/(ratio-1)**facstep
              vRF = (vR*hR-step*vRB)/hRF
              else
              vRB = zero
              vRF = (vR*hR)/hRF
              endif
 
            call fluxBound (hRF,vRB,sgn,FJ,VarJ)
            dflux(2,1:nvec) = dflux(2,1:nvec) - FJ
 
            call fluxBound (hR,vRB,sgn,FJ,VarJ)
            dflux(2,1:nvec) = dflux(2,1:nvec) + FJ
 
            vR = vRF
            hR = hRF
            endif
          endif
c -----------------------------------------------
150       continue
          dh = abs(hL-hR)
          dhjv(pl)=max(dhjv(pl),dh)
          dhjv(pr)=max(dhjv(pr),dh)
            if (dh.gt.dhmax) then
            dhmax = dh
            ldhmax = l
            endif
c -----------------------------------------------
c Riemann Solver
c -----------------------------------------------
        PrimL = (/vL(in),hL,vL(it)/)
        PrimR = (/vR(in),hR,vR(it)/)

        juncalc=1

        if (vtypeL.gt.1.and.kbound(pl).gt.0) then
        juncalc=0
        call primToVar (PrimL,VarJ)
        k = kbound(pl)
        vL = (/vxbound(k),vybound(k)/)
        hL = max(zbound(k)-zL,hmin)
        PrimL = (/vL(in),hL,vL(it)/)
        endif

        if (vtypeR.gt.1.and.kbound(pr).gt.0) then
        juncalc=0
        call primToVar (PrimR,VarJ)
        k = kbound(pr)
        vR = (/vxbound(k),vybound(k)/)
        hR = max(zbound(k)-zR,hmin)
        PrimR = (/vR(in),hR,vR(it)/)
        endif

        call riemann (PrimL,PrimR,FJ,VarJ,typeRiem,juncalc)
 
c typeRiem .eq. -4 if dp (prim. inc.)  < errprim
        upflux(pl) = upflux(pl).or.typeRiem.ne.-4
        upflux(pr) = upflux(pr).or.typeRiem.ne.-4
c -----------------------------------------------
c calculate  in the junction  (courant criteria)
c -----------------------------------------------
        if (varJ(ih).gt.ccrit) then
        cv = min(VarJ(ih),cminDem)
        lmax = cv+abs(VarJ(ivn))
        dtv = dxj/lmax
        dtmax = min(dtmax,dtv)
        endif
c======================================================================
       verjun = debug.and.l.eq.ldeb
c -----------------------------------------------
c input for slaves: hv, hj, hk, vv, dvdk, dcvdk

         if (verjun.or.(debug.and.dh.gt.maxdhmax)) then
          zwR = zwv(pr)-dzwdk(m,pr)*dsize(lev(pr))
          zwL = zwv(pl)+dzwdk(m,pl)*dsize(lev(pl))
         write (iout,*)
         write (iout,*) 'istep=',istep,'; jun=',l,'; pl=',pl,'; pr=',pr
         write (iout,*) 'nl=',kl(pl),';nr=',kl(pr),'; m=',m
         write (iout,*) 'vtypeL=',vtypeL,'; vtypeR=',vtypeR
         write (iout,*) 'zl=',zv(pl),';zr=',zv(pr)
         write (iout,*) 'zL=',zL,';zR=',zR,'; zjun=',zjun
         write (iout,*) 'zwl=',zwv(pl),';zwr=',zwv(pr)
         write (iout,*) 'zwL=',zwL,';zwR=',zwR
         write (iout,*) 'hl=',hv(pl),'; hr=',hv(pr)
         write (iout,*) 'hL=',hL,'; hR=',hR
         write (iout,*) 'vl=',vv(in,pl),'; vr=',vv(in,pr)
         write (iout,*) 'vL=',vL(in),'; vR=',vR(in)
         write (iout,*) 'hL-hR=',hL-hR,'; vL-vR=',vL-vR
         write (iout,*) 'dzdkL=',dzdk(m,pl),'; dzdkR=',dzdk(m,pr)
         write (iout,*) 'dzwdkL=',dzwdk(m,pl),'; dzwdkR=',dzwdk(m,pr)
         write (iout,*) 'hkL=',hk(m,pl),';hkR=',hk(m,pr)
         write (iout,*) 'typeRiem=',typeRiem
         write (iout,*) 'FJ=',FJ
         call varToPrim(VarJ,PrimJ)
         write (iout,*) 'VarJ=',VarJ
         write (iout,*) 'PrimJ(ivn,ih,ivt)=',PrimJ
         verjun = .false.
         endif
c -----------------------------------------------
c evaluate dtjun for which all the water is remover from the cell
c -----------------------------------------------  
          if (FJ(1).gt.zero.and.hv(pl).gt.hvel0) dtjun = min(dtjun,hv(pl)*dxj/max(FJ(1),hvel0*velmin))
          if (FJ(1).lt.zero.and.hv(pr).gt.hvel0) dtjun = min(dtjun,hv(pr)*dxj/max(-FJ(1),hvel0*velmin))
c -----------------------------------------------
c limit fluxes near dry state
c -----------------------------------------------  
          dhv  = FJ(1)*facL(l)*dtguess
            if (facdry*hv(pl)-dhv.lt.zero) then
              if (hv(pl).gt.hvel0) ncorr = ncorr+1
            call varToPrim(VarJ,PrimJ)
            PrimJ(ih) = min(facdry*hv(pl),PrimJ(ih))
            Primj(ivn) = min(dxj/dtguess,max(PrimJ(ivn),zero))
            PrimJ(ivt) = vv(it,pl)
            call primToFlux(Primj,FJ)
            endif
 
          dhv  = FJ(1)*facR(l)*dtguess
            if (facdry*hv(pr)+dhv.lt.zero) then
              if (hv(pl).gt.hvel0) ncorr = ncorr+1
            call varToPrim(VarJ,PrimJ)
            PrimJ(ih) = min(facdry*hv(pr),PrimJ(ih))
            Primj(ivn) = max(-dxj/dtguess,min(PrimJ(ivn),zero))
            PrimJ(ivt) = vv(it,pr)
            call primToFlux(Primj,FJ)
            endif
 
        dflux(1,1:nvec) = dflux(1,1:nvec) + FJ
        dflux(2,1:nvec) = dflux(2,1:nvec) + FJ
c------------------------------------------------
250     continue
c------------------------------------------------
        if (maskl.and.vtypeL.eq.1) then
        fhv(pl) = fhv(pl) - dflux(1,1)*facL(l)
        fvv(in,pl) = fvv(in,pl) - dflux(1,2)*facL(l)
        fvv(it,pl) = fvv(it,pl) - dflux(1,3)*facL(l)
        endif
 
        if (maskr.and.vtypeR.eq.1) then
        fhv(pr) = fhv(pr) + dflux(2,1)*facR(l)
        fvv(in,pr) = fvv(in,pr) + dflux(2,2)*facR(l)
        fvv(it,pr) = fvv(it,pr) + dflux(2,3)*facR(l)
        endif

200   continue
      enddo loop2


c=====================================================================
c end loop 2 
c=====================================================================
DEB       write(iout,*)'Done loop2: icore=',icore
DEB       write(iout,'(3(a,f6.2))')'dtmax=',dtmax,' dtjun=',dtjun,' dt=',dt

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        tag = tagJun
        block(tag) = 'Flux'
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	call cputime (cpuen)
        cpuCAL(tag) = cpuCAL(tag)+cpuen-cpust
        call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PARALLEL
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PUTBARRIER
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpuen)
        cpuBAR(tag) = cpuBAR(tag)+cpuen-cpust
        call cputime(cpust)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c output for master:  dtmax, dtjun, dhmax, ndry
      call S_real8_slave_to_master    (dtmax,  tag,Fmin)
      call S_real8_slave_to_master    (dtjun,  tag,Fmin)
      call S_real4_slave_to_master    (dhmax,  tag,Fmax)

      call S_integer4_slave_to_master (ndry,  tag,Fsum)
      call S_integer4_slave_to_master (ncorr, tag,Fsum)

        if (.not.existHolland) call logical1_putR (upflux, 1, tag,For)

      call real4_putR    (dhjv, 1, tag,Fmax)
      call real8_putR    (fhv, 1, tag,Fsum)
      call real8_putR    (fvv, 2, tag,Fsum)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 	call cputime (cpuen)
        cpuCOM(tag) = cpuCOM(tag)+cpuen-cpust
        call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

DEB       write(iout,*)'Before loop3: icore=',icore
DEB       write(iout,'(3(a,f6.2))')'dtmax=',dtmax,' dtjun=',dtjun,' dt=',dt


c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      do while (time.ge.toutL(itimeL).and.itimeL.lt.ntimeL) 
      itimeL = itimeL + 1
      dtout = dtoutL(itimeL)
      dtrest = dtrestL(itimeL)
      dtstat = dtstatL(itimeL)
      enddo


c---------------------------------------
c now calculate dt, adjusting  in respect to output time step
c---------------------------------------
      if (MASTER)  then
      dt = min(dtmax*courmax,max(dtjun*fluxnum,dtmax*0.1))

        if (dthydro.gt.0) dt=min(dt,dthydro)
 
      dtrem = min(timeout-time,timerest-time,timestat-time)
      nsteprem = int(dtrem/dt-0.01)
 
      if (nsteprem.le.0) then
      time = min(timeout,timerest,timestat)
      dt = time-timeold

      outnow = timeout.eq.time
      restnow = timerest.eq.time
      statnow = timestat.eq.time

      else if (nsteprem.lt.10.and.dt.gt.dtmin) then
      dt = dtrem / (nsteprem+1)
      time = timeold + dt
      outnow = .false.
      restnow = .false.
      statnow = .false.
 
      else if (dt.le.dtmin) then
      write (iout,*) 'dt =',dt,' set to dtmin = ',dtmin
      dt = dtmin
      time = timeold+dt
      outnow = .true.
      restnow = .false.
      statnow = .false.
      stepdtmin = stepdtmin + 1

      else if (istep.ge.istepmax) then
      time = timeold+dt
      outnow = .true.
      restnow = .true.
      statnow = .true.
 
      else
      time = timeold+dt
      outnow = .false.
      restnow = .false.
      statnow = .false.
      endif
 
      if (istep.lt.3) then
      write (iout,*)
      write (iout,*) 'istep=',istep,' dtmax=',dtmax,' dtjun=',dtjun
      write (iout,*) 'dtcalc=',dtmax*courmax,' dtrem=',dtrem ,' nsteprem=',nsteprem,' dt=',dt
      endif

      endif

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        tag = 30
        block(tag) = 'DTcalc'
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	call cputime (cpuen)
        cpuCAL(tag) = cpuCAL(tag)+cpuen-cpust
        call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PARALLEL
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PUTBARRIER
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpuen)
        cpuBAR(tag) = cpuBAR(tag)+cpuen-cpust
        call cputime(cpust)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        call S_real8_master_to_slave (dt,tag)
        call S_real8_master_to_slave (time,tag)

        call S_real4_master_to_slave (dhmax,tag)

        call S_logical4_master_to_slave (outnow,tag)
        call S_logical4_master_to_slave (restnow,tag)
        call S_logical4_master_to_slave (statnow,tag)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	call cputime (cpuen)
        cpuCOM(tag) = cpuCOM(tag)+cpuen-cpust
        call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
DEB       write(iout,*)'after dt calculation: icore=',icore
DEB       write(iout,'(3(a,f6.2))')'dtmax=',dtmax,' dtjun=',dtjun,' dt=',dt

      dtg = dt*g
      timeold = time
c--------------------------------------------------------------------
c explicit sources
c--------------------------------------------------------------------
c add rainfall
      if (rainmap.ne.' ') then
      facrain = dt/(3600d3) ! from mm/h to m/sec
        do l = iv1,iv2
c       --------------------------
          if (vtype(l).ne.1) cycle
c       --------------------------
        addrain(l) = addrain(l) + rain(l)*facrain
          if (addrain(l).gt.errprim) then
          hv(l) = hv(l) + addrain(l)
          upvol (l) = .true.
          addrain(l) = zero
          endif
        enddo
      endif

DEB       write(iout,*)'done explicit source: icore=',icore

c==================================================================
c loop 3 over volumes
c evaluate new primitives: loop over all cells
c==================================================================

      haver = 0
c--------------------------------------------------------------------
        loop3: do l = iv1, iv2
c---------------------------------------
          if (vtype(l).ne.1) go to 500
c---------------------------------------
        cvv(1:2,l) = hv(l)*vv(1:2,l)
c---------------------------------------
        fhv(l) = fhv(l)*dt
        fvv(1:2,l) = fvv(1:2,l)*dt
c--------------------------------------- 
      if (mapdeg) then
c coriolis
      fvv(1,l) = fvv(1,l) + hv(l)*fcor(l)*vv(2,l)*dt
      fvv(2,l) = fvv(2,l) - hv(l)*fcor(l)*vv(1,l)*dt
/*
c additional source from coord system transformation
      fhv(l) = fhv(l) + tanLatv(l)*hv(l)*vv(2,l)*dt
      fvv(1,l) = fvv(1,l) + hv(l)*tanLatv(l)*2*vv(1,l)*vv(2,l)*dt
      fvv(2,l) = fvv(2,l) + hv(l)*tanLatv(l)*(vv(2,l)**2-vv(1,l)**2)*dt
*/
c storm surge
        if (existHolland) then
        xcell = xstart + xv(l)*cellratio
        ycell = ystart - yv(l)*cellratio
        call holland(xcell,ycell,cosLatv(l),fcor(l),vv(1,l),vv(2,l),max(hv(l),hlimsurge),man(l),sx,sy,u10(l),depr(l))
        maxu10 = max (maxu10,u10(l))
        maxdepr = max (maxdepr,depr(l))
          if (hv(l).gt.hlimsurge) then
          spu(l) = sqrt(sx*sx+sy*sy)/hv(l)
          fvv(1,l) = fvv(1,l) - sx*dt
          fvv(2,l) = fvv(2,l) - sy*dt
          endif
        endif
      endif
c---------------------------------------
c increment due to fluxes and bottom slope
c---------------------------------------
      dfvv = fvv(1:2,l)-hk(1:2,l)*dzdkm(1:2,l)*dtg
c---------------------------------------
      upvol(l) = upvol(l).or.abs(fhv(l)).gt.errprim.or.upflux(l)
        if (.not.upvol(l)) go to 500
c---------------------------------------
c calculate depth and zsurf(depth, old slope)
        hv(l) = max(hv(l)+fhv(l),hmin)
c---------------------------------------
c inviscid velocity
c---------------------------------------
       vinv = (cvv(1:2,l)+dfvv)/max(hv(l),hvel0)
*       vinv = vv(1:2,l)+dfvv/max(hv(l),hvel0)
c---------------------------------------
c mannig factor
c if hv -> 0   then  man2 -> high value
          if (hv(l).le.hvel00) then
          man2 = man(l) + manh(l)
 
          else if (hv(l).lt.hvel0) then
          fach = log(hv(l)/hvel00)*invhvel0
          man2 = manh(l)*(one-fach) + man(l) 

          else
          man2 = man(l)
          endif
 
        vn(l) = sqrt(vinv(1)*vinv(1)+vinv(2)*vinv(2))
 
c roughness > 0
          if (man2.gt.zero) then
          Avel = man2/(hv(l)**fourthird)
          facvol = 2.d0/(1.d0+sqrt(1.d0+4.d0*dtg*Avel*vn(l)))
            do k = 1,2
            vv(k,l) = vinv(k)*facvol
            enddo
          vn(l) = sqrt(vv(1,l)*vv(1,l)+vv(2,l)*vv(2,l))

          if (slopeV(l)) then
#ifdef hkCalc
          do m=1,2
          sf(m,l) = -sign(1.d0,vv(m,l))*min(g*hk(m,l)*frnum,abs(vv(m,l))*vn(l))*man(l)/(max(hk(m,l),hvel00)**fourthird)
          enddo
#else
          sf(1:2,l) = -vv(1:2,l)*Avel*vn(l)
#endif
            if (mapdeg) then
            sf(1:2,l) = sf(1:2,l)*deg2m
            sf(1,l) = sf(1,l)*coslatv(l)
            endif
          else
          sf(1:2,l) = zero
          endif
c no roughness
          else
          vv(1:2,l) = vinv
          sf(1:2,l) = zero
          endif
c--------------------------------------------------------------
c evaluate zsurf(old_slope,new_depth)
c--------------------------------------------------------------
        if (deepV(l)) then
        zwv(l) = zv(l)+hv(l)
          if (abs(zwv(l)).lt.epsvar) then
          zwv(l)= zero
          hv(l) = zwv(l)-zv(l)
          endif
        dwdk = (dzwdk(1:2,l)-dzdk(1:2,l))*dsize(lev(l))
        hj(1,1:2,l) = hv(l) + dwdk
        hj(2,1:2,l) = hv(l) - dwdk
        hk(1:2,l) = hv(l)
        wetfrac(l) = 1
        case(l) = 0

        else  ! not deepV
*          if (hv(l).lt.epsvar) hv(l) = zero
          do m = 1,2
#ifdef hkCalc
          hvol = hk(m,l)
#else
          hvol = hv(l)
#endif
            if (hvol.lt.hshore0) then   
            dzwdk(m,l) = sf(m,l)
            else if (hvol.lt.hshore) then   
            facshore = log(hvol/hshore0)*invhshore
            dzwdk(m,l) = (one-facshore)*sf(m,l)  + facshore*dzwdk0(m,l)
            else 
            dzwdk(m,l) = dzwdk0(m,l)
            endif
          enddo
        call depth2surf (hv(l),zv(l),dzdk(1:2,l),dzwdk(1:2,l)
     &       ,hj(1:2,1:2,l),hk(1:2,l),zwv(l),wetfrac(l),case(l),dsize(lev(l)))
          if (abs(zwv(l)).lt.epsvar) zwv(l)= zero
        endif
c------------------------------------------------------
        go to 600
c------------------------------------------------------
500   continue
        if (abs(zwv(l)).lt.epsvar) then
        zwv(l)= zero
          if (deepV(l)) hv(l) = zwv(l)-zv(l)
        endif
*          if (hv(l).lt.epsvar) hv(l) = zero

600   continue
        if (vn(l).lt.epsvar) vv(1:2,l) = zero
        if (abs(vv(1,l)).lt.epsvar) vv(1,l) = zero
        if (abs(vv(2,l)).lt.epsvar) vv(2,l) = zero
      vn(l) = sqrt(vv(1,l)*vv(1,l)+vv(2,l)*vv(2,l))
      haver = haver + hv(l)
      enddo  loop3
c==========================================================
*      write (iout,*) 'zwv2: min =',minval(zwv(lpb(ivbound))),' max=',maxval(zwv(lpb(ivbound)))
c==========================================================
c end loop 3
c==========================================================
DEB       write(iout,*)'done loop3: icore=',icore,' cpu=',cpu,' elaps=',elaps,' haver=',haver
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      tag = 31
        block(tag) = 'Balance'
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	call cputime (cpuen)
        cpuCAL(tag) = cpuCAL(tag)+cpuen-cpust
        call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PARALLEL
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PUTBARRIER
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpuen)
        cpuBAR(tag) = cpuBAR(tag)+cpuen-cpust
        call cputime(cpust)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      call S_real4_slave_to_master (haver,  tag,Fsum)
      call S_real4_slave_to_master (maxfr,  tag,Fmax)
      call S_real4_slave_to_master (maxvn,  tag,Fmax)
      call S_real4_slave_to_master (maxzw,  tag,Fmax)
      call S_real4_slave_to_master (maxu10,  tag,Fmax)
      call S_real4_slave_to_master (maxdepr,  tag,Fmax)
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 	call cputime (cpuen)
        cpuCOM(tag) = cpuCOM(tag)+cpuen-cpust
        call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      maxzwmax = max(maxzw,maxzwmax)
      maxvnmax = max(maxvn,maxvnmax)
      haver = haver / nv
        if (first) haverIn=haver
      first = .false.
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      cpuComBound = 0
      cpuIoBound = 0
      cpuBarBound = 0

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        tag = tagBound
        block(tag) = 'Boundary'
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c  get boundary data from boundaryDir
*      write (iout,*)'time=',time,' nested=',nested
      if (nested) call setTimeBound (err)

c set boundary conditions according to boundary file/directory
      call setBound
*      write (iout,*) 'zwv3: min =',minval(zwv(lpb(ivbound))),' max=',maxval(zwv(lpb(ivbound)))
c--------------------------------------------------------------------
      if (outnow)  imap = imap+1
      if (outnow)  timeout = min(timeout + dtout, tmax)
      if (statnow) timestat = min(timestat + dtstat, tmax)
      if (restnow) timerest = min(timerest + dtrest, tmax)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       call cputime (cpuen)
       cpuCAL(tag) = cpuCAL(tag)+cpuen-cpust-cpuComBound-cpuIoBound-cpuBarBound
       cpuCOM(tag) = cpuCOM(tag)+cpuComBound
       cpuBAR(tag) = cpuBAR(tag)+cpuBarBound
       cpuIO(tag) = cpuIO(tag)+cpuIoBound
      call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      if (MASTER) CALL OUTPUTS 
      if (CALC) call makeStat
        if (outnow.or.statnow.or.restnow.or.err.ne.0) exit bigloop
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	call cputime (cpuen)
        cpuIO(tag) = cpuIO(tag)+cpuen-cpust
        call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PARALLEL
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PUTBARRIER
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpuen)
        cpuBAR(tag) = cpuBAR(tag)+cpuen-cpust
        call cputime(cpust)
#endif
#endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
/*
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       do i = 0,3
       tag=50+i
       write(block(tag),'(a,i1)')'case',i
       cpuCAL(tag) = cpuCAL(tag)+cpuShore(i)
       enddo
*/
c==========================================================
      enddo bigLoop
c==========================================================
      return
1000  format ( a,i2,a,i6,a,i2,a,f8.6 )
      end
