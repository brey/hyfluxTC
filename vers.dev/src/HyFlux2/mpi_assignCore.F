c============================================================================
      subroutine assignCore
c============================================================================
      use geometry_data
      use hydro_data
      use parameter_data
      use parallel_data
      use slope_data
      use statistics_data
      implicit none
 
      integer(i4) i,l,m, pl, pr, k, tag
      logical(i4) maskl, maskr
      integer(i4) nvcore,i1,i2,kj
c------------------------------------------------------
      real(r4)  cBound /0.553774E-06 /
     &       , cVol(2) /0.130141E-06 , 0.875422E-06 /
     &       , cSizj(2) /0.659066E-01 , 2.01348 /
     &       , cJun /  0.242937E-06 /
     &        , sizj1, cVol1, cpuCore1
c  cpuTot = cBound*nvb+cVol_1*notdeepV+cVol_2*sizv+cJun*sizj1

      real(r4), allocatable :: cpuTot1(:)

c------------------------------------------------------
      tag = 0
      call S_integer4_master_to_slave (nv,  tag)
      call S_integer4_master_to_slave (nj,  tag)
      allocate (corev(nv))
c------------------------------------------------------
      if (MASTER) then
c------------------------------------------------------
      allocate (cpuTot1(0:nv))
      cpuTot1(0) = 0
      do l = 1,nv
      sizj1 = cSizj(2)
      cVol1 = cVol(2)
*        if (.not.deepV(l)) then
        if (.not.(zv(l).lt.(-hlim).and.vtype(l).eq.1) ) then
        sizj1 = sizj1 + cSizj(1)
        cVol1 = cVol1 + cVol(1)
        endif       
      cpuTot1(l) = cpuTot1(l-1) + cVol1 + sizj1 * cJun
        if (vtype(l).gt.1) cpuTot1(l) = cpuTot1(l) + cBound
      enddo

      write (iout,*) 'estimated cpu time  per step =',cpuTot1(nv)


      cpuCore1 = cpuTot1(nv) / ncore
      i = 0
      i2 = 0
      do l = 1,nv
      corev(l) = i
        if (cpuTot1(l).ge.cpuCore1) then
        i1 = i2 + 1
        i2 = l
        nv1(i) = i1
        nv2(i) = min(i2,nv)
        write (iout,*) 'i=',i,' i1=',i1,' i2=',i2,' sizev=',i2-i1+1, ' cpuCore1=',cpuCore1
        cpuCore1 = cpuCore1 + cpuTot1(nv) / ncore
        i = i+1
        endif
      enddo

      if (i.eq.ncore-1) then
      i1 = i2 + 1
      i2 = nv
      nv1(i) = i1
      nv2(i) = i2
      write (iout,*) 'i=',i,' i1=',i1,' i2=',i2,' sizev=',i2-i1+1
      endif

/*
      nvcore = nv/ncore+1
c assign corev
      i2 = 0
        do i = 0,ncore-1
        i1 = i2 + 1
        i2 = min(i2+nvcore,nv)
        nv1(i) = i1
        nv2(i) = i2
        corev(i1:i2) = i
        write (iout,*) 'i=',i,' i1=',i1,' i2=',i2
        enddo
*/
c------------------------------------------------------
      maps = workdir(1:lw)//bs//'corev'//mapext
      val = mv
      val(kl) = corev
      call OUTGDAL (maps, nvx,nvy,4
     &           , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)
c------------------------------------------------------
c assign corej, nj1, nj2
        nj1(0) = 1
        do 100 k = 0,ncore-2
          do l = nj1(k), nj
          m = jtype(l)
            if (m.eq.1) cycle
          pr = jtor(l)
            if (pr.eq.nv2(k)) then
            nj2(k) = l
            nj1(k+1) = l+1
            goto 100
            endif
          enddo
100     continue
       nj2(ncore-1) = nj

c------------------------------------------------------
c assign nv3,nv4 for junction
      nv3 = nv
      nv4 = 0

       do k = 0,ncore-1
       do l = nj1(k),nj2(k)
       pl = jtol(l)
       pr = jtor(l)
       maskl = pl.gt.0
       maskr = pr.gt.0
         if (maskl.and.maskr) then
         nv3(k) = min(nv3(k),pl,pr)
         nv4(k) = max(nv4(k),pl,pr)
         else if (maskl) then
         nv3(k) = min(nv3(k),pl)
         nv4(k) = max(nv4(k),pl)
         else if (maskr) then
         nv3(k) = min(nv3(k),pr)
         nv4(k) = max(nv4(k),pr)
         endif
       enddo
      enddo
c------------------------------------------------------
c  re-assign nv3,nv4  for slopes

      do l = 1,nv
      k = corev(l)
        do m = 1,2
        pl = vtol(m,l)
        pr = vtor(m,l)
        maskl = pl.gt.0
        maskr = pr.gt.0
          if (maskl.and.maskr) then
          nv3(k) = min(nv3(k),pl,pr)
          nv4(k) = max(nv4(k),pl,pr)
          endif
        enddo
      enddo
c------------------------------------------------------
c count volume & junction for each core
        do i = 0,ncore-1
        nvv(i) = count(corev.eq.i)
        nvvL(i) = nv4(i)-nv3(i)+1
        njj(i) = nj2(i)-nj1(i)+1
        enddo
c------------------------------------------------------
      endif ! end MASTER section

c------------------------------------------------------
      write (chcore,'(i3)') icore
      call fillzero (chcore,3)

      call V_integer4_master_to_slave (nvv(0), ncore, tag)
      call V_integer4_master_to_slave (nvvL(0), ncore, tag)
      call V_integer4_master_to_slave (njj(0), ncore, tag)
      call V_integer4_master_to_slave (nv1(0), ncore, tag)
      call V_integer4_master_to_slave (nv2(0), ncore, tag)
      call V_integer4_master_to_slave (nj1(0), ncore, tag)
      call V_integer4_master_to_slave (nj2(0), ncore, tag)
      call V_integer4_master_to_slave (nv3(0), ncore, tag)
      call V_integer4_master_to_slave (nv4(0), ncore, tag)

      call V_integer4_master_to_slave (corev, nv, tag)

      call S_integer4_master_to_slave (nval,  tag)
      call S_integer4_master_to_slave (nvx,  tag)
      call S_integer4_master_to_slave (nvy,  tag)

      call S_real8_master_to_slave (xstart,  tag)
      call S_real8_master_to_slave (ystart,  tag)
      call S_real8_master_to_slave (cellsize,  tag)

      call S_integer4_master_to_slave (imap,  tag)
      call S_real8_master_to_slave (time,tag)
      call S_real8_master_to_slave (timestart,tag)
c-------------------------------------------------
        if (SLAVE) call parmap (.false.)
      write(iout, '(a,i2,a,g12.6,4(a,i6))') 'icore=',icore,' cellsize=',cellsize,' nval=',nval,' nvx=',nvx,' nvy=',nvy,' nv=',nv
c------------------------------------------------------
      iv1 = nv1(icore)
      iv2 = nv2(icore)
      ij1 = nj1(icore)
      ij2 = nj2(icore)
      ivL1 = nv3(icore)
      ivL2 = nv4(icore)
      sizj = ij2 - ij1 + 1
c------------------------------------------------------
      tag = 1

      if (SLAVE) allocate (lij(0:nvx+1,0:nvy+1),lk(nval),kl(nv),vtype(0:nv)
     &         ,lev(0:nv),vtol(2,0:nv),vtor(2,0:nv))

      call V_integer4_master_to_slave (lij(0,0),(nvx+2)*(nvy+2),tag)
      call V_integer4_master_to_slave (lk,nval,tag)
      call V_integer4_master_to_slave (kl,nv,tag)
      call V_integer4_master_to_slave (vtype(0),nv+1,tag)
      call V_integer4_master_to_slave (lev(0),nv+1,tag)
      call V_integer4_master_to_slave (vtol(1,0),2*(nv+1),tag)
      call V_integer4_master_to_slave (vtor(1,0),2*(nv+1),tag)

      if (SLAVE) allocate (jtol(ij1:ij2),jtor(ij1:ij2),jtype(ij1:ij2))
      call PJ_integer4_master_to_slave (jtol,1,tag)
      call PJ_integer4_master_to_slave (jtor,1,tag)
      call PJ_integer4_master_to_slave (jtype,1,tag)


c------------------------------------------------------
      LEFT  = icore.gt.0
      RIGHT = icore.lt.ncore-1

      if (LEFT) then
      pf(1) = 0
      pf(2) = nv1(icore)   - nv3(icore)
      endif

      if (RIGHT) then
      pf(3) = nv3(icore+1) - nv3(icore)
      pf(4) = nv1(icore+1) - nv3(icore)
      endif


      if (LEFT) then
      sc(1) = nv1(icore)   - nv3(icore)
      sc(2) = nv4(icore-1) - nv2(icore-1) 
      else
      sc(1) = 0
      sc(2) = 0
      endif

      if (RIGHT) then
      sc(3) = nv1(icore+1) - nv3(icore+1)
      sc(4) = nv4(icore)   - nv2(icore)
      else
      sc(3) = 0
      sc(4) = 0
      endif
c------------------------------------------------------
      sizb = maxval(sc)
      nvmax = max(sizb,1000)

      allocate (my_dval(nvmax*4))
      allocate (my_sval(nvmax*2))
      allocate (my_ival(nvmax))
      allocate (my_lval1(nvmax))
      allocate (my_ival1(nvmax))


      tag = 1
      
      sizbmax = sizb

      call mpi_barrier (mpi_comm_world, mpi_erro)

      call S_integer4_slave_to_master    (sizbmax, tag,Fmax)
      write (iout,*) 'icore=',icore,' sizb=',sizb,' nvmax=',nvmax,' sizbmax=',sizbmax
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      if (MASTER) then
      write (iout,*) '-----------------------------------------------------------'
      write (iout,'(13("	",a8))') '     nvv','    nvvL','     nv1','     nv2','     nv3','     nv4'
     &              ,'     njj','     nj1','     nj2','    sizL','    sizR','    LEFT','    RIGHT'
      do i = 0,ncore-1
      write (iout,'(11("	",i8),2("	",l8))') nvv(i),nvvL(i), nv1(i), nv2(i), nv3(i), nv4(i), njj(i), nj1(i), nj2(i)
     &               , nv1(i)-nv3(i), nv4(i)-nv2(i),i.gt.0,i.lt.ncore-1
      enddo
      write (iout,*) '-----------------------------------------------------------'
      endif
c------------------------------------------------------
      return
      end
