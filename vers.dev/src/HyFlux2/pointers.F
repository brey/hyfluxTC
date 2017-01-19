
c============================================================================
        subroutine pointers
c============================================================================
      use geometry_data
      use parameter_data
      use hydro_data
      use slope_data
      implicit none

      integer(i4) i,j, k,l,m, pl, pr,  il0,ir0, jl0,jr0, jsl, jsr, isl, isr
      logical maskl, maskr
c--------------------------

c hv, ip, jp, np

        nv = count(mask)

        allocate (il(nv),jl(nv),kl(nv),lij(0:nvx+1,0:nvy+1),lk(nval))
        allocate (xv(nv),yv(nv),vtype(0:nv),lev(0:nv))

        lk = 0; kl=0; lev=0; vtype=0

        lij = 0  ! for gost and MV cell

        l =0; k=0     
        do j = 1,nvy
          do i = 1,nvx
          k = k+1
            if (mask(i,j)) then
            l = l+1
            vtype(l) = 1
            il(l) = i
            jl(l) = j
            kl(l) = k
            lij(i,j) = l
            lk(k)  = l
            xv(l) = (float(i)-0.5)*cellsizeN
            yv(l) = (float(j)-0.5)*cellsizeN
            endif
          enddo
        enddo

c---------------------------------------------------------------------------------- 
c vtol, vtor
       allocate (vtol(2,0:nv),vtor(2,0:nv))
       vtol = 0; vtor=0

       m=1
        do l = 1,nv
        i = il(l)
        j = jl(l)
        il0 = i-1
        ir0 = i+1
        vtol(m,l) = lij(il0,j)
        vtor(m,l) = lij(ir0,j)
        enddo
 
       m=2
        do l = 1,nv
        i = il(l)
        j = jl(l)
        jl0 = j+1
        jr0 = j-1
        vtol(m,l) = lij(i,jl0)
        vtor(m,l) = lij(i,jr0)
        enddo
        write(iout,*)' Done vtol, vtor'

c-----------------------------------------------------
C evaluate the no. of junctions
 
      l = 0
 
      do j = 0,nvy
        do i = 0,nvx
c vertical junction
          if (j.gt.0) then
          il0 = i
          ir0 = i+1
          pl = lij(il0,j)
          pr = lij(ir0,j)
          maskl = pl.gt.0
          maskr = pr.gt.0
            if (maskl.or.maskr) l = l+1
          endif

c horizontal junction
          if (i.gt.0) then
          jl0 = j+1
          jr0 = j
          pl = lij(i,jl0)
          pr = lij(i,jr0)
          maskl = pl.gt.0
          maskr = pr.gt.0
            if (maskl.or.maskr) l = l+1
          endif
        enddo
      enddo
 
      nj = l
      write (iout,*) 'found ',nj,' junction'
 
c-----------------------------------------------------
      allocate (jtol(nj),jtor(nj),jtype(nj),xj(nj),yj(nj))
 
      jtol = 0
      jtor = 0
 
      l = 0
 
      do j = 0,nvy
        do i = 0,nvx
c vertical junction, x-direction
          if (j.gt.0) then
          m = 1
          il0 = i
          ir0 = i+1
          pl = lij(il0,j)
          pr = lij(ir0,j)
          maskl = pl.gt.0
          maskr = pr.gt.0
            if (maskl.or.maskr) then
            l = l+1
            jtype(l) = m
            xj(l) = i*cellsizeN
            yj(l) = (j-0.5)*cellsizeN
              if (maskl) jtol(l) = pl
              if (maskr) jtor(l) = pr
            endif
          endif

c horizontal junction, y-direction 
          if (i.gt.0) then
          m = 2
          jl0 = j+1
          jr0 = j
          pl = lij(i,jl0)
          pr = lij(i,jr0)
          maskl = pl.gt.0
          maskr = pr.gt.0
            if (maskl.or.maskr) then
            l = l+1
            jtype(l) = m
            xj(l) = (i-0.5)*cellsizeN
            yj(l) = j*cellsizeN
              if (maskl) jtol(l) = pl
              if (maskr) jtor(l) = pr
            endif
          endif
        enddo
      enddo
 
      return
      end
c============================================================================
        subroutine pointVol
c============================================================================
      use geometry_data
      use parameter_data
      use hydro_data
      use slope_data
      implicit none

      integer(i4) i,j,is,js, k,l,l0, m,n,ln, near(8), pl, pr, xy2ind, il0,ir0, jl0,jr0, x,y
     &           ,p1, p2, dlev, jsl, jsr, isl, isr, levl,levr, lev0, kx, ky
!      integer(i4), target :: lev0
      real(r4) hvmax, logBS, split, zmin, slope
      real(r4), allocatable :: dtloc(:)

      logical changed

      logBS = log(baseS)

c--------------------------
c  define c_lev based on water depth
c--------------------------

        nv0 = nv

        hvmax = maxval(hv)
        write (iout,*) 'hvmax=',hvmax,' hvmin=',minval(hv)

c c_lev from water depth

        allocate (c_lev(0:nv0),dtloc(nv0))
     
        c_lev = 0
        l = 0

        do l0 = 1,nv0 
          if (sumslope) then
          slope =   (abs(dzdk(1,l0))+abs(dzdk(2,l0)))
          else
          slope = max(abs(dzdk(1,l0)),abs(dzdk(2,l0)))
          endif
        zmin = zv(l0)-cellsize*slope/2
        hv(l0) = max(seaLevel-zmin,hmin)
        split = sqrt(g*hvmax)/(sqrt(g*hv(l0))+vmaxShore)
        split = max(split,1.) 
        lev0 = min(maxlev,int(log(split)/logBS) )
*          if (case(l0).gt.0) lev0 = min(lev0+1,maxlev)
          if (ms(lev0).gt.split) lev0 = max(lev0-1,0)
        dtloc(l0) = courmax*cellsizeM/(Ms(lev0)*(sqrt(g*hv(l0))+vmaxShore))

        c_lev(l0) = lev0
        l = l+Ms(lev0)*Ms(lev0)
        enddo

        write (iout,*) 'from zmin: hvmax=',maxval(hv),' hvmin=',minval(hv)

        nv = l
        write(iout,*) 'before regularization'
        write(iout,*) 'nv0=',nv0,' nv=',nv,' nv/nv0 =',float(nv)/float(nv0),' min dt=',minval(dtloc),' max dt=',maxval(dtloc)
        write(iout,*) 'calc. maxlev=',maxval(c_lev(1:nv0)),' min cellsize [m]=',cellsizeM/maxval(Ms)

c---------------------------------------------------------------------------------------
c regularize

        if (regularize.ne.0) then
        changed = .true.
        
        do while (changed)
        changed = .false.
        l = 0

        do l0 = 1,nv0 
        call nearl(l0,near,n)
*        write (iout,*) 'near=',(near(k),k=1,n)
          do k = 1,n
          ln = near(k)
            if (c_lev(ln).lt.(c_lev(l0)-1)) then
            l = l+1
              if (c_lev(l0).gt.1.and.regularize.lt.0) then
              c_lev(l0) = c_lev(l0)-1 ! more coarse
              else
              c_lev(ln) = c_lev(ln)+1 ! more refined
              endif
            changed = .true.
            endif
          enddo     
        enddo

        write (iout,*) 'regularized coarse cells no. = ',l
        enddo
        endif

c---------------------------------------------------------------------------------------
c count the total cell and allocate index
        allocate (c_ind(nv0))

        l = 0

        do l0 = 1,nv0 
        lev0 = c_lev(l0)
        allocate (c_ind(l0)%f_ind(Ms(lev0),Ms(lev0)))
          do js=1,Ms(lev0)
            do is=1,Ms(lev0)
            l=l+1
            c_ind(l0)%f_ind(is,js) = l
            enddo
          enddo
        dtloc(l0) = courmax*cellsizeM/(Ms(lev0)*(sqrt(g*hv(l0))+vmaxShore))
        enddo

        nv = l

        if (regularize.ne.0) then
        write (iout,*) 'after regularization'
        write(iout,*) 'nv0=',nv0,' nv=',nv,' nv/nv0 =',float(nv)/float(nv0),' min dt=',minval(dtloc),' max dt=',maxval(dtloc)
        write(iout,*) 'calc. maxlev=',maxval(c_lev(1:nv0)),' min cellsize [m]=',cellsizeM/maxval(Ms)
        endif
c----------------------------------------------------------------------------------
        if (allocated(val)) deallocate(val)
        nval = nvx*nvy
        allocate (val(nval))
c----------------------------------------------------------------------------------
        val = mv
        val(kl) = c_lev(1:nv0)
        maps = workdir(1:lw)//bs//'c_lev'//mapext
	call OUTGDAL (maps, nvx,nvy,4, xstart, ystart, cellsize,val, nval
     &               ,sizey,mapfmt,nband,error)
        write(iout,*)' Done c_lev'

        val = mv
        val(kl) = dtloc
        maps = workdir(1:lw)//bs//'dtloc'//mapext
	call OUTGDAL (maps, nvx,nvy,4, xstart, ystart, cellsize,val, nval
     &               ,sizey,mapfmt,nband,error)
        write(iout,*)' Done dtloc'
c----------------------------------------------------------------------------------
        deallocate (dtloc)
c----------------------------------------------------------------------------------
c xv,yv, vtype
        allocate (xv(nv),yv(nv),vtype(0:nv),lev(0:nv))
       vtype = 0
       lev = -1  

        do l0 = 1,nv0
        x = (il(l0)-1)*cellsizeN
        y = (jl(l0)-1)*cellsizeN
        lev0 = c_lev(l0)
          do js = 1,Ms(lev0)
            do is = 1,Ms(lev0)
            l = c_ind(l0)%f_ind(is,js)
            xv(l) = x + (2*is-1)*nsize(lev0)
            yv(l) = y + (2*js-1)*nsize(lev0)
            lev(l) = lev0
            vtype(l) = 1
            enddo
          enddo
        enddo
        write(iout,*)' Done xv,yv'
c---------------------------------------------------------------------------------- 
        allocate (qlev(0:maxlev))

        do lev0 = 0,maxlev
        ql => qlev(lev0)
        ql%cellsize = 2*dsize(lev0)
        ql%nvx = nvx*ms(lev0) 
        ql%nvy = nvy*ms(lev0) 
        ql%nval = ql%nvx * ql%nvy
        ql%nv = count(lev.eq.lev0)
        ql%l = 0
        ql%k = 0
        allocate (ql%ll(ql%nv),ql%kl(ql%nv))
        write (iout,*) 'lev0=',lev0,' ql%nv = ',ql%nv
        enddo
c---------------------------------------------------------------------------------- 
        lev(0) = 0
c---------------------------------------------------------------------------------- 

        do l0 = 1,nv0
        lev0 = c_lev(l0)
        i = il(l0) 
        j = jl(l0)
        ql => qlev(lev0)
          do js = 1,Ms(lev0)
            do is = 1,Ms(lev0)
            l = c_ind(l0)%f_ind(is,js)
            kx = (i-1)*ms(lev0)+is
            ky = (j-1)*ms(lev0)+js
            k = (ky-1)*ql%nvx + kx
            ql%k = k
            ql%l = ql%l + 1
            m = ql%l
            ql%ll(m) = l
            ql%kl(m) = k
            enddo
          enddo
*        write (iout,'(6(a,i8))') 'l0=',l0,' lev0=',lev0,' m=',m,' k=',k,' l=',l
        enddo
c---------------------------------------------------------------------------------- 
        do lev0 = 0,maxlev
        ql => qlev(lev0)
        write(iout,'(5(a,i10))') 'lev=',lev0,' ql%nv=',ql%nv,' ql%l=',ql%l,' ql%nval=',ql%nval,' ql%k=',ql%k
        enddo
c---------------------------------------------------------------------------------- 
c vtol0, vtor0
       allocate (vtol0(2,0:nv0),vtor0(2,0:nv0))
       vtol0 = 0 ; vtor0 = 0

       m=1
        do l0 = 1,nv0
        i = il(l0)
        j = jl(l0)
        il0 = i-1
        ir0 = i+1
        vtol0(m,l0) = lij(il0,j)
        vtor0(m,l0) = lij(ir0,j)
        enddo
 
       m=2
        do l0 = 1,nv0
        i = il(l0)
        j = jl(l0)
        jl0 = j-1
        jr0 = j+1
        vtol0(m,l0) = lij(i,jl0)
        vtor0(m,l0) = lij(i,jr0)
        enddo
        write(iout,*)' Done vtol0, vtor0'

c----------------------------------------------------------------------------------
       allocate (vtol(2,0:nv),vtor(2,0:nv))
       vtol = 0
       vtor = 0 

c vtol, vtor  horizontal direction (left=E, right=W)

       m=1

        do l0 = 1,nv0
        pl = vtol0(m,l0)
        pr = vtor0(m,l0)  
        lev0 = c_lev(l0)
        levl = c_lev(pl)
        levr = c_lev(pr)

          do js = 1,Ms(lev0)

          is = 1

          l = c_ind(l0)%f_ind(is,js)
            if (Ms(lev0).gt.1) vtor(m,l) = l+1

          dlev = levl-lev0

          p1 = 0 ; p2 = 0

c using xy2ind
            if (pl.gt.0) then
            x=xv(l)-nsize(lev0)-nsize(levl)
            y=yv(l)
              if (dlev.le.0) then
              p1 = xy2ind(x,y)
              else if (dlev.gt.0) then
              y=y-nsize(lev0)+nsize(levl)
              p1 = -xy2ind(x,y)  
              endif            
            endif

c using index calc
            if (pl.gt.0) then
            jsl = (js-1)*baseS**dlev+1
            p2 = c_ind(pl)%f_ind(ms(levl),jsl)
              if (dlev.gt.0) p2=-p2
            endif
            
            
            if (p1.ne.p2) then
            write (iout,*) 'Error in evaluating left pointer: p1=',p1,' p2=',p2 
            write (iout,*) 'l0=',l0,' is=',is,' js=',js,' jsl=',jsl
            write (iout,*) 'pl=',pl,' l0=',l0,' pr=',pr
            write (iout,*) 'msl=',ms(levl),' ms=',Ms(lev0),' msr=',ms(levr),' dlev=',dlev
            write (iout,*) 'l=',l,' xv(l)=',xv(l),' yv(l)=',yv(l),' x=',x,' y=',y
            stop
            else 
            vtol(m,l) = p1
            endif

c-------------------------------------
            do is = 2,Ms(lev0)-1
            l = c_ind(l0)%f_ind(is,js)
            vtol(m,l) = l-1
            vtor(m,l) = l+1
            enddo
c-------------------------------------

          is = Ms(lev0)

          l = c_ind(l0)%f_ind(is,js)
            if (Ms(lev0).gt.1) vtol(m,l) = l-1

          dlev = levr-lev0
          p1 = 0 ; p2 = 0

c using xy2ind
            if (pr.gt.0) then
            x=xv(l)+nsize(lev0)+nsize(levr)
            y=yv(l)
              if (dlev.le.0) then
              p1 = xy2ind(x,y)
              else if (dlev.gt.0) then
              y=y-nsize(lev0)+nsize(levr)
              p1 = -xy2ind(x,y)
              endif
            endif

c using index calc
            if (pr.gt.0) then
            jsr = (js-1)*baseS**dlev+1
            p2 = c_ind(pr)%f_ind(1,jsr)
              if (dlev.gt.0) p2=-p2
            endif
            
            if (p1.ne.p2) then
            write (iout,*) 'Error in evaluating right pointer: p1=',p1,' p2=',p2 
            write (iout,*) 'l0=',l0,' is=',is,' js=',js,' jsr=',jsr
            write (iout,*) 'pl=',pl,' l0=',l0,' pr=',pr
            write (iout,*) 'msl=',ms(levl),' ms=',Ms(lev0),' msr=',ms(levr),' dlev=',dlev
            write (iout,*) 'l=',l,' xv(l)=',xv(l),' yv(l)=',yv(l),' x=',x,' y=',y
            stop
            else
            vtor(m,l) = p1
            endif

          enddo
        enddo
         write(iout,*)' Done vtol, vtor,  horizontal direction '

c----------------------------------------------------------------------------------
c vtol, vtor  vertical direction (left=N, right=S)

       m=2

        do l0 = 1,nv0
        pl = vtol0(m,l0)
        pr = vtor0(m,l0)  
        lev0 = c_lev(l0)
        levl = c_lev(pl)
        levr = c_lev(pr)

          do is = 1,Ms(lev0)
          js = 1

          l = c_ind(l0)%f_ind(is,js)
            if (Ms(lev0).gt.1) vtor(m,l) = l+Ms(lev0)

          dlev = levl-lev0
          p1 = 0 ; p2 = 0

c using xy2ind
            if (pl.gt.0) then
            y=yv(l)-nsize(lev0)-nsize(levl)
            x=xv(l)
              if (dlev.le.0) then
              p1 = xy2ind(x,y)
              else if (dlev.gt.0) then
              x=x-nsize(lev0)+nsize(levl)
              p1 = -xy2ind(x,y)  
              endif            
            endif

c using index calc
            if (pl.gt.0) then
            isl = (is-1)*baseS**dlev+1
            p2 = c_ind(pl)%f_ind(isl,ms(levl))
              if (dlev.gt.0) p2=-p2
            endif
            
            if (p1.ne.p2) then
            write (iout,*) 'Error in evaluating left pointer: p1=',p1,' p2=',p2 
            write (iout,*) 'l0=',l0,' is=',is,' js=',js,' isl=',isl
            write (iout,*) 'pl=',pl,' l0=',l0,' pr=',pr
            write (iout,*) 'msl=',ms(levl),' ms=',Ms(lev0),' msr=',ms(levr),' dlev=',dlev
            write (iout,*) 'l=',l,' xv(l)=',xv(l),' yv(l)=',yv(l),' x=',x,' y=',y
            stop
            else
            vtol(m,l) = p1
            endif

c-------------------------------------
            do js = 2,Ms(lev0)-1
            l = c_ind(l0)%f_ind(is,js)
            vtol(m,l) = l-Ms(lev0)
            vtor(m,l) = l+Ms(lev0)
            enddo
c-------------------------------------
          js = Ms(lev0)

          l = c_ind(l0)%f_ind(is,js)
            if (Ms(lev0).gt.1) vtol(m,l) = l-Ms(lev0)

          dlev = levr-lev0
          p1 = 0 ; p2 = 0

c using xy2ind
            if (pr.gt.0) then
            y=yv(l)+nsize(lev0)+nsize(levr)
            x=xv(l)
              if (dlev.le.0) then
              p1 = xy2ind(x,y)
              else if (dlev.gt.0) then
              x=x-nsize(lev0)+nsize(levr)
              p1 = -xy2ind(x,y)
              endif
            endif

c using index calc
            if (pr.gt.0) then
            isr = (is-1)*baseS**dlev+1
            p2 = c_ind(pr)%f_ind(isr,1)
              if (dlev.gt.0) p2=-p2
            endif
            
            if (p1.ne.p2) then
            write (iout,*) 'Error in evaluating right pointer: p1=',p1,' p2=',p2 
            write (iout,*) 'l0=',l0,' is=',is,' js=',js,' isr=',isr
            write (iout,*) 'pl=',pl,' l0=',l0,' pr=',pr
            write (iout,*) 'msl=',ms(levl),' ms=',Ms(lev0),' msr=',ms(levr),' dlev=',dlev
            write (iout,*) 'l=',l,' xv(l)=',xv(l),' yv(l)=',yv(l),' x=',x,' y=',y
            stop
            else
            vtor(m,l) = p1
            endif

          enddo
        enddo

         write(iout,*)' Done vtol, vtor,  vertical direction '

        n = 0 ; m = 0

        if (maxlev.eq.0) then

        do l0 = 1,nv0
        l = c_ind(l0)%f_ind(1,1)
        n = n + count(vtol(1:2,l).ne.vtol0(1:2,l0))
        m = m + count(vtor(1:2,l).ne.vtor0(1:2,l0))
        enddo

        write (iout,*) 'count(vtol0.ne.vtol)=',n
        write (iout,*) 'count(vtor0.ne.vtor)=',m

        endif
c-------------------------------------

        return
        end

c============================================================================
      subroutine pointJun
c============================================================================
      use geometry_data
      use parameter_data
      use slope_data
      implicit none

      integer*1, allocatable :: typeP(:,:)  
      integer(i4) i,j,k,l,m,n, is,js, l0, lj, nvx1, nvy1, nsizeLoc, dxj(4),dyj(4), dxn(4),dyn(4),xx,yy,lev0, levl, levr
c                W, E, N, S
      data dxj /-1, 1, 0, 0/
      data dyj / 0, 0, -1,1/

      data dxn / 1, 1,-1,-1/
      data dyn /-1, 1, 1,-1/
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c  pointers jtol, jtor
c  jtype
c   1 vertical
c   2 horizontal
c-----------------------------------------------------
      nvx1 = nvx*cellsizeN
      nvy1 = nvy*cellsizeN
      allocate (typeP(0:nvx1,0:nvy1))
      typeP = 0
c-----------------------------------------------------
        do l0 = 1,nv0
        lev0 = c_lev(l0)
        nsizeLoc = nsize(lev0)
          do js = 1,Ms(lev0)
            do is = 1,Ms(lev0)
            l = c_ind(l0)%f_ind(is,js)
C define cell center
            xx = xv(l)
            yy = yv(l)
            typeP(xx,yy) = -1
C define cell nodes
               do k = 1,4
               xx = xv(l) + dxn(k)*nsizeLoc
               yy = yv(l) + dyn(k)*nsizeLoc
               typeP(xx,yy) = -2
               enddo
            enddo
          enddo
        enddo
c-----------------------------------------------------
C evaluate the no. of junctions
      nj = 0
        do l0 = 1,nv0
        lev0 = c_lev(l0)
        nsizeLoc = nsize(lev0)
          do js = 1,Ms(lev0)
            do is = 1,Ms(lev0)
            l = c_ind(l0)%f_ind(is,js)
               do k = 1,4
               xx = xv(l) + dxj(k)*nsizeLoc
               yy = yv(l) + dyj(k)*nsizeLoc
                 if (typeP(xx,yy).eq.0) then
                 nj = nj+1
                 typeP(xx,yy) = k
                 endif
               enddo
            enddo
          enddo
        enddo            

      write (iout,*) 'found ',nj,' junction'
c-----------------------------------------------------
C define junctions pointers
      allocate (jtol(nj),jtor(nj),jtype(nj),xj(nj),yj(nj))
      jtype = 0

      lj = 0
        do l0 = 1,nv0
        lev0 = c_lev(l0)
        nsizeLoc = nsize(lev0)
          do js = 1,Ms(lev0)
            do is = 1,Ms(lev0)
            l = c_ind(l0)%f_ind(is,js)
               do k = 1,4
               xx = xv(l) + dxj(k)*nsizeLoc
               yy = yv(l) + dyj(k)*nsizeLoc
                 if (typeP(xx,yy).gt.0) then
                 lj = lj+1
                   if (lj.gt.nj) then
                   write (iout,*) 'pointJun: lj > nj for l=',l,' nv=',nv
                   stop
                   endif
                 xj(lj) = xx
                 yj(lj) = yy
                    if (k.eq.1) then
                    jtype(lj) = 1
                    jtol(lj) = vtol(1,l)
                    jtor(lj) = l
                    else if (k.eq.2) then 
                    jtype(lj) = 1
                    jtol(lj) = l
                    jtor(lj) = vtor(1,l)

                    else if (k.eq.3) then 
                    jtype(lj) = 2
                    jtol(lj) = vtol(2,l)
                    jtor(lj) = l
                    else if (k.eq.4) then 
                    jtype(lj) = 2
                    jtol(lj) = l
                    jtor(lj) = vtor(2,l)
                    endif                 
                 typeP(xx,yy) = 0
                 endif
               enddo
            enddo
          enddo
        enddo  

        write (iout,*)' pointJun'
        write (iout,*)'count(jtype.eq.0)=',count(jtype.eq.0),' count(jtype.eq.1)=',count(jtype.eq.1)
     &                ,' count(jtype.eq.2)=',count(jtype.eq.2)
        write (iout,*)'count(jtol.lt.0)=',count(jtol.lt.0),' count(jtol.eq.0)=',count(jtol.eq.0)
        write (iout,*)'count(jtor.lt.0)=',count(jtor.lt.0),' count(jtor.eq.0)=',count(jtor.eq.0)
        
        deallocate (typeP)

        return
        end

c============================================================================
       subroutine nearL (l,near,k)
c============================================================================
      use geometry_data
      use parameter_data
      implicit none

      integer(i4) i,j,k,l, ii, jj, near(8)
      k=0
          do ii=-1,1
            i = il(l)+ii
              if (i.lt.1.or.i.gt.nvx) cycle
            do jj=-1,1
              if (ii.eq.jj) cycle
            j = jl(l)+jj
              if (j.lt.1.or.j.gt.nvy) cycle
              if (lij(i,j).eq.0) cycle
            k = k+1
            near(k) =lij(i,j) 
            enddo                    
          enddo
       return
       end

c============================================================================
      function xy2ind(x,y)
c============================================================================
      use constants
      use geometry_data
      implicit none

      integer(i4) xy2ind, i, j, is,js,l0, x,y, lev0

      i = x/cellsizeN + 1
      j = y/cellsizeN + 1
      l0 = lij(i,j)
        if (l0.eq.0) then
        write(iout,*) 'xy2ind: lij(i,j)=0 for i=',i,' j=',j 
        stop
        endif
      lev0 = c_lev(l0)
      is = mod(x,cellsizeN)/nsize(lev0)/2 + 1
      js = mod(y,cellsizeN)/nsize(lev0)/2 + 1
*      write(iout,*) 'is=',is,' js=',js,' lfrom=',lfrom,' l0=',l0
      xy2ind = c_ind(l0)%f_ind(is,js)

      return
      end
