#define DEB c
#define DEC c  
c============================================================================
        subroutine initWatch
c============================================================================
        use watch_data
        use parameter_data
        use statistics_data
        use geometry_data
        use hydro_data
        use parallel_data
        use map_data
        implicit none
c-------------------------------------------
        integer(i4) errtab, i,j,k,l, m,length, ncolumn, err, irec
        real(r4), allocatable :: mat(:,:)
        real(r4) lat1,long1,x,y,value
        real(r4) x0,y0, x1,y1
        logical existFile, eqmv, exist, found
c-------------------------------------------
        integer nf
        parameter (nf=4)
        character field(nf)*64
        field = (/'idPlace ','latNear ','longNear','cellNear'/) 
c-------------------------------------------
        call opentab (iinp,watchfile,ncolumn,nplace,errtab)
        watch = errtab.eq.0
          if (.not.watch) return
        write (iout,*) '1. Found globally nplace=',nplace

        allocate (mat(nf,nplace))
        call getcoltab (iinp,'latNear ','longNear',field,mat,nf,nplace,errtab)
        rewind (iinp)
        watch = errtab.eq.0
          if (.not.watch) then
          deallocate (mat)
          return
          endif

        allocate (idW(nplace),latW(nplace),longW(nplace),cellW(nplace)
     &            ,namePlace(nplace),filePlace(nplace),lpW(nplace))

        call getcoltab_char (iinp,'latNear ','longNear','$namePlace',namePlace,nplace,errtab)
        close (iinp)

        idW   = mat(1,1:nplace)
        latW  = mat(2,1:nplace)
        longW = mat(3,1:nplace)
        cellW = mat(4,1:nplace)

        deallocate (mat)

        k = 0
        do m = 1,nplace
        l = lk(cellW(m))
#ifdef PARALLEL
          if (.not.postProc.and.corev(l).ne.icore) cycle
#endif
        k = k + 1
        lpW(k) = l
        idW(k) = idW(m)
        latW(k) = latW(m)
        longW(k) = longW(m)
        cellW(k) = cellW(m)
        namePlace(k) = namePlace(m)
        filePlace(k) = watchdir(1:lwatch)//bs//namePlace(m)(1:length(namePlace(m)))//'.txt'
        enddo

         nplace = k

        write (iout,*) 'Found ',nplace,' places in core ',icore


        allocate (dxW(nplace),dyW(nplace),iunW(nplace),karr(nplace),arrived(nplace)
     &           ,tarrW(nplace),zmaxW(nplace),tzmaxW(nplace),vmaxW(nplace))

        arrived = .false.
        narrived = 0
        vmaxW = 0
        zmaxW = 0

        do k = 1,nplace
        l = lpW(k)
        x = longW(k)
        y = latW(k)

        j = cellW(k)/nvx + 1
        i = cellW(k)-(j-1)*nvx
        x0 = xstart + (i-0.5)*cellsize
        y0 = ystart - (j-0.5)*cellsize
        dxW(k) = x-x0
        dyW(k) = y-y0
        enddo

        if (nplace.gt.0) then
        write (iout,*) 'mindx =',minval(dxW),' maxdx =',maxval(dxW)
        write (iout,*) 'mindy =',minval(dyW),' maxdy =',maxval(dyW)
        endif
c-------------------------------------------------
        if (imap.eq.0) return
c-------------------------------------------------
        do k = 1,nplace
        l = lpW(k)
          if (eqmv(tarr(l))) cycle
          if (eqmv(zwmax(l))) cycle
          if (eqmv(tzwmax(l))) cycle
          if (abs(zwmax(l)-zwvIn(l)).lt.harr) cycle

          narrived = narrived + 1
*          write (iout,'(a,i6,a,i4,a,g12.5)') 'wave arrived at location ',idW(k),' order ',narrived,' at time ',tarr(l)
          iunW(k) = iwatch+narrived
          karr(narrived) = k
          call openfile (iunW(k),filePlace(k),'time',found)
            if (.not.found.or.imap.eq.0) then
            write (iunW(k),'(a,2(a,a))') 'time',tab,'zsurf',tab,'v'
            else
            endfile (iunW(k))
            endif
          close (iunW(k))
          arrived(k) = .true.
          tarrW(k) = tarr(l)
          zmaxW(k) = zwmax(l)
          tzmaxW(k) = tzwmax(l)
        enddo
c-------------------------------------------------
        if (narrived.eq.0) return
c-------------------------------------------------

#ifdef PARALLEL
        if (POST) then
        open (iwatch,file=watchdir(1:lwatch)//bs//'statistics.txt')
        else if (CALCWATCH) then
        open (iwatch,file=watchdir(1:lwatch)//bs//chcore//'statistics.txt')
        endif
#else
        open (iwatch,file=watchdir(1:lwatch)//bs//'statistics.txt')
#endif
        write (iwatch,'(a,5(a,a12))') 'idPlace',tab,'tarr',tab,'zmax',tab,'tzmax',tab,'vmax'
     &              ,tab,'$namePlace'

        do j = 1,narrived
        k = karr(j)
        l = lpW(k)
        write (iwatch,'(i6,4(a,f12.3),a,a)') 
     &           idW(k),tab, tarrW(k), tab,zmaxW(k), tab, tzmaxW(k), tab, vmaxW(k)
     &          ,tab,namePlace(k)(1:length(namePlace(k)))
        enddo

        close (iwatch)

        return
        end
c============================================================================
        subroutine putWatch 
c============================================================================
        use watch_data
        use parameter_data
        use statistics_data
        use hydro_data
        use geometry_data
        use parallel_data
        use slope_data
        implicit none
c-------------------------------------------------
        integer(i4) i,j,k,l, length, narrcore, tag
        real(r4) vx,vy,vwatch
        character fname*10, ccore*3, line*256, statfile*256
        logical(i4) first
c-------------------------------------------------
DEB      write(iout,*)'icore=',icore,' nplace=',nplace,' narrived=',narrived

        do k = 1,nplace
        l = lpW(k)
#ifdef PARALLEL
          if (.not.postProc.and.corev(l).ne.icore) cycle
#endif
          if (.not.arrived(k).and.abs(zwv(l)-zwvIn(l)).gt.harr) then
          narrived = narrived + 1
          write (iout,'(a,i6,a,i4,a,g12.5)') 'wave arrived at location ',idW(k),' order ',narrived,' at time ',time
          iunW(k) = iwatch+narrived
          karr(narrived) = k

          open (iunW(k),file=filePlace(k),err=10)
          write (iunW(k),'(a,2(a,a))') 'time',tab,'zsurf',tab,'v'
          close (iunW(k))

10          arrived(k) = .true.
          tarrW(k) = time
          zmaxW(k) = zwv(l)
          tzmaxW(k) = time
          endif

          if (arrived(k)) then
          vx = vv(1,l) 
          vy = vv(2,l) 
          vwatch = sqrt (vx*vx+vy*vy)
            if (zwv(l).gt.zmaxW(k)) then
            tzmaxW(k) = sngl(time)
            zmaxW(k) = zwv(l)
            endif
          vmaxW(k) = max(vmaxW(k),vwatch)

          open (iunW(k),file=filePlace(k),access='append',err=200)
          write (iunW(k),'(2(f12.3,a),f12.3)') 
     &             time, tab,zwv(l),tab,vwatch
          close (iunW(k))
200       continue
          endif

        enddo
c-------------------------------------------------
        if (.not.statnow) return
DEB      write(iout,*)'icore=',icore,' nplace=',nplace,' narrived=',narrived,' watch=',watch
        if (.not.watch) return
c-------------------------------------------------
        if (narrived.gt.0) then
#ifdef PARALLEL
        if (POST) then
        statfile = watchdir(1:lwatch)//bs//'statistics.txt'

        else if (CALCWATCH) then  
        statfile=watchdir(1:lwatch)//bs//chcore//'statistics.txt'
        endif
#else
        statfile = watchdir(1:lwatch)//bs//'statistics.txt'
#endif
        open (iwatch,file=statfile)
        write (iwatch,'(a,5(a,a12))') 'idPlace',tab,'tarr',tab,'zmax',tab,'tzmax',tab,'vmax'
     &              ,tab,'$namePlace'

        do j = 1,narrived
        k = karr(j)
        l = lpW(k)
        write (iwatch,'(i6,4(a,f12.3),a,a)') 
     &           idW(k),tab, tarrW(k), tab,zmaxW(k), tab, tzmaxW(k), tab, vmaxW(k)
     &          ,tab,namePlace(k)(1:length(namePlace(k)))
        enddo

        close (iwatch)
DEB        write (iout,*) 'postWatch: written file ',statfile(1:length(statfile))
        endif
c-------------------------------------------------
#ifdef PARALLEL
      if (postProc) return
DEB      write (iout,*) 'postWatch: processing statistic files'
c-------------------------------------------------
      tag = tagPost
      narrcore = narrived
c-------------------------------------------------
        if (SLAVE) then
DEB        write (iout,*)'icore=',icore,' narrcore=',narrcore
        call mpi_send (narrcore,1, mpi_integer4, 0, tag, mpi_comm_world, mpi_erro)
        return
        endif      
c       --------------------------------------------
c combine statistics.txt
c only MASTER pass from here
        first = .true.
        do 100 i = 0, ncore-1
          if (i.gt.0) call mpi_recv (narrcore,1, mpi_integer4, i, tag, mpi_comm_world, mpi_stato, mpi_erro)
DEB        write (iout,*)'i=',i,' narrcore=',narrcore

          if (narrcore.eq.0) goto 100

          if (first) then
          open (iwatch,file=watchdir(1:lwatch)//bs//'statistics.txt')
          write (iwatch,'(a,5(a,a12))') 'idPlace',tab,'tarr',tab,'zmax',tab,'tzmax',tab,'vmax'
     &              ,tab,'$namePlace'
          first = .false.
          endif    
c-------------------------------------------------        
        write (ccore,'(i3)') i
        call fillzero (ccore,3)
        open (iwatch1,file=watchdir(1:lwatch)//bs//ccore//'statistics.txt')
        read (iwatch1,*)
          do j = 1,narrcore
          read (iwatch1,'(a256)') line
          write (iwatch,'(a)') line(1:length(line))
          enddo
        close (iwatch1)
100     continue
c-------------------------------------------------
      close (iwatch)
#endif
c-------------------------------------------------
DEB        write (iout,*) 'done postWatch for time=',time
        return
        end
