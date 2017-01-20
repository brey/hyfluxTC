#define DEB c
c============================================================================
      subroutine shoreline
c============================================================================
      use parameter_data
      use map_data
      use window_data
      implicit none
c----------------------------------------------------
      real(rr8), allocatable :: x(:), y(:),x1(:),y1(:),x2(:),y2(:)
      real(r4), allocatable :: zv(:), dzdk(:,:)
      integer(i4), allocatable :: cell(:),places(:),watches(:),kl(:)
      logical*1, allocatable :: mask1(:,:)
      logical(i4), allocatable :: identical(:)

      real(rr8) a,b,c,xplace,yplace,xx(4),yy(4),xnear,ynear, disnear 

      real(r4) long1, lat1, numget ,zplace,znear, xsngl, ysngl, slope


      integer(i4) i,j,l,k ,kn,iseg, length, nplace, id, nv, nlongwin
      logical(i4) ok, exist, eqmv
      real (rr8) eps, zmin, zmax
c
      integer(i4) nmax,nh, i_id, i_long, i_lat, i_name, nvalue, ncolumn, errtab
      parameter (nmax=20)
      character fields(nmax)*64, header*254, buff*254, msg*3
c---------------------------------
      if (mapdeg) then
      eps = 1./deg2m
      else
      eps = 1
      endif
c---------------------------------
      allocate (x(nval),y(nval),mask1(nvx,nvy))
c---------------------------------
      if (allocated(val)) deallocate (val) 
      allocate (val(nval))

      allocate (zv(nval),dzdk(2,nval))
      call readmap ('dem',demmap,zv,.true.)
      call readmap ('dzdx',dzdxmap,val,.true.); dzdk(1,1:nval) = val
      call readmap ('dzdy',dzdymap,val,.true.); dzdk(2,1:nval) = val
      call readmap ('depth',' ',val,.false.)
c---------------------------------
      mask1 = .false.
      k = 0
      do j = 1,nvy
        do i = 1,nvx
        k = k+1
          if (.not.eqmv(zv(k))) then
            if (sumslope) then
            slope =   (abs(dzdk(1,k))+abs(dzdk(2,k)))
            else
            slope = max(abs(dzdk(1,k)),abs(dzdk(2,k)))
            endif
          zmin = zv(k)-cellsize*slope/2
          zmax = zv(k)+cellsize*slope/2
DEB          print '(5(a,g12.6))','zmin=',zmin,' zmax=',zmax,' seaLevel=',seaLevel,' slope=',slope,' zv=',zv(k)
          mask1(i,j) = val(k).gt.hout.and.zmin.lt.seaLevel.and.zmax.gt.seaLevel
*          mask1(i,j) = zmin.lt.seaLevel.and.zmax.gt.seaLevel
          endif
        enddo
      enddo

      nv = count(mask1)
      write (iout,*) 'no. of not MV = ',nv
 
      if (nv.gt.0) allocate (kl(nv),x1(nv),y1(nv),x2(nv),y2(nv),cell(nv),identical(nv))
 
      l = 0
      k = 0
      do j = 1,nvy
        do i = 1,nvx
        k = k+1
        x(k) = xstart + (i-0.5)*cellsize
        y(k) = ystart - (j-0.5)*cellsize
          if (mask1(i,j)) then
          l = l+1
          kl(l) = k
          endif
        enddo
      enddo

 
      if (shorefile.ne.' ') then
      open (ifree,file=shorefile)
      write (ifree,'(20a)') 'x1',tab,'y1',tab,'x2',tab,'y2',tab,'cell'
      endif

      i=0
      do l = 1,nv
      k = kl(l)
* a,b,c are the coefficient of a line obtained by intersecting z=dzdx*(x-x0) + dzdy*(y-y0) + z0 and zw=0
      a = dzdk(1,k)
      b = dzdk(2,k)
      c = zv(k)
      call shoresegm (a,b,c,cellsize/2.,x(k),y(k),xx,yy,ok,iseg)
        if (ok) then
        i=i+1
        x1(i) = xx(1)
        y1(i) = yy(1)
        x2(i) = xx(2)
        y2(i) = yy(2)
        cell(i) = k
        identical(i) = (x1(i)-x2(i))**2+(y1(i)-y2(i))**2 .lt. eps
          if (shorefile.ne.' ') then
          write (ifree,'(4(f15.9,a1),i2,a1,i10)') 
     &          xx(1), tab,yy(1), tab,xx(2), tab,yy(2), tab,k
          endif
*        else
*        write (iout,*) 'l=',l,' cell ',k,' case ',ival(k),' iseg ',iseg,' shore segm. is not calc.'
        endif
      enddo

      nv = i  ! nv is the no. of shore segments

        if (shorefile.ne.' ') then
        close (ifree)
	  write (iout,*) 'Created shorefile ',shorefile(1:length(shorefile))
        endif
c
      deallocate (mask1)
        if (nv.gt.0) deallocate (kL) 
c-----------------------------------------------------------------
      if (placefile.eq.' ') then
      deallocate (zv,dzdk,x,y,cell,identical,x1,y1,x2,y2)
      return
      endif

c-----------------------------------------------------------------
      allocate (places(nval),watches(nval))
      places = mv4
      watches = mv4
c-----------------------------------------------------------------
      inquire (file=placefile,exist=exist)
        if (.not.exist) then
        write (iout,*) 'not exist placefile = ',placefile(1:length(placefile))
        write (iout,*) 'connot create watchfile = ',watchfile(1:length(watchfile))
        return 
        endif

c-----------------------------------------------------------------
      call opentab (iinp,placefile,ncolumn,nplace,errtab)
        if (errtab.ne.0) return
        write (iout,*)
	write (iout,*) ' Read ',nplace,' places in placefile = ',placefile(1:length(placefile))
c-----------------------------------------------------------------
      if (watchfile.eq.' ') watchfile = workdir(1:lw)//bs//'watch.txt'
      open (ifree,file=watchfile)
c-----------------------------------------------------------------
	write (ifree,'(12(a,a1))')
     &            'idPlace',tab,'latPlace',tab
     &            ,'longPlace',tab,'zPlace',tab
     &            ,'cellNear',tab,'latNear',tab,'longNear',tab
     &            ,'zNear',tab,'distNear',tab,'$namePlace'

      read (iinp,'(a)') header
	call nocr(header)
      call proctab(header,fields,nmax,nh)
        do l = 1,nh
          if (fields(l).eq.'long') i_long = l
          if (fields(l).eq.'lat') i_lat = l
          if (fields(l).eq.'id') i_id = l
          if (fields(l).eq.'$name') i_name = l
        enddo


       write (iout,*) ' i_long=',i_long,' i_lat=',i_lat,' i_id=',i_id,' i_name=',i_name

       write (iout,*) 'latmin0=',latmin0,' latmax0=',latmax0
       write (iout,*) 'longmin0=',longmin0,' longmax0=',longmax0

      l = 0
          do while (l.lt.nplace)
          l = l+1
          read (iinp,'(a)',end=1000)  buff
	    call nocr(buff)
          call proctab(buff,fields,nh,nh)
          id = numget(fields(i_id),msg)
          lat1 = numget(fields(i_lat),msg)

            if (lat1.le.latmin0.or.lat1.ge.latmax0) cycle
            yplace = lat1

          long1 = numget(fields(i_long),msg)
          xplace = long1
            do nlongwin = 1,2
              if (xplace.gt.longmin0.and.xplace.lt.longmax0) go to 100
            xplace = xplace + 360
            enddo

            cycle

100         continue


            i = min(max(int((xplace-xstart)/cellsize)+1,2),nvx-1)
            j = min(max(int((ystart-yplace)/cellsize)+1,2),nvy-1)
            k = (j-1)*nvx+i

            if (eqmv(zv(k))) cycle

            zplace = zv(k)+(xplace-x(k))*dzdk(1,k)+(yplace-y(k))*dzdk(2,k)

DEB            write (iout,*) 'id=',id,' zplace=',zplace

              if (zplace.lt.0.) then
              xnear = xplace
              ynear = yplace
              disnear = 0.
              znear = zplace
              kn = k

              else
              call distsegm (xplace,yplace,x1,y1,x2,y2,identical,nv,xnear,ynear,i,disnear,eps)
DEB              write (iout,*) 'i=',i,' nv=',nv,' disnear=',disnear
                if (i.eq.0) cycle
              kn = cell(i)
DEB              write (iout,*) 'kn=',kn
                if (mapdeg) disnear = disnear*deg2m
              znear = zv(kn)+(xnear-x(kn))*dzdk(1,kn)+(ynear-y(kn))*dzdk(2,kn)
              endif

DEB            write (iout,*) 'k=',k,' kn=',kn

            places(k) = id
            watches(kn) = id

            write (ifree,'(i6,2(a,f14.9) ,a,f8.0 ,1(a,i8) ,2(a,f14.9),2(a,f8.0),a,a)')
     &            id ,tab,yplace ,tab,xplace ,tab,zPlace ,tab,kn ,tab,ynear ,tab,xnear 
     &          ,tab,znear ,tab,disnear,tab,fields(i_name)(1:length(fields(i_name)))

          enddo
      close (iinp)
      close (ifree)

      maps = workdir(1:lw)//bs//'place'//mapext
      call OUTGDAL (maps, nvx,nvy,2, xstart, ystart, cellsize,places,nval
     &           , sizey, mapfmt, nband,error)

      maps = workdir(1:lw)//bs//'watch'//mapext
      call OUTGDAL (maps, nvx,nvy,2, xstart, ystart, cellsize,watches,nval
     &           , sizey, mapfmt, nband,error)


      deallocate (zv,dzdk,x,y)
        if (nv.gt.0) deallocate (cell,identical,x1,y1,x2,y2)
      deallocate (places,watches)

      write (iout,*)
      write (iout,'(a)') 'Created watchfile ',watchfile(1:length(watchfile))
      write (iout,*) 'Created place.map and watch.map'
cr
      return
1000  print *,'nplace=',nplace,' l=',l
      end
 
      subroutine shoresegm (a,b,c,d,x,y,xx,yy,ok,i)
      use kind
      implicit none
      real(rr8) a,b,c,d,x,y, xx(4), yy(4)
      real(rr8) eps, val(4)
      logical(i4) ok,okk(4)
      integer(i4) i,j
c
      okk = .false.
      eps = d*1d-10
      i = 0; j=0
       
c intersect left or right ?
      if (abs(b).gt.eps) then
c left
      j=j+1
       val(j) = (-c+a*d)/b
         if (abs(val(j)).le.d) then
         i = i+1
         okk(i) = .true.
         xx(i) = x-d
         yy(i) = y+val(j)
         endif
c right
      j=j+1
      val(j) = (-c-a*d)/b
         if (abs(val(j)).le.d) then
         i = i+1
         okk(i) = .true.
         xx(i) = x+d
         yy(i) = y+val(j)
         endif
      endif
 
c intersect bottom and top ?
      if (abs(a).gt.eps) then
c bottom
      j=j+1
       val(j) = (-c+b*d)/a
         if (abs(val(j)).le.d) then
         i = i+1
         okk(i) = .true.
         xx(i) = x+val(j)
         yy(i) = y-d
         endif
c top
      j=j+1
       val(j) = (-c-b*d)/a
         if (abs(val(j)).le.d) then
         i = i+1
         okk(i) = .true.
         xx(i) = x+val(j)
         yy(i) = y+d
         endif
      endif
 
      if (i.eq.2) then
      ok = .true.
 
      else if (i.eq.0.and.abs(c).le.eps) then
      ok = .true.
      xx(1:2) = x
      yy(1:2) = y

      else
*      print *,'okk=',okk,' i=',i,' a=',a,' b=',b,' c=',c,' d=',d,' eps=',eps
*      print *,'val=',val
      ok = .false.
      endif
 
      return
      end

      function dist (x1,y1,x2,y2)
      use kind
      implicit none
      real(rr8) dist,x1,y1,x2,y2
      dist = (x1-x2)**2 + (y1-y2)**2
      return
      end


      subroutine distsegm (xplace,yplace,x1,y1,x2,y2,identical,nv,xnear,ynear,k,dismin,eps)
      use kind
      implicit none
      integer(i4) nv,k,i
      real(rr8) xplace, yplace,x1(nv),y1(nv),x2(nv),y2(nv),xnear,ynear
      real(rr8) dismin
      logical(i4) identical(nv),dentro, parallel
      real(rr8) a1,b1,c1, a2,b2,c2, dist,dis,xm,ym,eps
c
      k=0
      dismin = 1e37


      do i = 1,nv

      if (identical(i)) then
      xm = 0.5*(x1(i)+x2(i))
      ym = 0.5*(y1(i)+y2(i))
      dis = dist(xplace,yplace,xm,ym)

      else
      call abcret (x1(i),y1(i),x2(i),y2(i),a1,b1,c1)
      call perp (xplace,yplace,a1,b1,c1,a2,b2,c2)
      call intersret (a1,b1,c1,a2,b2,c2,xm,ym,parallel)
        if (parallel) stop 'program error at distsegm'
        if (.not.dentro(x1(i),y1(i),x2(i),y2(i),xm,ym)) then
           if ( dist(xplace,yplace,x1(i),y1(i)) .lt. dist(xplace,yplace,x2(i),y2(i)) ) then
           xm = x1(i) ; ym = y1(i)
           else
           xm = x2(i) ; ym = y2(i)
           endif 
        endif
      dis = dist(xplace,yplace,xm,ym)
      endif

        if (dis.lt.dismin) then
        dismin=dis
        k = i
        xnear = xm
        ynear = ym
        endif
      enddo
      dismin = sqrt(dismin)
      return
      end
c
      function dentro(x1,y1,x2,y2,x,y)
      use kind
      implicit none
      real(rr8) x1,y1,x2,y2,x,y,d,dd
      logical(i4) dentro

      d=(x-x1)*(x2-x1)+(y-y1)*(y2-y1)
      dd=(x2-x1)**2+(y2-y1)**2
      dentro = d.ge.0.0.and.d.le.dd
      return
      end
c
      subroutine abcret (x1,y1,x2,y2,a,b,c)
      use kind
      implicit none
      real(rr8) x1,y1,x2,y2,a,b,c

      a = y2-y1
      b = x1-x2
      c = y1*x2-y2*x1
      return
      end
c
      function distret (x0,y0,a,b,c)
      use kind
      implicit none
      real(rr8) a,b,c,x0,y0,distret

      distret = (a*x0+b*y0+c)/sqrt(a*a+b*b)
      return
      end
c
      subroutine perp(x0,y0,a1,b1,c1,a2,b2,c2)
      use kind
      implicit none
      real(rr8) x0,y0,a1,b1,c1,a2,b2,c2

      a2 = b1
      b2 = -a1
      c2 = a1*y0-b1*x0
      return
      end
c
      subroutine intersret (a1,b1,c1,a2,b2,c2,x,y,parall)
      use kind
      implicit none
      real(rr8) a1,b1,c1,a2,b2,c2,x,y,det
      logical(i4) parall

      det = a1*b2-a2*b1
      parall = abs(det).lt.1e-10
        if (parall) then
        x = 0
        y = 0
        return
        endif
      x = -(c1*b2-c2*b1)/det
      y = -(a1*c2-a2*c1)/det
      return
      end
