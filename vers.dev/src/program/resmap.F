      program resmap
c-------------------------------------------------------------
c Origin: adaptation of mars2mapgen
c-------------------------------------------------------------
c interface with map
c ncol, nrow referes to input map
c ncol1, nrow1 referes to output map
c--------------------------------------------
	implicit none
        character versdate*20 /"Version - 23Nov04"/
 
      integer*4  npcr, np, ncol, nrow, nmask, typemap
      integer*4  ncol1, nrow1, typemap1
      integer*4  ncol2, nrow2, typemap2
	logical*4 mask
	real*4, allocatable :: x(:), y(:)
     &       , xx(:), yy(:), ip(:), jp(:), pcr(:), val(:), cloneval(:)
        real*8, allocatable :: val8(:)
         
 	integer*4, allocatable :: p0(:), p1(:), p2(:), p3(:), p00(:),pp(:)
 
	logical*1, allocatable :: outmask(:),inpmask(:), clonemask(:)
 
	real*8 xstart,ystart,cellsize, sizey
	real*8 xstart1,ystart1,cellsize1, sizey1
	real*8 xstart2,ystart2,cellsize2, sizey2
        real*8 xshift, yshift
c--------------------------------------------
* gdal additional variables
        integer*4 nband, iband,error, nbyte
        character driver*64, datatype*64, inpfmt*64, outfmt*64
c--------------------------------------------
        character yesno*3 
	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)
 
	real*4 mv
	integer*4 mv4
	integer*1 mv1
 
c--------------------------------------------
c local variables
	integer*4 iun/10/, iargc, narg, nval
     &    ,i,j,l, it0, time, interp
	
	real*4 xf,yf,dx,dy, float
      real*4 nodata, setdata
	
	logical exist,  spaceint, eqmv
 
	character  ch*254,map*254
	character outmap*254, inpmap*254, clonemap*254, cloneFmt*254
 
	real*4 valget
 
#if defined WIN32
      character bs*1 /'\'/
#else
      character bs*1 /'/'/
#endif
	integer*4 length, lm, lout, linp
c-----------------------------------------------------------
	call getmv4(mv4)
	call getmv1(mv1)
        call getmv(mv)
 
	narg = iargc()
	it0 = time()
        inpmap = ' '
        outmap = ' '
        spaceint = .true.
        interp = 1
        mask = .false.
        nodata = mv
        setdata = mv
        xshift=0
        yshift=0
 
	  if (narg.eq.0) then
 
	print *,' '
	print *,'resmap help '
	print *,' '
	print *,'-clone clonemap'
      print *,'   clone map for window and format'
	print *,'   no DEFAULT '
	print *,' '
	print *,'-mask yes/no'
      print *,'   get from clonemap the area of resample (the not MV)'
	print *,'    DEFAULT is ',yesno(mask)
	print *,' '
	print *,'-nodata value'
        print *,'   nodata value in inpmap  '
	print *,'    DEFAULT is MV'
        print *
	print *,'-cloneFmt cloneFmt'
      print *,'   clone map for format'
	print *,'   DEFAULT is clonemap '
	print *,' '
	print *,'-i inpmap'
      print *,'   input maps'
	print *,'   no DEFAULT '
        print *,' '
        print *,'-int  interp'
        print *,'   interpolation flag for resampling to smaller grid'
        print *,'     0 no space interpolation'
        print *,'     1 bilinear interpolation'
        print *,'    DEFAULT is ',interp
        print *,'-xshift xshift'
        print *,'    DEFAULT is ',xshift
        print *,'-yshift yshift'
        print *,'    DEFAULT is ',yshift
	print *,' '
	print *,'-o outmap'
	print *,'   output map'
	print *,'   no DEFAULT'
	print *,' '
#if defined WIN32
	read *
#endif
	stop
	endif
	print *
	print *,'Program resmap: ', versdate
	print *
c-----------------------------------------------------------------
c set defaults
	inpmap= ' '
	outmap = ' '
        clonemap = ' '
c-----------------------------------------------------------------
c read arguments
	do i = 1,narg
	call getarg (i,ch)
c       ----------------------------------------------------
	  if (ch.eq.'-i') then
	  call getarg (i+1,inpmap)
c       ----------------------------------------------------
	  else if (ch.eq.'-o') then
	  call getarg (i+1,outmap)
c       ----------------------------------------------------
          else if (ch.eq.'-int') then
          call getarg(i+1,ch)
          interp = valget(ch)
c       ----------------------------------------------------
          else if (ch.eq.'-nodata') then
          call getarg(i+1,ch)
          nodata = valget(ch)
c       ----------------------------------------------------
          else if (ch.eq.'-xshift') then
          call getarg(i+1,ch)
          xshift = valget(ch)
c       ----------------------------------------------------
          else if (ch.eq.'-yshift') then
          call getarg(i+1,ch)
          yshift = valget(ch)
c       ----------------------------------------------------
          else if (ch.eq.'-setdata') then
          call getarg(i+1,ch)
          setdata = valget(ch)
c       ----------------------------------------------------
	  else if (ch.eq.'-clone') then
	  call getarg (i+1,clonemap)
c--------------------------------------------------- 
        else if ( ch .eq.'-mask') then
	  call getarg (i+1,ch)
		if (ch.eq.'yes') then
		mask = .true.
		else if (ch.eq.'no') then
		mask = .false.
		else
		mask = .true.
		endif
c       ----------------------------------------------------
         endif
c       ----------------------------------------------------
	enddo
c-----------------------------------------------------------------
        cloneFmt= clonemap

	do i = 1,narg
	call getarg (i,ch)
c       ----------------------------------------------------
	  if (ch.eq.'-cloneFmt') then
	  call getarg (i+1,cloneFmt)
          endif
        enddo
c---------------------------------------------------------------
        spaceint = interp.eq.1
c---------------------------------------------------------------
c get input map
	lm = length(inpmap)
	map = inpmap(1:lm)//null
 
	inquire (file=map, exist=exist)
	  if (.not.exist) Then
	print *,inpmap
	  print *,'The inpmap ',map(1:length(map)),' does not exist'
	  call exit(2)
	  endif
c	-------------------------------------------------------------------	
*	call OPENMAPTYPE1 (map,ncol,nrow,typemap, xstart, ystart, cellsize)
	call OPENGDAL     (map,ncol,nrow,typemap, xstart, ystart, cellsize,sizey,driver,nband,error)
	
	nval = ncol*nrow
 
	allocate (val(nval),inpmask(nval))
	val=mv
 
*	CALL GETVALMAP (val)
        iband = nband

        call GETGDALATTR (iband,nbyte,datatype)
        print *,'nbyte=',nbyte,' datatype=',datatype,' nval=',nval

        if (nbyte.eq.4) then
	CALL GETGDALVAL (iband,val)

        else if (nbyte.eq.8) then
	allocate (val8(nval))
	CALL GETGDALVAL (iband,val8)
*        print *,val8
        val = val8
        deallocate (val8)
        endif

        if (eqmv(nodata)) then
	Call checkmv (val,mv,inpmask,nval)
        inpmask = .not.inpmask

        else
          do l = 1,nval
          inpmask(l)=val(l).ne.nodata 
            if (.not.inpmask(l)) val(l) = setdata
          enddo
        endif
 
	print *,'ncol=',ncol,' nrow=',nrow
	print *,'input: min=',minval(val,mask=inpmask)
     &               ,' max=',maxval(val,mask=inpmask)
 
        xf = xstart
        yf = ystart
	dx =  cellsize
	dy =  sizey
c---------------------------------------------------------------
c get clone map for window
	lm = length(clonemap)
	map = clonemap(1:lm)//null
	inquire (file=map, exist=exist)
	  if (.not.exist) Then
	  print *,'The clonemap ',map(1:length(map)),' does not exist'
	  call exit(2)
	  endif 
	call OPENGDAL    (map,ncol1,nrow1,typemap1
     &               , xstart1, ystart1, cellsize1,sizey1,driver,nband,error)
	npcr = ncol1*nrow1
c       -------------------------------
        allocate (clonemask(npcr),cloneval(npcr))

        if (mask) then
        iband = nband
	CALL GETGDALVAL (iband,cloneval)
	Call checkmv (cloneval,mv,clonemask,npcr)
        clonemask = .not.clonemask
        deallocate (cloneval)

        else
        clonemask = .true.
        call CLOSEGDAL()
        endif


        print *,'Clone map for window ',map(1:lm)
        print *,' ncol1=',ncol1,' nrow1=',nrow1

c---------------------------------------------------------------
c get clone map for format
	lm = length(cloneFmt)
	map = cloneFmt(1:lm)//null
	inquire (file=map, exist=exist)
	  if (.not.exist) Then
	  print *,'The cloneFmt ',map(1:length(map)),' does not exist'
	  call exit(2)
	  endif
 
 
	call OPENGDAL    (map,ncol2,nrow2,typemap2
     &               , xstart2, ystart2, cellsize2,sizey2,driver,nband,error)

        outfmt = driver(1:length(driver))//null

        call GETGDALATTR (iband,nbyte,datatype)
        datatype = datatype(1:length(datatype))//null

        call CLOSEGDAL()

        print *,'Clone map for format ',map(1:lm)
	print *,'type2=',typemap2,' outfmt=',outfmt(1:length(outfmt))
c---------------------------------------------------
c  x,y of the output maps (center of the PCRaster pixels)
 
	allocate (x(npcr),y(npcr))
	
	l=0
	do j = 1,nrow1
		do i = 1,ncol1
	    l = l+1
	    x(l) = xstart1 + (i-0.5)*cellsize1
	    y(l) = ystart1 + (j-0.5)*sizey1
		enddo
	enddo
		
	allocate (p00(npcr),ip(npcr), jp(npcr),outmask(npcr))
	p00 = mv4
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  create pointers to input data
 
c  interpolation:
	if (spaceint) then
c pcr(ip,jp) are the inp index of PCRaster pixels into the grid delimited as follows
c
c  p00 = (int(jp)-1)*ncol + int(ip)
c
c   p0=p00(pp)    dx    p1=p00(pp)+1
c      +-----------------+
c      |                 |
c      |                 |
c      |                 |
c      |    pcr(ip,jp)   |
c      |                 |
c -dy  |                 |
c      |                 |
c      |                 |
c      |                 |
c      |                 |
c      +-----------------+
c  p2=p00(pp)+ncol    p3=p00(pp)+ncol+1
 
c caution: ip,jp are real values
c
	ip = (x-xf)/dx + 0.5
	jp = (y-yf)/dy + 0.5
 
	print *,'min ip ',minval(ip)
      print *,'max ip ',maxval(ip)
 
	print *,'min jp ',minval(jp)
      print *,'max jp ',maxval(jp)
 
c mask is true if PCRaster pixel is into input window
c plus a frame of 1 pixel (estrapolation)

	outmask = ip.ge.0..and.ip.le.float(ncol+1)
     &  .and.jp.ge.0..and.jp.le.float(nrow+1)

 
	nmask = count(outmask)
	print *,'nmask=',nmask
c-----------------------------------------
	allocate (xx(npcr),yy(npcr))
	allocate (pp(nmask),p0(nmask),p1(nmask),p2(nmask),p3(nmask))
c pp is a compact array of PCR pointers to the input map
	i = 0
	  do l = 1,npcr
	    if (outmask(l)) then
	    i = i + 1
	    pp(i) = l
	    endif
	  enddo
	print *,'nmask1=',i,' npcr=',npcr
c--------------------------------------
	xx = mv
	yy = mv
 
	xx(pp) = ip(pp)
	yy(pp) = jp(pp)
 
	ip = min(max(ip,1.),float(ncol-1))
	jp = min(max(jp,1.),float(nrow-1))
 
c xx, yy are the reaminder of ip, jp, which corrspond to
c xx is (x_pcr-x_inp)/dx_inp
c yy is (y_pcr-y_inp)/dy_inp
 
	do i = 1,nmask
	j = pp(i)
	xx(j) = xx(j)-int(ip(j))
	yy(j) = yy(j)-int(jp(j))
	enddo
c--------------------------------------------
	print *,'min xx ',minval(xx,mask=outmask)
      print *,'max xx ',maxval(xx,mask=outmask)
 
	print *,'min yy ',minval(yy,mask=outmask)
      print *,'max yy ',maxval(yy,mask=outmask)
 
      p00 = (int(jp)-1)*ncol + int(ip)
c----------------------------------------------
	p0 = p00(pp)
	p1 = p00(pp)+1
	p2 = p00(pp)+ncol
	p3 = p00(pp)+ncol + 1
	print *,'min p0 ',minval(p0),' max p0 ',maxval(p0)
	print *,'min p1 ',minval(p1),' max p1 ',maxval(p1)
	print *,'min p2 ',minval(p2),' max p2 ',maxval(p2)
	print *,'min p3 ',minval(p3),' max p3 ',maxval(p3)
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c no interpolation
c pcr(ip,jp) are the inp index of PCRaster pixels into the grid delimited as follows
c
c  p00 = (int(jp)-1)*ncol + int(ip)
c
c               dx
c      +-----------------+
c      |                 |
c      |  pcr(ip,jp)     |
c      |                 |
c      |     p0=p00(pp)  |
c -dy  |        +        |
c      |                 |
c      |                 |
c      |                 |
c      |                 |
c      +-----------------+
c
 
c caution: ip,jp are real values
c note that the algorith is different to the previous
	else
	ip = (x-xf)/dx + 1.
	jp = (y-yf)/dy + 1.
 
	print *,'min ip ',minval(ip)
      print *,'max ip ',maxval(ip)
 
	print *,'min jp ',minval(jp)
      print *,'max jp ',maxval(jp)
 
	outmask = ip.ge.0.5 .and.ip.lt.(ncol+0.5)
     &  .and.jp.ge.0.5 .and.jp.lt.(nrow+0.5)
 
 
	nmask = count(outmask)
	print *,'noutmask=',nmask
c--------------------------------------
	allocate (p0(nmask),pp(nmask))
 
	i = 0
	  do l = 1,npcr
	    if (outmask(l)) then
	    i = i + 1
	    pp(i) = l
	    endif
	  enddo
	print *,'nmask1=',i,' npcr=',npcr
c----------------------------------------------
 
	ip = min(max(ip,1.),float(ncol))
	jp = min(max(jp,1.),float(nrow))
 
      p00 = (int(jp)-1)*ncol + int(ip)
c----------------------------------------------
	p0 = p00(pp)
	endif
c----------------------------------------------
 
	deallocate (x,y,ip,jp)
 
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
	print *,'no. of not missing values value =',nmask
     &       ,' = ',float(nmask)*100./npcr, ' % '
c----------------------------------------------------------------
	allocate (pcr(npcr))
	pcr = mv
c-----------------------------------------------------------------
	pcr = mv
 		if (spaceint) then
c  interpolating inp dem
	    pcr(pp) = val(p0)
     &         + xx(pp)*(val(p1)-val(p0))
     &         + yy(pp)*(val(p2)-val(p0))
     &         + xx(pp)*yy(pp)*(val(p0)+val(p3)
     &                       -val(p1)-val(p2))
	    else
c  using inp dem
	    pcr(pp) = val(p0)
		endif

        do i = 1,npcr
          if (.not.clonemask(i)) pcr(i)=setdata
        enddo

        lout = length(outmap)
	map = outmap(1:lout)//null
        xstart1 = xstart1+xshift
        ystart1 = ystart1+yshift
	call OUTGDAL (map,ncol1,nrow1,typemap2
     &               , xstart1, ystart1, cellsize1,pcr,npcr, sizey1, outfmt, nband,error)
 
c------------------------------------------------------------
      print *,'time spent [min] = ', (time()-it0)/60.
	stop
 	end
