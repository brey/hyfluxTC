      program map2map
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
	logical*4 masksea
	real*4, allocatable :: outdem(:), inpdem(:),  x(:), y(:)
     &       , xx(:), yy(:), ip(:), jp(:), pcr(:), val(:), demint(:)
 
 	integer*4, allocatable :: p0(:), p1(:), p2(:), p3(:), p00(:),pp(:)
 
	logical*1, allocatable :: inpmask(:),outmask(:), maskreq(:)
	integer*1, allocatable :: beach(:)
 
	real*8 xstart,ystart,cellsize
	real*8 xstart1,ystart1,cellsize1
	real*8 xstart2,ystart2,cellsize2
 
	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)
 
	real*4 mv /z'ffffffff'/
	integer*4 imv /z'ffffffff'/, mv4
	integer*1 mv1
 
c--------------------------------------------
c local variables
	integer*4 iun/10/, iargc, narg, nval
     &    ,i,j,imap,l,ll, it0, time
     &    ,stepmap, first, last, fmap
	
	real*4 xf,yf,dx,dy, float,factcorr, factdcorr
     &      ,val1,val2, v0, v1, v2, v3
	real*8 xshift, yshift
	integer*4 ival1, ival2
	equivalence (val1,ival1),(val2,ival2)
 
	
	logical exist,  check, spaceint, cover, findbeach
 
	integer nmaplist_max, nmaplist, ntenvlist_max, ntenvlist
     &       ,nmaps,nmaps_max
	parameter (nmaplist_max=100, ntenvlist_max=10,nmaps_max=10)
 
	character  ch*132
	character outdir*132, inpdir*132
     &        ,outdemmap*132, inpdemmap*132, inpmap*132, outmap*132
     &        ,fname*132, pwd*132, dir*132
     &        ,inpext*5, outext*5, map*132
     &        ,maplist(nmaplist_max)*132, tdmap*8
     &        ,tenvlist(ntenvlist_max)*8, msg*3, maps(nmaps_max)*132
 
	integer*4 typecorr(nmaplist_max)
 
	real*4 radius, idp, numget
	integer*4 nradius, nmax
 
#if defined WIN32
      character bs*1 /'\'/
#else
      character bs*1 /'/'/
#endif
	integer*4 length, lm, lout, linp, interp
c-----------------------------------------------------------
	call getmv4(mv4)
	call getmv1(mv1)
 
	narg = iargc()
	it0 = time()
	idp = 1.
	nmax = 5
	radius = 10e3
	factcorr = 0.0065
	factdcorr = 0.002
	xshift=0.
	yshift=0.
	interp = 1
 
	  if (narg.eq.0) then
 
	print *,' '
	print *,'map2map help '
	print *,' '
	print *,'Resample to a smaller grid size a stack of maps '
        print *,'and correct temperature in respect to dem change'
        print *,'Used for ERA40 resampling'
	print *,' '
	print *,'-i inpdir'
      print *,'   inpdir is the directory name of the input maps'
	print *,'   no DEFAULT '
	print *,' '
	print *,'-maps maps'
	print *,'    maps are the input maps with full extension'
	print *,'Example: -maps dem.map'
	print *,'    no DEFAULT'
	print *,' '
	print *,'-maplist maplist'
	print *,'    maplist is the list of the input maps that'
     *       ,' are not corrected'
        print *,'    Note: include only the headers (without extension)'
	print *,'    Example: -maplist R24a Vel SSRD STR'
	print *,'    no DEFAULT'
	print *,' '
	print *,'-td td'
	print *,'    td is the list of dewpoint temperature maps corrected'
     *       ,' with factdcorr'
        print *,'    Note: include only the headers (without extension)'
	print *,'    Example: -td td'
	print *,'    no DEFAULT'
	print *,' '
	print *,'-tenvlist tenvlist'
	print *,'    tenvlist is the list of the enviroment'
     *       ,' temperature maps corrected by factcorr'
        print *,'    Note: include only the headers (without extension)'
	print *,'    Example: -tenv ta Tmin Tmax'
	print *,'    no DEFAULT'
	print *,' '
	print *,'-o outdir'
	print *,'   output: directory name of the output maps'
	print *,'   no DEFAULT'
	print *,' '
	print *,'-inpdem inpdem'
	print *,'   inpdem: the dem map name of input maps'
     	print *,'   no DEFAULT'
	print *,' '
	print *,'-outdem outdem'
	print *,'   outdem: the dem map name of output maps'
     	print *,'   no DEFAULT'
	print *,' '
	print *,'-from  first'
	print *,'    first: extension of the first input map'
	print *,'    DEFAULT is 1'
	print *,' '
	print *,'-to  last '
	print *,'    last: extension of the last input map'
	print *,'    DEFAULT is 1'
	print *,' '
	print *,'-fmap  ext '
	print *,'    ext: extention of the first output map'
	print *,'    DEFAULT is 1'
	print *,' '
	print *,'-stepmap  ext '
	print *,'    Increment extension for the output maps'
	print *,'    DEFAULT is 1'
	print *,' '
	print *,'-int  interp'
	print *,'   interpolation flag for resampling to smaller grid'
	print *,'     0 no space interpolation'
	print *,'     1 bilinear interpolation'
	print *,'    DEFAULT is ',interp
	print *,' '
	print *,'-m [yes/no]'
	print *,'    mask the sea in respect to outdem'
	print *,'    DEFAULT is no mask'
	print *,' '
	print *,'-C [yes/no]'
	print *,'   Check: produce additional output'
	print *,'    DEFAULT is no Check'
	print *,' '
	print *,'-cover  [yes/no]'
	print *,'   cover the MV of the requested input maps'
	print *,'   by inverse distance interpolation '
	print *,'   See parameters idp, radius, nmax'
	print *,'   DEFAULT is no cover'
        print *,'   Note: use the same algorithm of mars2mapgen'
	print *,' '
      print *,'-idp idp'
	print *,'   idp: exponent of inverse dist waighting value'
      print *,'   DEFAULT is ',idp
	print *,' '
	print *,'-radius radius'
	print *,'   radius: max distance [m] of the selected values to the MV'
	print *,'   DEFAULT is ',radius,' (looks to all values)'
	print *,' '
	print *,'-nmax nmax'
	print *,'   nmax: max no of selected values'
	print *,'   DEFAULT is ',nmax
	print *,' '
	print *,'-factcorr  factcorr'
	print *,'   temperature correction factor '
	print *,'   DEFAULT is',factcorr
	print *,' '
	print *,'-factdcorr  factdcorr'
	print *,'   dewpoint correction factor '
	print *,'   DEFAULT is',factdcorr
	print *,' '
	print *,'-xshift xshift'
        print *,'   x shift of the output maps'
	print *,'   DEFAULT is',xshift
	print *,' '
	print *,'-yshift yshift'
        print *,'   y shift of the output maps'
	print *,'   DEFAULT is',yshift
	print *,' '
	print *,'******** ',versdate,' ******************'
	print *,' '
	print *,'USAGE: arguments in [] are not strictly requested'
	print *,' '
	print *,'map2map -i inpdir -o outdir -inpdem inpdem -outdem outdem'
      print *,' [-maps maps]  [-maplist maplist] [-td td] '
     &       ,  '[-tenvlist tenvlist]'
	print *,' [-from  first] [-to  last] [-fmap  ext] [-stepmap  ext] '
	print *,' [-int interp] [-m [yes/no]] [-C [yes/no] [-cover [yes/no]]'
      print *,' [-idp idp] [-radius radius] [-nmax nmax] '
	print *,' [-factcorr  factcorr] [-factdcorr factdcorr]'
#if defined WIN32
	read *
#endif
	stop
	endif
	print *
	print *,'Program map2map: ', versdate
	print *
c-----------------------------------------------------------------
c set defaults
	check = .false.
	cover = .false.
	inpdir = ' '
	outdir = ' '
	inpdemmap = ' '
	outdemmap = ' '
	masksea = .false.
	fmap = 1
	stepmap = 1
	first = 1
	last = 1
	tdmap = ' '
	nmaps=0
	nmaplist = 0
	ntenvlist = 0
c-----------------------------------------------------------------
c read arguments
	do i = 1,narg
	call getarg (i,ch)
c       ----------------------------------------------------
	  if (ch.eq.'-i') then
	  call getarg (i+1,inpdir)
 
	  else if (ch.eq.'-maps') then
	    do j = 1,nmaps_max
            call getarg (i+j,ch)
	      if (ch(1:1).eq.'-'.or.ch.eq.' ') then
	      nmaps = j-1
	      exit
  	      endif
	    maps(j) = ch
	    nmaps = j
	    enddo
 
	  else if (ch.eq.'-maplist') then
	    do j = 1,nmaplist_max
            call getarg (i+j,ch)
	      if (ch(1:1).eq.'-'.or.ch.eq.' ') then
	      nmaplist = j-1
	      exit
  	      endif
	    maplist(j) = ch
	    typecorr(j) = 0
	    nmaplist = j
	    enddo
c       ----------------------------------------------------
	  else if (ch.eq.'-td') then
	  call getarg(i+1,tdmap)
c       ----------------------------------------------------
	  else if (ch.eq.'-tenvlist') then
	    do j = 1,ntenvlist_max
            call getarg (i+j,ch)
	      if (ch(1:1).eq.'-'.or.ch.eq.' ') then
	      ntenvlist = j-1
	      exit
  	      endif
	    tenvlist(j) = ch
	    ntenvlist = j
	    enddo
c       ----------------------------------------------------
	  else if (ch.eq.'-o') then
	  call getarg (i+1,outdir)
c       ----------------------------------------------------
	  else if (ch.eq.'-inpdem') then
	  call getarg (i+1,inpdemmap)
c       ----------------------------------------------------
	  else if (ch.eq.'-outdem') then
	  call getarg (i+1,outdemmap)
c       ----------------------------------------------------
	  else if (ch.eq.'-from') then
	  call getarg(i+1,ch)
	  read (ch,'(i6)') first
c       ----------------------------------------------------
	  else if (ch.eq.'-to') then
	  call getarg(i+1,ch)
	  read (ch,'(i6)') last
c       ----------------------------------------------------
	  else if (ch.eq.'-fmap') then
	  call getarg(i+1,ch)
	  read (ch,'(i3)') fmap
c       ----------------------------------------------------
	  else if (ch.eq.'-stepmap') then
	  call getarg(i+1,ch)
	  read (ch,'(i3)') stepmap
c       ----------------------------------------------------
	  else if (ch.eq.'-int') then
	  call getarg(i+1,ch)
	  read (ch,'(i3)') interp
c       ----------------------------------------------------
	  else if (ch.eq.'-m') then
	  call getarg (i+1,ch)
		if (ch.eq.'yes') then
		masksea = .true.
		else if (ch.eq.'no') then
		masksea = .false.
		else
		masksea = .true.
		endif
c       ----------------------------------------------------
	  else if (ch.eq.'-C') then
	  call getarg (i+1,ch)
		if (ch.eq.'yes') then
		check = .true.
		else if (ch.eq.'no') then
		check = .false.
		else
		check = .true.
		endif
c       ----------------------------------------------------
	  else if (ch.eq.'-cover') then
	  call getarg (i+1,ch)
		if (ch.eq.'yes') then
		cover = .true.
		else if (ch.eq.'no') then
		cover = .false.
		else
		cover = .true.
		endif
c       ----------------------------------------------------
	  else if (ch.eq.'-idp') then
	  call getarg (i+1,ch)
	  idp = numget(ch,msg)
c       ----------------------------------------------------
	  else if (ch.eq.'-nmax') then
	  call getarg (i+1,ch)
	  nmax = numget(ch,msg)
c       ----------------------------------------------------
	  else if (ch.eq.'-radius') then
	  call getarg (i+1,ch)
	  radius = numget(ch,msg)	
c       ----------------------------------------------------
	  else if (ch.eq.'-factcorr') then
	  call getarg(i+1,ch)
	  read (ch,'(f10.9)') factcorr
c       ----------------------------------------------------
	  else if (ch.eq.'-factdcorr') then
	  call getarg(i+1,ch)
	  read (ch,'(f10.9)') factdcorr
c       ----------------------------------------------------
	  else if (ch.eq.'-xshift') then
	  call getarg(i+1,ch)
	  read (ch,'(f10.9)') xshift
c       ----------------------------------------------------
	  else if (ch.eq.'-yshift') then
	  call getarg(i+1,ch)
	  read (ch,'(f10.9)') yshift
c       ----------------------------------------------------
	  endif
c       ----------------------------------------------------
	enddo
c---------------------------------------------------
	print *,'xshift=',xshift,' yshift=',yshift
	spaceint = interp.eq.1
	if (last.lt.first) then
	print *,'last map=',last,' is < of first=',first
	call exit(2)
	endif
c---------------------------------------------------------------
c Complete the map list and define type of correction
	if (ntenvlist.gt.0) then
	j = nmaplist
	  do i = 1,ntenvlist
	  j = j+1
	  maplist(j) = tenvlist(i)
	  typecorr(j) = 1
	  enddo
	nmaplist = j
	endif
 
	if (tdmap.ne.' ') then
	nmaplist = nmaplist + 1
	maplist(nmaplist) = tdmap
	typecorr(nmaplist) = 2
	endif
 
	if (nmaps.gt.0) then
	j = nmaplist
	  do i = 1,nmaps
	  j = j+1
	  maplist(j) = maps(i)
	  typecorr(j) = -1
	  enddo
	nmaplist = j
	endif
 
	if (check) then
	print *,'variable, Type of correction'
	do i = 1,nmaplist
	print *,maplist(i)(1:length(maplist(i))),typecorr(i)
	enddo
	endif
c---------------------------------------------------------------
c get inpdem map
	lm = length(inpdemmap)
	map = inpdemmap(1:lm)//null
 
	inquire (file=map, exist=exist)
	  if (.not.exist) Then
	print *,inpdem
	  print *,'The inpdem ',map(1:length(map)),' does not exist'
	  call exit(2)
	  endif
c	-------------------------------------------------------------------	
	call OPENMAPTYPE1 (map,ncol,nrow,typemap, xstart, ystart, cellsize)
	
	nval = ncol*nrow
 
	allocate (val(nval),inpdem(nval),inpmask(nval),beach(nval))
	inpdem=mv
	beach = mv1
 
	CALL GETVALMAP (inpdem)
	Call checkmv (inpdem,mv,inpmask,nval)
        inpmask = .not.inpmask
 
	print *,'ncol=',ncol,' nrow=',nrow
	print *,'inpdem: min=',minval(inpdem,mask=inpmask)
     &               ,' max=',maxval(inpdem,mask=inpmask)
 
        xf = xstart
        yf = ystart
	dx =  cellsize
	dy = -cellsize
c---------------------------------------------------------------
c get outdem map
	lm = length(outdemmap)
	map = outdemmap(1:lm)//null
	inquire (file=map, exist=exist)
	  if (.not.exist) Then
	  print *,'The outdem ',map(1:length(map)),' does not exist'
	  call exit(2)
	  endif
 
	call OPENMAPTYPE1 (map,ncol1,nrow1,typemap1
     &               , xstart1, ystart1, cellsize1)
	print *,'type=',typemap1
	npcr = ncol1*nrow1
 
	allocate (outdem(npcr),outmask(npcr))
	outdem=0.
 
	CALL GETVALMAP (outdem)
	Call checkmv (outdem,mv,outmask,npcr)
	
	do i = 1,npcr
	  if (outmask(i)) outdem(i) = -1000
	enddo
 
	outmask = .not.outmask
 
	print *,'ncol1=',ncol1,' nrow1=',nrow1
	print *,'outdem: min=',minval(outdem,mask=outmask)
     &              ,' max=',maxval(outdem,mask=outmask)
c---------------------------------------------------
c  x,y of the output maps (center of the PCRaster pixels)
 
	allocate (x(npcr),y(npcr))
	
	l=0
	do j = 1,nrow1
		do i = 1,ncol1
	    l = l+1
	    x(l) = xstart1 + (i-0.5)*cellsize1
	    y(l) = ystart1 - (j-0.5)*cellsize1
		enddo
	enddo
		
	allocate (p00(npcr),ip(npcr), jp(npcr))
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

 
c mask the sea
	  if (masksea) then
	    do i = 1,npcr
	    outmask(i) = outmask(i) .and. outdem(i) . gt . -50.
	    enddo
	  endif
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
 
	outmask = ip.ge.0. .and.ip.lt.float(ncol+1)
     &  .and.jp.ge.0. .and.jp.lt.float(nrow+1)
 
	 if (masksea) outmask = outmask .and. outdem . gt . -50.
 
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
c-----------------------------------------------------------------------
c create output directory if does not exist
c-----------------------------------------------------------------------
	if (inpdir.eq.' ') then
	print *,'You must specify the input directory'
	call exit(2)
	endif
 
 	linp = length(inpdir)
c-----------------------------------------------------------------------
c create output directory if does not exist
c-----------------------------------------------------------------------
	if (outdir.eq.' ') then
	print *,'You must specify the output directory'
	call exit(2)
	endif
 
	lout = length(outdir)
 
	inquire (file=outdir, exist=exist)
	    if (.not.exist) call system ('mkdir '//outdir)
c----------------------------------------------------------------
	allocate (demint(npcr),pcr(npcr))
	pcr = mv
c-----------------------------------------------------------------
c demint is the corresponding PCR dem obtained by
	demint = mv
 		if (spaceint) then
c  interpolating inp dem
	    demint(pp) = inpdem(p0)
     &         + xx(pp)*(inpdem(p1)-inpdem(p0))
     &         + yy(pp)*(inpdem(p2)-inpdem(p0))
     &         + xx(pp)*yy(pp)*(inpdem(p0)+inpdem(p3)
     &                       -inpdem(p1)-inpdem(p2))
	    else
c  using inp dem
	    demint(pp) = inpdem(p0)
		endif
c-----------------------------------------------------------------
c define input maskreq
	allocate (maskreq(nval))
	maskreq = .false.
	if (spaceint) then
	  do i = 1,nmask
	  maskreq(p0(i)) = .true.
	  maskreq(p1(i)) = .true.
	  maskreq(p2(i)) = .true.
	  maskreq(p3(i)) = .true.
	  enddo
 
	else
	  do i = 1,nmask
	  l = p0(i)
	  maskreq(l) = .true.
	  enddo
	endif
c-------------------------------------------------
c some maps for controls
	if (check) then
c inp dem
	map = outdir(1:lout)//bs//'demint.map'//null
	call  NEWMAPtype1 (map, demint, npcr,4)
 
c p00 pointers
        pcr(pp) = p0
	map = outdir(1:lout)//bs//'p00.map'//null
      call  NEWMAPTYPE1 (map, pcr, npcr,4)
	endif	
c-------------------------------------------------
c now read the data and resample maps
	imap = fmap
c+++++++++++++ start of the time steps loop ++++++++++++
 
	do j = first, last, stepmap
 
c extension of input map
	write (inpext,'(f5.3)') float(j)*0.001
	write (outext,'(f5.3)') float(imap)*0.001
 
 
c+++++++++++ maplist loop  ++++++++++++++++
	do 100 i = 1,nmaplist
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c Read the input map
	if (typecorr(i).ge.0) then
	fname = maplist(i)(1:7)//inpext
	call fillzero (fname,12)
	else
	  if (j.eq.first) then
	  fname = maps(i)
	  else
	  go to 100
	  endif
	endif
 
	inpmap = inpdir(1:linp)//bs//fname
	lm = length(inpmap)
 
	inquire (file=inpmap,exist=exist)
	  if (.not.exist) then
	  print *,'map ',inpmap(1:lm),' not exist'
	  go to 100
	  endif
	
	map = inpmap(1:lm)//null
c	print *,'inpmap=',map(1:length(map))
	call OPENMAPTYPE1 (map,ncol2,nrow2,typemap2
     &               , xstart2, ystart2, cellsize2)
	  if (ncol2.ne.ncol. or. nrow2.ne.nrow .or. cellsize2.ne.cellsize
     &      .or. xstart2.ne.xstart.or. ystart2.ne.ystart) then
		print *,'The inpmap ',inpmap(1:lm),' has not the same attribute'
     &           ,' of inpdem ',inpdemmap(1:length(inpdemmap))
	  call exit(2)
	  endif
 
	val=mv
 
	CALL GETVALMAP (val)
 
c------------------------------------------------
c	subroutine covervalgrid (mask,val,beach,findbeach,nmax
c     &                  ,nradius,idp,npcr,ncol,nrow)

c cover the values
	if (cover) then
	nradius = int(radius/cellsize + 0.5)
	call covervalgrid (maskreq,val,beach,findbeach,nmax
     &                  ,nradius,idp,nval,ncol,nrow)
	endif
c-------------------------------------------------------------------
c Interpolate if requested, otherwise assign neighborhood value
	pcr = mv
	  if (spaceint) then
	  pcr(pp) = val(p0) + xx(pp)*(val(p1)-val(p0))
     &        + yy(pp)*(val(p2)-val(p0))
     &        + xx(pp)*yy(pp)*(val(p0)+val(p3)-val(p1)-val(p2))
 	  else
	  pcr(pp) = val(p0)
	  endif
c-------------------------------------------------------------------
c correct in respect to new dem
	if (typecorr(i).eq.1) then
	    do ll = 1,nmask
	    l = pp(ll)
	    pcr(l) = pcr(l)-factcorr*(max(outdem(l),0.)-max(demint(l),0.))
	    enddo
	else if (typecorr(i).eq.2) then
	    do ll = 1,nmask
	    l = pp(ll)
	    pcr(l) = pcr(l)-factdcorr*(max(outdem(l),0.)-max(demint(l),0.))
	    enddo
	endif
c-------------------------------------------------------------------
c write the map	
	if (typecorr(i).ge.0) then
	fname = maplist(i)(1:7)//outext
	call fillzero (fname,12)
	else
	  if (j.eq.first) then
	  fname = maps(i)
	  else
	  go to 100
	  endif
	endif
 
	outmap = outdir(1:lout)//bs//fname
	lm = length(outmap)
 
	map = outmap(1:lm)//null
	call NEWMAPTYPE2 (map, ncol1,nrow1,4
     & 		   , xstart1+xshift, ystart1+yshift, cellsize1,pcr,npcr)
 
*	print *,'Inp=',inpmap(1:length(inpmap)),' Out=',map(1:length(map))
	print *,map(1:length(map))
     &  ,' min=',minval(pcr,mask=outmask)
     &  ,' max=',maxval(pcr,mask=outmask)
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
100	continue
c+++++++++++ end of variables loop +++++++++++++++++++++++++++
 
	imap = imap+stepmap
 
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	enddo
c+++++++++++++++ end of time step loop ++++++++++++++++++++++
 	if (cover) then
	if (findbeach) then
	print *,'caution! See the accumulated MV in new_beach.map'
	map = outdir(1:lout)//bs//'new_beach.map'//null
c	void NEWMAPTYPE2 (name, ncol,nrow,typ
c		   , xstart, ystart, cellsize,ldum,buf,nval)
	call NEWMAPTYPE2 (map, ncol,nrow,1
     & 		   , xstart1, ystart1, cellsize1,beach,nval)
 
	else
	print *,'all the MV have been covered !'
	endif	
	endif
c------------------------------------------------------------
      print *,'time spent [min] = ', (time()-it0)/60.
	stop
 	end
