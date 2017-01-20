      program mapind
	implicit none
 
c-------------------------------------------------------------
c interface with maps
 
      integer*4  npcr, np, ncol, nrow, nmask
 
	integer*4, allocatable :: ind(:), ll(:,:), river(:), dow(:)
     &         ,ii(:),jj(:), from(:,:), iups(:), outlet(:), nfrom(:)
	real*4, allocatable :: x(:), y(:), slx(:), sly(:), sll(:), slf(:)
     & ,rind(:),ups(:),dem(:), leng(:), ango(:)
     & , sla(:), bot(:),demcorr(:)
	integer*1, allocatable :: ldd(:), ldd0(:), mainriver(:),lddxy(:)
	logical*1, allocatable :: mask(:)
 
	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)
 
	real*4     mv /z'ffffffff'/, ang, conv, ang1, dbot, numget, minval
     &       , upsmin
	parameter (conv=180/3.14159265)
	integer*4  mv4, imv/z'ffffffff'/
	integer*1  mv1
c--------------------------------------------
c local variables
	integer*4 iout/10/, iargc, narg, typ, im1, ip1, jm1, jp1, icount
     &          ,i,j,k,l,m,n,it0, time, il, jl, ir, jr, ito, jto, nriver
     &          ,l1,l2,lddx(9),lddy(9), ix,iy, jx,jy, lx, ly
 
	real*8 xor, yor, size, dsize, sl, sr
	
	character demmap*132, maps*132, ch*132, indmap*132, lddmap*132
     & ,upsmap*132,xmap*132, ymap*132, slopelmap*132, slopefmap*132
     & , slopexmap*132, slopeymap*132, lengthmap*132, demcorrmap*132
     & ,slopemap*132, angmap*132, rivermap*132, botmap*132,lddxymap*132
     & , mainrivermap*132, outletmap*132, msg*3
#if defined WIN32
     &          ,bs*1 /'\'/
#else
     &          ,bs*1 /'/'/
#endif
	integer*4 length, lm
	
	logical*4 ok, exist, eqmv, notnode
c-----------------------------------------------------------
	narg = iargc()
	it0 = time()
	call getmv1(mv1)
	call getmv4(mv4)
 
	  if (narg.eq.0) then
 
	print *,' '
	print *,'mapind help'
	print *,' '
	print *,' Input'
	print *,' '
	print *,'-dem dem map'
	print *,'   DEFAULT is dem.map'
	print *,' '
	print *,'-ldd ldd map'
	print *,'   no DEFAULT '
	print *,' '
	print *,'-river rivers map (nominal map with values > 0)'
	print *,'   if river map is not defined, bot and slopel maps'
     &       ,' are not generated'
	print *,'   no DEFAULT'
	print *,' '
	print *,'-ups upstream area map'
	print *,'   if ups map is not defined, mainriver map is not generated'
	print *,'   no DEFAULT'
	print *,' '
	print *,'-upsmin minimum upstream area'
	print *,'   DEFAULT is ',upsmin
	print *,' '
	print *,'-outlet outlets map (nominal map with values > 0)'
	print *,'   if outlet map is not defined,'
     &       ,' mainriver map is not generated'
	print *,'   no DEFAULT'
	print *,' '
	print *,' Output'
	print *,' '
      print *,'-index index map'
      print *,'   no DEFAULT '
	print *,' '
      print *,'-x x-coord. map'
      print *,'   no DEFAULT '
	print *,' '
      print *,'-y y coord. map'
      print *,'   no DEFAULT '
	print *,' '
      print *,'-slopex dem slope in x direction'
      print *,'   no DEFAULT '
	print *,' '
      print *,'-slopey dem slope in y direction'
      print *,'   no DEFAULT '
	print *,' '
      print *,'-slope max dem slope: sqrt(slopex^2+slopey^2)'
      print *,'   no DEFAULT '
	print *,' '
      print *,'-ang direction of max dem slope (0-360 deg) '
      print *,'   no DEFAULT '
	print *,' '
      print *,'-lddxy force ldd to follow x & y direction '
      print *,'   no DEFAULT '
	print *,' '
      print *,'-slopef dem slope in LDD direction (can be negative)'
      print *,'   no DEFAULT '
	print *,' '
      print *,'-slopel dem slope in lateral LDD direction'
      print *,'   no DEFAULT '
	print *,' '
	print *,'-lenght distance to the downstream cell'
	print *,'   no DEFAULT '
	print *,' '
	print *,'-bot corrected bottom of the rivers (remove pecks)'
	print *,'     (river map must be defined)'
	print *,'   no DEFAULT'
	print *,' '
	print *,'-mainriver main river ldd map '
     &       ,'(follow ldd from outlets to upstream)'
	print *,'     (ups and outlets maps must be defined)'
	print *,'   no DEFAULT'
	print *,' '
	print *,'-demcorr remove pit'
	print *,'   no DEFAULT'
#if defined WIN32
	read *
#endif
	stop
	endif
c-----------------------------------------------------------------
c read arguments
	demmap = 'dem.map'
	lddmap = ' '
	rivermap = ' '
	indmap = ' '
	xmap = ' '
	ymap = ' '
	slopexmap = ' '
	slopeymap = ' '
	slopelmap = ' '
	slopefmap = ' '
	lengthmap = ' '
	slopemap = ' '
	angmap   = ' '
	lddxymap   = ' '
	demcorrmap = ' '
	botmap = ' '
	upsmap = ' '
	mainrivermap = ' '
	outletmap = ' '
	dbot=0
	upsmin = 1e35
 
	do i = 1,narg
	call getarg (i,ch)
	  if (ch.eq.'-dem') then
	  call getarg (i+1,demmap)
 
	  else if (ch.eq.'-ldd') then
	  call getarg (i+1,lddmap)
 
	  else if (ch.eq.'-river') then
	  call getarg (i+1,rivermap)
 
	  else if (ch.eq.'-ups') then
	  call getarg (i+1,upsmap)
 
	  else if (ch.eq.'-outlet') then
	  call getarg (i+1,outletmap)
 
	  else if (ch.eq.'-mainriver') then
	  call getarg (i+1,mainrivermap)
 
	  else if (ch.eq.'-index') then
	  call getarg (i+1,indmap)
 
	  else if (ch.eq.'-x') then
	  call getarg (i+1,xmap)
 
	  else if (ch.eq.'-y') then
	  call getarg (i+1,ymap)
 
	  else if (ch.eq.'-slopex') then
	  call getarg (i+1,slopexmap)
 
	  else if (ch.eq.'-slopey') then
	  call getarg (i+1,slopeymap)
 
	  else if (ch.eq.'-slope') then
	  call getarg (i+1,slopemap)
 
	  else if (ch.eq.'-ang') then
	  call getarg (i+1,angmap)
 
	  else if (ch.eq.'-lddxy') then
	  call getarg (i+1,lddxymap)
 
	  else if (ch.eq.'-slopel') then
	  call getarg (i+1,slopelmap)
 
	  else if (ch.eq.'-slopef') then
	  call getarg (i+1,slopefmap)
 
	  else if (ch.eq.'-length') then
	  call getarg (i+1,lengthmap)
 
	  else if (ch.eq.'-bot') then
	  call getarg (i+1,botmap)
 
	  else if (ch.eq.'-dbot') then
	  call getarg (i+1,ch)
	  dbot = numget(ch,msg)
 
	  else if (ch.eq.'-upsmin') then
	  call getarg (i+1,ch)
	  upsmin = numget(ch,msg)
 
	  else if (ch.eq.'-demcorr') then
	  call getarg (i+1,demcorrmap)
	  endif
	enddo
c---------------------------------------------------------------
c get dem map and ldd
 
	if (demmap.eq.' ') then
	print *,'the dem map has not been specified'
	stop
	endif
 
	lm = length(demmap)
	maps = demmap(1:lm)//null
 
	call OPENMAPtype1 (maps,ncol,nrow,typ, xor, yor, size)
 
	print *,'ncol=',ncol,' nrow=',nrow
	
	npcr = ncol*nrow
 
	allocate (ind(npcr),mask(npcr),dem(npcr),rind(npcr))
 
	call GETVALMAP (dem)
 
	print *,'get dem'
c    -------------------------------------------------
 
	call checkmv (dem,mv,mask,npcr)
 
*       do l = 1,npcr
*       if (mask(l)) dem(l)=0.
*       if (dem(l).lt.0.) dem(l)=0.
*       enddo
 
 
c---------------------
c write x, y, slx, sly, index maps
 
	dsize = 0.5d0/size
 
	allocate (x(npcr),y(npcr),ll(ncol,nrow),ii(npcr),jj(npcr))
 
	l = 0
	do j = 1,nrow
		do i = 1,ncol
		l = l+1
		ind(l) = l
	    ll(i,j) = l
	    x(l) = xor + (i-0.5)*size
	    y(l) = yor - (j-0.5)*size
	    ii(l) = i
	    jj(l) = j
		enddo
	enddo
 
	
	if (indmap.ne.' ') then
	lm = length(indmap)
	maps = indmap(1:lm)//null
	print *,'Created ',maps(1:lm)
	rind = ind
	call NEWMAPTYPE1 (maps, rind, npcr,4)
	endif
	
	if (xmap.ne.' ') then
	lm = length(xmap)
	maps = xmap(1:lm)//null
	print *,'Created ',maps(1:lm)
	call NEWMAPTYPE1 (maps, x, npcr,4)
	endif
	
	if (ymap.ne.' ') then
	lm = length(ymap)
	maps = ymap(1:lm)//null
	print *,'Created ',maps(1:lm)
	call NEWMAPTYPE1 (maps, y, npcr,4)
	endif
	
 
        deallocate (rind,ind)
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

        if (slopexmap.ne.' '.or.slopeymap.ne.' '.or.slopemap.ne.' '
     &       .or.angmap.ne.' ') then

	allocate (slx(npcr),sly(npcr),ango(npcr),sla(npcr))
	slx = mv
	sly = mv
	sla = mv
	slf = mv
	leng = mv
	ango = mv
 
	do i = 1,ncol
	  do 10 j = 1,nrow
	  l = ll(i,j)
	    if (mask(l)) go to 10
	  im1 = max(i-1,1)
	  ip1 = min(i+1,ncol)
	  l1 = ll(ip1,j)
	  l2 = ll(im1,j)

            if (mask(l1)) l1 = l
            if (mask(l2)) l2 = l
	    if (l1.ne.l2) then
          slx(l) = dsize*(dem(l1)-dem(l2))
              if (l1.eq.l.or.l2.eq.l) slx(l)=slx(l)*2
	    else
	    slx(l) = 0.
	    endif

	  jm1 = max(j-1,1)
	  jp1 = min(j+1,nrow)
	  l1 = ll(i,jp1)
	  l2 = ll(i,jm1)
            if (mask(l1)) l1 = l
            if (mask(l2)) l2 = l
	    if (l1.ne.l2) then
	    sly(l) = -dsize*(dem(l1)-dem(l2))
              if (l1.eq.l.or.l2.eq.l) sly(l)=sly(l)*2
	    else
	    sly(l) = 0.
	    endif
 
          if (eqmv(slx(l)).or.eqmv(sly(l)))  go to 10
 
	  sla(l) = sqrt(slx(l)*slx(l)+sly(l)*sly(l))
 
          if (sla(l).ne.0.) then
          ango(l) = ang(slx(l),sly(l),0.,0.)
          else
          ango(l) = -1
          endif
 
10	  continue
	enddo
	
	if (slopexmap.ne.' ') then
	lm = length(slopexmap)
	maps = slopexmap(1:lm)//null
	print *,'Created ',maps(1:lm)
 
	call NEWMAPTYPE1 (maps, slx, npcr,4)
	endif
	
	if (slopeymap.ne.' ') then
	lm = length(slopeymap)
	maps = slopeymap(1:lm)//null
	print *,'Created ',maps(1:lm)
	call NEWMAPTYPE1 (maps, sly, npcr,4)
	endif
	
	if (slopemap.ne.' ') then
	lm = length(slopemap)
	maps = slopemap(1:lm)//null
	print *,'Created ',maps(1:lm)
	call NEWMAPTYPE1 (maps, sla, npcr,4)
	endif
	
	if (angmap.ne.' ') then
	lm = length(angmap)
	maps = angmap(1:lm)//null
	print *,'Created ',maps(1:lm)
	call NEWMAPTYPE1 (maps, ango, npcr,5)
	endif
 
 
	deallocate (slx,sly,sla,ango)
        endif
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	if (demcorrmap.eq.' ') go to 1000
	allocate (demcorr(npcr))
	demcorr=dem
 
	do i = 2,ncol-1
	  do 1200 j = 2,nrow-1
	  l = ll(i,j)
	  if (mask(l)) go to 1200
	
	minval=1e35
	do m = i-1,i+1
	do 1250 n = j-1,j+1
	k = ll(m,n)
	  if(k.eq.l) go to 1250
	  if(mask(k)) go to 1200
	  if(dem(k).ge.minval) goto 1250
	minval = dem(k)
1250	continue
	enddo
 
	if (minval.le.dem(l)) goto 1200
	if (minval.eq.1e35) goto 1200
 
	demcorr(l) = minval
        print *,'l=',l,' dem=',dem(l),' demcorr=',demcorr(l)
 
1200	  continue
	enddo
 
	lm = length(demcorrmap)
	maps = demcorrmap(1:lm)//null
	print *,'Created ',maps(1:lm)
	call NEWMAPTYPE1 (maps, demcorr, npcr,4)
        deallocate (demcorr)
 
c	-----------------------
1000	if (lddmap.eq.' ') then
	print *,'the ldd map has not been specified'
	go to 1100
	endif
 
	allocate (ldd(npcr),ldd0(npcr))
 
	lm = length(lddmap)
	maps = lddmap(1:lm)//null
 
	print *,'open ldd'
	call OPENMAPtype1 (maps,ncol,nrow,typ, xor, yor, size)
 
	call GETVALMAP (ldd)
	print *,'get ldd'
 
        ldd0 = ldd
            do l = 1,npcr
	      if (ldd(l).eq.mv1) ldd(l)=5
	    enddo
 
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	if (lddxymap.ne.' ') then
	allocate (lddxy(npcr))
         lddxy = ldd
 
         lddx(1) = 4
         lddx(3) = 6
         lddx(7) = 4
         lddx(9) = 6
         lddy(1) = 2
         lddy(3) = 2
         lddy(7) = 8
         lddy(9) = 8
 
	do i = 1,ncol
	  do 600 j = 1,nrow
	  l = ll(i,j)
 
   	    if (ldd(l).eq.5) go to 600
            if (mod(ldd(l),2).eq.0) goto 600

	    call lddind (ldd(l),i,j,nrow,ncol,ito,jto,il,jl,ir,jr)
	    k = ll(ito,jto)
c-----------------------------------------------------------------------
c lddxy
          lx = ll(ito,j)
	      if (ldd(lx).eq.5) goto 610
	    call lddind (ldd(lx),ito,j,nrow,ncol,ix,jx,il,jl,ir,jr)
 
          ly = ll(i,jto)
	      if (ldd(ly).eq.5) goto 620
	    call lddind (ldd(ly),i,jto,nrow,ncol,iy,jy,il,jl,ir,jr)
 
	      if (ll(ix,jx).eq.k.and.ll(iy,jy).eq.k) goto 630
 
            if (ll(ix,jx).eq.k) goto 610
            if (ll(iy,jy).eq.k) goto 620
c-----------------------------------------------------------------------
            if (ll(ix,jx).eq.l) then
	      lddxy(lx) = lddy(ldd(l))
	      goto 610
	      endif
 
            if (ll(iy,jy).eq.l) then
	      lddxy(ly) = lddx(ldd(l))
	      goto 620
	      endif
c-----------------------------------------------------------------------
	      if (ldd(k).eq.5) goto 600
	    call lddind (ldd(k),ito,jto,nrow,ncol,ito,jto,il,jl,ir,jr)
 
            if (ll(ito,jto).eq.lx) goto 610
            if (ll(ito,jto).eq.ly) goto 620
c-----------------------------------------------------------------------
            if (dem(lx).lt.dem(ly)
     &     .and.notnode(ldd,ll,ii,jj,nrow,ncol,lx,l)) goto 610
            if (notnode(ldd,ll,ii,jj,nrow,ncol,ly,l)) goto 620
c-----------------------------------------------------------------------
          goto 600
c-----------------------------------------------------------------------
 
630         if (dem(lx).lt.dem(ly)) goto 610
	    goto 620
 
610       lddxy(l) = lddx(ldd(l))
c	    lddxy(lx) = lddy(ldd(l))
          goto 600
620       lddxy(l) = lddy(ldd(l))
c	    lddxy(ly) = lddx(ldd(l))
          goto 600
c-----------------------------------------------------------------------
 
600       continue
	enddo
 
	
	lm = length(lddxymap)
	maps = lddxymap(1:lm)//null
	print *,'Created ',maps(1:lm)
          do i = 1,npcr
            if (ldd0(i).eq.mv1) lddxy(i) = mv1
          enddo
	call NEWMAPTYPE1 (maps, lddxy, npcr,6)
        deallocate (lddxy)
	endif
c-------------------------------------------------------------------
	if (slopefmap.ne.' '.or.lengthmap.ne.' ') then
	allocate (slf(npcr),leng(npcr))
	do i = 1,ncol
	  do j = 1,nrow
	  l = ll(i,j)
 
   	    if (ldd(l).eq.5) then
	    slf(l) = 0.
	    leng(l) = 0.
 
	    else
	    call lddind (ldd(l),i,j,nrow,ncol,ito,jto,il,jl,ir,jr)
	    k = ll(ito,jto)
	    leng(l) = sqrt((x(k)-x(l))**2 + (y(k)-y(l))**2)
	    slf(l)  = (dem(l)-dem(k))/leng(l)
            endif
          enddo
        enddo
 
	if (slopefmap.ne.' ') then
	lm = length(slopefmap)
	maps = slopefmap(1:lm)//null
	print *,'Created ',maps(1:lm)
	call NEWMAPTYPE1 (maps, slf, npcr,4)
	endif
 
	if (lengthmap.ne.' ') then
	lm = length(lengthmap)
	maps = lengthmap(1:lm)//null
	print *,'Created ',maps(1:lm)
	call NEWMAPTYPE1 (maps, leng, npcr,4)
	endif

	deallocate (slf,leng,x,y)

        endif
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	deallocate (ldd0)
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	if (mainrivermap.eq.' ') go to 500
	
	if (upsmap.eq.' ') then
	print *,'ups map not defined'
	go to 500
	endif
 
	
	if (outletmap.eq.' ') then
	print *,'outlet map not defined'
	go to 500
	endif
	
	allocate (outlet(npcr),ups(npcr),mainriver(npcr),from(0:8,npcr)
     &         ,iups(npcr),nfrom(npcr))
 
	lm = length(upsmap)
	inquire (file=upsmap,exist=exist)
	  if (.not.exist) then
	  print *,'file ',upsmap(1:lm),' not exist'
	  goto 500
	  endif
 
	maps = upsmap(1:lm)//null
	call OPENMAPtype1 (maps,ncol,nrow,typ, xor, yor, size)
	call GETVALMAP (ups)
	call OPENMAPtype1 (maps,ncol,nrow,typ, xor, yor, size)
	call GETVALMAP (iups)
 
	lm = length(OUTLETmap)
	inquire (file=OUTLETmap,exist=exist)
	  if (.not.exist) then
	  print *,'file ',outletmap(1:lm),' not exist'
	  goto 500
	  endif
 
	maps = outletmap(1:lm)//null
	call OPENMAPtype1 (maps,ncol,nrow,typ, xor, yor, size)
	call GETVALMAP (outlet)
 
c define from pointers according to upstream area
	
	do l = 1,npcr
	nfrom(l) = -1
	do j = 0,8
	from(j,l) = mv4
	enddo
	enddo
 
	do i = 1,ncol
	  do 550 j = 1,nrow
	  l = ll(i,j)
		if (ldd(l).eq.5.or.ldd(l).eq.mv1.or.iups(l).eq.imv) go to 550
 
	  call lddind (ldd(l),i,j,nrow,ncol,ito,jto,il,jl,ir,jr)
	  k = ll(ito,jto)
c	      if (iups(k).eq.mv4) go to 550
 
	      if (nfrom(k).eq.-1) then
	      from(0,k) = l
	      from(1,k) = l
	      nfrom(k) = 1
	      else
	      nfrom(k) = nfrom(k) + 1
	        if (nfrom(k).gt.8) print *,'k=',k,' n=',nfrom(k)
	      from(nfrom(k),k) = l
	        if (ups(l).gt.ups(from(0,k))) from(0,k) = l
	      endif
 
550	    continue
	enddo
 
c estract main river according to outlets and from pointers
	mainriver = mv1
 
	do 510 l = 1, npcr
	  if (outlet(l).eq.mv4) go to 510
	  if (nfrom(l).eq.-1) go to 510
 
c it's an outlet
 
	do 530 j = 0,nfrom(l)

	k = from(j,l)
	  if (k.eq.mv4) go to 530
        if (ups(k).le.upsmin) go to 530
c	  if (j.ge.1.and.from(j,k).eq.from(0,k)) go to 530
 
	mainriver(l) = 5
	mainriver(k) = ldd(k)
 
520   continue
	k = from(0,k)
	  if (k.eq.mv4) go to 530
	mainriver(k) = ldd(k)
	go to 520	

530	continue
 
 
510	continue
 
	lm = length(mainrivermap)
	maps = mainrivermap(1:lm)//null
	print *,'Created ',maps(1:lm)
	call NEWMAPTYPE1 (maps, mainriver, npcr,6)
 
	deallocate (outlet,ups,mainriver,from,iups)
	
c-------------------------------------------------
500	if (rivermap.eq.' ') go to 1100
 
	allocate (dow(npcr),bot(npcr),river(npcr))
 
 
	lm = length(rivermap)
	maps = rivermap(1:lm)//null
	
	print *,'open river'
	call OPENMAPtype1 (maps,ncol,nrow,typ, xor, yor, size)
 
	call GETVALMAP (river)
	print *,'get river'
c----------------------------------------------
 
	dow = 0
 
	do 200 l = 1,npcr
 
		if (ldd(l).eq.5) go to 200
	    if (dow(l).lt.0) go to 200
	    if (river(l).eq.mv4) go to 200
	    if (river(l).le.0) go to 200
 
	i = mod(l-1,ncol) + 1
	j = (l-1)/ncol + 1
	call lddind (ldd(l),i,j,nrow,ncol,ito,jto,il,jl,ir,jr)
	k = ll(ito,jto)
	dow(l) = k
 
	  if (ldd(k).eq.5) go to 200
	  if (dow(k).gt.0) then
	  dow(k) = -dow(k)
	  go to 200
	  endif
	
100	continue	
	i = ito
	j = jto
	call lddind (ldd(k),i,j,nrow,ncol,ito,jto,il,jl,ir,jr)
	m = ll(ito,jto)
	dow(k) = -m
		
	    if (ldd(m).eq.5) go to 200
		if (dow(m).gt.0) then
	    dow(m) = - dow(m)
	    go to 200
	    endif
 
	k = m
	go to 100
 
200	continue
 
c-----------------------------------------------------------------------
 
	bot = mv
 
	do 210 l = 1,npcr
 
	    if (river(l).eq.mv4) go to 210
	    if (river(l).le.0) go to 210
	bot(l) = dem(l)
210	continue
 
 
	do 300 l = 1,npcr
	m = l
	k = dow(m)
	  if (k.le.0) go to 300
 
	bot(m) = dem(m)
 
400	continue	
	bot(k) = min(bot(m)-dbot,dem(k))
c	dow(m) = 0
	m = k
	k = -dow(m)
	  if (k.gt.0) go to 400
 
300	continue
c     ----------------------------
c repeat in reverse order
 
	do 310 l = npcr,1,-1
	m = l
	k = dow(m)
	  if (k.le.0) go to 310
 
c	bot(m) = dem(m)
 
410	continue	
c	bot(k) = min(bot(m),dem(k))
	bot(k) = min(bot(m)-dbot,bot(k))
c	dow(m) = 0
	m = k
	k = -dow(m)
	  if (k.gt.0) go to 410
 
310	continue
c---------------------------------------		
	if (botmap.ne.' ') then
	lm = length(botmap)
	maps = botmap(1:lm)//null
	print *,'Created ',maps(1:lm)
	call NEWMAPTYPE1 (maps, bot, npcr,4)
	endif
c-------------------------------------------------------------------
 
	if (slopelmap.eq.' ') go to 1100
 
	allocate (sll(npcr))
 
	if (botmap.eq.' ') then
	print *,' if bot map is not requested, the slopel map cannot be done'
	go to 1100
	endif
 
 
	do i = 1,ncol
	  do j = 1,nrow
	  l = ll(i,j)
	    if (river(l).eq.mv4.or.river(l).le.0) then
	    sll(l) = mv
		else if (ldd(l).eq.5) then
		sll(l) = 0.
		else
		call lddind (ldd(l),i,j,nrow,ncol,ito,jto,il,jl,ir,jr)
		k = ll(il,jl)
		sl = abs(dem(k)-bot(l))/sqrt((x(k)-x(l))**2 + (y(k)-y(l))**2)
		k = ll(ir,jr)
		sr = abs(dem(k)-bot(l))/sqrt((x(k)-x(l))**2 + (y(k)-y(l))**2)
		sll(l) = 0.5*(sl+sr)
	    endif
	  enddo
	enddo
 
	if (slopelmap.ne.' ') then
	lm = length(slopelmap)
	maps = slopelmap(1:lm)//null
	print *,'Created ',maps(1:lm)
	call NEWMAPTYPE1 (maps, sll, npcr,4)
	endif
c------------------------------------------------------------------
1100  print *,'time spent [min] = ', (time()-it0)/60.
 
	stop
	end
C
      function notnode (ldd,ll,ii,jj,nrow,ncol,lstart,lfrom)
	implicit none
	integer*4 i,j,l,lstart,lfrom,nrow,ncol,ito,jto,il,jl,ir,jr
	integer*1 ldd(nrow*ncol)
	integer*4 ll(ncol,nrow),ii(ncol*nrow),jj(ncol*nrow)
	logical notnode
 
	notnode=.true.
	l=lstart
	do while (notnode)
	call lddind (ldd(l),ii(l),jj(l),nrow,ncol,ito,jto,il,jl,ir,jr)
	l=ll(ito,jto)
	notnode=l.ne.lfrom
	if (ldd(l).eq.5) return
	enddo
	return
	end
