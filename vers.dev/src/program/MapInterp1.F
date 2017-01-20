      program MapInterp1
*	USE DFlib
	implicit none
 
c-------------------------------------------------------------
c interface with maps
 
      integer*4  npcr, np, ncol, nrow, nmask
 
	real*4,    allocatable :: val(:),weightval(:),dist0(:)
	integer*1, allocatable :: imask(:)
        integer*4, allocatable :: beach(:)
	logical*1, allocatable :: mask(:)
        integer*4, allocatable :: mask4(:)
        real*4, allocatable :: rmask(:)


	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)
 
	character zero*8/'00000000'/
 
	real*4     mv /z'ffffffff'/
	integer*4  mv4, imv /z'ffffffff'/
	integer*1  mv1
	integer*4  typ
 
	real*8 xor, yor, size
 
c--------------------------------------------
c local variables
	integer*4 iout/10/, iargc, narg
     &          ,i,j,k, l,it0, time, nbeach, ival1, nmax,method,nv
	real*4 val1
	equivalence (ival1,val1)
	
	real*4 numget, idp, radius, dist1
 
	character clonemap*132, file*132, ch*132, inpmap*132
     &          ,base*5,outmap*132, msg*3
#if defined WIN32
     &          ,bs*1 /'\'/
#else
     &          ,bs*1 /'/'/
#endif
	integer*4 length, lm, lout, lb,lex,linp, nradius
	logical*4 ok, findbeach, distance, exist
        logical eqmv
c------------------------------------------------
	integer*4 nmap_max,nwild, imap, nmaps, ldir
	parameter (nmap_max=20000)
	
	character maps(nmap_max)*132, dir*132
     &          ,dirmaps(nmap_max)*132
     &         ,wildmap(nmap_max)*132, yesno*3
     &         ,weightmap*132
c-----------------------------------------------------------
        real*4 param(10), filter, clean, tsolve, tfit, typlag(100)
     &        , typlagtss(100)
        integer ncover,nlag,naver, typvar,nmaxexp,nparam, typfilt
        logical existweight,recalc
        real*4, allocatable :: typlagval(:), varval(:)
        integer*1, allocatable :: filtered(:)
        character pointerFile*132
c-----------------------------------------------------------
	call getmv1(mv1)
	call getmv4(mv4)
 
	narg = iargc()
	it0 = time()
	idp = 3.
	nmax = 5
	radius = 0.
        method = 0
        clean = 10.
        distance = .false.
        nv = 3
c----------------------------------------------
        param(1) = 0.
        param(2) = 1.
        param(3) = 1.
        param(4) = 1.
c----------------------------------------------
        weightmap = ' '
        clean = 10.
        filter = 10.
        pointerFile = ' '
        recalc = .false.
        typlag = 3
        nlag = 1
        typvar = 1
        typfilt = 0
c----------------------------------------------
	  if (narg.eq.0) then
 
	print *,' '
	print *,'MapInterp help '
 	print *,' '
      print *,'Cover the MV with interpolated values'
	print *,'using the similar PCRaster algorithm'
      print *,' (see also mars2map and mars2glacier)'
 
	print *,' '
	print *,'-m clonemap'
	print *,'   clonemap: mask map'
	print *,'   no DEFAULT'
	print *,' '
      print *,'-i list'
	print *,'   list of input map with MV (wild card are admitted)'
	print *,'   Caution: the maps must be scalar'
      print *,'   no DEFAULT'
	print *,' '
      print *,'-idp idp'
	print *,'   idp: exponent of inverse dist waighting value'
      print *,'   DEFAULT is ',idp
	print *,' '
      print *,'-nv nv'
      print *,'   DEFAULT is ',nv
	print *,' '
	print *,'-method method'
	print *,'   cover the MV of the requested STATION data '
	print *,'   0 weighted average'
	print *,'   1 weighted fit'
	print *,'   2 ordinary kriging'
	print *,'   3 cross kriging'
	print *,'   See parameters idp, radius, nmax for the weight'
	print *,'   DEFAULT is ',method
	print *,' '
	print *,'-typlag typlag[s]'
	print *,'   type of lag in exp. variogram: only for method = 2'
	print *,'   1 linear'
	print *,'   11 logarithmic'
	print *,'   2 spherical'
	print *,'   3 linear & exponential '
	print *,'   4 linear & quasi-exponential '
	print *,'   5 gaussian '
	print *,'   DEFAULT is ',typlag(1:nlag)

	print *,' '
	print *,'-typvar typvar'
	print *,'   type of variance function: only for method  2 and 3'
	print *,'   1: variance=(v1-v2)^2'
	print *,'   2: variance=log((v1-v2)^2)'
	print *,'   DEFAULT is ',typvar

	print *,' '
	print *,'-typfilt typfilt'
	print *,'   type of filtering'
	print *,'    0: filter under and over recalculated stations'
	print *,'   -1: filter under recalculated stations'
	print *,'    1: filter over recalculated stations'
	print *,'   DEFAULT is ',typfilt

	print *,' '
	print *,'-clean clean'
	print *,'   clean variogram: only for method 2 and 3'
	print *,'   DEFAULT is ',clean
	print *,' '
	print *,'-filter filter'
	print *,'   filter station data: for all methods'
	print *,'   DEFAULT is ',filter
	print *,' '
	print *,'-weight weightmap'
	print *,'   weightmap: map to weight the point data'
	print *,'   no DEFAULTS'
	print *,' '
	print *,'-recalc yes/no'
	print *,'   recalculate the NOT MV during the covering process'
	print *,'   DEFAULT is ',yesno(recalc)
	print *,' '
	print *,'-pointer pointer'
	print *,'   pointer: file name of the neighboroud pointers'
	print *,'   DEFAULT is ',pointerFile(1:length(pointerFile))
	print *,' '
	print *,'-radius radius'
	print *,'   radius: max distance [m] of the selected values to the MV'
	print *,'   DEFAULT is ',radius,' (looks to all values)'
	print *,' '
	print *,'-nmax nmax'
	print *,'   nmax: max no of selected values'
	print *,'   DEFAULT is ',nmax
        print *,' '
        print *,'-distance yes/no]'
        print *,'   generate distance map'
        print *,'  DEFAULT is ',yesno(distance)
	print *,' '
      print *,'-base base'
	print *,'   base name of the output map (last digits)'
      print *,'   DEFAULT is none (output map has the same name'
     &       ,' of the input map)'
 	print *,' '
 	print *,'NOTE: is you set -n 1 it will take the nearest value'
     & ,' in the area defined by radius'
 	print *,' '
	print *,'USAGE: '
	print *,'MapInterp1 -m clonemap -i list(scalar)'
     &  ,' [-idp idp] [-radius radius(meters)] [-nmax nmax]'
     &  ,' [-base basename] [-distance [yes/no]] '
#ifdef WIN32
        read *
#endif
	stop
	endif
c-----------------------------------------------------------------
c read arguments
	clonemap = ' '
	inpmap = ' '
	outmap = ' '
	base = ' '
 
	do i = 1,narg
	call getarg (i,ch)
	  if (ch.eq.'-m') then
	  call getarg (i+1,clonemap)
 
	  else if (ch.eq.'-i') then
		do j = i+1,narg
	    call getarg (j,ch)
		  if (ch(1:1).eq.'-') exit
	    wildmap(j-i) = ch
	    nwild = j-i
		enddo
 
	  else if (ch.eq.'-base') then
	  call getarg (i+1,base)
 
c	  --------------------------------------------------------
	  else if (ch.eq.'-distance') then
	  call getarg (i+1,ch)
		if (ch.eq.'yes') then
		distance = .true.
		else if (ch.eq.'no') then
		distance = .false.
		else
		distance = .true.
		endif
c         ----------------------------------------
	  else if (ch.eq.'-idp') then
	  call getarg (i+1,ch)
	  idp = numget(ch,msg)
c         ----------------------------------------
	  else if (ch.eq.'-nv') then
	  call getarg (i+1,ch)
	  nv = numget(ch,msg)
c         ----------------------------------------
	  else if (ch.eq.'-method') then
	  call getarg (i+1,ch)
	  method = numget(ch,msg)
c         ---------------------------------------------
	  else if (ch.eq.'-nmax') then
	  call getarg (i+1,ch)
	  nmax = numget(ch,msg)
c         ----------------------------------------
	  else if (ch.eq.'-radius') then
	  call getarg (i+1,ch)
	  radius = numget(ch,msg)	
c	  --------------------------------------------------------
	  else if (ch.eq.'-pointer') then
	  call getarg (i+1,pointerFile)
c	  --------------------------------------------------------
	  else if (ch.eq.'-weight') then
	  call getarg (i+1,weightmap)
c	  --------------------------------------------------------
	  else if (ch.eq.'-recalc') then
	  call getarg (i+1,ch)
		if (ch.eq.'yes') then
		recalc = .true.
		else if (ch.eq.'no') then
		recalc = .false.
		else
		recalc = .true.
		endif
c	  --------------------------------------------------------
	  else if (ch.eq.'-clean') then
	  call getarg (i+1,ch)
	  clean = numget(ch,msg)
c	  --------------------------------------------------------
	  else if (ch.eq.'-filter') then
	  call getarg (i+1,ch)
	  filter = numget(ch,msg)
c	  --------------------------------------------------------
	  else if (ch.eq.'-typlag') then
	    do j = 1,100
            call getarg (i+j,ch)
	      if (ch(1:1).eq.'-'.or.ch.eq.' ') then
	      nlag = j-1
	      exit
  	      endif
	    typlag(j) = numget(ch,msg)
	    enddo
c	  --------------------------------------------------------
	  else if (ch.eq.'-typvar') then
	  call getarg (i+1,ch)
	  typvar = numget(ch,msg)
c	  --------------------------------------------------------
	  else if (ch.eq.'-typfilt') then
	  call getarg (i+1,ch)
	  typfilt = numget(ch,msg)
c       ----------------------------------------------------
	  else if (ch.eq.'-param') then
	    do j = 1,10
            call getarg (i+j,ch)
	      if (ch(1:1).eq.'-'.or.ch.eq.' ') then
	      nparam = j-1
	      exit
  	      endif
	    param(j) = numget(ch,msg)
	    enddo
c         ----------------------------------------
	  endif
	enddo
c---------------------------------------------------------------
        nmaxexp = nmax
c---------------------------------------------------------------
      call filelist (nmap_max,wildmap,nwild,maps,nmaps)
c---------------------------------------------------------------
c get clone map
 
	if (clonemap.eq.' ') then
	print *,'the clone map has not been specified'
	stop
	endif
 
	lm = length(clonemap)
	file = clonemap(1:lm)//null
	call OPENMAPtype1 (file,ncol,nrow,typ, xor, yor, size)
	
	print *,'get ',clonemap(1:lm)
	print *,'ncol=',ncol,' nrow=',nrow,' typ=',typ

	npcr = ncol*nrow
	allocate (mask(npcr))
 
	if (typ.eq.1.or.typ.eq.6) then
	allocate (imask(npcr))
	call GETVALMAP (imask)
	  do i = 1,npcr
	  mask(i) = imask(i).ne.mv1 .and. imask(i).gt.0
	  enddo
	deallocate (imask)

	else if (typ.eq.2) then
	allocate (mask4(npcr))
	call GETVALMAP (mask4)
	  do i = 1,npcr
	  mask(i) = mask4(i).ne.mv4 .and. mask4(i).gt.0
	  enddo
	deallocate (mask4)

	else if (typ.eq.4) then
	allocate (rmask(npcr))
	call GETVALMAP (rmask)
	  do i = 1,npcr
	  mask(i) = .not.eqmv(rmask(i)) .and. rmask(i).gt.0
	  enddo
	deallocate (rmask)
	endif
	
 
 
c mask.map=boolean(code.map)
 
c---------------------------------------------------
c get weight
	allocate (weightval(npcr))

	if (weightmap.ne.' ') then
	file = weightmap(1:length(weightmap))//null
	inquire (file=file, exist=exist)
	  if (.not.exist) Then
	  print *,' '
	print *,'Error: the map ',file(1:length(file)-1),' does not exist'
	  call exit(2)
	  endif
 
	call OPENMAPTYPE1 (file,ncol,nrow,typ, xor, yor,size)
 
	CALL GETVALMAP (weightval)
        existweight = .true.

        else
        existweight = .false.
        weightval = 1.
	endif
c---------------------------------------------------------------
 
	  if (radius.eq.0.) radius = max(ncol,nrow)*size
 
	nradius = int(radius/size + 0.5)
	print *,'nradius=',nradius
 
c---------------------------------------------------------------
c get input maps
c---------------------------------------------------------------
	dir = ' '
	allocate (val(npcr),beach(npcr),dist0(npcr))
        allocate (typlagval(npcr),varval(npcr),filtered(npcr))

	beach = 0
	findbeach = .false.
 
	do imap = 1,nmaps
	
	inpmap = maps(imap)
 
	if (inpmap.eq.' ') then
	print *,'the input map has not been specified'
	stop
	endif
 
	linp = length(inpmap)
	file = inpmap(1:linp)//null
 
	call OPENMAPtype1 (file,ncol,nrow,typ, xor, yor, size)
 
c	print *,'get ',inpmap(1:linp)
c	print *,'ncol=',ncol,' nrow=',nrow,' typ=',typ
 
	if (nrow*ncol.ne.npcr) then
	print *,'the clone map and the input map have not the '
     &       ,'same no. of pixel'
	stop
	endif
 
		if (typ.ne.4) then
		print *,'The input map must be a  real*4 map'
		stop
		endif
 
	call GETVALMAP (val)
c------------------------------------------------------------------

	call covervalstat (mask,val,beach,findbeach,nmax,nmaxexp
     &                  ,nradius,idp,npcr,ncol,nrow,recalc,method,nv
     &                  ,clean,filter,naver,ncover,typlag,nlag,typvar,typfilt
     &                  ,param,existweight,weightval,dist0,pointerFile
     &                  ,typlagval,typlagtss
     &                  ,filtered,varval,tfit,tsolve)
c------------------------------------------------------------------
c	print *,'base=',base

        

	if (base.eq.' ') then
	outmap = inpmap
 
	else
	lm = length(base)

          do i = 1,linp
 	    if (inpmap(i:i).eq.'.') then
	    lb = min(i-1,8-lm)
            lex = i
            exit
            endif
          enddo

          if (lex.gt.9) then
	  do i = 1,linp
	    if (inpmap(i:i+lm-1).eq.zero(1:lm)) then
	    lb = min(i-1,8-lm)
	    lex = min(lb+lm,8)+1
            exit
            endif
	  enddo
          endif
 
c	print *,'lb=',lb,' lm=',lm,' lex=',lex,' linp=',linp
	outmap = inpmap(1:lb)//base(1:lm)//inpmap(lex:linp)
	endif
 
	
	if (outmap.ne.' ') then
	lm = length(outmap)
	print *,'inpmap=',inpmap(1:lm),' outmap=',outmap(1:lm)
	file = outmap(1:lm)//null
	call NEWMAPTYPE1 (file, val, npcr,4)
	endif
	
	enddo	
 
c------------------------------------------------------------------
        if (distance) then
	file = 'idist.map'//null
	call NEWMAPTYPE1 (file, dist0, npcr,4)
	file = 'dist.map'//null
        dist0 = dist0*size/1e3
	call NEWMAPTYPE1 (file, dist0, npcr,4)
        endif
c------------------------------------------------------------------
	if (findbeach) then
	print *,'caution! See in new_beach.map the accumulated MV'
	file = 'new_beach.map'//null
	call NEWMAPTYPE1 (file, beach, npcr,2)

 
	else
	print *,'all the MV have been covered !'
	endif	
	
c------------------------------------------------------------------
      print *,'time spent [min] = ', (time()-it0)/60.
 
	stop
	end
