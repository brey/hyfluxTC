		program MapInterp
	implicit none
 
c-------------------------------------------------------------
c interface with maps
 
      integer*4  npcr, np, ncol, nrow, nmask
 
	real*4,    allocatable :: val(:),dist(:)
	integer*1, allocatable :: imask(:), beach(:)
	logical*1, allocatable :: mask(:)
        integer*4, allocatable :: mask4(:)
        real*4, allocatable :: rmask(:)
	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)
 
	character zero*8/'00000000'/
 
	real*4     mv 
	integer*4  mv4, imv 
	integer*1  mv1
	integer*4  typ
 
	real*8 xor, yor, size
 
c--------------------------------------------
c local variables
	integer*4 iout/10/, iargc, narg
     &          ,i,j,k, l,it0, time, nbeach, ival1, nmax
	real*4 val1
	equivalence (ival1,val1)
	
	real*4 numget, idp, radius, dist1
 
	character clonemap*132, file*132, ch*132, inpmap*132
     &          ,base*5,outmap*132, msg*3, ext*20
#if defined WIN32
     &          ,bs*1 /'\'/
#else
     &          ,bs*1 /'/'/
#endif
	integer*4 length, lm, lout, lb,lex,linp, nradius
	logical*4 ok, findbeach, eqmv, noclone, done
c------------------------------------------------
	integer*4 nmap_max,nwild, imap, nmaps, ldir
	parameter (nmap_max=20000)
	
	character maps(nmap_max)*132, dir*132
     &          ,dirmaps(nmap_max)*132
     &         ,wildmap(nmap_max)*132
c--------------------------------------------
* gdal additional variables
        integer*4 nband, iband,error, nbyte
        character driver*64, datatype*64, inpfmt*64, outfmt*64
	real*8 sizey
c-----------------------------------------------------------
	call getmv(mv)
	call getmv(imv)
	call getmv1(mv1)
	call getmv4(mv4)
 
	narg = iargc()
	it0 = time()
	idp = 3.
	nmax = 5
	radius = 0.
        noclone = .false.
 
	  if (narg.eq.0) then
 
	print *,' '
	print *,'MapInterp help '
 	print *,' '
      print *,'Cover the MV with interpolated values'
	print *,'using the similar PCRaster algorithm'
      print *,' produces also dist.map and idist.map'
 
	print *,' '
	print *,'-m clonemap'
	print *,'   clonemap: mask map'
	print *,'   DEFAULT is the first input map'
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
	print *,'-radius radius'
	print *,'   radius: max distance [m] of the selected values to the MV'
	print *,'   DEFAULT is ',radius,' (looks to all values)'
	print *,' '
	print *,'-nmax nmax'
	print *,'   nmax: max no of selected values'
	print *,'   DEFAULT is ',nmax
	print *,' '
      print *,'-base base'
	print *,'   base name of the output map (last digits)'
      print *,'   DEFAULT is none (output map has the same name'
     &       ,' of the input map)'
	print *,' '
      print *,'-ext ext'
	print *,'   extension name of the output map (last digits)'
      print *,'   no DEFAULT (output map has the same name'
     &       ,' of the input map)'
 	print *,' '
 	print *,'NOTE: is you set -n 1 it will take the nearest value'
     & ,' in the area defined by radius'
 	print *,' '
	print *,'USAGE: '
	print *,'MapInterp -m clonemap -i list(scalar)'
     &  ,' [-idp idp] [-radius radius(meters)] [-nmax nmax]'
     &  ,' [-base basename] [-ext extension] '
 
        read *
	stop
	endif
c-----------------------------------------------------------------
c read arguments
	clonemap = ' '
	inpmap = ' '
	outmap = ' '
	base = ' '
        ext = ' '
 
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
 
	  else if (ch.eq.'-ext') then
	  call getarg (i+1,ext)
 
	  else if (ch.eq.'-idp') then
	  call getarg (i+1,ch)
	  idp = numget(ch,msg)
 
	  else if (ch.eq.'-nmax') then
	  call getarg (i+1,ch)
	  nmax = numget(ch,msg)
 
	  else if (ch.eq.'-radius') then
	  call getarg (i+1,ch)
	  radius = numget(ch,msg)	
 
	  endif
	enddo
c---------------------------------------------------------------
      call filelist (nmap_max,wildmap,nwild,maps,nmaps)
c---------------------------------------------------------------
c get clone map
 
	if (clonemap.eq.' ') then
	print *,'the clone map has not been specified'
	print *,'first input map is assumed as clone map'
        clonemap=maps(1)
        noclone=.true.
	endif
 
	lm = length(clonemap)
	file = clonemap(1:lm)//null
*	call OPENMAPtype1 (file,ncol,nrow,typ, xor, yor, size)
	call OPENGDAL     (file,ncol,nrow,typ, xor, yor, size,sizey,driver,nband,error)

        iband = nband
        call GETGDALATTR (iband,nbyte,datatype)
        print *,'nbyte=',nbyte,' datatype=',datatype
        outfmt = driver(1:length(driver))//null

	
	print *,'get ',clonemap(1:lm)
	print *,'ncol=',ncol,' nrow=',nrow,' typ=',typ
 
	npcr = ncol*nrow
	allocate (mask(npcr))
 
	if (datatype(1:4).eq.'Byte') then
	allocate (imask(npcr))
*	call GETVALMAP (imask)
	CALL GETGDALVAL (iband,imask)

	  do i = 1,npcr
	  mask(i) = imask(i).ne.mv1 .and. imask(i).gt.0
	  enddo
	deallocate (imask)

	else if (datatype(1:5).eq.'Int32') then
	allocate (mask4(npcr))
*	call GETVALMAP (mask4)
	CALL GETGDALVAL (iband,mask4)
	  do i = 1,npcr
	  mask(i) = mask4(i).ne.mv4 .and. mask4(i).gt.0
	  enddo
	deallocate (mask4)

	else if (datatype(1:7).eq.'Float32') then
	allocate (rmask(npcr))
*	call GETVALMAP (rmask)
	CALL GETGDALVAL (iband,rmask)
	  do i = 1,npcr
	  mask(i) = .not.eqmv(rmask(i)) .and. rmask(i).gt.0
	  enddo
	deallocate (rmask)
	endif
	
 
        if (noclone) mask = .true. 
 
c mask.map=boolean(code.map)
 
c---------------------------------------------------------------
 
	  if (radius.eq.0.) radius = max(ncol,nrow)*size
 
	nradius = int(radius/size + 0.5)
	print *,'nradius=',nradius
 
c---------------------------------------------------------------
c get input maps
c---------------------------------------------------------------
	dir = ' '
	allocate (val(npcr),beach(npcr))
	beach = mv1
	findbeach = .false.
 
	do imap = 1,nmaps
	
	inpmap = maps(imap)
 
	if (inpmap.eq.' ') then
	print *,'the input map has not been specified'
	stop
	endif
 
	linp = length(inpmap)
	file = inpmap(1:linp)//null
 
*	call OPENMAPtype1 (file,ncol,nrow,typ, xor, yor, size)
 	call OPENGDAL     (file,ncol,nrow,typ, xor, yor, size,sizey,driver,nband,error)
        iband = nband
        call GETGDALATTR (iband,nbyte,datatype)
 
c	print *,'get ',inpmap(1:linp)
c	print *,'ncol=',ncol,' nrow=',nrow,' typ=',typ
 
	if (nrow*ncol.ne.npcr) then
	print *,'the clone map and the input map have not the '
     &       ,'same no. of pixel'
	stop
	endif
 
		if (datatype(1:7).ne.'Float32') then
		print *,'The input map must be a  real*4 map'
		stop
		endif
 
*	call GETVALMAP (val)
	CALL GETGDALVAL (iband,val)
c------------------------------------------------------------------
	call covervalgrid (mask,val,beach,findbeach,nmax
     &                  ,nradius,idp,npcr,ncol,nrow)
c------------------------------------------------------------------
	print *,'base=',base,' ext=',ext

	if (base.ne.' ') then
	lm = length(base)
	  do i = 1,linp
 
	    if (inpmap(i:i+lm-1).eq.zero(1:lm)) then
	    lb = min(i-1,8-lm)
	    lex = min(lb+lm,8)+1
            exit
            endif
 
	    if (inpmap(i:i).eq.'.') then
	    lb = min(i-1,8-lm)
            lex = i
            exit
            endif 
	  enddo
 
c	print *,'lb=',lb,' lm=',lm,' lex=',lex,' linp=',linp
	outmap = inpmap(1:lb)//base(1:lm)//inpmap(lex:linp)

	else if (ext.ne.' ') then
	lm = length(ext)
        done = .false.
	  do i = 1,linp 
	    if (inpmap(i:i).eq.'.') then
  	    outmap = inpmap(1:i-1)//ext(1:lm)//inpmap(i:linp)
            done = .true.
            exit
            endif 
	  enddo
 
          if (.not.done) outmap = inpmap(1:linp)//ext(1:lm)

        else
        outmap = inpmap
	endif
       
 
	
	if (outmap.ne.' ') then
	lm = length(outmap)
	print *,'inpmap=',inpmap(1:lm),' outmap=',outmap(1:lm)
	file = outmap(1:lm)//null
*	call NEWMAPTYPE1 (file, val, npcr,4)
	call OUTGDAL (file,ncol,nrow,4, xor, yor, size, val, npcr, sizey, outfmt, nband,error)
	endif
	
	enddo	
 
c------------------------------------------------------------------
        if (outfmt(1:8).eq.'PCRaster') then
	allocate(dist(npcr))
	call getdist(dist,npcr)
	file = 'idist.map'//null
*	call NEWMAPTYPE1 (file,dist,npcr,4)
	call OUTGDAL (file,ncol,nrow,4, xor, yor, size, dist, npcr, sizey, outfmt, nband,error)
	dist = dist*size/1e3
	file = 'dist.map'//null
*	call NEWMAPTYPE1 (file,dist,npcr,4)
	call OUTGDAL (file,ncol,nrow,4, xor, yor, size, dist, npcr, sizey, outfmt, nband,error)
        endif
c------------------------------------------------------------------
      print *,'time spent [min] = ', (time()-it0)/60.
 
	stop
	end
