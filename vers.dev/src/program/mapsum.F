	program MapSum
	implicit none
 
c-------------------------------------------------------------
c interface with maps
 
      integer*4  npcr, ncol, nrow
 
	real*4,    allocatable :: val(:),dist(:),sum(:),site(:)
	logical*1, allocatable :: mask(:)
	integer*1, allocatable :: imask(:)
 
	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)
 
	real*4     mv /z'ffffffff'/
	integer*1  mv1
	integer*4  typ
 
	real*8 xor, yor, size
c--------------------------------------------
c local variables
	integer*4 iargc, narg,i,j,k, l,it0, time
	
	real*4 numget, radius
 
	character maskmap*132, file*132, ch*132, inpmap*132, sitemap*132
     &          ,summap*132, distmap*132, idistmap*132, msg*3
#if defined WIN32
     &          ,bs*1 /'\'/
#else
     &          ,bs*1 /'/'/
#endif
	integer*4 length, lm, lout, linp, nradius
      logical nosum
c-----------------------------------------------------------
	call getmv1(mv1)
 
	narg = iargc()
	it0 = time()
	radius = 0.
 
	if (narg.eq.0) then
 	print *,' '
	print *,'mapsum help '
 	print *,' '
      print *,'Sum the pixel values in a defined radius and evaluate'
      print *,'minimum distance.'
      print *,'Note: the valuas are considered absolute, not density'
	print *,' '
	print *,'-m maskmap'
	print *,'   mask map (boolean or ldd)'
	print *,'   map which define where the computation is requested'
	print *,'   DEFAULT: if maskmap is not specified'
      print *,'            the computation is done for the full area'
	print *,' '
      print *,'-i inpmap'
	print *,'   input map '
	print *,'   Caution: the maps must be scalar'
      print *,'   no DEFAULT'
	print *,' '
	print *,'-radius radius [m]'
	print *,'   DEFAULT is ',radius
	print *,' '
      print *,'-sum output sum map'
      print *,'   no DEFAULT'
	print *,' '
      print *,'-dist output distance map [km]'
      print *,'   no DEFAULT'
	print *,' '
      print *,'-idist output distance map [no. of pixel]'
      print *,'   no DEFAULT'
	print *,' '
      print *,'-site output site map (no. of pixel within radius)'
      print *,'   no DEFAULT'
#if defined WIN32
        read *
#endif
	stop
	endif
c-----------------------------------------------------------------
c read arguments
	maskmap = ' '
	inpmap = ' '
	summap = ' '
 	distmap = ' '
 	sitemap = ' '
 	idistmap = ' '
 
	do i = 1,narg
	call getarg (i,ch)
	  if (ch.eq.'-m') then
	  call getarg (i+1,maskmap)
 
	  else if (ch.eq.'-i') then
	  call getarg (i+1,inpmap)
 
	  else if (ch.eq.'-sum') then
	  call getarg (i+1,summap)
 
	  else if (ch.eq.'-dist') then
	  call getarg (i+1,distmap)
 
	  else if (ch.eq.'-idist') then
	  call getarg (i+1,idistmap)
 
	  else if (ch.eq.'-site') then
	  call getarg (i+1,sitemap)
 
	  else if (ch.eq.'-radius') then
	  call getarg (i+1,ch)
	  radius = numget(ch,msg)	
 
	  endif
	enddo
c---------------------------------------------------------------
	if (summap.eq.' '.and.distmap.eq.' '.and.idistmap.eq.' ') then
      print *,'no output maps specified'
      call exit(2)
      endif
c---------------------------------------------------------------
c get input maps
c---------------------------------------------------------------
	if (inpmap.eq.' ') then
	print *,'the input map has not been specified'
	call exit(2)
	endif
 
	linp = length(inpmap)
	file = inpmap(1:linp)//null
 
	call OPENMAPtype1 (file,ncol,nrow,typ, xor, yor, size)
	npcr = ncol*nrow
 
	allocate (val(npcr))
 		if (typ.ne.4) then
		print *,'The input map must be a  real*4 map'
		stop
		endif
 	call GETVALMAP (val)
c---------------------------------------------------------------
c get mask map
c---------------------------------------------------------------
      if (maskmap.eq.' ') then
      allocate (mask(npcr))
      mask = .true.
 
      else
	lm = length(maskmap)
	file = maskmap(1:lm)//null
	call OPENMAPtype1 (file,ncol,nrow,typ, xor, yor, size)
	
	print *,'get ',maskmap(1:lm)
	print *,'ncol=',ncol,' nrow=',nrow,' typ=',typ
 
		if (typ.ne.1.and.typ.ne.6) then
		print *,'the clone map must be a integer*1 map (boolean or ldd)'
		stop
		endif
	
	if (nrow*ncol.ne.npcr) then
	print *,'the clone map and the input map have not the '
     &       ,'same no. of pixel'
	call exit(2)
	endif
 
	allocate (imask(npcr),mask(npcr))
 
	call GETVALMAP (imask)
 
	do i = 1,npcr
	mask(i) = imask(i).ne.mv1 .and. imask(i).gt.0
	enddo
 
	deallocate (imask)
      endif
c---------------------------------------------------------------
	print *,'no. of requested values =',count(mask)
c---------------------------------------------------------------
	nradius = int(radius/size + 0.5)
	print *,'nradius=',nradius
        if (nradius.eq.0) then
        print *,'radius not defined'
        call exit(2)
        endif
c------------------------------------------------------------------
      nosum = summap.eq.' '.and.sitemap.eq.' '
	allocate (sum(npcr),dist(npcr),site(npcr))
	call sumvalgrid (mask,val,sum,dist,site
     &                  ,nradius,npcr,ncol,nrow,nosum)
c------------------------------------------------------------------
	if (summap.ne.' ') then
	lm = length(summap)
	file = summap(1:lm)//null
	call NEWMAPTYPE1 (file, sum, npcr,4)
	endif
c------------------------------------------------------------------
	if (sitemap.ne.' ') then
	lm = length(sitemap)
	file = sitemap(1:lm)//null
	call NEWMAPTYPE1 (file, site, npcr,4)
	endif
c------------------------------------------------------------------
	if (idistmap.ne.' ') then
	lm = length(idistmap)
	file = idistmap(1:lm)//null
	call NEWMAPTYPE1 (file,dist,npcr,4)
      endif
c------------------------------------------------------------------
	if (distmap.ne.' ') then
	lm = length(distmap)
	file = distmap(1:lm)//null
	dist = dist*size/1e3
	call NEWMAPTYPE1 (file,dist,npcr,4)
      endif
c------------------------------------------------------------------
      print *,'time spent [min] = ', (time()-it0)/60.
 
	stop
	end
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      module index_mod
      implicit none
      integer*4, allocatable :: iP(:), jP(:)
      real*4, allocatable :: distP(:)
      end module index_mod
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine sumvalgrid (mask,val,sumval, dist, site
     &                  ,nradius,npcr,ncol,nrow,nosum)
      use index_mod
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      implicit none
      integer*4 npcr,nradius,ncol,nrow
      logical*1 mask(npcr)
      real*4 val(npcr),sumval(npcr),dist(npcr),site(npcr)
 
c local variables
 
        integer*4, allocatable :: jj(:), ii(:), LP(:,:)
      logical*4, allocatable :: mval(:)
 
	real*4 dx, dy
	integer*4 i, j, L, M, K, nv, np
 
	real*4     mv /z'ffffffff'/
	integer*4  length
        logical*4 eqmv,nosum
c---------------------------------------------
	allocate (mval(npcr),ii(npcr),jj(npcr),LP(ncol,nrow))
 
        L=0
          do j = 1,nrow
            do i = 1,ncol
            L = L+1
            ii(L) = i
            jj(L) = j
            LP(i,j) = L
            enddo
          enddo
 
c define mask of the MV
 
	  do L = 1,npcr
	  mval(L) = eqmv(val(L))
	  enddo
 
       print *,'no. of defined values=',npcr-count(mval)
c------------------------------------------------------------------
c construct indeces of neighboroud
c------------------------------------------------------------------
      call makeindex (nradius,np)
c------------------------------------------------------------------
c calculate sum and min distance
c------------------------------------------------------------------
        dist = mv
	sumval = mv
        site  = mv
 
	do 100 L = 1,npcr
	  if (.not.mask(L)) go to 100
 
 	sumval(L) = 0.
        nv = 0
	  do 200 K = 1,np
	  i = ii(L)+iP(K)
          j = jj(L)+jP(K)
	    if (i.lt.1.or.i.gt.ncol) go to 200
	    if (j.lt.1.or.j.gt.nrow) go to 200
          M = LP(i,j)
            if (mval(M)) go to 200
	  nv = nv+1
          if (nv.eq.1) then
          dist(L) = distP(K)
            if (nosum) go to 100
          endif
	  sumval(L) = sumval(L) + val(M)
200     continue
          if (nv.ge.1) then
          site(L) = nv
          else
          sumval(L) = mv
          site(L)   = mv
          endif
100    continue
 
       deallocate (ii,jj,LP,mval)
 
      return
      end
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine makeindex (nradius,np)
        use index_mod
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	implicit none
	integer*4 nradius, np
 
c	local variables
	real*4     dist, radius
 
	integer*4  k_square, i, j, k
        real*4, allocatable :: dist0(:)
	integer*4, allocatable :: ii0(:), jj0(:), order(:)
c-------------------------------------------------------------
 
c k_square is the no of pixels in the square (minus the pixel to interpolate)
c (for nradius=1, k_square=9)
 
	k_square = (nradius*2+1)**2
 
	radius = float(nradius)
 
	allocate (order(k_square),ii0(k_square),jj0(k_square)
     &     ,dist0(k_square))
 
c now evaluate the relative index and dist
	
	k = 0
	do i = -nradius, nradius
	  do j = -nradius, nradius
	  dist = sqrt(float(i**2 + j**2))
          if (dist.le.radius) then
	    k = k+1
	    ii0(k) = i
	    jj0(k) = j
	    dist0(k) = dist
	    endif
	  enddo
	enddo
 
	np = k
 
	print *,'k_square=',k_square,' np=',np
 
c put in the right order and evaluate weight
 
	call sort_list(dist0,order,np)
 
	allocate (iP(np),jp(np),distP(np))
 
	do i = 1,np
	iP(i) = ii0(order(i))
	jP(i) = jj0(order(i))
      distP(i) = dist0(order(i))
	enddo
 
	deallocate (ii0,jj0,dist0,order)
 
	return
        end
