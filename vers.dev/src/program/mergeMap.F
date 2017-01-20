      program mergeMap
c-------------------------------------------------------------
c Origin: adaptation of resmap
c-------------------------------------------------------------
	implicit none
        character versdate*20 /"Version - 28May2011"/
 
      integer*4  ncol, nrow, typemap, nval
      integer*4  ncol1, nrow1, typemap1
	real*4, allocatable :: x(:), y(:), val(:), val1(:,:)
 
	real*8 xstart,ystart,cellsize
	real*8 xstart1,ystart1,cellsize1
c--------------------------------------------
* gdal additional variables
        integer*4 nband, iband,error, nbyte
        character driver*64, datatype*64, inpfmt*64, outfmt*64
        real*8 cellsizey
        real*8 cellsizey1
c--------------------------------------------

	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)
 
	real*4 mv
	integer*4 imv /z'ffffffff'/, mv4
	integer*1 mv1
 
c--------------------------------------------
c local variables
	integer*4 iun/10/, iargc, narg, ii,jj
     &    ,i,j,l, it0, time
	
	real*4 xf,yf,dx,dy, float
	
	logical exist, eqmv
 
	character  ch*1000,map*1000
	character outmap*1000, largemap*1000, smallmap*1000
 
	real*4 numget, eps /1e-6/
 
#if defined WIN32
      character bs*1 /'\'/
#else
      character bs*1 /'/'/
#endif
	integer*4 length, lm, lout, ls
c-----------------------------------------------------------
	call getmv4(mv4)
	call getmv1(mv1)
        call getmv(mv)
 
	narg = iargc()
	it0 = time()
        largemap = ' '
        smallmap = ' '
        outmap = ' '
 
	  if (narg.eq.0) then
 
	print *,' '
	print *,'mergeMap help '
        print *,'get the small map[s] values in the cell center of the large map'
        print *,'and put in output map'
	print *,' '
	print *,'-l large window map'
	print *,'   no DEFAULT '
	print *,' '
	print *,'-s small window map[s]'
	print *,'   no DEFAULT '

	print *,'-o outmap'
	print *,'   no DEFAULT'
	print *,' '
#if defined WIN32
	read *
#endif
	stop
	endif
c-----------------------------------------------------------------
c set defaults
	largemap= ' '
	outmap = ' '
c-----------------------------------------------------------------
c read arguments
	do i = 1,narg
	call getarg (i,ch)
c       ----------------------------------------------------
	  if (ch.eq.'-l') then
	  call getarg (i+1,largemap)
c       ----------------------------------------------------
	  else if (ch.eq.'-o') then
	  call getarg (i+1,outmap)
          endif
c       ----------------------------------------------------
	enddo
c---------------------------------------------------------------
c get large map
	lm = length(largemap)
	map = largemap(1:lm)//null
 
	inquire (file=map, exist=exist)
	  if (.not.exist) Then
	  print *,'The largemap ',map(1:length(map)),' does not exist'
	  call exit(2)
	  endif

	call OPENGDAL     (map,ncol,nrow,typemap, xstart, ystart, cellsize,cellsizey,driver,nband,error)	
	nval = ncol*nrow 
*        print *,'ncol=',ncol,' nrow=',nrow,' nval=',nval
	allocate (val(nval))
 
        iband = nband
	CALL GETGDALVAL (iband,val)

c  x,y of the large map
 
	allocate (x(nval),y(nval))
	
	l=0
	do j = 1,nrow
	  do i = 1,ncol
	  l = l+1
	  x(l) = xstart + (i-0.5)*cellsize
	  y(l) = ystart + (j-0.5)*cellsizey
          enddo
	enddo

c---------------------------------------------------
c get small maps
	do i = 1,narg
	call getarg (i,ch)
	  if (ch.ne.'-s') cycle
        j = i+1
        call getarg (j,ch)
c       ----------------------------------------------------
	  do while (ch(1:1).ne.'-'.and.ch.ne.' '.and.j.le.narg) 
          smallmap = ch            
	  ls = length(smallmap)
	  map = smallmap(1:ls)//null
 
          inquire (file=map, exist=exist)
	    if (.not.exist) Then
	    print *,'The smallmap ',map(1:length(map)),' does not exist'
	    go to 100
	    endif
*          print *,'smallmap=',map(1:ls)

	  call OPENGDAL (map,ncol1,nrow1,typemap1, xstart1, ystart1, cellsize1,cellsizey1,driver,nband,error)	
*          print *,'ncol1=',ncol1,' nrow1=',nrow1

	  allocate (val1(ncol1,nrow1))
 
          iband = nband
	  CALL GETGDALVAL (iband,val1)
c         ----------------------------------------------------
            do l = 1,nval
            ii = int((x(l)-xstart1)/cellsize1+eps)+1
              if (ii.lt.1.or.ii.gt.ncol1) cycle
            jj = int((y(l)-ystart1)/cellsizey1+eps)+1
              if (jj.lt.1.or.jj.gt.nrow1) cycle
              if (eqmv(val1(ii,jj))) cycle
*              if (val1(ii,jj).eq.0.) cycle
*            print *,'ii=',ii,' jj=',jj
            val(l) = val1(ii,jj)
            enddo
          deallocate (val1)
c         ----------------------------------------------------
100       continue
          j = j+1
          call getarg (j,ch)
          enddo
c       ----------------------------------------------------
        exit
        enddo

c	-------------------------------------------------------------------	
	lm = length(largemap)
	map = largemap(1:lm)//null
 
	inquire (file=map, exist=exist)
	  if (.not.exist) Then
	  print *,'The largemap ',map(1:length(map)),' does not exist'
	  call exit(2)
	  endif
	call OPENGDAL     (map,ncol,nrow,typemap, xstart, ystart, cellsize,cellsizey,driver,nband,error)	
        outfmt = driver(1:length(driver))//null

        call GETGDALATTR (nband,nbyte,datatype)
        datatype = datatype(1:length(datatype))//null
        call CLOSEGDAL()
c	-------------------------------------------------------------------	

        lout = length(outmap)
	map = outmap(1:lout)//null

	call OUTGDAL (map,ncol,nrow,typemap
     &               , xstart, ystart, cellsize,val,nval, cellsizey, outfmt, nband,error)

c------------------------------------------------------------
*      print *,'time spent [min] = ', (time()-it0)/60.
	stop
 	end