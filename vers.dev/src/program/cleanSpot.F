      program shift
*	USE DFlib
	implicit none
 
c-------------------------------------------------------------
c interface with maps
 
      integer*4  nval, np, ncol, nrow, nmask, typemap
 
	real*4, allocatable :: val(:,:)
        integer*4, allocatable :: seed(:,:), countseed(:)
	logical*1, allocatable :: mask(:,:)
        
 	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)
	real*4     mv /z'ffffffff'/
	integer*4  mv4, imv/z'ffffffff'/
	integer*1  mv1
	integer*4  typ
 
c--------------------------------------------
c local variables
	integer*4 iout/10/, iargc, narg, length
     &          ,i,j,k, it0, time, lm, ii, jj
     &          , iseed, nseed, countmax

	real*8 xstart,ystart,cellsize, sizey
	
	character inpmap*254, ch*254, map*254
     &          ,outmap*254,msg*3, seedmap*254
     
#if defined WIN32
     &          ,bs*1 /'\'/
#else
     &          ,bs*1 /'/'/
#endif
	real*4 numget
        logical exist, spread
c--------------------------------------------
* gdal additional variables
        integer*4 nband, iband,error, nbyte
        character driver*64, datatype*64, inpfmt*64, outfmt*64	
c-----------------------------------------------------------
	call getmv1(mv1)
	call getmv4(mv4)
 
	narg = iargc()
	it0 = time()
        inpmap = ' '
        outmap = ' ' 
        seedmap = ' ' 

	  if (narg.eq.0) then
        call getarg(0,ch)
	print *,' '
	print *,ch(1:length(ch)),' help'
	print *,' '
	print *,'clean spots'
	print *,' '
	print *,'-i input map'
	print *,'-o output map'
	print *,'-s seed map'
 
        read *
	stop
	endif
c-----------------------------------------------------------------

	do i = 1,narg
	call getarg (i,ch)
	  if (ch.eq.'-i') then
	  call getarg (i+1,inpmap)
 
	  else if (ch.eq.'-o') then
	  call getarg (i+1,outmap)
 
	  endif
	enddo
c------------------------------------------------------------------
c open inpmap
c------------------------------------------------------------------
	lm = length(inpmap)
	map = inpmap(1:lm)//null

	inquire (file=map, exist=exist)
	  if (.not.exist) Then
	print *,inpmap
	  print *,'The inpmap ',map(1:length(map)),' does not exist'
	  call exit(2)
	  endif

	call OPENGDAL     (map,ncol,nrow,typemap, xstart, ystart, cellsize,sizey,driver,nband,error)
        outfmt = driver(1:length(driver))//null

        nval = ncol*nrow
	allocate (val(ncol,nrow))
	val=mv
 
        iband = nband

        call GETGDALATTR (iband,nbyte,datatype)
        print *,'nbyte=',nbyte,' datatype=',datatype(1:length(datatype))

	CALL GETGDALVAL (iband,val)

c------------------------------------------------------------------      
c mask map and initialize seed
	allocate (mask(ncol,nrow),seed(ncol,nrow))

	Call checkmv (val,mv,mask,nval)
        seed = 0

       do j = 1,nrow
         do i = 1,ncol
           if (mask(i,j)) seed(i,j) = -1
         enddo
       enddo

       print *,'seed=0:',count(seed.eq.0), 'seed<0:',count(seed.lt.0)

c------------------------------------------------------------------      
c put seed

      iseed = 0
1     iseed = iseed+1

       do j = 2,nrow-1
         do i = 2,ncol-1
            if (seed(i,j).eq.0) then
            seed(i,j) = iseed
            goto 100
            endif
          enddo
       enddo

       go to 1000

c------------------------------------------------------------------
c spread seed

100   continue
      spread = .false.
       do j = 2,nrow-1
         do 11 i = 2,ncol-1
            if (seed(i,j).ne.iseed) goto 11
            do jj = -1,1
              do ii = -1,1
                 if (seed(i+ii,j+jj).eq.0) then
                 seed(i+ii,j+jj) = iseed
                 spread = .true.
                 endif
              enddo
           enddo
11       continue
       enddo


       if (spread) goto 100

        print *,'iseed=',iseed,' count=',count(seed.eq.iseed)
       goto 1

1000  continue
c------------------------------------------------------------------
c  count seed

      countmax = 0
      nseed = iseed-1
      allocate (countseed(nseed))
        do i = 1,nseed
        countseed(i) = count(seed.eq.i)
          if (countseed(i).gt.countmax) then
          iseed = i
          countmax = countseed(i)
          endif
        enddo
        print *,'iseed=',iseed,' countmax=',countmax

c------------------------------------------------------------------
c clean 

       do j = 1,nrow
         do i = 1,ncol
           if (seed(i,j).ne.iseed) val(i,j)=mv
         enddo
       enddo

c------------------------------------------------------------------
c output
        if (outmap.ne.' ') then
        lm = length(outmap)
	map = outmap(1:lm)//null
	call OUTGDAL (map,ncol,nrow,typemap
     &               , xstart, ystart, cellsize,val,nval, sizey, outfmt, nband,error)
        endif
c------------------------------------------------------------------
        if (seedmap.ne.' ') then
        lm = length(seedmap)
	map = seedmap(1:lm)//null
	call OUTGDAL (map,ncol,nrow,2
     &               , xstart, ystart, cellsize,seed,nval, sizey, outfmt, nband,error)
        endif
c------------------------------------------------------------------
      print *,'time spent [sec] = ', time()-it0
	stop
	end
