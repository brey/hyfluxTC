!> @file ldd2path.F
!! Convert ldd to map
!! @note parameters:  

       program ldd2path
	implicit none
 
c-------------------------------------------------------------
c interface with maps
 
      integer*4  npcr, np, ncol, nrow, nmask
 
	real*4, allocatable :: x(:), y(:)
	integer*1, allocatable :: ldd(:)
	integer*4, allocatable :: ll(:,:)
 
	character null*1
        integer*1 inull /z'00'/
        equivalence (null,inull)
 
	real*4     mv 
	integer*4  mv4
	integer*1  mv1
c--------------------------------------------
c local variables
	integer*4 iout/10/, iargc, narg, typ
     &          ,i,j,k,l,m,n,it0, time, il, jl, ir, jr, ito, jto

        real*4 xstart,ystart, numget
	real*8 xor, yor, size

        integer nmaps_max, nmaps
        parameter (nmaps_max=100)	
	character map*132, ch*132, lddmap*132, maps(nmaps_max)*30, outfile*132, msg*3
#if defined WIN32
     &          ,bs*1 /'\'/
#else
     &          ,bs*1 /'/'/
#endif
	integer*4 length, lm
	
	logical*4 ok, exist, eqmv
c-----------------------------------------------------------

	narg = iargc()
	it0 = time()
	call getmv(mv)
	call getmv1(mv1)
	call getmv4(mv4)
 

	  if (narg.eq.0) then
 
	print *,' '
	print *,'ldd2path help'
	print *,' '
	print *,' Input'
	print *,' '
	print *,'-ldd ldd input map'
	print *,'   no DEFAULT '
	print *,' '
	print *,'-xstart xstart'
	print *,'   no DEFAULT '
	print *,' '
	print *,'-ystart ystart'
	print *,'   no DEFAULT '
	print *,' '
	print *,'-ldd ldd map'
	print *,'   no DEFAULT '
	print *,' '
        print *,'-maps maps'
        print *,'    maps are the input maps with full extension'
        print *,'Example: -maps dem.map'
        print *,'    no DEFAULT'

	print *,'-o output file name'
	print *,'   Contain columns of x, y, id along the path identifyed by ldd'

*	print *,'-path output path map'
*	print *,'   ldd path '


#if defined WIN32
	read *
#endif
	stop
	endif
c-----------------------------------------------------------------
c read arguments
	lddmap = ' '
        maps = ' '
        outfile = ' '
        xstart = mv
        ystart = mv

 	do i = 1,narg
	call getarg (i,ch)
	  if (ch.eq.'-ldd') then
	  call getarg (i+1,lddmap)

	  else if (ch.eq.'-xstart') then
	  call getarg(i+1,ch)
	  xstart = numget(ch,msg)

	  else if (ch.eq.'-ystart') then
	  call getarg(i+1,ch)
	  ystart = numget(ch,msg)

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
		
	  else if (ch.eq.'-o') then
	  call getarg(i+1,outfile)

          endif
	enddo
c---------------------------------------------------------------
c get  ldd
 
	if (lddmap.eq.' ') then
	print *,'the ldd map has not been specified'
	stop
	endif
 
	lm = length(lddmap)
	maps = lddmap(1:lm)//null
 
	call OPENMAPtype1 (maps,ncol,nrow,typ, xor, yor, size)
 
	print *,'ncol=',ncol,' nrow=',nrow
	
	npcr = ncol*nrow
 
	allocate (ldd(npcr),x(npcr),y(npcr),ll(ncol,nrow))
 
	call GETVALMAP (ldd)
 
	print *,'get ldd'

        l = 0
        do j = 1,nrow
          do i = 1,ncol
          l = l+1
          x(l) = xor + (i-0.5)*size
          y(l) = yor - (j-0.5)*size
          ll(i,j) = l
           if (ldd(l).eq.mv1) ldd(l)=5
	  enddo
        enddo

          i=int((xstart-xor)/size)+1         
          j=int((yor-ystart)/size)+1        
          i=min(max(i,1),ncol)  
          j=min(max(j,1),nrow)  

          l = ll(i,j)

          if (outfile.ne.' ' ) then
          open (iout,file=outfile)
          else
          print *,'outfile not defined'
          iout = 6
          endif

          write (iout,*) 'x  y l '
          write (iout,*)x(l),y(l),l
          k = 0
          do while (ldd(l).ne.5)    
	  call lddind (ldd(l),i,j,nrow,ncol,ito,jto,il,jl,ir,jr)
	  i = ito
          j = jto
          l = ll(i,j)
          write (iout,*)x(l),y(l),l
          k = k+1
	  enddo
          print *,'distance to sea [km]',k*size/1000
c------------------------------------------------------------------
1100  print *,'time spent [min] = ', (time()-it0)/60.
 
	stop
	end
