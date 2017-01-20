      subroutine findindexstat (maskval,npcr,nradius0,ncol,nrow,nmax)
        use findind_mod
        use distance_mod
	implicit none
	integer*4 npcr, nradius0,nradius, nrow, ncol, step_max,nmaxpt0
     &          , nmax 
	logical*1 maskval(npcr)
 
c	local variables
	real*4     mv /z'ffffffff'/, radius, radius0, sumdist, dist00,rmax
	integer*4  imv /z'ffffffff'/
 
	integer*4  k_square, k_radius, i, j, L, k, n, i0, j0, L0, m, iun
 
	integer*4, allocatable :: ii(:), jj(:), ii0(:), jj0(:), order(:), pt0(:)
	real*4, allocatable :: dist0(:),dist(:)
	logical*4 ok
	save jj, dist
c-------------------------------------------------------------
 
c increase nradius in order to satisfy the condition that at least nmax points
c are in the area identified by nradius0 to nradius
        nradius = min(nradius0*4,max(ncol,nrow))

c step_max is the size of the square 
	step_max = nradius*2+1
 
c k_square is the no of pixels in the square (minus the pixel to interpolate)
c (for nradius=1, k_square=8)
 
	k_square = step_max**2 
 
	radius = float(nradius)
	radius0 = float(nradius0)
 
        allocate (order(k_square),ii0(k_square),jj0(k_square),dist0(k_square))

	allocate (ii(k_square),jj(k_square),dist(k_square))

c now evaluate the relative index and dist
#ifdef DOIT
	k = 0
 
	do i = -nradius, nradius
	  do j = -nradius, nradius
 
	  dist00 = sqrt(float(i*i + j*j))

c          if (dist00.gt.0.5.and.dist00.le.radius) then
           if (dist00.le.radius) then
	    k = k+1
	    ii0(k) = i
	    jj0(k) = j
	    dist0(k) = dist00
	    endif
	
	  enddo
	enddo
#endif
         call square (ii0,jj0,dist0,nradius,k_square,k)
c-----------------------------------------------------------------
	k_radius = k
	print *,'nradius0=',nradius0,' nradius=',nradius
     &        ,' max loops=',k_radius
c-----------------------------------------------------------------
c put in the right order and evaluate dist
 
	call sort_list(dist0,order,k_radius)
 
	do i = 1,k_radius
	ii(i) = ii0(order(i))
	jj(i) = jj0(order(i))
        dist(i) = dist0(order(i))
*       print *,'ii=',ii(i),' jj=',jj(i),' dist=',dist(i)
	enddo
c------------------------------------------------------------------
	deallocate (ii0,jj0,dist0,order)
c------------------------------------------------------------------
        rmax = float(nreq)*float(k_radius)*0.1
        print *,'rmax=',rmax
        nmaxpt = min(rmax,10e6)
        print *,'guess nmaxpt =',nmaxpt/1e6,'e6'

        allocate (pt2(nreq),npt(nreq))
        allocate (pt1(nmaxpt))
c------------------------------------------------------------------
c assign pointers to beach

        m = 0
        ok = .true.
 
	do ir = 1,nreq
	
        L = Lreq(ir)
	i = ix(L)
	j = iy(L)
 
        n = 0
        pt2(ir) = m
        
c	print *,'ir=',ir,' ind=',Lreq(ir),' i=',i,' j=',j
	
	  do 100 k = 1,k_radius
	  i0 = i+ii(k)
	  j0 = j+jj(k)
	    if (i0.lt.1.or.i0.gt.ncol) go to 100
	    if (j0.lt.1.or.j0.gt.nrow) go to 100
	  L0 = (j0-1)*ncol + i0
	    if (.not.maskval(L0)) go to 100
	  n = n+1
c----------------------------------------------------
          m = m+1
	  pt1(m) = L0
c----------------------------------------------------
c the no of guess pointers is wrong
            if (m.ge.nmaxpt) then
c save ol pointers
            call iunfree(iun)
            open (iun,status='scratch',form='unformatted')
            write (iun) pt1
            nmaxpt0 = nmaxpt
            endfile (iun)
            rewind (iun)
c store old pointers in new space
            nmaxpt = nmaxpt0*1.5
            deallocate (pt1)
            allocate (pt1(nmaxpt))            
            read (iun) pt1(1:nmaxpt0)
            close (iun)
            print *,'form guess nmaxpt0 =',nmaxpt0/1e6,'e6  to nmaxpt='
     &             ,nmaxpt/1e6,'e6'

c remove old pointers
            endif
c----------------------------------------------------
*          if  (.not.maskval(L).and.nearest) go to 200
c----------------------------------------------------
           if  (n.ge.nmax.and.dist(k).ge.radius0) go to 200
c----------------------------------------------------
*         if (ir.le.2) print *,'m=',m,' from pt=',pt2(ir)+n
100	  continue
 
c	print *,'i0=',i0,' j0=',j0
	
200	npt(ir) = n
 
	enddo

        print *,'nreq=',nreq,' npt min=',minval(npt),' max=',maxval(npt)
        print *,'nmaxpt=',nmaxpt/1e6,'e6   necessary=',m/1e6,'e6'
        nmaxpt = m

	deallocate (ii,jj,dist)
 
	return
	end
