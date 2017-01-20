c-----------------------------------------------------------------------
      subroutine findindex (mask,npcr,ind,pt,weight1,npt
     &                            ,nmax,nbeach,nradius,ncol,nrow,idp)
c    -------------------------------------------------------------------
	implicit none
	integer*4 npcr,nmax,nbeach, nradius, nrow, ncol, step_max
	integer*4 ind(nbeach), pt(nmax,nbeach), npt(nbeach)
	logical*1 mask(ncol,nrow)
        logical*4, allocatable :: nmask(:,:)
	real*4 weight1(nmax,nbeach), idp
 
c	local variables
	real*4     mv /z'ffffffff'/, sumweight, dist, radius
	integer*4  imv /z'ffffffff'/
 
	integer*4  k_square, k_radius, i, j, l, k, n, i0, j0, l0
     &             ,i1,i2,j1,j2
        
	integer*4, allocatable :: ii(:), jj(:), ii0(:), jj0(:), order(:)
	real*4, allocatable :: weight(:), dist0(:)
	logical*4 first
	data first /.true./
	save first, k_radius, ii, jj, weight
c-------------------------------------------------------------
c	weight, weight1: see the dimensions
c     weight is calculates oly once (function of nradius, nmax)
c	weight1 is evaluated every time (function of mask)
c------------------------------------------------------------------
	if (first) then
 
c step_max is the size of the square (for nradius=1, stepmax=3)
 
	step_max = nradius*2+1
 
c k_square is the no of pixels in the square (minus the pixel to interpolate)
c (for nradius=1, k_square=8)
 
	k_square = step_max**2 - 1
 
	radius = float(nradius)
 
	allocate (ii(k_square),jj(k_square),weight(k_square)
     &  ,order(k_square),ii0(k_square),jj0(k_square),dist0(k_square))
 
c now evaluate the relative index and dist
	
	k = 0
 
	do i = -nradius, nradius
	
	  do j = -nradius, nradius
 
	  dist = sqrt(float(i**2 + j**2))
c          if (dist.gt.0.5.and.dist.le.radius) then
           if (dist.le.radius) then
	    k = k+1
	    ii0(k) = i
	    jj0(k) = j
	    dist0(k) = dist
	    endif
	
	  enddo
	
	enddo
 
	k_radius = k
 
*	print *,'k_square=',k_square,' k_radius=',k_radius
 
c put in the right order and evaluate weight
 
	call sort_list(dist0,order,k_radius)
 
	do i = 1,k_radius
	ii(i) = ii0(order(i))
	jj(i) = jj0(order(i))
        dist = dist0(order(i))
C this is a point value to recalculate: weight is zero
          if (dist.lt.0.5) then
          weight(i) = 0
C point value not to recalculate
          else
	  weight(i) = 1./(dist**idp)
          endif
	enddo
 
	deallocate (ii0,jj0,dist0,order)
 
	first = .false.
	endif
c------------------------------------------------------------------
*        print *,'no. of mask ',count(mask),' not mask = ',count(.not.mask)
*        allocate (nmask(ncol,nrow))
*        do j = 1,ncol
*          do i = 1,nrow
*          nmask(i,j) = .not.mask(i,j)
*          enddo
*        enddo
c------------------------------------------------------------------
c assign pointers to beach
 
	do l = 1,nbeach
	
	i = mod(ind(l)-1,ncol) + 1
	j = (ind(l)-1)/ncol + 1

        i1 = max(1,i-nradius); i2=min(ncol,i+nradius)
        j1 = max(1,j-nradius); j2=min(nrow,j+nradius)

*	print *,'l=',l,' ind=',ind(l),' i=',i,' j=',j
	
	n = 0
	sumweight = 0

          if (count(mask(i1:i2,j1:j2)).eq.0) go to 200

	  do 100 k = 1,k_radius
	  i0 = i+ii(k)
	  j0 = j+jj(k)
	    if (i0.lt.1.or.i0.gt.ncol) go to 100
	    if (j0.lt.1.or.j0.gt.nrow) go to 100
	  l0 = (j0-1)*ncol + i0
            if (.not.mask(i0,j0)) go to 100
	  n = n+1
	  pt(n,l) = l0
	  weight1(n,l) = weight(k)
	  sumweight = sumweight + weight(k)
	    if (n.ge.nmax) go to 200
100	  continue
 
c	print *,'i0=',i0,' j0=',j0
	
200	npt(l) = n
 
	  do i = 1,n
	  weight1(i,l) = weight1(i,l)/sumweight
	  enddo
 
	enddo

*       deallocate (nmask)
 
	return
	end
