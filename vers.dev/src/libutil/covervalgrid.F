        module distGrid_mod
        implicit none
        logical distance
        real*4, allocatable :: dist(:)
        end module distGrid_mod
*================================================================================
      module map_cover_data
*================================================================================
      implicit none
c     ---------------------------
      real(4)  idp_cover /1./
      integer(4)  nmax_cover /1/
      integer(4)  nradius_cover 
      logical(4)  cover
      end module map_cover_data
c==========================================================================================
        subroutine coverMap (z,nx,ny,maskreq,iout)
c==========================================================================================
        use map_cover_data
        implicit none
c--------------------------------------------------------------------
        integer(4)  nx, ny,nval, iout
        real(4) z(nx*ny)
      logical*1 maskreq(nx*ny)
c--------------------------------------------------------------------
c local variables
        integer(4) i,j
        logical(4) eqmv,findbeach
      integer*1, allocatable :: beach(:)
c--------------------------------------------------------------------
        nval = nx*ny
c--------------------------------------------------------------------
	allocate (beach(nval))
 
        j = 0
        do i = 1,nval
          if (maskreq(i).and.eqmv(z(i))) j=j+1
        enddo
        if (iout.gt.0) then
	write (iout,*)
	write (iout,*) 'coverMap'
      write (iout,*) 'nmaskreq=',count(maskreq),' no. of MV to cover =',j
	write (iout,*) 'nmax_cover=',nmax_cover,' nradius_cover=',nradius_cover,' idp_cover=',idp_cover
        endif

        findbeach = .false.
          if (j.gt.0) then
  	call covervalgrid (maskreq,z,beach,findbeach,nmax_cover
     &                    ,nradius_cover,idp_cover,nval,nx,ny)
            if (findbeach.and.iout.gt.0) then
            write (iout,*) 'CAUTION: not all MV have been covered'
            endif
          endif
        deallocate (beach)
c----------------------------------------------------------------
        return
        end
c---------------------------------------------
	subroutine covervalgrid (mask,val,beach,findbeach,nmax
     &                  ,nradius,idp,npcr,ncol,nrow)
        use distGrid_mod
	implicit none
	integer*4 npcr,nmax,nradius,ncol,nrow
	logical*1 mask(npcr)
	real*4 val(npcr), idp
	integer*1 beach(npcr)
        logical findbeach
c local variables
 
	real*4,    allocatable :: weight(:,:)
	integer*4, allocatable :: ind(:), pt(:,:),npt(:)
        integer*4, allocatable :: ix(:), iy(:)
	logical*1, allocatable :: maskval(:), maskold(:)
 
	logical*4 first /.true./, ok, eqmv
	real*4 dx, dy
	integer*4 i, j, l, nbeach
 
	real*4     mv, sumweight
	integer*4  mv4, length
	integer*1  mv1
 
	save first,maskval, maskold, ind, pt, npt, weight
c---------------------------------------------
	if (first) 	then
	call getmv(mv)
	call getmv1(mv1)
	call getmv4(mv4)

*	print *,'npcr=',npcr
	allocate (maskval(npcr),maskold(npcr),dist(npcr),ix(npcr),iy(npcr))
        L=0
          do j = 1,nrow
            do i = 1,ncol
            L = L+1
            ix(L) = i
            iy(L) = j
            enddo
          enddo
	endif
 
c define mask of the available values
 
	  do i = 1,npcr
	  maskval(i) = .not.eqmv(val(i))
	  enddo
 
c see if new map has the same mask of old map
 
	if (.not.first) then
	ok = .true.
	  do i =1,npcr
	    if (.not.maskold(i).and.maskval(i)) ok = .false.
	    if (maskold(i).and..not.maskval(i)) ok = .false.
	  enddo
 
	  if (ok) go to 400
	
	deallocate (ind,pt,npt,weight)
c	print *,' evaluate new point int.'
	endif
 
	maskold = maskval
c-------------------------------------------------------------------
c find no. of beach values
 
	j = 0
	do i = 1,npcr
	  if (mask(i).and..not.maskval(i)) then
	  j = j+1
	  endif
	enddo
 
	nbeach = j
*       print *,'coverval: no. of MV to cover =',nbeach
 
c assign indeces
 
	allocate (ind(nbeach),pt(nmax,nbeach),npt(nbeach)
     &         ,weight(nmax,nbeach))
 
	ind = mv4
 
	j = 0
	do i = 1,npcr
	  if (mask(i).and..not.maskval(i)) then
	  j = j+1
	  ind(j)=i
	  endif
	enddo
 
c------------------------------------------------------------------
c construct indeces of neighboroud
 
      call findindex (maskval,npcr,ind,pt,weight,npt,nmax,nbeach
     &                  ,nradius,ncol,nrow,idp)
	
c       dist = val*0
        dist = mv
        do i = 1,npcr
          if (maskval(i)) dist(i) = 0
        enddo

	do  i = 1,nbeach
        L = ind(i)
	  if (npt(i).eq.0) then
	  beach(L) = 1
	  findbeach = .true.

          else
          dx = ix(L)-ix(pt(1,i))
          dy = iy(L)-iy(pt(1,i))
          dist(L) = sqrt(dx*dx+dy*dy)
	  endif
	enddo
 
c------------------------------------------------------------------
c cover MV
 
400	continue
 
	do 300 i = 1,nbeach
	  if (npt(i).eq.0) go to 300
	l = ind(i)
	val(l) = 0.
	  do j = 1,npt(i)
	  val(l) = val(l) + val(pt(j,i))*weight(j,i)
	  enddo
*	print *,'valnew=',val(l),(val(pt(j,i)),j=1,npt(i))
300   continue
 
	do i = 1,npcr
	  if (.not.mask(i)) val(i) = mv
	enddo
 
	first = .false.
	return
	end

	subroutine getdist(dist1,npcr)
        use distGrid_mod
	implicit none
	integer npcr
	real*4 dist1(npcr)
	dist1 = dist
	return
	end
