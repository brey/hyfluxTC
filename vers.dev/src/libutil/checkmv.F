	Subroutine checkmv (val,mv,mask,nval)
	implicit none
	integer*4 nval, mv, i
	integer*4 val(nval)
c caution: mask is logical*1 !!!
	logical*1 mask(nval)
 
 
	do i = 1,nval
	mask(i) = val(i).eq.mv
	enddo
 
	return
	end
c-----------------------------------------
/*
      function eqmv (val)
      implicit none
      integer*2 val(2)
      logical eqmv
*      integer*2 MV(2)/z'ffff',z'ffff'/
      integer*2 MV(2)/-1,-1/
      eqmv = val(1) .eq. MV(1).and.val(2).eq.MV(2)
      return
      end
*/
c-----------------------------------------
      function eqmv (val)
      implicit none
      integer*4 val, mv
      logical eqmv
      call getmv(mv)
      eqmv = val .eq. MV
      return
      end
