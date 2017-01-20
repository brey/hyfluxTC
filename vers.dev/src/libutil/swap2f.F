      subroutine swap2f(buf,nbuf)
      implicit none
      integer*4 nbuf,i
      integer*2 buf(nbuf)
c--------------------------------------------------------------------
	integer*2 pp
	integer*1 pp2(2), tmp
	equivalence (pp,pp2)
        integer*2 mask
	logical first /.true./, ok
        data mask/z'EFFF'/
	save first, ok, mask
c--------------------------------------------------------------------
            if (first) then
	    pp = 2
	      if (pp2(2).eq.2) then
              ok = .true.
	      else
	      ok = .false.
	      endif
	    first = .false.
	    endif
c--------------------------------------------------------------------
        if (ok) return
c--------------------------------------------------------------------
        do i = 1,nbuf
        pp = buf(i)
        tmp = pp2(1)
        pp2(1) = pp2(2)
        pp2(2) = tmp
#ifdef DOIT
          if (pp.lt.0) then
          pp = iand(pp,mask)
          pp = -pp
          endif
#endif
        buf(i) = pp
        enddo
        return
        end
