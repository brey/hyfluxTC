      subroutine swap4f(buf,nbuf)
      implicit none
      integer*4 nbuf,i
      integer*4 buf(nbuf)
c--------------------------------------------------------------------
	integer*4 pp
	integer*1 pp4(4), tmp
	equivalence (pp,pp4)
	logical first /.true./, ok
	save first, ok
c--------------------------------------------------------------------
            if (first) then
	    pp = 2
	      if (pp4(4).eq.2) then
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
        tmp = pp4(1)
        pp4(1) = pp4(4)
        pp4(4) = tmp
        tmp = pp4(2)
        pp4(2) = pp4(3)
        pp4(3) = tmp
        buf(i) = pp
        enddo
        return
        end
