      function avgval (val,nval,mask)
	implicit none
	integer*4 nval, i,j
	real*4 val(nval), avgval
      logical*1 mask(nval)
      logical*4 eqmv
 
	avgval = 0.
	j =0
 
	do i = 1,nval
          if (mask(i).and..not.eqmv(val(i))) then
          j = j+1
          avgval = avgval + val(i)
          endif
	enddo
 
	avgval = avgval / float(j)
 
	return
	end
