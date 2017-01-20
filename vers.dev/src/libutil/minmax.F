      subroutine minmax (val,nval,vmin,vmax)
	implicit none
	integer nval, i
	real*4 val(nval), vmin, vmax
 
	vmin = 1e37
	vmax = -1e37
 
	do i = 1,nval
	vmin = amin1 (val(i),vmin)
	vmax = amax1 (val(i),vmax)
	enddo
 
	return
	end
