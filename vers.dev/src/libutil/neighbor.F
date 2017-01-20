	subroutine neighbor (step_max,k_max,ii,jj)
	implicit none
	integer*4 i,j,k,step,step_max, i0, j0, k_max
	integer*4 ii(k_max), jj(k_max)
 
	i = 0; j=0; k=0
 
	do step = 1,step_max,2
 
	j  = j-1
 
	i0 = i
		do i = i0,i0+step
		k = k+1
		ii(k) = i
		jj(k) = j
		enddo
	i  = i0+step
	
	j0 = j + 1
		do j = j0,j0+step
		k = k+1
		ii(k) = i
		jj(k) = j
		enddo
	j  = j0+step
 
	i0 = i-1
	    do i = i0,i0-step,-1
		k = k+1
		ii(k) = i
		jj(k) = j
		enddo
	i = i0-step
 
	j0 = j-1
	    do j = j0,j0-step,-1
		k = k+1
		ii(k) = i
		jj(k) = j
		enddo
	j = j0-step
 
	enddo
 
	return
	end
