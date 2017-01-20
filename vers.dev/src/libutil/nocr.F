	subroutine nocr (arg)
	implicit none
	character arg*(*), char*1
	integer*4 len, l, i, k
#if defined linux 
	l=len(arg)
 
	do i = l,1,-1
        k = i
	  if (arg(i:i).ne.' '.and.arg(i:i).ne.char(13)) go to 100
	enddo
 
100	continue
	arg = arg(1:i)
#endif
	return
        end
