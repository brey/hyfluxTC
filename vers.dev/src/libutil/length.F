	function length (arg)
	implicit none
	character arg*(*), char*1, null*1
	integer*4 len, length, l, i
 
	l=len(arg)
        null = char(0)
 
	do i = l,1,-1
	  if (arg(i:i).ne.' '.and.arg(i:i).ne.null) go to 100
	enddo
100	continue
	length = i
	return
        end
