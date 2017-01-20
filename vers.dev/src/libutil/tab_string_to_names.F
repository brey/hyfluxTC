	subroutine tab_string_to_names (string,names,nmax,n)
c input: string with fields separated by tab
c output: fields
	implicit none
	integer*4 len, nmax,n, l, i, length, k
	character string*(*),names(nmax)*(*)
 
	l=length(string)
 
	do i = 1,nmax
	names(i) = ' '
	enddo
 
	n=1
	k=0
 
	do i = 1,l
c separator is a tab
	  if (string(i:i).eq.'	') then
	    if (n.ge.nmax) return
	  n=n+1
	  k=0
	  else
	  k=k+1
	  names(n)(k:k) = string(i:i)
	  endif
 
	enddo
	return
	end
