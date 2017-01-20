	subroutine string_to_names (string,names,nmax,n)
c input: string with fields separated by spaces
c output: fields
	implicit none

	integer*4 len, nmax,n, i
	character string*(*),names(nmax)*(*)
        integer*4, allocatable :: i1(:),i2(:)

        allocate (i1(nmax),i2(nmax))

        call valuta (string, i1,i2,n)

 
	do i = 1,n
	names(i) = string(i1(i):i2(i))
	enddo

        deallocate (i1,i2)
 
	return
	end
