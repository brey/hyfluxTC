      function indstring(string,nstring,name)
	  implicit none
	  integer*4 indstring, i, nstring
      character string(nstring)*(*), name*(*)
 
      do  i=1,nstring
		if (string(i).eq.name) then
		indstring = i
		return
		endif
	  enddo
	  indstring = -1
      return
      end
