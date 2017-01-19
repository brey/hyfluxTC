	subroutine getm (pcr,mask,npcr,pcr1,np)
	implicit none
	integer*4 npcr,np,i, pcr(npcr),pcr1(npcr)
      logical*4 mask(npcr)
 
	np = 0
	  do i = 1,npcr
	    if (mask(i)) then
	    np = np+1
	    pcr1(np) = pcr(i)
	    endif
	  enddo
	return
	end
 
	subroutine putm (pcr,mask,npcr,pcr1,np)
	implicit none
	integer*4 npcr,np,i, pcr(npcr),pcr1(np)
      logical*4 mask(npcr)
 
	np = 0
	  do i = 1,npcr
	    if (mask(i)) then
	    np = np+1
	    pcr(i) = pcr1(np)
	    endif
	  enddo
	return
	end
 
