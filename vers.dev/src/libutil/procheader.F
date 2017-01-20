	subroutine procheader (header,names,nmax,n,nvalue)
! input: header with fields separated by space
! output: fields
	implicit none
	integer*4 len, nmax,n, l, i,j, length, k, nvalue
	character header*(*),names(nmax)*(*)
	logical next
 
	l = length(header)
 
	j = 0
	k = 0
	next = .true.
 
	do i = 1,nmax
	names(i) = ' '
	enddo
 
	do i =1,l
 
	if (header(i:i).ne.' ') then
	  if (next) then
	  j = j+1
	  k = 1
	  else
	  k = k+1
	  endif
	names(j)(k:k) = header(i:i)
	next = .false.
 
	else
	next = .true.
	endif
 
	enddo
	
	l = 0
	do i = 1,j
	  if (names(i)(1:4).ne.'fmt=') then
	  l = l+1
	  names(l) = names(i)
	  endif
	enddo
 
	read (names(l),'(i8)',err=100) nvalue
	n = l-1
        return

100     continue
        n = l
        nvalue = 0
 
	return
	end

c=======================================================================
	subroutine proctab (header,names,nmax,n)
c=======================================================================
! input: header with fields separated by tab
! output: fields
	implicit none
	integer*4 nmax,n
	character header*(*),names(nmax)*(*)
        call str2name (header,names,nmax,n,'	')
        return
        end

c=======================================================================
	subroutine procspace (header,names,nmax,n)
c=======================================================================
! input: header with fields separated by space
! output: fields
	implicit none
	integer*4 nmax,n
	character header*(*),names(nmax)*(*)
        call str2name (header,names,nmax,n,' ')
        return
        end

c=======================================================================
	subroutine procfree (header,names,nmax,n)
c=======================================================================
! input: header with fields separated by tab, comma or space
! output: fields
	implicit none
	integer*4 ll, nmax,n, l, i,j, length, k
	character header*(*),names(nmax)*(*)
	logical next
 
	l = length(header)
 
	j = 0
	k = 0
	next = .true.
 
	do i = 1,nmax
	names(i) = ' '
	enddo
 
	do i =1,l
 
	if (header(i:i).ne.'	'.and.header(i:i).ne.' '.and.header(i:i).ne.',') then
	  if (next) then
	  j = j+1
	  k = 1
	  else
	  k = k+1
	  endif
	names(j)(k:k) = header(i:i)
	next = .false.
 
	else
	next = .true.
	endif
 
	enddo
	
        n = j
c move to left
        do i = 1,n
        ll=length(names(i))
          loop1: do j=1,ll
          k = j
            if (names(i)(j:j).ne.' ') exit loop1
          enddo loop1
        names(i) = names(i)(k:ll)
        enddo         

	return
	end


c=======================================================================
	subroutine str2name (header,names,nmax,n,tab)
c=======================================================================
! input: header with fields separated by tab
! output: fields
	implicit none
	integer*4 ll, nmax,n, l, i,j, length, k
	character header*(*),names(nmax)*(*), tab*1
	logical next
 
	l = length(header)
 
	j = 0
	k = 0
	next = .true.
 
	do i = 1,nmax
	names(i) = ' '
	enddo
 
	do i =1,l
 
	if (header(i:i).ne.tab) then
	  if (next) then
	  j = j+1
	  k = 1
	  else
	  k = k+1
	  endif
	names(j)(k:k) = header(i:i)
	next = .false.
 
	else
	next = .true.
	endif
 
	enddo
	
        n = j
c move to left
        do i = 1,n
        ll=length(names(i))
          loop1: do j=1,ll
          k = j
            if (names(i)(j:j).ne.' ') exit loop1
          enddo loop1
        names(i) = names(i)(k:ll)
        enddo         

	return
	end

c=======================================================================
      subroutine name2str (name,n,str,lstr,tab)
c=======================================================================
! input: fields
! output: string with fields separated by tab
      implicit none
      integer n,m        
      character name(n)*(*),str*(*), tab*1
      integer length,lstr,i,l

      l = length(name(1))
        do m = 1,l
           if (name(1)(m:m).ne.' ') exit
        enddo
      str = name(1)(m:)
      lstr = l

      do i = 2,n
      l = length(name(i))
        do m = 1,l
           if (name(i)(m:m).ne.' ') exit
        enddo
      str(lstr+1:) = tab//name(i)(m:l)
      lstr = lstr+1 + (l-m+1)
      enddo

      return
      end
 
c=======================================================================
      subroutine getcol (iun,cval,val,nval,error)
c=======================================================================
*      use constants
      implicit none
c-----------------------------------------------------------------
      integer(4) iun, nval, error, nmax, nh, i,j, ival, length, lname, ldata
      parameter (nmax=100,ldata=256,lname=32)
      character header*(ldata),  msg*3, namecol(nmax)*(lname), cval*(*)
      real(4) numget, val(*)
      logical exist
c-----------------------------------------------------------------
      error = 0
c-----------------------------------------------------------------
      read (iun,'(a)') header
      call nocr(header)
      call proctab (header,namecol,nmax,nh)
      ival = 0
        do i = 1,nh
          if (namecol(i).eq.cval) ival = i
        enddo
c-----------------------------------------------------------------
      if (ival.eq.0) then
      error = 1
      write (*,*) 'not exist ',cval
      rewind (iun)
      return
      endif
c-----------------------------------------------------------------
      nval = 0
          do while (.true.)
          read (iun,'(a)',end=101)  header
          nval = nval+1
          call nocr(header)
          call proctab(header,namecol,nmax,nh)
          val(nval) = numget(namecol(ival),msg)
          enddo
101   continue
      rewind (iun)
      return
      end

c=======================================================================
      subroutine findrec (iun,cval,clog,val,error,irec)
c=======================================================================
*      use constants
      implicit none
c-----------------------------------------------------------------
      integer(4) iun, error, nmax, nh, i,j, ival, length, lh, lname, ldata,irec
      parameter (nmax=100,ldata=256,lname=32)
      character header*(ldata),  msg*3, namecol(nmax)*(lname), cval*(*), clog*2, delim*1
c clog= lt,le,eq,ge,gt
      real(4) numget, val,val1
      logical exist
c-----------------------------------------------------------------
      error = 0
c-----------------------------------------------------------------
      read (iun,'(a)',end=100,err=100) header
      call nocr(header)
      lh = length(header)
      delim=' '
        do i=1,lh
          if (header(i:i).eq.'	') then
          delim='	'
          exit
          endif
        enddo

      call str2name (header,namecol,nmax,nh,delim)
      ival = 0
        do i = 1,nh
          if (namecol(i).eq.cval) ival = i
        enddo
c-----------------------------------------------------------------
      if (ival.eq.0) then
      error = 1
      write (*,*) 'not exist ',cval
      do i= 1,nh
      write (*,*) 'namecol=',namecol(i)(1:length(namecol(i)))
      enddo
      rewind (iun)
      return
      endif
c-----------------------------------------------------------------
      irec=1
          do while (.true.)
          read (iun,'(a)',end=101,err=100)  header
          call nocr(header)
          call str2name (header,namecol,nmax,nh,delim)
          irec = irec+1
          val1 = numget(namecol(ival),msg)
            if (clog.eq.'ge'.and.val1.ge.val) goto 101
            if (clog.eq.'gt'.and.val1.gt.val) goto 101
            if (clog.eq.'eq'.and.val1.eq.val) goto 101
            if (clog.eq.'lt'.and.val1.lt.val) goto 101
            if (clog.eq.'le'.and.val1.le.val) goto 101
          enddo
100   error = 2
      return

101   continue
      backspace (iun)
      return
      end
