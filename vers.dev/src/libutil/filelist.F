      subroutine filelist (nmap_max,wildmap,nwild,maps,nmaps)
#ifdef WIN32
	USE DFlib
	implicit none
c-------------------------------------------------------------
c local variables
	integer*4 i,j,k,nmaps, handle, nwild, nmap_max
	
	character maps(nmap_max)*132
     &         ,files*132, wildmap(nwild)*132
     &          ,bs*1 /'\'/
	
	logical*4 ok, toomap
 
	TYPE (FILE$INFO) info
c-----------------------------------------------------------
	i = 0
 
	do j = 1,nwild
	files = wildmap(j)
 
	handle = FILE$FIRST
 
	  DO WHILE (.TRUE.)
	  k = GETFILEINFOQQ(files, info, handle)
 
	    IF ((handle .EQ. FILE$LAST) .OR. k.eq.0 .or.
     & 	   (handle .EQ. FILE$ERROR)) exit
	  toomap = i.gt.nmap_max
		if (toomap) exit
	  i = i+1
	  maps(i)= info%name
	  end do
 
	enddo
 
	nmaps = i
c---------------------------------------------------------------
#else
	integer*4 i,j,k,nmaps, nwild, nmap_max, lw, length, iun
	
	character maps(nmap_max)*132
     &         ,files*132, wildmap(nwild)*132
c-----------------------------------------------------------
      character command*132
 
      j = 0
 
      do i = 1,nwild
      lw = length(wildmap(i))
 
      command = 'ls '//wildmap(i)(1:lw)//' 1> scratch.nob 2>err.nob'
      print *,command
      call system(command)
      call opnfree (iun,'scratch.nob')
 
        do while (.true.)
        read(iun,'(a)',end=100) files
          if (files(1:3).eq.'ls:') go to 100
        j = j + 1
        maps(j) = files
        enddo
 
100   continue
      enddo
 
      nmaps = j
 
      close(iun,status='delete')
#endif
	return
	end
c-------------------------------------------------------------
      subroutine CheckList (wildmap,nwild,nmaps)
#ifdef WIN32
	USE DFlib
	implicit none
c-------------------------------------------------------------
c local variables
	integer*4 i,j,k,it0, time, nmaps, nwild, ldir, lpwd
	
	character clonemap*132, ch*132
     &         ,files*132, wildmap(nwild)*132
     &          ,bs*1 /'\'/
	integer*4 length, handle, lm
	
	logical*4 ok
 
	TYPE (FILE$INFO) info
 
c-----------------------------------------------------------
	i = 0
 
 
	do j = 1,nwild
	files = wildmap(j)
 
	handle = FILE$FIRST
 
	  DO WHILE (.TRUE.)
	  k = GETFILEINFOQQ(files, info, handle)
 
	    IF ((handle .EQ. FILE$LAST) .OR. k.eq.0 .or.
     & 	   (handle .EQ. FILE$ERROR)) exit
	  i = i+1
	  enddo
 
	enddo
	
	nmaps = i
c---------------------------------------------------------------
#else
	integer*4 i,j,k,nmaps, nwild, nmap_max, lw, length, iun
	
	character files*132, wildmap(nwild)*132
c-----------------------------------------------------------
      character command*132
 
      j = 0
 
      do i = 1,nwild
      lw = length(wildmap(i))
 
      command = 'ls '//wildmap(i)(1:lw)//' 1>scratch.nob 2>err.nob'
	print *,command
      call system(command)
      call opnfree (iun,'scratch.nob')
 
        do while (.true.)
        read(iun,*,end=100)
        j = j + 1
        enddo
 
100   continue
 
      enddo
 
      nmaps = j
 
      close(iun,status='delete')
#endif
	return
	end
c--------------------------------------------------------
#ifdef WIN32
      function  getdir (pwd)
c returns the current directory and the no of characters of pwd
      use DFlib
      implicit none
      integer*4 getdir, length, l
      character pwd*(*)
      getdir = GETDRIVEDIRQQ (pwd)
      return
      end
c--------------------------------------------------------
      function  changedir (pwd)
c change directory and the no of characters of pwd
      use DFlib
      implicit none
      integer*4 changedir, l, length
      character pwd*(*)
      changedir = CHANGEDIRQQ (pwd)
      return
      end
#elif linux
      function  getdir (pwd)
c returns the current directory and the no of characters of pwd
      implicit none
      integer*4 getdir, length, l
      character pwd*(*)
      integer*4 GETCWD
      l = GETCWD (pwd)
        if (l.eq.0) then
        getdir = length(pwd)
        else
        getdir = 0
        endif
      return
      end
c--------------------------------------------------------
      function  changedir (pwd)
c change directory and the no of characters of pwd
      implicit none
      integer*4 changedir, l, length
      character pwd*(*)
      integer*4 chdir
      l = chdir (pwd)
        if (l.eq.0) then
        changedir = length(pwd)
        else
        changedir = 0
        endif
      return
      end
#endif
