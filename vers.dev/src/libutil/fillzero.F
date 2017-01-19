      subroutine fillzero (str,len)
	  implicit none
	  character str*(*)
	  integer*4 len, i
 
	  do i = 1,len
	    if (str(i:i).eq.' ') str(i:i)='0'
	  enddo
	  return
	  end

      subroutine getscratch (file)
      character cpid*12,file*(*)
      integer pid, getpid
      pid=getpid()
      write (cpid,'(i12)') pid
      call fillzero(cpid,12)
      file="/tmp/scratch"//cpid//".nob"
      return
      end
c
      subroutine sendcomm(comm,result)
      character*(*) comm, result
      character command*256, scratch*256
      integer iun,length

      call getscratch (scratch)

      result = ' '
      ll = length(comm)
      command = comm(:ll)//" > "//scratch(1:length(scratch))
      call system(command)
      call opnfree (iun,scratch)
        if (iun.le.0) return
      read(iun,'(a)',end=10,err=20) result
10    continue
      close(iun,status='delete')
      return
c
20    continue
      print *,'sendcomm: error from file ',scratch(1:length(scratch))
      print *,'err.result=',result(1:length(result))
      close (iun,status='delete')
      return
      end
