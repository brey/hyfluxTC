
*====================================================================================
       subroutine cputime(cpu)
*====================================================================================
	real*4 cpu
	call cpu_time(cpu)
	return
	end

*====================================================================================
       subroutine elapstime(xn)
*====================================================================================
       IMPLICIT none

       REAL*4, save :: offs
       real*4 xn
       integer*4, save :: count0

       integer*4 count, clocks_per_sec, count_max

       data count0 /-1/, offs/0.d0/
C
       call system_clock (count,clocks_per_sec,count_max)
         if (count.lt.count0) offs = float(count_max)/float(clocks_per_sec)
       xn = offs + float(count) / float(clocks_per_sec)
       count0 = count

       RETURN
       END