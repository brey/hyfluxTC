       subroutine cputime_new(xn)
       IMPLICIT none

       REAL*4 Xn, offs

       integer*4 mc, mc0, clocks_per_sec

       SAVE

       data mc0 /-1/, offs/0.d0/
C
       call clock_new (mc,clocks_per_sec)
         if (mc.lt.mc0) offs = offs + 2.d0**32/clocks_per_sec
       xn = offs + float(mc) / float(clocks_per_sec)
*      print *,'mc=',mc,' cloks..=',clocks_per_sec,' mc0=',mc0,' xn=',xn
       mc0 = mc

       RETURN
       END
c 
       subroutine clockm (it0)
       implicit none
       integer*4 it0
       real*4 xn
       call cputime_new(xn)
       it0 = xn*1000
       return
       end