       function toldd(val)
       implicit none
       integer*1 toldd, tolddfun(0:128), mv1
       integer*2 val
       logical*4 first
       data first /.true./
       save first, tolddfun
c
       if (first) then
       call getmv1 (mv1)
       tolddfun(0) = 5
       tolddfun(1) = 6
       tolddfun(2) = 3
       tolddfun(4) = 2
       tolddfun(8) = 1
       tolddfun(16) = 4
       tolddfun(32) = 7
       tolddfun(64) = 8
       tolddfun(128) = 9
       first = .false.
       endif

       if (val.lt.0.or.val.gt.128) then
       print *,'val=',val
       stop
       endif

       toldd = tolddfun(val)
        
       return
       end
