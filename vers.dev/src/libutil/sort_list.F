      subroutine sort_list (cmd,icmd,ncmd)
	implicit none
	integer*4 ncmd, ncmd1
      real*4 cmd(ncmd)
      integer*4 icmd(ncmd)
      integer*4 j1,j2, i,j,k
      logical*4 is
c
      do 10 i=1,ncmd
10    icmd(i)=i

      entry sort_list1 (cmd,icmd,ncmd)
 
      ncmd1=ncmd-1
 
	do 11 i=1,ncmd1
      is=.true.
      k = 0
        do 12 j=1,ncmd1
        j1 = icmd(j)
        j2 = icmd(j+1)
          if(cmd(j1).le.cmd(j2))go to 12
        icmd(j)   = j2
        icmd(j+1) = j1
        k = k+1
        is=.false.
12      continue
          if(is) go to 13
      if (mod(i,10000).eq.0) then
*     print *,'sorting loop ',i,' changes in last loop =',k
      endif
11    continue
 
13    continue
*     print *,'sorted in ',i,' loops'
 
      return
      end
