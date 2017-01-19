c
      subroutine valuta(buff,i1,i2,ns)
      implicit none
      character buff*(*),app*1
      integer i1(*),i2(*),ns,nc,i,length
      data app/'"'/
c
      nc=length(buff)

      ns=0
      i=0
c
310   i=i+1
      if(i.gt.nc)go to 312
      if(buff(i:i).eq.' ')go to 310
      if(buff(i:i).eq.app)go to 10
      ns=ns+1
      i1(ns)=i
c
313   continue
      i=i+1
      if(i.gt.nc)go to 311
      if(buff(i:i).ne.' ')go to 313
      i2(ns)=i-1
      go to 310
c
10    continue
      ns=ns+1
      i=i+1
      i1(ns)=i
13    continue
        if(buff(i:i).eq.app)then
        i=i+1
          if(i.ge.nc)then
          i2(ns)=nc-1
          go to 312
          endif
          if(buff(i:i).ne.app)go to 14
        endif
      i=i+1
      if(i.gt.nc) go to 311
      go to 13
14    i2(ns)=i-2
      go to 310
c
311   i2(ns)=nc
312   continue
      return
      end
