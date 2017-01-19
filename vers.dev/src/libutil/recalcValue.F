       subroutine recalcValues (valold,maskvalold,dist1,val,nval
     &                          ,weightval,existweight
     &                          ,pt,dist,weight,nmax,nradius,idp)
c recalc values and distance using inverse distance method
c called if filtval or cross kriging is requested
       use findind_mod
       implicit none
       integer i,j, L, L1, n, nmax,nval,nradius, np
       real*8 dist1(nval)
       real*4 valold(nval),val(nval),weightval(nval)
     &       ,dist(nmax),weight(nmax), idp, distance, sumweight
        real*4     mv /z'ffffffff'/
       integer*4 pt(nmax)
       logical*1 maskvalold(nval)
       logical existweight
c
c---------------------------------------------------------
c start of loop over requested pixels to cover
c---------------------------------------------------------

	do 300 i = 1,nreq
*       if (i.le.2) print *,'i=',i
c---------------------------------------------------------
	L = Lreq(i)
c---------------------------------------------------------
        n = 0
*       print *,'L=',L,' maskval=',maskval(L),' npt=',npt(i),' nmax=',nmax

          if(maskvalold(L)) then
            do j = 2,npt(i)
            L1 = pt1(pt2(i)+j)
              if (maskvalold(L1)) then
              n = n+1
              pt(n) = L1
              dist(n) = distance(L1,L)
                if (dist(n).gt.float(nradius).or.n.ge.nmax) go to 10
              endif
            enddo
          endif
*------------------------------------------------
10        continue

c pt are the current pointers

          np = n
*------------------------------------------------
          if (np.le.0) then
          dist1(L) = mv
          val(L) = mv
          go to 300         
          endif
*------------------------------------------------
	  val(L) = 0.
          dist1(L) = 0.
          sumweight = 0.

            do j = 1,np
            L1 = pt(j)
            weight(j) = 1/max(dist(j),0.01)**idp
              if (existweight) weight(j)=weight(j)*weightval(L1)
	    val(L) = val(L) + valold(L1)*weight(j)
            dist1(L) = dist1(L) + dist(j)*weight(j)
            sumweight = sumweight + weight(j)
            enddo

         dist1(L) = dist1(L)/sumweight
         val(L) = val(L)/sumweight

c---------------------------------------------------------
c end of loop 
300   continue
      return
      end
