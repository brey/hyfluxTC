      subroutine square (ii,jj,dist,nradius,kmax,k)
      implicit none
      integer nradius,i,j,L,kmax,k
      integer*4 ii(kmax),jj(kmax)
      real*4  dist(kmax),dist1,radius 

      radius = float(nradius)

      k=1
      ii(k) = 0
      jj(k) = 0
      dist(k) = 0
c
      do 200 j = 1,nradius

      dist1 = float(j)

      k=k+1
      ii(k) = j
      jj(k) = 0
      dist(k) = dist1

      k=k+1
      ii(k) = -j
      jj(k) = 0
      dist(k) = dist1

      k=k+1
      ii(k) = 0
      jj(k) = j
      dist(k) = dist1

      k=k+1
      ii(k) = 0
      jj(k) = -j
      dist(k) = dist1

      do 100 i = 1,j-1

      dist1 = sqrt(float(i*i)+float(j*j)) 
        if (dist1.gt.radius) go to 100

      k=k+1
      ii(k) = i
      jj(k) = j
      dist(k) = dist1

      k=k+1
      ii(k) = -i
      jj(k) = j
      dist(k) = dist1

      k=k+1
      ii(k) = i
      jj(k) = -j
      dist(k) = dist1

      k=k+1
      ii(k) = -i
      jj(k) = -j
      dist(k) = dist1

      k=k+1
      ii(k) = j
      jj(k) = i
      dist(k) = dist1

      k=k+1
      ii(k) = -j
      jj(k) = i
      dist(k) = dist1

      k=k+1
      ii(k) = j
      jj(k) = -i
      dist(k) = dist1

      k=k+1
      ii(k) = -j
      jj(k) = -i
      dist(k) = dist1

100   continue

      dist1 = sqrt(float(j*j)+float(j*j)) 
        if (dist1.gt.radius) go to 200

      k=k+1
      ii(k) = j
      jj(k) = j
      dist(k) = dist1

      k=k+1
      ii(k) = -j
      jj(k) = j
      dist(k) = dist1

      k=k+1
      ii(k) = j
      jj(k) = -j
      dist(k) = dist1

      k=k+1
      ii(k) = -j
      jj(k) = -j
      dist(k) = dist1

200   continue
#ifdef DOIT
      do L=1,k
      print *,'ii=',ii(L),' jj=',jj(L),' dist=',dist(L)
      enddo
#endif
      return
      end


  
