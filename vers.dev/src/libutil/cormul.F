      subroutine cormul(x,y,w,nr,nrmax,b,nv,ier)
      integer ier
c
c     correlazione multipla
c
c     nr    = n.ro rilevazioni
c     nv    = n.ro variabili o termini dell'espressione da calcolare
c     b     = coefficienti dell' espressione
c
      real*4 x(nrmax,nv),y(nrmax),w(nrmax)
      real*8 b(nv)
c
      real*8, allocatable :: wk(:), a(:,:)

      allocate (a(nv,nv),wk(nv))
c
      if(nr.eq.nv)then
      do 1 i=1,nv
      b(i)  =y(i)
      do 1 j=1,nv
      a(i,j)=x(i,j)
1     continue
c
      else
      do 2 i=1,nv
      b(i)  =0.d0
      do 2 j=1,nv
      a(i,j)=0.d0
2     continue

      do 3 i = 1,nr
         do 4 j = 1,nv
*         wk(j) = x(i,j)
          wk(j) = x(i,j)*w(i)
4        continue

         do 5 j = 1,nv
         b(j) = b(j) + wk(j)*y(i)
            do 6 k = 1,nv
*            a(j,k) = a(j,k) + wk(j)*wk(k)
            a(j,k) = a(j,k) + wk(j)*x(i,k)
6           continue
5        continue
3     continue
      endif
c
      call simeq(a,b,nv,nv,ier)
c
      deallocate (a,wk)

      return
      end
c
      subroutine simeq(a,b,n,nmax,kerr)
      implicit real*8(a-h,o-z)
c
c     questa subroutine risolve un sistema di equazioni lineari ax=b
c     a = matrice dei coefficienti del sistema
c     b = vettore delle n costanti del sistema
c     n = ordine del sistema
c     kerr = 1 errore
c            0 nessun errore
c
      parameter (n_max=1000)
c
      dimension a(nmax,nmax), b(nmax), l(n_max), m(n_max)

        if (nmax.gt.n_max) then
        write (*,*)'simeq: nmax=',nmax,' change n_max=',n_max
        kerr=1
        return
        endif
      epsqr=1.d-30
c
      kerr=0
c
c     pulisce i vettori di permutazione
c
      do 3 i=1,n
      m(i)=0
3     l(i)=0
c
c     loop per n punti di pivot
c
      do 14 kp=1,n
      p=0.
      psqr=0.

      do 7 i=1,n
      do 7 j=1,n
      if(m(i))7,4,7
4     if(l(j))7,5,7
5     t=a(i,j)
      tsqr=t*t
      if(tsqr-psqr)7,7,6
6     p=t
      psqr=tsqr
      kr=i
      kc=j
7     continue

      if(psqr-epsqr)17,17,8

8     m(kr)=kc
      l(kc)=1
c
c     divide la riga chiave per il pivot
c
      do 10 j=1,n
      if(l(j))10,9,10
9     a(kr,j)=a(kr,j)/p
10    continue

      bkr=b(kr)/p
      b(kr)=bkr
c
c     sottrae il multiplo della riga chiave dalle altre righe
c
      do 14 i=1,n
      if(i-kr)11,14,11
11    r=a(i,kc)

      do 13 j=1,n
      if(l(j))13,12,13
12    a(i,j)=a(i,j)-r*a(kr,j)
13    continue

      b(i)=b(i)-r*bkr
14    continue
c
c     riordina i risultati
c
      do 15 i=1,n
      ip=m(i)
15    a(ip,1)=b(i)

      do 16 i=1,n
16    b(i)=a(i,1)
      return
c
c     errore
c
17    kerr=1
      return
      end
