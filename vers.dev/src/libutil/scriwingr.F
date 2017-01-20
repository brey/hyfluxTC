      Subroutine ScriWinGr1 (ddaata,names,nc,np,fout)
	implicit none
c scrive un file WinGraf
 
c nc = no curve, np=no punti
 
	integer*4 nc,np
      real*4   ddaata(nc, np)
	character names(nc)*20
c
c-------------------------------------------------
c	local variables
#if defined WIN32
      logical ftn /.true./
#else
      logical ftn /.false./
#endif
 
      integer *4 in(250)
      INTEGER*4  i, j, k, id, np1, i1, j1, i2, j2, n1, n2
     &          ,j0, iun
	real*4 xx,yy
      REAL*8   YMIN , YMAX, a0
 
      character riga*80,riga1*81,risp*4 , name0*40
      character fout*(*)
c------------------------------------------------
	call  iunfree(iun)
c------------------------------------------------
      if(ftn)then
         write(6,*) 'ftn=true'
         open(unit=iun,file=fout,form='formatted',recl=80)
      else
         write(6,*) 'ftn=false'
         open(unit=iun,file=fout,form='formatted',recl=81)
      endif
 
 
      do 280  i=1,nc
 
         do 210 j=1,80
210      in(j)=69
 
c        write(6,*) 'nc=',nc,'np=',np
         ymin=1.e30
         ymax=-1.e30
 
         do 220 id=1,np
            if(ddaata(i,id).lt.ymin) ymin=ddaata(i,id)
            if(ddaata(i,id).gt.ymax) ymax=ddaata(i,id)
220      continue
 
         if(ymax.eq.ymin)ymax=ymax+1.e-9
 
         if(i.eq.1) names(i)='time'
 
         name0=names(i)
         write(riga1,12800) name0(1:8),np,name0(9:16),ymin,ymax,nc
         if(ftn) then
            write(iun,12900) riga1(1:80)
         else
            write(iun,13000) riga1(1:80)
         endif
12800    format(a8,i4,t15,a8,t30,e11.4,t50,e11.4,t76,i4,t80,'B')
12900    format(a80)
13000    format(a80 ,t81,'*')
230      continue
 
13100    format(1x, a8,i4,t15,a8,t30,e11.4,t50,e11.4,t76,i4)
         np1=(int((np-1.)/1000.)+1)*1000-1
 
	   do 280 i1=1,np1,40
            do 270 j1=1,40
               j=j-1
               i2=i1+j1-1
               n1=69
               n2=69
               if(i2.gt.np) then
                  goto  240
               endif
               yy=ddaata(i,i2)
               if(yy.gt.ymax.or.yy.lt.ymin) then
			    yy=ymax
                  write (6,*) ' !!!!!! '
               endif
 
               if(ymax.eq.ymin) goto  240
               a0   =int(((yy-ymin)/(ymax-ymin))*(255.**2))
               goto  250
240            a0=1
250            n2=int(a0/255)
               n1=a0-n2*255
260            j0=j1*2-1
               j2=j0+1
c         if (n1.lt.0.or.n2.lt.0)   write(6,*)np1,i2,yy,j0,j2,n1,n2c
c       write (6,*) a0,n1,n2,yy,ymin,ymax
               in(j0)=n1
270         in(j2)=n2
 
            write (riga1,13200)(char(in(k)),k=1,80)
            if(ftn) then
               write(iun,12900) riga1(1:80)
            else
               write(iun,13000) riga1(1:80)
            endif
 
280   continue
 
13200 format (80a1)
c per risc 6000
c201     format (80a1,'E')
      close(iun)
      write (6,*) 'written ',nc,' descriptors on ',fout
 
      return
      end
c-------------------------------------------------------------------
      Subroutine ScriWinGr2 (ddaata,names,nc,np)
	implicit none
c scrive un file WinGraf: database 2
 
c nc = no curve, np=no punti
 
	integer*4 nc,np
      real*4   ddaata(nc, np)
	character names(nc)*40, char*1
c
c-------------------------------------------------
c	local variables
 
      integer *4 in(250)
      INTEGER*4  i, j, k, l, id, np1
     &          , iun, itipo, npl, nbyte, nlines
	real*4 xx,yy
      REAL*8   YMIN , YMAX, a0
 
      character riga*80,risp*4 , name0*40
      character fout*(*)
	logical*4 comp
	save iun
c------------------------------------------------
	go to 10
	entry openWinGr2 (fout)
#if defined WIN32
         write(6,*) 'ftn=true'
         open(unit=iun,file=fout,form='formatted',recl=80)
#else
         write(6,*) 'ftn=false'
         open(unit=iun,file=fout,form='formatted',recl=81)
#endif
	return
c------------------------------------------------
10	continue
      do 280  i=1,nc
 
         do 210 j=1,80
210      in(j)=69
 
c        write(6,*) 'nc=',nc,'np=',np
         ymin=1.e30
         ymax=-1.e30
 
         do 220 id=1,np
            if(ddaata(i,id).lt.ymin) ymin=ddaata(i,id)
            if(ddaata(i,id).gt.ymax) ymax=ddaata(i,id)
220      continue
 
         if(ymax.eq.ymin)ymax=ymax+1.e-9
 
         if(i.eq.1) then
	   itipo = 1
	   comp = .false.
	   npl =20
	   nbyte = 4
 
	   else
	   itipo = 2
	   npl = 40
	   nbyte = 2
	   comp = .true.
	   endif
c------------------------------------------------
         name0=names(i)
	   riga = ' '
         write(riga,12800) name0,ymin,ymax,itipo
12800    format(a40,1x,'WgD2.1',2x,e12.6,3x,e12.6,3x,i1)
         print *,riga
	   call writedb (iun,riga)
c	--------------------------------
	 riga = ' '
	 write (riga(1:9),'(i9)') np
	 call WriteDb (iun,riga)
	 riga = names(1)
	 call WriteDb (iun,riga)
c------------------------------------------------
 
	nlines = ((int(np-1.)/1000+1)*1000)/npl
	np1 = nlines*npl
 
	k = 1	
	do j = 1,np1 	
 
	  if (ymax.le.ymin.or.j.gt.np) then
c	  in(k) = 1
c	  in(k+1:k+nbyte-1) = 0
	  in(k:k+nbyte-1) = 0
	  else
	  yy= (ddaata(i,j)-ymin)/(ymax-ymin)
	  call CompVal (yy,comp,in(k))
	  endif
 
	k = k + nbyte
	   if (k.ge.80) then
         write (riga,'(80a1)')(char(in(l)),l=1,80)
	   call WriteDb (iun,riga)
	   k = 1
	   endif
	enddo
 
	if (k.ne.1) print *,'i=',i,' k=',k
 
280	continue
 
c      close(iun)
      write (6,*) 'written ',nc,' descriptors'
 
      return
      end
c-------------------------------------------------------------------
      Subroutine ScriWinGr0 (ddaata,names,nc,np,fout)
	implicit none
c scrive un file WinGraf: database 2
 
c nc = no curve, np=no punti
 
	integer*4 nc,np
      real*4   ddaata(nc, np)
	character names(nc)*20
c
c-------------------------------------------------
c	local variables
 
      integer *4 in(250)
      INTEGER*4  i, j, k, l, id, np1
     &          , iun, itipo, npl, nbyte, nlines
	real*4 xx,yy
      REAL*8   YMIN , YMAX, a0
 
      character riga*80,risp*4 , name0*40
      character fout*(*)
	logical*4 comp
c------------------------------------------------
	call  iunfree(iun)
c------------------------------------------------
 
      open(unit=iun,file=fout,form='formatted',recl=80)
 
      do 280  i=1,nc
 
         do 210 j=1,80
210      in(j)=69
 
c        write(6,*) 'nc=',nc,'np=',np
 
         ymin=1.e30
         ymax=-1.e30
 
         do 220 id=1,np
            if(ddaata(i,id).lt.ymin) ymin=ddaata(i,id)
            if(ddaata(i,id).gt.ymax) ymax=ddaata(i,id)
220      continue
 
         if(ymax.eq.ymin)ymax=ymax+1.e-9
 
	   itipo = 2
	   npl = 40
	   nbyte = 2
	   comp = .true.
c------------------------------------------------
         name0=names(i)
	   riga = ' '
         write(riga,12800) name0(1:8),np,name0(9:16),ymin,ymax,nc
12800    format(a8,i4,t15,a8,t30,e11.4,t50,e11.4,t76,i4,t80,'B')
         print *,riga
	   call writedb (iun,riga)
c------------------------------------------------
 
	nlines = ((int(np-1.)/1000+1)*1000)/npl
	np1 = nlines*npl
 
	k = 1	
	do j = 1,np1 	
 
	  if (ymax.le.ymin.or.j.gt.np) then
c	  in(k) = 1
c	  in(k+1:k+nbyte-1) = 0
	  in(k:k+nbyte-1) = 0
	  else
	  yy= (ddaata(i,j)-ymin)/(ymax-ymin)
	  call CompVal (yy,comp,in(k))
	  endif
 
	k = k + nbyte
	   if (k.ge.80) then
c	   print *,(in(l),l=1,80)
         write (riga,'(80a1)')(char(in(l)),l=1,80)
	   call WriteDb (iun,riga)
	   k = 1
	   endif
	enddo
 
	if (k.ne.1) print *,'i=',i,' k=',k
 
280	continue
 
      close(iun)
      write (6,*) 'written ',nc,' descriptors on ',fout
 
      return
      end
c------------------------------------------------
      subroutine WriteDB (iun,riga)
	implicit none
	integer*4 iun
	character riga*80, char*1
c
       write(iun,100) riga(1:80)
#if defined WIN32
100    format(a80)
#else
100    format(a80 ,t81,'*')
#endif
	return
	end
 
	subroutine CompVal(yy,comp,in)
	implicit none
	real*4 yy
	logical comp, first
	integer*4 in(4), i
	real*8 pot(4), ipot(4),a0
	data first /.true./
	save first, pot
 
	if (first) then
	do i = 1,4
	pot(i) = 255.d0**i
	ipot(i) = 1.d0/pot(i)
	enddo
	first = .false.
	endif
 
	if (comp) then
	a0  = yy*pot(2)
      in(2) = int(a0*ipot(1))
      in(1) = int(a0-in(2)*pot(1))
 
	else
	a0 = yy*pot(4)
	in(4) = int(a0*ipot(3))
	in(3) = int((a0-in(4)*pot(3))*ipot(2))
	in(2) = int((a0-in(4)*pot(3)-in(3)*pot(2))*ipot(1))
	in(1) = int( a0-in(4)*pot(3)-in(3)*pot(2)-in(2)*pot(1))
	endif
 
	return
	end
