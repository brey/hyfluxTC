      subroutine opnfree (iun,name)
      implicit none
      character name*(*)
      integer iun,ios
      logical open
c
        do iun = 50,1000
        inquire (iun,opened=open)
          if (.not.open) go to 100
        enddo
c
      print *,'no free file'
      iun = 0
      return
c
100   open (iun,file=name,status='unknown',err=200,iostat=ios)
      return
c
200   print *,'error opening file ',name,' on unit ',iun
      print *,'status = ',ios
      iun = 0
      return
      end
c-------------------------------------------
      subroutine iunfree (iun)
      implicit none
      integer iun,ios
      logical open
c
        do iun = 101,1000
        inquire (iun,opened=open)
          if (.not.open) go to 100
        enddo
c
      print *,'no free file'
      iun = 0
      return
c
100   return
      end
c
      subroutine lowstr(buf)
      implicit none
      character buf*(*),cap(26)*1,low(26)*1
      integer ich(0:255)
      logical first
      integer i,lbuf, len
      save first,low,cap,ich
      data low/'a','b','c','d','e','f','g','h','i','j'
     &,'k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'/
      data cap/'A','B','C','D','E','F','G','H','I','J'
     &,'K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
      data first/.true./
c
      if (first)then
      do i=0,255
      ich(i)=i
      enddo
      do i=1,26
      ich(ichar(cap(i)))=ichar(low(i))
      enddo
      first=.false.
      endif
c
      lbuf = len(buf)

      do i=1,lbuf
      buf(i:i)=char(ich(ichar(buf(i:i))))
      enddo
c
      return
      end
