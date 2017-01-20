        subroutine currentdate (cdate,ctime)
        character cdate*11, ctime*8
        integer*4 iy,im,id, ih, imin, isec, values(8), time, t(9), ia(3)
        character cmo(12)*3 /'Jan','Feb','Mar','Apr','May','Jun','Jul'
     &          ,'Aug','Sep','Oct','Nov','Dec'/

#if WIN32
      call DATE_AND_TIME (values=values)
	iy = values(1)
	im = values(2)
	id = values(3)
      write (cdate,'(i4,2i2)') iy,im,id
#elif defined lf95
/*
c  Greenwich mean time
      call gmtime (time(),t)
      print *,' t=',t
      iy = t(6) + 1900
      im = t(5)+1
      id = t(4)
      ih = t(3)
      imin = t(2)
      isec = t(1)
*/
c Real time
      call itime (ia)
      ih = ia(1)
      imin=ia(2)
      isec = ia(3)
      call idate (ia)
      id = ia(1)
      im = ia(2)
      iy = ia(3)

      write (cdate,'(i2,1x,a,1x,i4)') id,cmo(im),iy
      write (ctime,'(i2,a1,i2,a1,i2)') ih,':',imin,':',isec

#elif defined linux
      call DATE_AND_TIME (cdate)
      read (cdate,'(i4,2i2)') iy,im,id
#endif
      call fillzero(ctime,8)
      return
      end
