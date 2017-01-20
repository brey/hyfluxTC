      subroutine getgdalval (iband,val)
      implicit none
      integer*4 iband, nval, i
      real*4 val(*), mv
      real*8 nodata

      call getmv (mv)      

      call GETGDALVAL0 (iband,val,nval,nodata)

      do i = 1,nval
        if (val(i).eq.nodata) val(i) = mv
      enddo
      return
      end

      subroutine getmv(mv)
      implicit none
      integer*4 mv
      mv = -1
      return
      end

      subroutine getgdalval1 (iband,val)
      implicit none
      integer*4 iband, nval, i
      integer*1 val(*), mv
      real*8 nodata

      call getmv1 (mv)      

      call GETGDALVAL0 (iband,val,nval,nodata)

      do i = 1,nval
        if (val(i).eq.nodata) val(i) = mv
      enddo
      return
      end


      subroutine getgdalval2 (iband,val)
      implicit none
      integer*4 iband, nval, i
      integer*2 val(*), mv
      real*8 nodata

      call getmv2 (mv)      

      call GETGDALVAL0 (iband,val,nval,nodata)

      do i = 1,nval
        if (val(i).eq.nodata) val(i) = mv
      enddo
      return
      end

      subroutine getgdalval4 (iband,val)
      implicit none
      integer*4 iband, nval, i
      integer*4 val(*), mv
      real*8 nodata

      call getmv4 (mv)      

      call GETGDALVAL0 (iband,val,nval,nodata)

      do i = 1,nval
        if (val(i).eq.nodata) val(i) = mv
      enddo
      return
      end

      subroutine OUTGDAL (name, ncol,nrow, PCRtyp, xstart, ystart, cellsizex, buf, nval, cellsizey, outfmt, nband, error)
      implicit none
      character*(*) name , outfmt 
      integer ncol, nrow, nband, nval, PCRtyp
      integer error
      double precision xstart, ystart, cellsizex,  cellsizey      
      real*4 buf(*)
      real*4, allocatable :: outbuf(:)
      real*8 nodata /99999.d0/
      logical eqmv
      integer i


      if ( outfmt(1:8).eq."PCRaster" )  then
      call OUTGDAL0 (name, ncol,nrow, PCRtyp, xstart, ystart, cellsizex, buf, nval, cellsizey, outfmt, nband, error)
      else
      allocate  (outbuf(nval))
        do i = 1,nval
          if (eqmv(buf(i))) then
          outbuf(i) = nodata
          else
          outbuf(i) = buf(i)
          endif
        enddo
      call OUTGDAL0 (name, ncol,nrow, PCRtyp, xstart, ystart, cellsizex, outbuf, nval, cellsizey, outfmt, nband, error)
      deallocate (outbuf)
      endif

      return 
      end
