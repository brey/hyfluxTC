#include <modules.F>
        use parameter_data
        use window_data
        use map_interp_data
        use map_cover_data
	implicit none
C---------------------------------------------------------------------|
        logical check !produce control maps
c-------------------------------------------------------------
C interface with srtm30 data
 
        real(rr8) dcell1
        real(r4) dcell
        parameter (dcell=30./3600.)  ! srtm30 cell size in deg
 
        integer(i4) range_lat, range_long, narc
        parameter (narc=3600/30)  ! no of cell per deg
 
        integer(i4) nlong,nlat,nval
 
        integer*2, allocatable :: ival2(:,:),z2(:,:)  
 
        integer(i4) nlong2, nlat2, nval2, range2
        parameter (range2=10,nlong2=range2*narc,nlat2=range2*narc)
 
c--------------------------------------------
 
        integer(i4) i, j, ii,jj,i0,j0,i1,j1, is, js
 
	character maps*132,  msg*3,ext*132, fullpath*132, fullpath1*132, fil*132
     &           ,srtmpath1*132, ch*132
	integer(i4) length, lm, linp, linp1, narg, iargc, lext
        real(r4) numget
        logical exist
 
c-----------------------------------------------------------
c constant
        ext = '.Bathymetry.srtm'
        iout = 6
c-----------------------------------------------------------
c set defaults
        range_lat = 50
        range_long = 40
        ii = -180
        jj = 90
c---------------------------------------------------------------
      narg = iargc()
      if (narg.eq.0) then
      print *,'srtmResample Help file'
      print *,' '
      print *,'-i srtmpath'
      print *,'   path to input srtm data '
      print *,' DEFAULTS none'
      print *,'-o srtmpath1'
      print *,'   path to output srtm data '
      print *,' DEFAULTS none'
      print *,'-long longitude'
      print *,'   longitude of input window'
      print *,' DEFAULT=',ii
      print *,'-lat latitude'
      print *,'   latitude of input window'
      print *,' DEFAULT=',jj
      print *,'-dlong window range longitude '
      print *,'   longitude range of input window'
      print *,' DEFAULT=',range_long
      print *,'-dlat window range latitude '
      print *,'   longitude range of input window'
      print *,' DEFAULT=',range_lat
      stop
      endif
c---------------------------------------------------------------
      do i = 1,narg
      call getarg (i,ch)
c       ----------------------------------------------------
        if (ch.eq.'-i') then
        call getarg (i+1,srtmpath)
 
        else if (ch.eq.'-o') then
        call getarg (i+1,srtmpath1)

        else if (ch.eq.'-lat') then
        call getarg (i+1,ch)
        jj = numget(ch,msg)

        else if (ch.eq.'-long') then
        call getarg (i+1,ch)
        ii = numget(ch,msg)

        else if (ch.eq.'-dlat') then
        call getarg (i+1,ch)
        range_lat = numget(ch,msg)

        else if (ch.eq.'-dlong') then
        call getarg (i+1,ch)
        range_long = numget(ch,msg)

        endif
      enddo
c---------------------------------------------------------------
      print *,'ii=',ii,' jj=',jj,' range_lat=',range_lat,' range_long=',range_long
c---------------------------------------------------------------
        nlong=range_long*narc
        nlat=range_lat*narc
c---------------------------------------------------------------

c---------------------------------------------------------------
c extract strm30 data (big window)
       linp = length(srtmpath)
       linp1 = length(srtmpath1)
       lext = length(ext)
 
       allocate (z2(nlong2,nlat2),ival2(nlong,nlat))
       nval = nlat*nlong
       nval2 = nlat2*nlong2
c---------------------------------------------------------------
         write (fil,'(i4,i3,a)') abs(ii),abs(jj),ext(1:lext)
         call fillzero(fil,7+lext)
           if (ii.gt.0) then
           fil(1:1)='e'
           else
           fil(1:1)='w'
           endif
 
           if (jj.gt.0) then
           fil(5:5)='n'
           else
           fil(5:5)='s'
           endif
 
         fullpath=srtmpath(1:linp)//bs//fil(1:7+lext)//null
         inquire (file=fullpath,exist=exist)
         write (iout,*) 'file ',fullpath(1:length(fullpath)),' exist = ',exist
           if (.not.exist) stop
c---------------------------------------------------------------
         call getbin1(fullpath,nval,2,ival2)
c---------------------------------------------------------------
      do j = 1,range_lat/range2
        do i = 1,range_long/range2

        i0=(i-1)*nlong2
        j0=(j-1)*nlat2
 
        z2 = ival2(i0+1:i0+nlong2,j0+1:j0+nlat2) 
        is = ii+(i-1)*range2
        js = jj-(j-1)*range2
c---------------------------------------------------------------
         write (fil,'(i4,i3,a)') abs(is),abs(js),ext(1:lext)
         call fillzero(fil,7+lext)
           if (is.gt.0) then
           fil(1:1)='e'
           else
           fil(1:1)='w'
           endif
 
           if (js.gt.0) then
           fil(5:5)='n'
           else
           fil(5:5)='s'
           endif
 
         fullpath=srtmpath1(1:linp1)//bs//fil(1:7+lext)//null
         write (iout,*) 'generated file ',fullpath(1:length(fullpath))
c---------------------------------------------------------------
         call putbin1(fullpath,nval2,2,z2)
c---------------------------------------------------------------
        enddo
      enddo
c---------------------------------------------------------------
      stop
      end
