      module ps_rgb
      integer*4      iops
      real*4 fred(0:10000), fgreen(0:10000), fblue(0:10000)
      real*4 scale
c
      character*14 setlinewidth
      character*14 setcmykcolor
      character*13 setrgbcolor
      character*12 centershow
      character*11 closepath
      character*9  newpath
      character*8  moveto,lineto,stroke
      character*6  fill,show
      character*5  arc
      character*1  bracop,braclos
c
      save prefix
      save setlinewidth
      save setcmykcolor
      save setrgbcolor
      save centershow
      save closepath
      save newpath
      save moveto,lineto,stroke
      save fill,show
      save arc
      save bracop,braclos
      save scale
c
c
      data setlinewidth   /' setlinewidth '/
      data setcmykcolor   /' setcmykcolor '/
      data setrgbcolor    /' setrgbcolor '/
      data centershow     /' centershow '/
      data closepath      /' closepath '/
      data newpath        /' newpath '/
      data moveto         /' moveto '/
      data lineto         /' lineto '/
      data stroke         /' stroke '/
      data fill           /' fill '/
      data show           /' show '/
      data arc            /' arc '/
      data bracop,braclos /'(',')'/
      end module ps_rgb
 
      subroutine pscalls
      use ps_rgb
      implicit none
# include <intfgraf.inc>
c---------------------------------------------------------------------|
c task      : contains the calls to the postscript interface          |
c project   : reactive gas flows                                      |
c module    : diglib                                                  |
c authors   : g. peter                                                |
c modified  : 09.02.94                                                |
c modified by franchello : 08.08.05                                   |
c                                                                     |
c      *      (c) commission of the european communities              |
c   *     *                                                           |
c *         *     institute for safety technology                     |
c *         *                                                         |
c *         *     process engineering division (cpa)                  |
c   *     *                                                           |
c      *          jrc - ispra, i-21020 ispra (va)                     |
c---------------------------------------------------------------------|
      integer*4 i,l,lenstr,length, ico, ios, lchar
      integer*4 numpol,num,in,io,arcanf,arcend,ixmax,iymax,ixor,iyor
     &         ,xsizewin0,ysizewin0
c
      real   *4 x0,y0,refxdim0,refydim0,xmax0,ymax0, xor0, yor0, scale0
c
      real   *4 ia,ib,ic
      real   *4 ipx(numpol),ipy(numpol)
      real   *4 fac,ficol,ficol0,ficol1, linewidth
      integer*4 linestyle
      character*(*) psfilename,string,creator
      character*80 filenam
      character*80 line
      character char*1
	character curdate*80 /' '/
      character*30 fontps(0:2)
     &   /'/Courier-Bold findfont '
     &   ,'/Helvetica-Bold findfont '
     &   ,'/Helvetica-Bold findfont '/
      logical close, pseps0, pseps
c
      data arcanf, arcend /0, 360/
c
      real*4         pscx,pscy
c
 1003 format (A1,I3,A1,A6)
 1005 format (A1,I5,A1,A6)
 1011 format (3f12.4,2I5,A10)
 1012 format (3f12.4,2F7.2,A10)
 1111 format (2f12.4,A10)
 2222 format (2f12.4,A10)
 3333 format (A80)
      return
c ***********************************************************
      entry psstart(io,psfilename,xor0,yor0,xmax0,ymax0,pseps0,creator,scale0)
      iops = io
      scale = scale0
c
c     open postscript output file
c
      open (iops,file=psfilename)
c
      pseps = pseps0
c
      if (pseps) then
        ixor = xor0
        iyor = yor0
        ixmax = int(xmax0)
        iymax = int(ymax0)
          if (float(ixmax).lt.xmax0) ixmax=ixmax+1
          if (float(iymax).lt.ymax0) iymax=iymax+1
        write (iops,'(a)')      '%!PS-Adobe-3.0 EPSF-3.0'
*       write (iops,'(a,4i9)')  '%%BoundingBox: ',ixor,iyor,ixmax,iymax
        write (iops,'(a,4f16.4)')  '%%BoundingBox: ',xor0*scale,yor0*scale,xmax0*scale,ymax0*scale
        write (iops,'(a)') '%%Title: '//psfilename
        write (iops,'(a)') '%%Creator: '//creator
        write (iops,'(a)') '%%CreationDate: '//curdate
        write (iops,'(a)') '%%EndComments  '
        write (iops,'(a)') '%%BeginProlog  '
 
        write (iops,*) '/tofrac { 255 div} bind def'
        write (iops,*) '/col {setrgbcolor} bind def'
        write (iops,*) '/box {newpath moveto lineto lineto lineto closepath } def'
        write (iops,*) '/psline {newpath moveto lineto closepath } def'
        write (iops,*) '/circ {newpath 0 360 arc closepath } def'
 
        write (iops,'(a)') '%%EndProlog'
 
      end if
c
      write (iops,'(2(1x,f12.4),a)')0.,0.,' translate'
      write (iops,'(f12.4,1x,f12.4,a)') scale, scale, ' scale'
      return
c ***********************************************************
      entry psend (close)
 
*       write (iops,'(a)') 'stroke'
        write (iops,'(a)') 'showpage'
 
        if (.not.close) return
 
 
c writes closing commands
      write (iops,'(a)') '%%EOF'
      close (iops)
      return
c ***********************************************************
      entry pschgfont (font,fsize,rotate,italic)
c changes fonts
      write (iops,'(a,f8.4,a)') fontps(font)
     &   ,fsize,' scalefont setfont '
      return
c ***********************************************************
      entry pschgline (linewidth,linestyle)
c changes lines attributes
      write (iops,'(f12.4,a)') linewidth,' setlinewidth'
      return
c ***********************************************************
      entry psline(x1,y1,x2,y2,col)
c creates a line in postscript
c
#if defined DOIT
      write (iops,*) newpath
      write (iops,1111) x1,y1,moveto
      write (iops,1111) x2,y2,lineto
#endif
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor(col)
      write (iops,'(4f12.4,a)') x1,y1,x2,y2,' psline stroke'
      return
c ***********************************************************
      entry psstringc(x1,y1,string,lenstr,col)
c writes a text in postscript (centre aligned)
c
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor (col)
      write (iops,1111) x1,y1,moveto
      write (iops,*) bracop,string(1:lenstr), braclos,centershow
c
      return
c ***********************************************************
      entry psstring(x1,y1,string,lenstr,col)
c writes a text in postscript (left aligned)
c
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor (col)
      write (iops,1111) x1,y1,moveto
      write (iops,*) bracop,string(1:lenstr),braclos,show
c
      return
c ***********************************************************
      entry psnum(x1,y1,num,l,col)
c writes a text in postscript (left aligned)
c
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor (col)
      write (iops,1111) x1,y1,moveto
      if (num .lt. 100) then
          write (iops,1003) bracop,num,braclos,show
          return
      end if
      if (num .lt. 10000) then
          write (iops,1005) bracop,num,braclos,show
      else
          write (iops,*) bracop,num,braclos,show
      end if
c
      return
c ***********************************************************
      entry psrect(x1,y1,x2,y2,col)
c creates a rectangle in postscript
c
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor (col)
      write (iops,'(8f12.4,a)') x1,y1,x1,y2,x2,y2,x2,y1,' box stroke'
c
      return
c ***********************************************************
      entry psfillrect(x1,y1,x2,y2,col)
c creates a filled rectangle in postscript
c
#ifdef DOIT
      write (iops,*) newpath
      write (iops,1111) x1,y1,moveto
      write (iops,1111) x1,y2,lineto
      write (iops,1111) x2,y2,lineto
      write (iops,1111) x2,y1,lineto
      write (iops,*) fill
#endif
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor(col)
      write (iops,'(8f12.4,a)') x1,y1,x1,y2,x2,y2,x2,y1,' box fill'
c
      return
c ***********************************************************
      entry pspolyline(numpol,ipx,ipy,col)
c creates a polyline in postscript
c
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor(col)
      write (iops,*) newpath
      write (iops,1111) ipx(1),ipy(1),moveto
      do 300 i = 1, numpol
        write (iops,1111) ipx(i),ipy(i),lineto
  300 continue
      write (iops,'(a)') 'closepath stroke'
c
      return
c ***********************************************************
      entry pspoly(numpol,ipx,ipy,col)
c creates a polygon in postscript
c
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor(col)
      write (iops,*) newpath
      write (iops,1111) ipx(1),ipy(1),moveto
      do 350 i = 1, numpol
        write (iops,1111) ipx(i),ipy(i),lineto
  350 continue
      write (iops,'(a)') 'closepath stroke'
c
      return
c ***********************************************************
      entry psfillpoly(numpol,ipx,ipy,col)
c creates a filled polygon in postscript
c
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor(col)
      write (iops,*) newpath
      write (iops,1111) ipx(1),ipy(1),moveto
      do 400 i = 1, numpol
      write (iops,1111) ipx(i),ipy(i),lineto
  400 continue
      write (iops,'(a)') 'closepath fill'
c
      return
c ***********************************************************
      entry psarc(x1,y1,ia,ib,ang1,ang2,col)
c creates an arc in postscript
c !! has to be verified
c
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor(col)
      ic = (ia+ib)/2
      write (iops,*) newpath
      write (iops,1012) x1,y1,ic
     *                 ,ang1,ang2,arc
      write (iops,'(a)') 'closepath stroke'
c
      return
c ***********************************************************
      entry pscirc(x1,y1,ia,ib,col)
c creates a circle in postscript
c ia,ib are the radius
c
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor(col)
      ic = (ia+ib)*0.5
      write (iops,'(3f12.4,a)') x1,y1,ic,' circ stroke'
c
      return
c ***********************************************************
      entry psfillcirc(x1,y1,ia,ib,col)
c creates a filled circle in postscript
c
      ic = (ia+ib)*0.5
      if (fred(col).lt.0. .or.fgreen(col).lt.0. .or.fblue(col).lt.0.) return
      call pscolor(col)
      write (iops,'(3f12.4,a)') x1,y1,ic,' circ fill'
c
      return
c ***********************************************************
      entry pscomment (string)
      write (iops,*) '% '//string
      return
c ***********************************************************
      entry psflush
*     write (iops,*) stroke
      return
c ***********************************************************
      entry pssetcol(icol0,frac_red, frac_green, frac_blue)
      fred(icol0) = frac_red
      fgreen(icol0) = frac_green
      fblue(icol0) = frac_blue
      return
c ***********************************************************
      entry pscopycol(icol0,col)
      fred(col) = fred(icol0)
      fgreen(col) = fgreen(icol0)
      fblue(col) = fblue(icol0)
      return
c ***********************************************************
      entry psgetcol(icol0,frac_red, frac_green, frac_blue)
      frac_red = fred(icol0)
      frac_green = fgreen(icol0)
      frac_blue = fblue(icol0)
      return
      end
c ***********************************************************
      subroutine pscolor(col)
      use ps_rgb
      implicit none
c
c changes color in postscript
c
      integer*4 col,colold
c
      data colold /-1/
      save colold
c
      if (colold .eq. col) return
 
      entry forcepscolor (col)
c
*     write (iops,'(a10,3f6.2,a)') stroke, fred(col), fgreen(col), fblue(col)
*    &     , setrgbcolor
      write (iops,'(3f12.7,a)') fred(col), fgreen(col), fblue(col),' col'
*     write (iops,'(3i4,a)') int(fred(col)*255), int(fgreen(col)*255), int(fblue(col)*255),' col'
c
      colold = col
      return
      end
