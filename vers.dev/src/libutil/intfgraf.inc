C---------------------------------------------------------------------|
C Task      : declarations for graphic interfaces                     |
C Project   : Reactive Gas Flows                                      |
C Module    : Graphic Includes                                        |
C Authors   : G. Peter                                                |
C Version   : 2.1                                                     |
C Modified  : 09.02.94                                                |
C                                                                     |
C      *      (C) Commission of the European Communities              |
C   *     *                                                           |
C *         *     Institute for Safety Technology                     |
C *         *                                                         |
C *         *     Process Engineering Division (CPA)                  |
C   *     *                                                           |
C      *          JRC - Ispra, I-21020 Ispra (VA)                     |
C---------------------------------------------------------------------|
c
c     declarations for the different graphics interfaces
c
c     string
      character*8  cdate,ctime
c
c     mouse status , key to get
      integer*4 bs, key, ichar
c
c     color number, number of points for polygon, number of beeps
      integer*4  col , nump , numbep
c
c     number of seconds to sleep
      real*4 secslep
c
c     scaling factor in x and y for PostScript output
      real*4 pscalx, pscaly
c
c     coordinates
      real   *4  x1, y1, x2, y2
c
c     width and height of circle
      real   *4 cwidth,cheight
c
c     angles
      real*4 ang1, ang2
c
c     fonts
      integer*4 font
      real*4    fsize,italic,rotate,strwif
c
c     Lines attributes
      integer*4 lwidth,lstyle
c
c     random number
      real*4 rnum
      integer*4 rinit
c
c     windows
      integer*4 winx1, winy1, winx2, winy2, winid
c
c     colors and colormap
      integer*4  icol0, icol1
      real*4     frac_red,  frac_green,  frac_blue
      real*4     frac_red0, frac_green0, frac_blue0
      real*4     frac_red1, frac_green1, frac_blue1
c
c     end of declarations


