C---------------------------------------------------------------------|
C Task      : commons for PostScript                                  |
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
      integer*4      iops
      common /ps_i/  iops
      real*4         pscx,pscy
      common /ps_r/  pscx,pscy
      save /ps_i/, /ps_r/
c
      character*19 prefix /'/usr/local/vpl/dat/'/
      character*14 setlinewidth
      character*14 setcmykcolor
      character*13 setrgbcolor
      character*12 centershow
      character*11 closepath
      character*9  newpath
      character*8  moveto,lineto,stroke
      character*7  scale
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
      save scale
      save fill,show
      save arc
      save bracop,braclos
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
      data scale          /' scale '/
      data fill           /' fill '/
      data show           /' show '/
      data arc            /' arc '/
      data bracop,braclos /'(',')'/
c
 1003 format (A1,I3,A1,A6)
 1005 format (A1,I5,A1,A6)
 1011 format (3F8.2,2I5,A10)
 1012 format (3F8.2,2F7.2,A10)
 1111 format (2F8.2,A10)
 2222 format (2F10.5,A10)
 3333 format (A80)
c
