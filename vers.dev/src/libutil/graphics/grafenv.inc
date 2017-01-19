C---------------------------------------------------------------------|
C Task      : common for graphic environment                          |
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
C COMMON FOR GRAPHICS ENVIRONMENT DEFINITION
      logical              x11, ftn, ps, vb, pseps, bwc
     &                    ,exswin(9),keepwinratio1(9)
      common /grafenv_lo/  x11, ftn, ps, vb, pseps, bwc
     &                    ,exswin,keepwinratio1
      save /grafenv_lo/  

      integer*4            xorwin,yorwin,xsizewin, ysizewin
     *                   ,xsizeps,  ysizeps
     *                   ,xsizevb,  ysizevb
     &                    ,winid1
      character*80 wintit

      common /grafenv_i4/  xorwin,yorwin,xsizewin, ysizewin
     *                   , xsizeps,  ysizeps
     *                   ,xsizevb,  ysizevb
     &                    ,winid1,wintit
      save /grafenv_i4/ 

      real*4               drwxmin, drwxmax
     *                   , drwymin, drwymax
     *                    , drwxdim , drwydim
     *                    , refxdim , refydim
     &                    ,drwxmin1(9), drwymin1(9)
     *                    , refxdim1(9) , refydim1(9)
      common /grafenv_r4/  drwxmin, drwxmax
     *                   , drwymin, drwymax
     *                    , drwxdim , drwydim
     *                    , refxdim , refydim
     *                    , refxdim1, refydim1
      save /grafenv_r4/ 
c
      INTEGER*4  curunit, mioform, gsyst
      common /grafmode_i4/ curunit, mioform, gsyst
      save /grafmode_i4/
c
      REAL*4                xscalwin, yscalwin, scalwin
     *                    , xscalps , yscalps , scalps
     *                    , xscalvb , yscalvb , scalvb
      common /grafmode_r4/  xscalwin, yscalwin, scalwin
     *                    , xscalps , yscalps , scalps
     *                    , xscalvb , yscalvb , scalvb
      save /grafmode_r4/


