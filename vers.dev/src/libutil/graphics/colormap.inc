C---------------------------------------------------------------------|
C Task      : common for colormaps                                    |
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
C
C COMMON FOR THE COLORMAPS
      integer*4   icol, white, black, red, green, blue, cyan, yellow
     *                , brown, magenta, gray, wwhite, bblack
     *                , lred, lgreen, lblue, lcyan, lmagenta, lgray
      real*4  fred(0:1024), fblue(0:1024), fgreen(0:1024)
      COMMON /COLMAP_I/ white, black, red, green, blue, cyan, yellow
     *               ,  brown, magenta, gray, wwhite, bblack
     *               ,  lred, lgreen, lblue, lcyan, lmagenta, lgray
     *               ,  fred, fblue, fgreen
      save /COLMAP_I/ 
c
c Common for fonts
c
      integer*4         fontm,fontd
c
      real*4            italicm,rotatem
     *                , italicd,rotated
     *                , fsfac, fsmenu, fsdigit, fssmall
     *                , fstitb, fstitm, fstits
c
      COMMON /FONTS_R/  fontm,fontd
     *                , italicm,rotatem
     *                , italicd,rotated
     *                , fsfac, fsmenu, fsdigit, fssmall
     *                , fstitb, fstitm, fstits
      save /FONTS_R/


