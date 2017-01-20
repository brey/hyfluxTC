C---------------------------------------------------------------------|
C Task      : Commons for documentation                               |
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
      integer*4 docstrmax
      Parameter(docstrmax=10)
      character*60 docstr
      character*30 curfilnam
      character*8  curdate,curtime
      common /docum/ docstr(docstrmax)
     *             , curfilnam,curdate,curtime
      save /docum/ 


