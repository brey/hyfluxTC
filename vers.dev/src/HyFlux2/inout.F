c========================================================================
      subroutine set_defaults
c========================================================================
      use kind
      use parameter_data
      use boundary_data
      use window_data
      use geometry_data
      use statistics_data
      use map_cover_data
      use parallel_data
      implicit none
c----------------------------------------------------------
      integer(i4) length,i 
c-------------------------------------------------------
      call getmv1(mv1)
      call getmv4(mv4)
      call getmv(mv)
c----------------------------------------------------------
c getenv
      call getenv ('srtmpath',srtmpath)
      call getenv ('places',placefile)
      call getenv ('configHF',confdeck)
c----------------------------------------------------------
c directories & path
      workdir='work'
      outdir=' '
      watchdir = ' '
      boundaryDir='none'
      boundaryFac=1
c----------------------------------------------------------
c files
      monitor = 'monitor.txt'
      freqmon = 10

      paramfile='parameters.txt'
      consolefile = 'console.txt'
      logoutfile = 'logout.txt' 
      loginfile = 'login.txt' 
      watchfile = ' ' 
      shorefile = ' '
      boundaryFile=' '
      inpdeck = ' '
      hollandFile = ' '
c-------------------------------------
c maps
      demmap= ' '
      demnodemap= ' '
      dzdxmap=' '
      dzdymap=' '
      batmap=' '
      batnode=' '
      vxmap= ' '
      vymap= ' '
      faultmap=' '
      maskmap=' '
      depthmap=' '
      zsurfmap=' '
      rainmap=' '
      fcormap = ' '
      manmap = ' '
      manslopemap = ' '

      noSlopemap = 'none'

c----------------------------------------------------------
c outmaps
      noutmaps=3
      outmaps = ' '
      outmaps(1:noutmaps) = (/'zsurf','vx000','vy000'/)

      nrestmaps=1
      restmaps = ' '
      restmaps(1:nrestmaps) = (
     &/'zsurf'/)

      nstatmaps=9
      statmaps = ' '
      statmaps(1:nstatmaps) = (
     &/'tarr   '
*     &,'update '
*     &,'limvals'
     &,'dhjv   '
     &,'vnmax  '
     &,'u10max '
     &,'deprmax'
     &,'zwmax  '
     &,'tzwmax '
     &,'hzwmax '
     &,'wzwmax '/)


c-------------------------------------------------------------
      mapfmt = 'PCRaster'//null
      outfmt = 'GTiff'//null
      mapext = '.map'//null
      outext = '.tif'//null
c-------------------------------------
c cover parameters
	nmax_cover = 1
	idp_cover = 1.
	nradius_cover = 2
	cover = .true.
*-------------------------------------
*      parameter (pi=3.14159265,rad2deg=180./pi,deg2rad=pi/180)
      pi = acos(-1.d0)
      rad2deg=180.d0/pi     ! [deg/-]
      deg2rad=pi/180.d0		! [-/deg]

*-------------------------------------
      boundaryCode = 5
      boundaryCode0 = 5
      errprim = 1e-6
      errstep = 1e-6
      epsvar = 1e-12
 
      dthydro = -1


      tmax         = 3600
      istepmax     = 1 000 000

      ntimeL = 1
      dtoutL(ntimeL) = 60
      dtstatL(ntimeL) = -1
      dtrestL(ntimeL) = 600
 
      varjun = 0
      varHLLE = 1
 
      solver = 2
      g = 9.806
      lamtrans  = 0.01
      strans = 0
      cdmax = 2

*-------------------------------------
* second order / slopes
      ilim = 4
*      limval = -1
      limval = -0.5  ! 2011.02.21
*      limval = -1  ! 2011.11.27  Samoa

*      limCoeff(1:nlimcoeff) = (/0.5,10.,1.,1./)
*      limCoeff(1:nlimcoeff) = (/1.,10.,1.,1./)
*      limCoeff(1:nlimcoeff) = (/0.5,10.,1.,1./)  ! 2010.09.13 8:13
*      limCoeff(1:nlimcoeff) = (/0.5,1.,1.,1./)  ! 2011.01.14 
*      limCoeff(1:nlimcoeff) = (/0.5,1.,2.,1./)  ! 2011.02.14 
*      limCoeff(1:nlimcoeff) = (/0.5,1.,2.,0.5/)  ! 2011.03.21 
      limCoeff(1:nlimcoeff) = (/0.5,1.,2.,0.1/)  ! 2012.01.11 
      limV = mv
      limZW = mv

      maxdhmax = 100
      runUpLim = 50  ! the slopes are calculated for open sea or zminv > runUpLim (see slopeV)
*-------------------------------------

      limnoise=.false.
      verify = .false.
      debug = .false.
      postProc = .false.
      entrfix = .true.
      faultDem = .true.
      faultzsurf = .true.
      mapdeg = .true.
      nocase = .true.
      useMinDem=.false.

      noSpot = .true.
      check = .false.
      outTTT = .false.
        if (code.eq.'HyFlux2') then
        onlyDem = .false.
        else
        onlyDem = .true.
        endif
      facstep = 2
      ldeb=0
 
      manval      = 0.01
      manslopeval = 0.01
      expslope    = 0.2
      manReef = 0.
      demReef = -10
 
      man0   = 0.01
      velmin = 0.001

      hprim  = 1e-6
      hmin   = 1e-6
      hlim   = 100    ! for deep water set 100
      hcrit  = 1      ! caution: problems in shoreline if hcrit = 1e-6
 
      hvel0  = 1e-2
      hshore = 0.1   ! hshore < hlim0 = hlim/10

      harr = 0.05 ! store tarr if abs(zwv-zwvIn) > harr
      hout   = 1e-2  ! evaluate zwmin, zwmax, tzwmax, vnmax, ... if   abs(zwv-zwvIn) > hout
      wout   = 1e-2
 
      smallArg  = -1
      smallNew  = 1.
      seaLevel  = 0.
      imap      = 0
      timestart = 0
c-------------------------------------
      icore = 0
      ncore = 1
      MASTER = icore.eq.0
      SLAVE  = icore.ne.0
c-------------------------------------
c window
      longmin0 = 16
      latmax0  = 41
      nlat0    = -1
      nlong0   = -1
      dlong0   = -1
      dlat0    = -1

      radius0 = 6378388.

c mask 
      maxDist = 20000  ! km, maxDist offshore & inland
      maxDistLand = 20  ! km, maxDist inland

      minDistLand = 5  ! km, minDist inland
      minDistSea = 5  ! km, minDist offshore

      minDem = -20000
      maxDem = 50
      maxDepth = 20000

c srtm
      intsrtm = 1
c-------------------------------------

      baseS = 2.
      maxlev = 0
      regularize = 0

c-------------------------------------
      courmax = 0.25
      fluxnum = 5
      dtmin   = 0.01
      maxstepdtmin = 2

      return
      end

c========================================================================
      subroutine move_vars 
c========================================================================
      use kind
      use parameter_data
      use deck_data
      use window_data
      use map_data
      use boundary_data
      use statistics_data
      implicit none
c----------------------------------------------------------
      real*4 getnumvar, numget
      integer*4 length, l
      character getcharvar*(ldata), msg*3
      character str*(ldata),names(ntimeMax)*(ldata)

      logical existvar, existFile
c----------------------------------------------------------
c from inpdeck

      if (inpdeck.ne.' '.and.existFile(outdir(1:lout)//bs//inpdeck)) then
      if (existvar('fanning')) manval = getnumvar('fanning',0.01)

c     -----------------------------------------------------
      if (existvar('minDem')) minDem = getnumvar('minDem',-20000.)
      if (existvar('maxDem')) maxDem = getnumvar('maxDem', 50.)
      if (existvar('maxDepth')) maxDepth = getnumvar('maxDepth', 20000.)

      if (existvar('minDistSea')) minDistSea = getnumvar('minDistSea',5.)
      if (existvar('minDistLand')) minDistLand = getnumvar('minDistLand',5.)
      if (existvar('maxDist')) maxDist = getnumvar('maxDist',20000.)

      dlat0 = getnumvar('batgrid',mv)/60
      dlong0 = getnumvar('batgrid',mv)/60
      longmin0 = getnumvar('lonmin',mv)
      latmax0 = getnumvar('latmax',mv)
      nlong0 = int((getnumvar('lonmax',mv)-longmin0)/dlong0+0.5)
      nlat0 = int((latmax0-getnumvar('latmin',mv))/dlat0+0.5)        
c     -----------------------------------------------------
        if (getnumvar('batgrid',1.).gt.1.) then
        intsrtm = 0
        else
        intsrtm = 1
        endif
c     -----------------------------------------------------
      if (code.eq.'HyFlux2') then
c     -----------------------------------------------------
      timestart = getnumvar('InTime',1.)*3600
      tmax = getnumvar('FinTime',1.)*3600
        if (existvar('foldertored')) boundaryDir = getcharvar ('foldertoread','none')
        if (existvar('hollandFile')) hollandFile = getcharvar ('hollandFile',' ')

        if (existvar('Tout')) then
        str = getcharvar('Tout',tmax/3600)
*        write (iout,*)'Tout: str=',str(1:length(str))
      	call str2name (str,names,ntimeMax,ntimeL,' ')
*        write (iout,*)'ntimeL=',ntimeL,' names=',(names(l)(1:1+length(names(l))),l=1,ntimeL) 
          do l = 1,ntimeL
          toutL(l) = numget(names(l),msg)*3600         
          enddo        
        else
        ntimeL = 1
        toutL(ntimeL) = tmax
        endif

        if (existvar('Tsave')) then
        str = getcharvar('Tsave',1.)
*        write (iout,*)'Tsave: str=',str(1:length(str))
      	call str2name (str,names,ntimeMax,ntimeL,' ')
*        write (iout,*)'ntimeL=',ntimeL,' names=',(names(l)(1:1+length(names(l))),l=1,ntimeL) 
          do l = 1,ntimeL
          dtoutL(l) = numget(names(l),msg)*60    
          dtoutL(l) = int(dtoutL(l)+0.5)     
          enddo        
	dtrestL = tmax/4
        dtstatL = tmax/4
        endif

      endif
c----------------------------------------------------------
      endif
c---------------------------------------------------------- 
c from confdeck
      mapfmt = getcharvar ('mapfmt','PCRaster'); mapfmt = mapfmt(1:length(mapfmt))//null
      outfmt = getcharvar ('outfmt','GTiff'); outfmt = outfmt(1:length(outfmt))//null
      mapext = getcharvar ('mapext','.map'); mapext = mapext(1:length(mapext))//null
      outext = getcharvar ('outext','.tif'); outext = outext(1:length(outext))//null
      houtmin = getnumvar('houtmin',1e-3)
      voutmin = getnumvar('voutmin',1e-4)


      radius0 = getnumvar('radius',6378388.)

      return
      end

c========================================================================
      subroutine get_parameters (dohelp)
c========================================================================
      use parameter_data
      use boundary_data
      use geometry_data
      use statistics_data
      use window_data
      use parallel_data
      implicit none
c----------------------------------------------------------
      integer(i4) narg, iargc, l, j,i, length, ntime
      character ch*132
      real(r4) numget
      character msg*3, yesno*3, helpfile*256
      logical(i4) dohelp, existFile
      integer(i4) iarg 
c-----------------------------------------------------------------------
      integer values(8),im, id, iy,iw,ihh,imin,isec
	character cmo(12)*3 /'Jan','Feb','Mar','Apr','May','Jun','Jul'
     &          ,'Aug','Sep','Oct','Nov','Dec'/
        character cwe(7)*3 /'Mon','Tue','Wed','Thu','Fri','Sat','Sun'/
c-----------------------------------------------------------------------
      call DATE_AND_TIME (values=values)
	iy = values(1)
	im = values(2)
	id = values(3)
        iw = values(4)
	ihh = values(5)
	imin=values(6)
	isec=values(7)
      write (execution,'(a,i3,i3,a,i2,a,i2,a,i5)') cmo(im),id,ihh,':',imin,':',isec,' CEST',iy
      call getarg(0,ch)     
      ch = 'which '//ch(1:length(ch))
      call sendcomm (ch,executable)
*      print *,'code=',ch(1:length(ch)),' executable=',executable(1:length(executable))
c-------------------------------------------------------------------------------------
      narg = iargc()
c-------------------------------------------------------------------------------------
c get input and configuration deck names
      do iarg = 1,narg
      call getarg(iarg,ch)
c       ----------------------------------------------------
        if (ch.eq.'-i') then
        call getarg(iarg+1,inpdeck)

        else if (ch.eq.'-conf') then
        call getarg(iarg+1,confdeck)

        else if (ch.eq.'-o') then
        call getarg(iarg+1,outdir)

        else if (ch.eq.'-nocase') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                nocase = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                nocase = .false.
                else
                nocase = .true.
                endif

        endif
      enddo
c-------------------------------------------------------------------------------------       
c process decks
      lout = length(outdir)
      if (inpdeck.ne.' '.and.inpdeck.ne.'none') then
        if (existFile(outdir(1:lout)//bs//inpdeck))  then
        call proc_deck (outdir(1:lout)//bs//inpdeck,nocase)
        else
        print *,'inpdeck=',inpdeck(1:length(inpdeck)),' not stored in outdir'
        stop
        endif
      endif

      if (existFile(confdeck)) call proc_deck (confdeck,nocase)
      call move_vars
c-------------------------------------------------------------------------------------
c process arguments
      do iarg = 1,narg
      call getarg(iarg,ch)
c       ----------------------------------------------------
        if (ch.eq.'-srtmpath') then
        call getarg(iarg+1,srtmpath)
 
        else if (ch.eq.'-ncore') then
        call getarg(iarg+1,ch)
        ncore = numget(ch,msg)
 
        else if (ch.eq.'-w') then
        call getarg(iarg+1,workdir)
 
        else if (ch.eq.'-console') then
        call getarg(iarg+1,consolefile)
 
        else if (ch.eq.'-login') then
        call getarg(iarg+1,loginfile)
 
        else if (ch.eq.'-logout') then
        call getarg(iarg+1,logoutfile)
 
        else if (ch.eq.'-place') then
        call getarg(iarg+1,placefile)
 
        else if (ch.eq.'-watch') then
        call getarg(iarg+1,watchfile)
 
        else if (ch.eq.'-watchdir') then
        call getarg(iarg+1,watchdir)
 
        else if (ch.eq.'-shore') then
        call getarg(iarg+1,shorefile)
 
        else if (ch.eq.'-radius') then
        call getarg(iarg+1,ch)
        radius0 = numget(ch,msg)
 
        else if (ch.eq.'-baseS') then
        call getarg(iarg+1,ch)
        baseS = numget(ch,msg)
 
        else if (ch.eq.'-maxlev') then
        call getarg(iarg+1,ch)
        maxlev = numget(ch,msg)
 
        else if (ch.eq.'-regularize') then
        call getarg(iarg+1,ch)
        regularize = numget(ch,msg)
 
        else if (ch.eq.'-longmin') then
        call getarg(iarg+1,ch)
        longmin0 = numget(ch,msg)
 
        else if (ch.eq.'-latmax') then
        call getarg(iarg+1,ch)
        latmax0 = numget(ch,msg)
 
        else if (ch.eq.'-dlong') then
        call getarg(iarg+1,ch)
        dlong0 = numget(ch,msg)
 
        else if (ch.eq.'-dlat') then
        call getarg(iarg+1,ch)
        dlat0 = numget(ch,msg)
 
        else if (ch.eq.'-nlong') then
        call getarg(iarg+1,ch)
        nlong0 = numget(ch,msg)
 
        else if (ch.eq.'-nlat') then
        call getarg(iarg+1,ch)
        nlat0 = numget(ch,msg)
c       ----------------------------------------------------
 
        else if (ch.eq.'-dem') then
        call getarg(iarg+1,demmap)
 
        else if (ch.eq.'-demNode') then
        call getarg(iarg+1,demNodemap)
 
        else if (ch.eq.'-depth') then
        call getarg(iarg+1,depthmap)
 
        else if (ch.eq.'-zsurf') then
        call getarg(iarg+1,zsurfmap)

        else if (ch.eq.'-noSlope') then
        call getarg(iarg+1,noSlopemap)
 
        else if (ch.eq.'-fault') then
        call getarg(iarg+1,faultmap)
 
        else if (ch.eq.'-mask') then
        call getarg(iarg+1,maskmap)

        else if (ch.eq.'-dzdx') then
        call getarg(iarg+1,dzdxmap)
 
        else if (ch.eq.'-bathymetry') then
        call getarg(iarg+1,batmap)
 
        else if (ch.eq.'-batNode') then
        call getarg(iarg+1,batnode)
 
        else if (ch.eq.'-dzdy') then
        call getarg(iarg+1,dzdymap)
 
        else if (ch.eq.'-rain') then
        call getarg(iarg+1,rainmap)
 
        else if (ch.eq.'-vx') then
        call getarg(iarg+1,vxmap)
 
        else if (ch.eq.'-vy') then
        call getarg(iarg+1,vymap)
 
        else if (ch.eq.'-boundary') then
        call getarg(iarg+1,boundaryFile)
 
        else if (ch.eq.'-hollandFile') then
        call getarg(iarg+1,hollandFile)
 
        else if (ch.eq.'-boundaryDir') then
        call getarg(iarg+1,boundaryDir)
 
        else if (ch.eq.'-boundaryFac') then
        call getarg(iarg+1,ch)
        boundaryFac = numget(ch,msg)
 
        else if (ch.eq.'-monitor') then
        call getarg(iarg+1,monitor)
 
        else if (ch.eq.'-parameters') then
        call getarg(iarg+1,paramfile)
 
        else if (ch.eq.'-outmaps') then
            do j = 1,nout_max
            call getarg(iarg+j,ch)
              if (ch(1:1).eq.'-'.or.ch.eq.' ') then
              noutmaps = j-1
              exit
              endif
            outmaps(j) = ch
            noutmaps = j
            enddo
 
        else if (ch.eq.'-statmaps') then
            do j = 1,nout_max
            call getarg(iarg+j,ch)
              if (ch(1:1).eq.'-'.or.ch.eq.' ') then
              nstatmaps = j-1
              exit
              endif
            statmaps(j) = ch
            nstatmaps = j
            enddo
 
        else if (ch.eq.'-restmaps') then
            do j = 1,nout_max
            call getarg(iarg+j,ch)
              if (ch(1:1).eq.'-'.or.ch.eq.' ') then
              nrestmaps = j-1
              exit
              endif
            restmaps(j) = ch
            nrestmaps = j
            enddo
 
        else if (ch.eq.'-outfmt') then
        call getarg(iarg+1,outfmt)
        outfmt = outfmt(1:length(outfmt))//null 
 
        else if (ch.eq.'-outext') then
        call getarg(iarg+1,outext)
        outext = outext(1:length(outext))//null 
 
        else if (ch.eq.'-mapfmt') then
        call getarg(iarg+1,mapfmt)
        mapfmt = mapfmt(1:length(mapfmt))//null 
 
        else if (ch.eq.'-mapext') then
        call getarg(iarg+1,mapext)
        mapext = mapext(1:length(mapext))//null 

        else if (ch.eq.'-imap') then
        call getarg(iarg+1,ch)
        imap = numget(ch,msg)

        else if (ch.eq.'-freqmon') then
        call getarg(iarg+1,ch)
        freqmon = numget(ch,msg)
 
        else if (ch.eq.'-timestart') then
        call getarg(iarg+1,ch)
        timestart = numget(ch,msg)
 
        else if (ch.eq.'-tmax') then
        call getarg(iarg+1,ch)
        tmax = numget(ch,msg)
 
        else if (ch.eq.'-dthydro') then
        call getarg(iarg+1,ch)
        dthydro = numget(ch,msg)
 
        else if (ch.eq.'-dtout') then
        ntime = 0
            do j = 1,ntimeMax
            call getarg(iarg+j,ch)
              if (ch(1:1).eq.'-'.or.ch.eq.' ') exit
            dtoutL(j) = numget(ch,msg)
            ntime = j
            enddo
        ntimeL = max(ntime,ntimeL)
          do j = ntime+1,ntimeL
          dtoutL(j) = dtoutL(j-1)
          enddo    
          
        else if (ch.eq.'-dtrest') then
        ntime = 0
            do j = 1,ntimeMax
            call getarg(iarg+j,ch)
              if (ch(1:1).eq.'-'.or.ch.eq.' ') exit
            dtrestL(j) = numget(ch,msg)
            ntime = j
            enddo
        ntimeL = max(ntime,ntimeL)
          do j = ntime+1,ntimeL
          dtrestL(j) = dtrestL(j-1)
          enddo    
 
        else if (ch.eq.'-dtmin') then
        call getarg(iarg+1,ch)
        dtmin = numget(ch,msg)
 
        else if (ch.eq.'-small') then
        call getarg(iarg+1,ch)
        smallArg = numget(ch,msg)
 
        else if (ch.eq.'-smallNew') then
        call getarg(iarg+1,ch)
        smallnew = numget(ch,msg)
 
        else if (ch.eq.'-courmax') then
        call getarg(iarg+1,ch)
        courmax = numget(ch,msg)
 
        else if (ch.eq.'-fluxnum') then
        call getarg(iarg+1,ch)
        fluxnum = numget(ch,msg)
 
        else if (ch.eq.'-lamtrans') then
        call getarg(iarg+1,ch)
        lamtrans = numget(ch,msg)
 
        else if (ch.eq.'-hmin') then
        call getarg(iarg+1,ch)
        hmin = numget(ch,msg)
 
        else if (ch.eq.'-errprim') then
        call getarg(iarg+1,ch)
        errprim = numget(ch,msg)
 
        else if (ch.eq.'-epsvar') then
        call getarg(iarg+1,ch)
        epsvar = numget(ch,msg)

        else if (ch.eq.'-errstep') then
        call getarg(iarg+1,ch)
        errstep = numget(ch,msg)
 
        else if (ch.eq.'-facstep') then
        call getarg(iarg+1,ch)
        facstep = numget(ch,msg)
 
        else if (ch.eq.'-hcrit') then
        call getarg(iarg+1,ch)
        hcrit = numget(ch,msg)
 
        else if (ch.eq.'-hprim') then
        call getarg(iarg+1,ch)
        hprim = numget(ch,msg)
 
        else if (ch.eq.'-hlim') then
        call getarg(iarg+1,ch)
        hlim = numget(ch,msg)
 
        else if (ch.eq.'-hvel0') then
        call getarg(iarg+1,ch)
        hvel0 = numget(ch,msg)
 
        else if (ch.eq.'-hout') then
        call getarg(iarg+1,ch)
        hout = numget(ch,msg)
 
        else if (ch.eq.'-harr') then
        call getarg(iarg+1,ch)
        harr = numget(ch,msg)
 
        else if (ch.eq.'-wout') then
        call getarg(iarg+1,ch)
        wout = numget(ch,msg)
 
        else if (ch.eq.'-hshore') then
        call getarg(iarg+1,ch)
        hshore = numget(ch,msg)
 
        else if (ch.eq.'-g') then
        call getarg(iarg+1,ch)
        g = numget(ch,msg)
 
        else if (ch.eq.'-cdmax') then
        call getarg(iarg+1,ch)
        cdmax = numget(ch,msg)
 
        else if (ch.eq.'-manslope') then
        call getarg(iarg+1,ch)
        manslopeval = numget(ch,msg)
          if (msg.ne.' ') then
          manslopeval = 0
          manslopemap = ch
          endif
 
        else if (ch.eq.'-expslope') then
        call getarg(iarg+1,ch)
        expslope = numget(ch,msg)
 
        else if (ch.eq.'-minDem') then
        call getarg(iarg+1,ch)
        minDem = numget(ch,msg)
 
        else if (ch.eq.'-maxDem') then
        call getarg(iarg+1,ch)
        maxDem = numget(ch,msg)
 
        else if (ch.eq.'-maxDepth') then
        call getarg(iarg+1,ch)
        maxDepth = numget(ch,msg)
 
        else if (ch.eq.'-maxDist') then
        call getarg(iarg+1,ch)
        maxDist = numget(ch,msg)
 
 
        else if (ch.eq.'-manval') then
        call getarg(iarg+1,ch)
        manval = numget(ch,msg)
 
        else if (ch.eq.'-manReef') then
        call getarg(iarg+1,ch)
        manReef = numget(ch,msg)
 
        else if (ch.eq.'-demReef') then
        call getarg(iarg+1,ch)
        demReef = numget(ch,msg)
 
        else if (ch.eq.'-man0') then
        call getarg(iarg+1,ch)
        man0 = numget(ch,msg)
 
        else if (ch.eq.'-manmap') then
        call getarg(iarg+1,manmap)
 
        else if (ch.eq.'-man') then
        call getarg(iarg+1,ch)
        manval = numget(ch,msg)
          if (msg.ne.' ') then
          manval = 0
          manmap = ch
          endif
 
        else if (ch.eq.'-fcor') then
        call getarg(iarg+1,fcormap)
 
        else if (ch.eq.'-strans') then
        call getarg(iarg+1,ch)
        strans = numget(ch,msg)
 
        else if (ch.eq.'-entrfix') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                entrfix = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                entrfix = .false.
                else
                entrfix = .true.
                endif
 
        else if (ch.eq.'-mapdeg') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                mapdeg = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                mapdeg = .false.
                else
                mapdeg = .true.
                endif
 
        else if (ch.eq.'-verify') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                verify = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                verify = .false.
                else
                verify = .true.
                endif
 
        else if (ch.eq.'-check') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                check = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                check = .false.
                else
                check = .true.
                endif
  
        else if (ch.eq.'-outTTT') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                outTTT = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                check = .false.
                else
                outTTT = .true.
                endif
 
        else if (ch.eq.'-noSpot') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                noSpot = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                noSpot = .false.
                else
                noSpot = .true.
                endif
 
        else if (ch.eq.'-onlyDem') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                onlyDem = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                onlyDem = .false.
                else
                onlyDem = .true.
                endif
  
  
        else if (ch.eq.'-faultDem') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                faultDem = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                faultDem = .false.
                else
                faultDem = .true.
                endif
   
        else if (ch.eq.'-useMinDem') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                useMinDem = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                useMinDem = .false.
                else
                useMinDem = .true.
                endif
 
        else if (ch.eq.'-faultZsurf') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                faultZsurf = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                faultZsurf = .false.
                else
                faultZsurf = .true.
                endif

        else if (ch.eq.'-debug') then
        call getarg(iarg+1,ch)
        ldeb=numget(ch,msg)
        debug = msg.eq.' '
          if (.not.debug) then
          call logout(2,'STAUS=errorDebug on reading ldeb variable')
          endif
 
        else if (ch.eq.'-limnoise') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                limnoise = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                limnoise = .false.
                else
                limnoise = .true.
                endif
 
        else if (ch.eq.'-postProc') then
        call getarg(iarg+1,ch)
                if (ch.eq.'yes'.or.ch.eq.'true') then
                postProc = .true.
                else if (ch.eq.'no'.or.ch.eq.'false') then
                postProc = .false.
                else
                postProc = .true.
                endif
 
        else if (ch.eq.'-solver') then
        call getarg(iarg+1,ch)
        solver = numget(ch,msg)
 
        else if (ch.eq.'-maxdhmax') then
        call getarg(iarg+1,ch)
        maxdhmax = numget(ch,msg)
 
        else if (ch.eq.'-runUpLim') then
        call getarg(iarg+1,ch)
        runUpLim = numget(ch,msg)
 
        else if (ch.eq.'-ilim') then
        call getarg(iarg+1,ch)
        ilim = numget(ch,msg)
 
        else if (ch.eq.'-limval') then
        call getarg(iarg+1,ch)
        limval = numget(ch,msg)
 
        else if (ch.eq.'-limCoeff') then
        j = 0
          do while (j.lt.nlimcoeff.and.msg.eq.' ')
          j = j+1
          call getarg(iarg+j,ch)
          limCoeff(j) = numget(ch,msg)
            if (msg.ne.' ') then
            write (*,*) 'expected number when reading limCoeff, not the string:'
            write (*,*) ch(1:length(ch))
            stop
            endif
          enddo
 
        else if (ch.eq.'-limZW') then
        call getarg(iarg+1,ch)
        limZW = numget(ch,msg)
 
        else if (ch.eq.'-limV') then
        call getarg(iarg+1,ch)
        limV = numget(ch,msg)
 
        else if (ch.eq.'-stepmax') then
        call getarg(iarg+1,ch)
        istepmax = numget(ch,msg)
 
        else if (ch.eq.'-velmin') then
        call getarg(iarg+1,ch)
        velmin = numget(ch,msg)
 
        else if (ch.eq.'-seaLevel') then
        call getarg(iarg+1,ch)
        seaLevel = numget(ch,msg)
 
        endif
      enddo
 
 
*-------------------------------------------------------------------------------------
c defaults are function of other parmeters
*-------------------------------------------------------------------------------------
        if (solver.eq.0) then
        varjun = 2
        varHLLE = 1
        endif

        if (maxlev.gt.0) then
        intsrtm = 2
        endif

      nested = boundaryDir.ne.' '.and.boundaryDir.ne.'none'

        if (nested) then
        boundaryCode = 2
        else
        boundaryCode = boundaryCode0
        endif

      if (dtstatL(1).lt.0) dtstatL = tmax/4 


      toutL(ntimeL) = tmax
*-------------------------------------------------------------------------------------
      maxDistLand = min(maxDist/2,maxDistLand)
      minDistSea = min(maxDist/4,minDistSea)
      minDistLand = min(minDistSea,minDistLand)
*-------------------------------------------------------------------------------------
      do iarg = 1,narg
      call getarg(iarg,ch)
        if (ch.eq.'-varjun') then
        call getarg(iarg+1,ch)
        varjun = numget(ch,msg)
 
        else if (ch.eq.'-varHLLE') then
        call getarg(iarg+1,ch)
        varHLLE = numget(ch,msg)

        else if (ch.eq.'-boundaryCode') then
        call getarg(iarg+1,ch)
        boundaryCode = numget(ch,msg)
        boundaryCode0 = boundaryCode

        else if (ch.eq.'-maxDistLand') then
        call getarg(iarg+1,ch)
        maxDistLand = numget(ch,msg)
 
        else if (ch.eq.'-minDistLand') then
        call getarg(iarg+1,ch)
        minDistLand = numget(ch,msg)
 
        else if (ch.eq.'-minDistSea') then
        call getarg(iarg+1,ch)
        minDistSea = numget(ch,msg)

        else if (ch.eq.'-intsrtm') then
        call getarg(iarg+1,ch)
        intsrtm = numget(ch,msg)
 
        else if (ch.eq.'-dtstat') then
        ntime = 0
            do j = 1,ntimeMax
            call getarg(iarg+j,ch)
              if (ch(1:1).eq.'-'.or.ch.eq.' ') exit
            dtstatL(j) = numget(ch,msg)
            ntime = j
            enddo
        ntimeL = max(ntime,ntimeL)
          do j = ntime+1,ntimeL
          dtstatL(j) = dtstatL(j-1)
          enddo    

        else if (ch.eq.'-tout') then
        ntime = 0
            do j = 1,ntimeMax
            call getarg(iarg+j,ch)
              if (ch(1:1).eq.'-'.or.ch.eq.' ') exit
            toutL(j) = numget(ch,msg)
            ntime = j
            enddo
        ntimeL = max(ntime,ntimeL)
          do j = ntime+1,ntimeL
          toutL(j) = toutL(j-1)
          enddo    
        endif

      enddo
c----------------------------------------------------------
      if (narg.eq.0.and.dohelp) then
      call getenv ('hlpHyFlux2',helpfile)
      print *
      print '(a,a)','code:	',code(1:length(code))
      print '(a,a)','executable:	',executable(1:length(executable))
      print '(a,a)','help file:	`',helpfile(1:length(helpfile))
      print '(a,a)','source folder:	',pwdsoft(1:length(pwdsoft))
      print '(a,a)','compile option:	',more_flag(1:length(more_flag))
      print '(a,a)','version date:	',version(1:length(version))
      print '(a,a)','execution date:	',execution(1:length(execution))
      print *,' '

      if (existFile(helpfile)) then
      call system ('cat '//helpfile)
      else
      print *,'helpfile not exist'
      endif

#if defined WIN32
        read *
#endif
      stop
      endif
*---------------------------------------------------------
      return
      end

c========================================================================
      subroutine derived_parameters 
c========================================================================
      use parameter_data
      use boundary_data
      use window_data
      use statistics_data
      use parallel_data
      use map_data
      implicit none
c------------------------------------
      character ext*8, message*132, message2*132, list*132, ch*132, pwd*256
      logical(i4) exist, existFile, eqmv
      integer(i4) length,  l, narg, iargc,makedir, filetype, mkerr, err, tag, i, iun
	integer(i4) iarg, errlist, ncolumn
c-----------------------------------------------------------------------
c create working directory if does not exist
	message = ' '
	message2 = ' '
       lw = length(workdir)
c-----------------------------------------------------------------------
       if (MASTER) then
       call statdir (workdir(1:lw)//null,filetype)
            if (filetype.eq.0) then
            mkerr = makedir (workdir(1:lw))
	        if (mkerr.eq.0) then
        	err=0
	        message = 'Created workdir = '//workdir(1:lw)
	        else
	        message = 'CANNOT Created workdir = '//workdir(1:lw)
        	err=2
	        endif
            else if (filetype.eq.1) then
            err=0
	    message = 'Yet exist workdir = '//workdir(1:lw)
            endif
       endif
#ifdef PARALLEL
        call S_integer4_master_to_slave    (err,  99)
#endif
          if (err.gt.0) call logout (err,message)
c-----------------------------------------------------------------------
c create output directory if does not exist
        if (outdir.eq.' ') outdir = workdir

        lout = length(outdir)
c-----------------------------------------------------------------------
        if (MASTER) then
        call statdir  (outdir(1:lout)//null,filetype)
            if (filetype.eq.0) then
            mkerr = makedir (outdir(1:lout))
              if (mkerr.eq.0) then
              err=0
              message2 = 'Created outdir = '//outdir(1:lout)
              else
              err=2
              message2 = 'CANNOT create outdir = '//outdir(1:lout)
              endif
            else if (filetype.eq.1) then
            err=0
	    message2 = 'Yet exist outdir = '//outdir(1:lout)
            endif
        endif
#ifdef PARALLEL
        call S_integer4_master_to_slave    (err,  99)
#endif
        if (err.gt.0) call logout (err,message2)
c-----------------------------------------------------------------------
      iout = iout0 + icore
      write (ext(1:3),'(i3)') icore
      call fillzero (ext,3)
c-----------------------------------------------------------------------
c create console file for printout controls (if requested)
        if (consolefile.eq.' '.or.consolefile.eq.'none') then
          if (MASTER) then
          iout = 6
          endif
        console = .false.

        else if (code.eq.'pre_HyFlux2') then
        open (unit=iout,file=workdir(1:lw)//bs//consolefile)
        console = .true.

        else if (MASTER) then
        open (unit=iout,file=outdir(1:lout)//bs//consolefile)
        console = .true.

        else if (SLAVE.or.POST) then
        open (unit=iout,file=outdir(1:lout)//bs//ext(1:3)//consolefile)
        console = .true.
        endif
c-----------------------------------------------------------------------
        call getenv ('PWD',pwd)
        write (iout,*)
        write (iout,*) '==============================================='
        write (iout,*)     '            code:',code(1:length(code)),' icore=',icore,' MASTER=',MASTER
        write (iout,'(a)') '            executable:'//executable(1:length(executable))
        write (iout,'(a)') '            source folder: '//pwdsoft(1:length(pwdsoft))
        write (iout,'(a)') '            compile option: '//more_flag(1:length(more_flag))
        write (iout,'(a)') '            version date: '//version(1:length(version))
        write (iout,'(a)') '            execution date: '//execution(1:length(execution))
        write (iout,'(a)') '            current folder: '//pwd(1:length(pwd))
        write (iout,*) '==============================================='
        write (iout,*)
	if (message.ne.' ') write (iout,'(a)') message(1:length(message))
	if (message2.ne.' ') write (iout,'(a)') message2(1:length(message2))
c-----------------------------------------------------------------------
c create watch directory if does not exist
        if (watchdir.eq.' ') then
          if (code.eq.'pre_HyFlux2') then
          watchdir = workdir
          else
          watchdir = workdir(1:lw)//bs//'watch'
          endif
        endif
        lwatch = length(watchdir)

c------------------------------
        if (nested) then
        lbdir = length(boundaryDir)
        call statdir  (boundaryDir(1:lbdir)//null,filetype)
          if (filetype.eq.0) then 
          message = 'Not exist boundaryDir '//boundaryDir(1:lbdir)
          call logout (2,message)
          endif
        endif
c------------------------------------
        if (eqmv(limZW)) limZW=limval
        if (eqmv(limV)) limV=limval
c------------------------------
c re-set dtout as a multiple of dthydro (if defined)
      if (dthydro.gt.0.) then
        do l = 1,ntimeL
        dtoutL(l) = int(dtoutL(l)/dthydro+0.5)*dthydro
        enddo
      endif

        do l = 1,ntimeL
c re-set dtrest as a multiple of dtout
        dtrestL(l) = int(dtrestL(l)/dtoutL(l)+0.5)*dtoutL(l)
c re-set dtstat as a multiple of dtout
        dtstatL(l) = int(dtstatL(l)/dtoutL(l)+0.5)*dtoutL(l)
        enddo
c------------------------------
c set derived constants
c------------------------------
      cmin = sqrt(g*hmin)
      cmin0 = sqrt(g*hmin/10)
      hmin2=hmin*2

      if (hcrit.lt.hvel0*2) then
      write (iout,*) 'CAUTION: hcrit < hvel0*2: it has been adjusted'
      hcrit = hvel0*2
      endif
 
      ccrit = sqrt(g*hcrit)
      ccrit0 = sqrt(g*hcrit/10)
 
      hvel00 = max(hvel0/10,hmin2)
      hlim0 = max(hlim/10,hmin2)
      hshore = min(hshore,hlim0)
      hshore0 = max(hshore/10,hmin2)

      hprim0 = hprim/10
 
      halfg = half*g
      inv2g = 1./(2*g)
c     -------------------------
      invhlim = 1./log(hlim/hlim0)
      invcrit = 1./log(ccrit/ccrit0)
      invhvel0 = 1./log(hvel0/hvel00)
      invhshore = 1./log(hshore/hshore0)
      invsf = 1./log(hvel00/hmin)
c----------------------------------------
      longmax0 = longmin0 + dlong0*nlong0
      latmin0 = latmax0 - dlat0*nlat0
      nval0 = nlong0*nlat0
c------------------------------
      if (imap.gt.0) then
      write (ext,'(f7.3)') float(imap)/1000.
      call fillzero (ext,7)
      zsurfmap = workdir(1:lw)//bs//'zsurf'//ext(1:7)//mapext
      vxmap = workdir(1:lw)//bs//'vx000'//ext(1:7)//mapext
      vymap = workdir(1:lw)//bs//'vy000'//ext(1:7)//mapext
      faultmap = ' '
      timestart = imap*dtoutL(1)

      else if (timestart.gt.0) then
      write (ext,'(i8)') int(timestart+0.5)
      call fillzero (ext,8)
      zsurfmap = outdir(1:lout)//bs//'TIF_H_'//ext(1:8)//outext
      vxmap = outdir(1:lout)//bs//'TIF_U_'//ext//outext
      vymap = outdir(1:lout)//bs//'TIF_V_'//ext//outext
      faultmap = ' '
      imap = timestart/dtoutL(1)
      write (iout,*) ' icore=',icore,' timestart=',timestart,' imap=',imap,' dtout=',dtoutL(1)
      endif

c------------------------------
        if (code.eq.'HyFlux2') then
        call mapDefault (workdir(1:lw),'demNode'//mapext,demNodemap)
        call mapDefault (workdir(1:lw),'demSea'//mapext,demmap)
        call mapDefault (workdir(1:lw),'demSea'//mapext,maskmap)
  	call mapDefault (workdir(1:lw),'bathymetry'//outext,batmap)
  	call mapDefault (workdir(1:lw),'batNode'//outext,batnode)
        call mapDefault (workdir(1:lw),'noSlope'//mapext,noslopemap)
        call mapDefault (workdir(1:lw),'dzdx'//mapext,dzdxmap)
        call mapDefault (workdir(1:lw),'dzdy'//mapext,dzdymap)
        call mapDefault (workdir(1:lw),'fcor'//mapext,fcormap)
        call mapDefault (workdir(1:lw),'man'//mapext,manmap)
        call mapDefault (workdir(1:lw),'watch.txt',watchfile)
        call mapDefault (workdir(1:lw),'shore.txt',shorefile)
c search the maps in outdir if they do not exist in workdir
  	  if (batmap.eq.' ') call mapDefault (outdir(1:lout),'bathymetry'//outext,batmap)
  	  if (batNode.eq.' ') call mapDefault (outdir(1:lout),'batNode'//outext,batnode)
        endif
c------------------------------
        boundaryFile=workdir(1:lw)//bs//'boundary.txt'
c------------------------------
        if (placefile.ne.' '.and.watchfile.eq.' ') then
        inquire(file=placefile,exist=exist)
          if (exist) watchfile=workdir(1:lw)//bs//'watch.txt'
        endif
c------------------------------
        if (placefile.ne.' '.and.shorefile.eq.' ') then
        inquire(file=placefile,exist=exist)
          if (exist) shorefile=workdir(1:lw)//bs//'shore.txt'
        endif
c------------------------------
100    if (err.gt.0) call logout(err,message)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      existHolland = existFile(hollandFile)
        if (.not.existHolland.and.hollandFile.ne.' '.and.hollandFile.ne.'none') then
        message= 'Not exist holland file '//hollandFile(1:length(hollandFile))
        call logout(2,message)
        endif
*---------------------------------------------------------
      write(iout,*) 'done derived_parameters'
*---------------------------------------------------------
      return
      end
c============================================================================
      subroutine initfiles
c============================================================================
      use parameter_data
      use statistics_data
      use parallel_data
      implicit none
      integer filetype, mkerr, makedir, err, i
      character message*132
      logical(l4)  lmon, lmon1, lcpu
c-----------------------------------------------------------------------
        if (MASTER.and.code.eq.'HyFlux2') then
        call statdir  (watchdir(1:lwatch)//null,filetype)
            if (filetype.eq.0) then
            mkerr = makedir (watchdir(1:lwatch))
              if (mkerr.eq.0) then
              write(iout,*) 'created ',watchdir(1:lwatch)
              err=0
              else
              message = 'CANNOT create '//watchdir(1:lwatch)
              err=2
              endif
            endif
#ifdef PARALLEL
        call S_integer4_master_to_slave    (err,  99)
#endif
          if (err.gt.0) call logout (err,message)
        endif
c-----------------------------------------------------------------------
c open control files
      if (code.eq.'HyFlux2'.and.MASTER) then
      call openfile (imon,outdir(1:lout)//bs//monitor,'time',lmon)
      call openfile (imon1,outdir(1:lout)//bs//'listfiles.txt','*time',lmon1)
#ifdef PARALLEL
      call openfile (icpu,outdir(1:lout)//bs//'cpus.txt','time',lcpu)
#endif
*     ---------------------------------------------------
        if (.not.lmon.or.imap.eq.0) then
          if (existHolland) then
          write (imon,'(26a12)') 'istep','imap','velmax','dhmax','frmax','dt'
     &                       ,'avgdepth','courmax','fluxnum','time','cpu','elaps','zwmax','u10max','deprmax','ncorr'
          else
          write (imon,'(26a12)') 'istep','imap','velmax','dhmax','frmax','dt'
     &                       ,'avgdepth','courmax','fluxnum','time','cpu','elaps','zwmax','ncorr'
          endif
        endif
        if (.not.lmon1.or.imap.eq.0)
     &   write (imon1,'(a,1x,4(a5,16x))') '*time       ','HA   ','U    ','V    ','IDTime'
#ifdef PARALLEL
        if (.not.lcpu.or.imap.eq.0)
     &   write (icpu,'(a10,a10,128(i7,"cpu"))') 'istep','time',(i,i=0,ncore1) 
#endif
      endif
      return
      end
c============================================================================
      subroutine openfile (iun,file,var,found)
c============================================================================
      use parameter_data
      use statistics_data
      use parallel_data
      implicit none
      integer iun, err, length, irec
      character var*(*), file*(*)
      real*4 value
      logical exist, found
      found=.true.
      inquire (file=file,exist=exist)
      open (unit=iun,file=file)
        if (imap.eq.0.or..not.exist) return
      value = timestart
      call  findrec (iun,var,'gt',value,err,irec)
      found = err.eq.0
      return
      end
c============================================================================
      subroutine writeoutline 
c============================================================================
      use parameter_data
      use parallel_data
      implicit none
      integer i, length,l

        do i =1,noutline
        l = length(outline(i))
        write (iout,'(a)') outline(i)(1:l)
          if (iout.ne.6.and.MASTER) write (6,'(a)') outline(i)(1:l)
        enddo

      return
      end    
c============================================================================
      subroutine closeParam (err)
c============================================================================
      use constants
      use parallel_data
      implicit none
      character ch*254
      integer(i4) length, err
      logical first /.true./

      close (ipar)

        if (err.gt.0) call logout (err,'STATUS=errorParameter see parameters file')

	if (.not.first.or.iout.eq.6.or.SLAVE) return

      rewind (iout)

        do while (.true.)
        read (iout,'(a)',end=10) ch
        write(6,'(a)') ch(1:length(ch))
        enddo

10    backspace (iout)
      first = .false.
      return
      end
c========================================================================
        subroutine logout (err,message)
c========================================================================
        use parameter_data
        use parallel_data
        implicit none
        character message*(*)
        integer(i4) length, err, local_err
	logical(i4) exist
c
        if (MASTER) then
	inquire (file=outdir(1:lout)//bs//logoutfile,exist=exist)
	  if (exist) then
          open (ierr,file=outdir(1:lout)//bs//logoutfile)
          close (ierr,status='delete')
	  endif          
        open (ierr,file=outdir(1:lout)//bs//logoutfile)
        write (ierr,'(a)') message(1:length(message))
        close (ierr)

	write (iout,'(a)') 'logout '//message(1:length(message))
	  if (iout.ne.6) write (6,'(a)') 'logout '//message(1:length(message))
        endif
c-------------------------------
c do not exit if err < 0
        if (err.ge.0) then
            if (iout.ne.6) close (iout)
c-------------------------------
#ifdef PARALLEL
       call mpi_barrier (mpi_comm_world, local_err)
       call mpi_finalize (local_err)
#endif
        call exit (err)
        endif

        return
        end
c========================================================================
        subroutine login (message)
        use parameter_data
        implicit none
        character message*(*)
	logical(i4) exist
	integer(i4) length, ios
	real(r4) cpustart, cpu
c
	inquire (file=outdir(1:lout)//bs//loginfile,exist=exist)
	  if (.not.exist) then
	  message = ' '
	  return
	  endif

	ios = -1

	do while (ios.ne.0)
	call cputime (cpu) 
      cpustart=cpu
	  do while (cpu-cpustart.gt.1.)
	  call cputime (cpu) 
	  enddo

      open (ierr,file=outdir(1:lout)//bs//loginfile,iostat=ios,action='readwrite')
*	  if (ios.ne.0) write (iout,*) 'error in open file ',loginfile(1:length(loginfile)),' iostat=',ios
	enddo

        read (ierr,'(a)') message
        close (ierr,status='delete')

	  write (iout,*) 'login '//message(1:length(message))

	  return
        end
c========================================================================
	subroutine mapDefault (workdir,nameDef,nameMap)
	use constants
	use map_data
	implicit none
*---------------------------------------------------------
	character workdir*(*), nameMap*(*), nameDef*(*)
	logical(i4) exist
*---------------------------------------------------------
	if (nameMap.eq.'none') then
	nameMap=' '
	return
	
	else if (nameMap.ne.' ') then
	return
	endif
 
	maps = workdir//bs//nameDef
	inquire (file=maps,exist=exist)
	  if (exist) nameMap=maps	
*---------------------------------------------------------
	return
	end
c========================================================================
      subroutine show_parameters
c========================================================================
      use kind
      use parameter_data
      use boundary_data
      use statistics_data
      use window_data
      use geometry_data
      use parallel_data
      implicit none
c----------------------------------------------------------
      character yesno*3, list*132, ch*132, msg*3, pwd*256
     &         ,fmtc*20, fmtr*20, fmti*20, fmta*20, command*1000, ext*3
      integer(i4) length, i, l, narg, iargc
	real(r4) numget,num
	integer(i4) iarg
c----------------------------------------------------------
      ipar = ipar0 + icore
      write (ext(1:3),'(i3)') icore
      call fillzero (ext,3)
c-----------------------------------------------------------------------
c create console file for printout controls (if requested)
        if (code.eq.'pre_HyFlux2') then
        open (unit=ipar,file=workdir(1:lw)//bs//paramfile)
        else if (MASTER) then 
        open (unit=ipar,file=outdir(1:lout)//bs//paramfile)
        else 
          if (verify) then
          open (unit=ipar,file=outdir(1:lout)//bs//ext//paramfile)
          else
          open (unit=ipar,status='scratch')
          endif
        endif
c-----------------------------------------------------------------------
       fmtc = '(a,a,a)'
       fmti = '(a,i8,a)'
       fmtr = '(a,1pg14.6,a)'
       fmta = '(a,100(1pg14.6))'
        call getenv ('PWD',pwd)

        write (ipar,*)
        write (ipar,*)
        write (ipar,*)'============================================================'
        write (ipar,*) '            code: ',code(1:length(code)),' icore=',icore
        write (ipar,'(a)') '            executable:'//executable(1:length(executable))
        write (ipar,'(a)') '            source folder: '//pwdsoft(1:length(pwdsoft))
        write (ipar,'(a)') '            compile option: '//more_flag(1:length(more_flag))
        write (ipar,'(a)') '            version date: '//version(1:length(version))
        write (ipar,'(a)') '            execution date: '//execution(1:length(execution))
        write (ipar,'(a)') '            current folder: '//pwd(1:length(pwd))
        write (ipar,*)'============================================================'
        write (ipar,*)
        write (ipar,*)
        write (ipar,*)'============================================================'
        write (ipar,*)'                   Argument list'
        write (ipar,*)'============================================================'
        write (ipar,*)
        narg = iargc()
        iarg = 1
        do while (iarg.lt.narg)
        call getarg(iarg,ch)
*        print *,'ch=',ch(1:length(ch))
        list = ch
        l = length(list)
        iarg = iarg+1
        call getarg(iarg,ch)
*        print *,'ch=',ch(1:length(ch))
	  num = numget(ch,msg)
          do while (.not.(ch(1:1).eq.'-'.and.msg.ne.' ').and.ch.ne.' '.and.iarg.le.narg)
          list = list(1:l+1)//ch
          l = length(list)
          iarg = iarg+1
            if (iarg.lt.narg) then
		call getarg(iarg,ch)
*        print *,'ch=',ch(1:length(ch))
	    num = numget(ch,msg)
	    endif
          enddo
        write (ipar,fmtc)list(1:l)
        enddo

        write (ipar,*)
        write (ipar,*)'============================================================'
        write (ipar,*)'                   Deck data'
        write (ipar,*)'============================================================'
        write (ipar,*)
        call listvar (ipar)

        write (ipar,*)
        write (ipar,*)'============================================================'
        write (ipar,*)'                   Used parameters '
        write (ipar,*)'============================================================'
      write(ipar,*)
      write(ipar,fmti) 'ncore =',ncore
      write(ipar,*)
      write(ipar,fmtc) 'inpdeck =',inpdeck(1:length(inpdeck))
      write(ipar,fmtc) 'confdeck =',confdeck(1:length(confdeck))
      write(ipar,fmtc) 'srtmpath =',srtmpath(1:length(srtmpath))
      write(ipar,fmtc) 'workdir =',workdir(1:lw)
      write(ipar,fmtc) 'outdir =',outdir(1:lout)
      write(ipar,fmtc) 'watchdir=',watchdir(1:lwatch)
      write(ipar,*)
      write(ipar,fmtr) 'radius =',radius0
      write(ipar,fmtr) 'longmin =',longmin0,' [deg]'
      write(ipar,fmtr) 'longmax =',longmin0+dlong0*nlong0,' [deg]'
      write(ipar,fmtr) 'latmin =',latmax0-dlat0*nlat0,' [deg]'
      write(ipar,fmtr) 'latmax =',latmax0,' [deg]'
      write(ipar,fmti) 'nlong=',nlong0
      write(ipar,fmti) 'nlat =',nlat0
      write(ipar,fmtr) 'dlong=',dlong0,' [deg]'
      write(ipar,fmtr) 'dlat =',dlat0,' [deg]'
      write(ipar,fmtr) 'dlong=',dlong0*60," [min]"
      write(ipar,fmtr) 'dlat =',dlat0*60," [min]"
      write(ipar,*)
      write(ipar,fmti) 'intsrtm =',intsrtm
      write(ipar,fmti) 'maxlev =',maxlev
      write(ipar,fmtr) 'baseS =',baseS
      write(ipar,fmti) 'reegularize =',regularize
      write(ipar,*)
      write(ipar,fmtc) 'demNode =',demNodemap(1:length(demNodemap))
      write(ipar,fmtc) 'dem =',demmap(1:length(demmap))
      write(ipar,fmtc) 'mask =',maskmap(1:length(maskmap))
      write(ipar,fmtc) 'dzdx =',dzdxmap(1:length(dzdxmap))
      write(ipar,fmtc) 'dzdy =',dzdymap(1:length(dzdymap))
      write(ipar,fmtc) 'bathymetry =',batmap(1:length(batmap))
      write(ipar,fmtc) 'batNode =',batnode(1:length(batnode))
      write(ipar,fmtc) 'depth =',depthmap(1:length(depthmap))
      write(ipar,fmtc) 'zsurf =',zsurfmap(1:length(zsurfmap))
      write(ipar,fmtc) 'noSlope =',noSlopemap(1:length(noSlopemap))
      write(ipar,fmtc) 'fault =',faultmap(1:length(faultmap))
      write(ipar,fmtc) 'rain =',rainmap(1:length(rainmap))
      write(ipar,fmtc) 'fcor =',fcormap(1:length(fcormap))
      write(ipar,fmtc) 'vx =',vxmap(1:length(vxmap))
      write(ipar,fmtc) 'vy =',vymap(1:length(vymap))
      write(ipar,fmtc) 'hollandFile =',hollandFile(1:length(hollandFile))
      write(ipar,fmtc) 'boundary    =',boundaryFile(1:length(boundaryFile))
      write(ipar,fmtc) 'boundaryDir =',boundaryDir(1:length(boundaryDir))
      write(ipar,fmti) 'boundaryCode0 =',boundaryCode0
      write(ipar,fmti) 'boundaryCode =',boundaryCode
      write(ipar,fmtr) 'boundaryFac =',boundaryFac
      write(ipar,fmtc) 'monitor     =',monitor(1:length(monitor))
      write(ipar,fmti) 'freqmon     =',freqmon
      write(ipar,fmtc) 'parameters  =',paramfile(1:length(paramfile))
      write(ipar,fmtc) 'console     =',consolefile(1:length(consolefile))
      write(ipar,fmtc) 'login       =',loginfile(1:length(loginfile))
      write(ipar,fmtc) 'logout      =',logoutfile(1:length(logoutfile))
      write(ipar,fmtc) 'place       =',placefile(1:length(placefile))
      write(ipar,fmtc) 'watch       =',watchfile(1:length(watchfile))
      write(ipar,fmtc) 'shore       =',shorefile(1:length(shorefile))
      write(ipar,'(20a)') 'outmaps     =',(outmaps(i)(1:10),' ',i=1,noutmaps)
      write(ipar,'(20a)') 'restmaps     =',(restmaps(i)(1:10),' ',i=1,nrestmaps)
      write(ipar,'(20a)') 'statmaps     =',(statmaps(i)(1:10),' ',i=1,nstatmaps)
      write(ipar,*)
      write(ipar,fmtc) 'mapfmt       =',mapfmt(1:length(mapfmt))
      write(ipar,fmtc) 'mapext       =',mapext(1:length(mapext))
      write(ipar,fmtc) 'outfmt       =',outfmt(1:length(outfmt))
      write(ipar,fmtc) 'outext       =',outext(1:length(outext))

      write(ipar,fmtr) 'errprim = ',errprim
      write(ipar,fmtr) 'epsvar = ',epsvar
      write(ipar,fmtr) 'errstep = ',errstep
      write(ipar,fmtr) 'facstep = ',facstep
      write(ipar,fmtr) 'hmin = ',hmin
      write(ipar,fmtr) 'hlim = ',hlim
      write(ipar,fmtr) 'hcrit = ',hcrit
      write(ipar,fmtr) 'hprim = ',hprim
      write(ipar,fmtr) 'g = ',g
      write(ipar,fmtr) 'cdmax = ',cdmax
      write(ipar,*)
      write(ipar,fmtr) 'minDem = ',minDem
      write(ipar,fmtr) 'maxDem = ',maxDem
      write(ipar,fmtr) 'maxDepth = ',maxDepth
      write(ipar,fmtr) 'maxDist = ',maxDist
      write(ipar,fmtr) 'maxDistLand = ',maxDistLand
      write(ipar,fmtr) 'minDistLand = ',minDistLand
      write(ipar,fmtr) 'minDistSea = ',minDistSea
      if (manmap.eq.' ') then
      write(ipar,fmtr) 'man = ',manval
      else
      write(ipar,fmtc) 'man = ',manmap(1:length(manmap))
      endif
      if (manslopemap.eq.' ') then
      write(ipar,fmtr) 'manslope = ',manslopeval
      else
      write(ipar,fmtc) 'manslope = ',manslopemap(1:length(manslopemap))
      endif
      write(ipar,fmtr) 'manReef = ',manReef
      write(ipar,fmtr) 'demReef = ',demReef
      write(ipar,fmtr) 'expslope = ',expslope
      write(ipar,fmtr) 'hvel0 = ',hvel0
      write(ipar,fmtr) 'hout = ',hout
      write(ipar,fmtr) 'harr = ',harr
      write(ipar,fmtr) 'wout = ',wout
      write(ipar,fmtr) 'man0 = ',man0
      write(ipar,fmtr) 'velmin = ',velmin
      write(ipar,fmtr) 'hshore = ',hshore
      write(ipar,*)
      write(ipar,fmtr) 'courmax = ',courmax
      write(ipar,fmtr) 'fluxnum = ',fluxnum
      write(ipar,fmti) 'solver = ',solver
      write(ipar,fmti) 'varjun = ',varjun
      write(ipar,fmtr) 'maxdhmax = ',maxdhmax
      write(ipar,fmtr) 'runUpLim = ',runUpLim
      write(ipar,fmti) 'varHLLE = ',varHLLE
      write(ipar,fmti) 'ilim = ',ilim
      write(ipar,fmtr) 'limval = ',limval
      write(ipar,fmta) 'limCoeff = ',limCoeff
      write(ipar,fmtr) 'limZW = ',limZW
      write(ipar,fmtr) 'limV = ',limV
      write(ipar,fmtr) 'seaLevel = ',seaLevel
      write(ipar,fmtr) 'smallArg = ',smallArg
      write(ipar,fmtr) 'smallNew = ',smallNew
      write(ipar,fmtr) 'dthydro = ',dthydro
      write(ipar,fmti) 'ntimeL = ',ntimeL
      write(ipar,fmta) 'tout = ',(toutL(l),l=1,ntimeL)
      write(ipar,fmta) 'dtout   = ',(dtoutL(l),l=1,ntimeL)
      write(ipar,fmta) 'dtrest  = ',(dtrestL(l),l=1,ntimeL)
      write(ipar,fmta) 'dtstat  = ',(dtstatL(l),l=1,ntimeL)
      write(ipar,fmtr) 'dtmin = ',dtmin
      write(ipar,fmtr) 'strans=',strans
      write(ipar,fmtc) 'mapdeg=',yesno(mapdeg)
      write(ipar,fmtc) 'entrfix=',yesno(entrfix)
      write(ipar,fmtc) 'verify=',yesno(verify)
      write(ipar,fmtc) 'check=',yesno(check)
      write(ipar,fmtc) 'noSpot=',yesno(noSpot)
      write(ipar,fmtc) 'onlyDem=',yesno(onlyDem)
      write(ipar,fmtc) 'nocase=',yesno(nocase)
      write(ipar,fmtc) 'faultDem=',yesno(faultDem)
      write(ipar,fmtc) 'faultZsurf=',yesno(faultZsurf)
      write(ipar,fmtc) 'useMinDem=',yesno(useMinDem)
      write(ipar,fmti) 'debug=',ldeb
      write(ipar,fmtc) 'postProc=',yesno(postProc)
      write(ipar,fmtc) 'limnoise=',yesno(limnoise)
      write(ipar,fmtr) 'lamtrans = ',lamtrans
      write(ipar,fmti) 'stepmax = ',istepmax
      write(ipar,fmtr) 'timestart = ',timestart
      write(ipar,fmtr) 'tmax = ',tmax
      write(ipar,fmti) 'imap = ',imap
      write(ipar,fmtr) 'hlim0 = ',hlim0
      write(ipar,fmtr) 'cmin = ',cmin
      write(ipar,fmtr) 'ccrit = ',ccrit
      write(ipar,fmtr) 'ccrit0 = ',ccrit0
      write(ipar,fmtr) 'cmin0 = ',cmin0
      write (ipar,*)'============================================================'
      write (ipar,*)
        if (check) call logout(0,'STATUS=check')
      return
      end
 

*====================================================================================
      function reqmap(outmap)
*====================================================================================
      use parameter_data
      implicit none
      character outmap*(*)
      logical(i4) reqmap
      integer(i4) i
 
      reqmap = .false.
 
        do i = 1,noutmaps
          if (outmap.eq.outmaps(i)) then
          reqmap = .true.
          exit
          endif
        enddo
      return
      end
c============================================================================
      subroutine readmap(var,fullpath,value,callexit)
c============================================================================
      use constants
      use geometry_data
      use parameter_data
      implicit none
c
      logical*1 , allocatable :: mask1(:,:)

      real(r4) value(nvx,nvy)
      character fullpath*(*),var*(*)
      integer(i4) length,lm, nval1, i,j
      logical(i4) exist,callexit, eqmv
c
      if (fullpath.eq.' ') then
      maps = workdir(1:lw)//bs//var(1:length(var))//mapext
      lm = length(maps)
      maps = maps(1:lm)//null
      
      else
      lm = length(fullpath)
      maps = fullpath(1:lm)//null
      endif

      inquire (file=maps, exist=exist)
	  if (.not.exist) Then
             if (callexit) then
             write (iout,*) ' '
             write (iout,*) 'The map ',maps(1:lm),' does not exist'
	     call closeParam (2)
             else
             return
             endif
	  endif
 
        call OPENGDAL (maps,nvx,nvy,typemap, xstart, ystart, cellsize,sizey,driver,nband,retcode)

        if (retcode.ne.0) then
        write (iout,*) 'error in reading map ',maps(1:lm)
        call closeParam (2)
        endif

        CALL GETGDALVAL (nband,value)      

        allocate (mask1(nvx,nvy))

        do j = 1,nvy
          do i = 1,nvx
          mask1(i,j) = .not.eqmv(value(i,j))
          enddo
        enddo

      write (iout,*) 'got ',maps(1:lm),'nvalue=',nvx*nvy,' notmv=',count(mask1),' min=',minval(value,mask=mask1)
     &          ,' max=',maxval(value,mask=mask1)

      deallocate (mask1)

      return
      end
c============================================================================
      subroutine map2outval (var,fullpath,outvalue)
c============================================================================
      use constants
      use geometry_data
      use parameter_data
      implicit none

      character var*(*),fullpath*(*)
      integer(i4) length,lm
      logical(i4) exist, eqmv
      real(r4) outvalue(nv)
c
      if (fullpath.eq.' ') then
      lm = length(var)
      maps = workdir(1:lw)//bs//var(1:lm)//mapext
      
      else
      lm = length(var)
      maps = fullpath(1:lm)
      endif

      lm = length(maps)
      maps = maps(1:lm)//null


      inquire (file=maps, exist=exist)
	  if (.not.exist) Then
          outvalue = mv
          return
	  endif
 
      call OPENGDAL (maps,nvx,nvy,typemap, xstart, ystart, cellsize,sizey,driver,nband,retcode)
        if (retcode.ne.0) then
        write (iout,*) 'error in reading map ',maps(1:lm)
        call closeParam (2)
        endif

      CALL GETGDALVAL (nband,val)      
      outvalue = val(kl)
      write (iout,*) 'read map ',maps(1:lm)
      write (iout,*) 'minval =',minval(outvalue),' maxval=',maxval(outvalue)
      return
      end
c============================================================================
      subroutine verifyMaps
c============================================================================
      use parameter_data
      use geometry_data
      use hydro_data
      use slope_data
      use window_data
      implicit none
c
      integer(i4) mkerr, makedir, lve, m, pl, pr, i,j, l, nvx1, nvy1, filetype, vtypeL, vtypeR
      real(r4) zL, zR
      logical(i4) maskl, maskr
      character verdir*1000
 
      real(r4), allocatable :: demj(:,:),demh(:,:)
c-------------------------------------------------
        write (iout,*) 'allocated kl ? ',allocated(kl)
        write (iout,*) 'allocated mask ? ',allocated(mask)
        write (iout,*) 'allocated il ? ',allocated(il)
        write (iout,*) 'allocated jl ? ',allocated(jl)
        write (iout,*) 'allocated vtype ? ',allocated(vtype)
        write (iout,*) 'allocated jtype ? ',allocated(jtype)
        write (iout,*) 'allocated jtol ? ',allocated(jtol)
        write (iout,*) 'allocated jtor ? ',allocated(jtor)
        write (iout,*) 'allocated zv ? ',allocated(zv)
        write (iout,*) 'allocated val ? ',allocated(val)
        write (iout,*) 'allocated ival1 ? ',allocated(ival1)
        write (iout,*) 'allocated ival4 ? ',allocated(ival4)
        write (iout,*) 'allocated zj ? ',allocated(zj)
        write (iout,*) 'allocated dzdk ? ',allocated(dzdk)
c       -----------------------------------------
        verdir = workdir(1:lw)//bs//'verify'
        lve=lw+7
        call statdir (verdir(1:lve)//null,filetype)
            if (filetype.eq.0) then
            mkerr = makedir (verdir(1:lve)//null)
              if (mkerr.ne.0) then
              call logout (2,'STATUS=errorCreate cannot create directory '//verdir(1:lve))
              endif
            endif
c       -----------------------------------------
          val(kl) = zv
          maps = verdir(1:lve)//bs//'dem'//mapext
          call OUTGDAL (maps, nvx,nvy,4
     &               , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)
          val(kl) = dzdk(1,1:nv)
          maps = verdir(1:lve)//bs//'dzdx'//mapext
          call OUTGDAL (maps, nvx,nvy,4
     &               , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)
          val(kl) = dzdk(2,1:nv)
          maps = verdir(1:lve)//bs//'dzdy'//mapext
          call OUTGDAL (maps, nvx,nvy,4
     &               , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)
c-------------------------------------------------
	val(kl) = vtype(1:nv)
          maps = verdir(1:lve)//bs//'vtype'//mapext
          call OUTGDAL (maps, nvx,nvy,2
     &               , xstart, ystart, cellsize,val,nval
     &           , sizey, mapfmt, nband,error)

       return
       end

c=======================================================================
      subroutine opentab (iun,filename,nh,nplace,error)
c=======================================================================
      use constants
      implicit none
c-----------------------------------------------------------------
      integer(i4) iun, nplace, error, nmax, nh, i,j, length
      parameter (nmax=100)
      character filename*(*), header*254,  msg*3, namecol(nmax)*64
      logical exist
c-----------------------------------------------------------------
      error = 0
c-----------------------------------------------------------------
      inquire (file=filename,exist=exist)
        if (.not.exist) then
        write (iout,*) 'not exist ',filename(1:length(filename))
        error = -1
        return
        endif
c-----------------------------------------------------------------
*      open (iun,file=filename,action='read')
      write (iout,*) 'iun=',iun,' filename=',filename(1:length(filename))
      open (iun,file=filename)
      read (iun,'(a)') header
      call nocr(header)
      call proctab (header,namecol,nmax,nh)
c-----------------------------------------------------------------
      nplace = 0
          do while (.true.)
          read (iun,'(a)',end=101)  header
            nplace = nplace+1
          enddo
101   continue
      rewind (iun)
      return
      end

c=======================================================================
      subroutine getcoltab (iun,lat,long,field,mat,nfield,nplace,error)
c=======================================================================
      use constants
      use window_data
      implicit none
c-----------------------------------------------------------------
      integer(i4) iun, nplace,nfield, error, nmax, nh, i,j, ilong, ilat, length
      parameter (nmax=100)
      character header*254, field(nfield)*(*), namecol(nmax)*64, msg*3,lat*(*),long*(*)
      real(r4) mat(nfield,nplace)
      integer(i4), allocatable :: ifield(:)
      real(r4) numget, lat1, long1
c-----------------------------------------------------------------
      error = 0
c-----------------------------------------------------------------
      allocate (ifield(nfield))
      ifield = 0
      read (iun,'(a)') header
      call proctab (header,namecol,nmax,nh)
      call nocr(header)
        do i = 1,nh
          if (namecol(i).eq.long) ilong = i
          if (namecol(i).eq.lat) ilat = i
          do j = 1,nfield
            if (namecol(i).eq.field(j)) ifield(j) = i
          enddo
        enddo

      if (minval(ifield).eq.0) then
      write (iout,*) 'some requested field not found'
      error = 3
      endif
c-----------------------------------------------------------------
      write (iout,*)
      write (iout,*) (namecol(i)(1:length(namecol(i))+1),i=1,nh)
       do i=1,nfield
       write (iout,*) ifield(i),field(i)
       enddo
c-----------------------------------------------------------------
        j = 0
          do while (j.lt.nplace)
          read (iun,'(a)')  header
          call proctab(header,namecol,nmax,nh)
          call nocr(header)
          lat1 = numget(namecol(ilat),msg)
          long1 = numget(namecol(ilong),msg)
*            if (lat1.ge.latmin0.and.lat1.le.latmax0
*     &     .and.long1.ge.longmin0.and.long1.le.longmax0) then
            j = j+1
              do i = 1,nfield
              read (namecol(ifield(i)),*) mat(i,j)
              enddo
*            endif
          enddo
      return
      end
c=======================================================================
      subroutine getcoltab_char (iun,lat,long,field,name,nplace,error)
c=======================================================================
      use constants
      use window_data
      implicit none
c-----------------------------------------------------------------
      integer(i4) iun, nplace, error, nmax, nh, i,j, ilong, ilat, length,ifield
      parameter (nmax=100)
      character header*254, namecol(nmax)*64, msg*3,lat*(*),long*(*)
      character*(*)  name(*),field
      real(r4) numget,lat1,long1
c-----------------------------------------------------------------
      error = 0
c-----------------------------------------------------------------
      ifield = 0
      read (iun,'(a)') header
      call proctab (header,namecol,nmax,nh)
      call nocr(header)
        do i = 1,nh
          if (namecol(i).eq.long) ilong = i
          if (namecol(i).eq.lat) ilat = i
          if (namecol(i).eq.field) ifield = i
        enddo
c-----------------------------------------------------------------
      if (ifield.eq.0) then
      write (iout,*) 'field ',field,' not found'
      error = 3
      return
      endif
       write (iout,*) ifield,field
c-----------------------------------------------------------------
        j = 0
          do while (j.lt.nplace)
          read (iun,'(a)')  header
          call proctab(header,namecol,nmax,nh)
          call nocr(header)
          lat1 = numget(namecol(ilat),msg)
          long1 = numget(namecol(ilong),msg)
*            if (lat1.ge.latmin0.and.lat1.le.latmax0
*     &     .and.long1.ge.longmin0.and.long1.le.longmax0) then
            j = j+1
            name(j) = namecol(ifield)
*            endif
          enddo
      return
      end

c===================================================================
      subroutine outputs 
c===================================================================
      use parameter_data
      use geometry_data
      use hydro_data
      use statistics_data
      use parallel_data
      implicit none
c--------------------------------------------------------------------
      integer(i4) l,i,length
c--------------------------------------------------------------------
      if (istep.lt.3) then
      write(iout,*) 'nupvol=',count(upvol),' nupflux=',count(upflux)
     &       ,' nuptot=',count(upvol.or.upflux)
      write(iout,*) 'maxiter2=',maxiter2,' maxiter3=',maxiter3
 
      else if (istep.eq.3) then
	write(iout,*)
      call closeParam(0)
      endif
c---------------------------------------------------------------------
c monitor

      if (mod(istep,freqmon).eq.0.or.outnow) then
        if (existHolland) then
      write (imon,'(2i12,7(f12.4),6f12.3,i12)') istep, imap
     &        ,maxvn, dhmax, maxfr, dt, haver
     &        ,dt/dtmax, dt/dtjun
     &        ,time,  cpu,elaps, maxzw, maxu10, maxdepr
     &        ,ncorr
        else
      write (imon,'(2i12,7(f12.4),4f12.3,i12)') istep, imap
     &        ,maxvn, dhmax, maxfr, dt, haver
     &        ,dt/dtmax, dt/dtjun
     &        ,time,  cpu,elaps, maxzw
     &        ,ncorr
        endif
      endif

c---------------------------------------------------------------------
c console
      if (outnow) then
         if (imap.eq.1) write(iout,*) '============  start iteration ========================'
 
        write (ext1,'(f7.3)') float(imap)/1000.
        call fillzero (ext1,7)

      outline=' '
      write(outline(1),'( a,i7,4(a,f6.2)
     &       ,a,f8.3 
     &       ,a,i3,a,i2,a,f4.1 
     &       ,a,i3,a,i2,a,f4.1 
     &       ,a,f9.4 ,a,f4.2 
     &       ,a,a)' )
     &     'istep=',istep,' maxVel=',maxvn,' maxDh=',dhmax,' maxZw=',maxzw,' maxZwMax=',maxzwmax
     &       ,' dt=',dt
     &       ,' time[h:min:sec]=',int(time/3600),':',mod(int(time/60.),60),':',mod(time,60.)
     &       ,' elaps[h:min:sec]=',int(elaps/60),':',mod(int(elaps),60),':',mod(elaps*60.,60.)
     &     ,' avgDepthChange=',haver-haverIn,' courmax=',dt/dtmax
     &       ,' map ext. ',ext1
      noutline=1
      call writeoutline
      endif

      return
      end
