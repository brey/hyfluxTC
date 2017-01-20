      program mergeLoc
c-------------------------------------------------------------
c Origin: adaptation of mergeMapLoc
c-------------------------------------------------------------
	implicit none
        character versdate*20 /"Version - 5May2011"/
 
c--------------------------------------------
	character null*1, ap*1/'"'/ 
        integer*1 inull /z'00'/
        equivalence (null,inull)
 
	real*4 mv
      integer*4 imv /z'ffffffff'/, mv4
      integer*1 mv1

c--------------------------------------------
c local variables
      integer*4 iun/10/, itab/11/, iout/12/, iargc, narg, ii,jj
     & ,i,j,k,l, it0, time, hh, mm, lh, np, nvar, nvar1, nw, nwmax
     & ,date1, date2, ni, no, lname, ldata, lstr, ncmax

      parameter (lname=32,ldata=64,lstr=512)
      parameter (ncmax=50,nwmax=10)
      
      real*4 zmax, zmax1, maxzmax, maxHeightmin, maxHeightmax

      real*4 valget, th, getnumvar, trigAng, hhmm2val, tdummy
            
      logical exist, eqmv, ok, tsunamiCalc, existVar, verify, tarrShore, tmaxShore
     &        ,existFile, switch
 
      character czmax*6, yesno*3

      character*(lstr) inpkml(5), outkml(5), outLoc, largeLoc, smallLoc
     &         , ch, inpdeck, plot, plot0, command, DateStartCalc, imgFile
     &         , header,procline(100), getcharvar, windows(nwmax), cvar(100)

      character*(ldata)  values(ncmax), values1(ncmax), values2(ncmax)
      character*(ldata) date, placezmax, latzmax, lonzmax, sortval1

      character*(lname) nameval(ncmax), names(ncmax), names1(ncmax), names2(ncmax)
     &         , var(100), sortedby

      integer  colid, colzmax, colplace, colsort
     &          , colid1  ,colzmax1, colplace1, colsort1
     &       , coltarr, coltzmax,coldatearr, coldatezmax, collat, collon, colfile
     &       , nc, nc1, nc2,  nv, id, ncc, indstring, k1, k2


      integer icol(ncmax), lcol(ncmax), ndig(ncmax), sortsign, lwin(nwmax)

      integer ival(ncmax), kval(ncmax), nval, nname

      

      real*4, allocatable :: sortval(:)
      integer*4, allocatable :: order(:)

c-----------------------------------------------------------
      type locations
      character*(ldata) from, sortval
      character*(ldata) place, lat, lon, valcol(ncmax)
      real*4 zmax
      integer*4 id
      type(locations), pointer :: next
      end type locations

      type(locations), pointer :: first, ip, iplist(:), first0,ip0, back      
c-----------------------------------------------------------
#if defined WIN32
      character bs*1 /'\'/
#else
      character bs*1 /'/'/
#endif
      integer*4 length, lm, lout, ls
c-----------------------------------------------------------
      nname = 10
      nameval(1:nname) = (/'MaxHei       '
     &                    ,'maxHeight    '
     &                    ,'zmaxSea      '
     &                    ,'zmaxShore    '
     &                    ,'simRunup     '
     &                    ,'simInundation'
     &                    ,'simMaxHeight '
     &                    ,'simDistRunup '
     &                    ,'popInundated '
     &                    ,'areaInundated'
     &  /)

      ndig(1:nname) = (/2,2,2,2,2,2,2,-1,-1,2/)

      call getmv4(mv4)
      call getmv1(mv1)
        call getmv(mv)
 
      narg = iargc()
      it0 = time()
      largeLoc = 'none'
      smallLoc = 'none'
      outkml = ' '
      outLoc = ' '
      inpdeck = 'none'
      sortedby = 'ArrTime'
      sortsign = 1
      maxHeightmin = 0.05
      maxHeightmax = 100
      windows = '.'
      verify = .false.
      imgFile = ' '
      ni=0
      no=0
         
        if (narg.eq.0) then
 
      print *,' '
      print *,'mergeLoc help '
      print *,'merge location files'
      print *,'and create outkml'
      print *,' '
      print *,'-i input deck'
      print *,'   Tsunami input deck '
      print *,' DEFAULT is ',inpdeck(1:length(inpdeck))
      print *,' '
      print *,'-l large window location file'
      print *,'   no DEFAULT '
      print *,' '
      print *,'-maxHeightmin min. value of Max Height to be processed'
      print *,'   DEFAULT is ',maxHeightmin,' (m)'
      print *,' '
      print *,'-maxHeightmax max. value of Max Height to be processed'
      print *,'   DEFAULT is ',maxHeightmax,' (m)'
      print *,' '
      print *,'-s small window location file[s]'
      print *,' NOTE: first is less important'          
      print *,'   no DEFAULT '

      print *,'-w windows where plots are stored'
      print *,' NOTE: first is less important'          
      print *,'   no DEFAULT '

      print *,'-img  image list file'
      print *,'   no DEFAULT '

      print *,'-verify yes/no'
      print *,'    verify data processing'
      print *,'    DEFAULT is ',yesno(verify)
      print *,' '

      print *,'-iProc input procedure[s] (template)'
      print *,'   no DEFAULT'
      print *,' '

      print *,'-oProc output procedure[s] '
      print *,'   no DEFAULT'
      print *,' '

      print *,'-o outLoc'
      print *,'   no DEFAULT'
      print *,' '

      print *,'-sortedby var'
        print *,' The locations are sorted by "var" from min value to max value'
        print *,' Ex: -sortedby ArrTime '
        print *,' Note: specifing "-var" the loaction are sorted from max value to min value'
        print *,' Ex: -sortedby -MaxHei '
      print *,'   DEFAULT is ',sortedby
      print *,'NOTE: defining -sortedby none no sort is done' 
      print *,' '
#if defined WIN32
      read *
#endif
      stop
      endif
c-----------------------------------------------------------------
c read arguments
      do i = 1,narg
      call getarg (i,ch)
c       ----------------------------------------------------
        if (ch.eq.'-l') then
        call getarg (i+1,largeLoc)
c       ----------------------------------------------------
        else if (ch.eq.'-img') then
        call getarg (i+1,imgFile)
c       ----------------------------------------------------
        else if (ch.eq.'-i') then
        call getarg (i+1,inpdeck)
c------------------------------------------------------------
        else if ( ch .eq.'-verify') then
	  call getarg (i+1,ch)
		if (ch.eq.'yes') then
		verify = .true.
		else if (ch.eq.'no') then
		verify = .false.
		else
		verify = .true.
		endif
c------------------------------------------------------------
        else if (ch.eq.'-w') then
          j = i+1
          call getarg (j,ch)
c         ----------------------------------------------------
          l = 0
          do while (ch(1:1).ne.'-'.and.ch.ne.' '.and.j.le.narg) 
            l =l+1
            windows(l) = ch
            lwin(l) = length(ch)
            j = j+1
            call getarg (j,ch)
            enddo         
          nw = l
c------------------------------------------------------------
        else if (ch.eq.'-iProc') then
          j = i+1
          call getarg (j,ch)
c         ----------------------------------------------------
          l = 0
          do while (ch(1:1).ne.'-'.and.ch.ne.' '.and.j.le.narg) 
            l =l+1
            inpkml(l) = ch
            j = j+1
            call getarg (j,ch)
            enddo         
          ni = l
c-------------------------------------------------------------------------
        else if (ch.eq.'-oProc') then
          j = i+1
          call getarg (j,ch)
c         ----------------------------------------------------
          l = 0
          do while (ch(1:1).ne.'-'.and.ch.ne.' '.and.j.le.narg) 
            l =l+1
            outkml(l) = ch
            j = j+1
            call getarg (j,ch)
            enddo         
          no = l
c       ----------------------------------------------------
        else if (ch.eq.'-o') then
        call getarg (i+1,outLoc)
c       ----------------------------------------------------
          else if (ch.eq.'-maxHeightmin') then
          call getarg(i+1,ch)
          maxHeightmin = valget(ch)
c       ----------------------------------------------------
          else if (ch.eq.'-maxHeightmax') then
          call getarg(i+1,ch)
          maxHeightmax = valget(ch)
c----------------------------------------------------
        else if (ch.eq.'-sortedby') then
        call getarg (i+1,sortedby)
            if (sortedby(1:1).eq.'-') then
            sortsign = -1
            sortedby= sortedby(2:)
            endif
          endif
c       ----------------------------------------------------
      enddo
c---------------------------------------------------------------
        if (ni.ne.no) then
        print *,'ni=',ni,' no=',no,' the no of i/o procedures must be the same'
        call exit
        endif

        ncc = 0
c------------------------------------------------------------
c get input deck
c------------------------------------------------------------       
       if (.not.existFile(inpdeck)) goto 9
       call proc_deck (inpdeck,.false.)
       call getvars (var,cvar,nvar)

       tsunamiCalc = existVar('strike')
           if (tsunamiCalc) then
           th      = getnumvar('strike',mv)
           trigAng = 450-th
             if (trigAng.gt.360.) trigAng=trigAng-360
           nvar = nvar+1
           var(nvar) = 'FaultAngle' ; write (cvar(nvar),'(f6.2)') trigAng
           endif
       if (verify) print *,'done inpdeck'
9      continue
       dateStartCalc = '$DateTsunami '
       call replaceVar (dateStartCalc,lh,var,cvar,nvar)
         if (verify) print *,'dateStartCalc=',dateStartCalc(1:length(dateStartCalc))

c---------------------------------------------------------------
        coldatearr = -1
        coldatezmax = -1
c---------------------------------------------------------------
c get large locations file
c---------------------------------------------------------------
* ID      country      place      MaxHei      ArrTime      tMaxHei      ArrivalTS      lon      lat      popest      cityclass      Th1m      h1m

        if (.not.existFile(largeLoc)) then 

        print*, '-l file not there'
        goto 11 
        endif

        if (verify) print *,'-----------------------------------------'
        if (verify) print *,'processing ',largeLoc(1:length(largeLoc))

        open (file=largeLoc,unit=itab,action='read')
        read (itab,'(a)') header
        call nocr(header)
        if (verify) print *,'header= ',header(1:length(header))

      call proctab (header,names,ncmax,nc)
      ncc = nc
c       ---------------------
          ncc = ncc+1
          names(ncc)='locFile'
          colfile=ncc
c       ---------------------
        colid = -1
        colzmax = -1
        colplace = -1
        collat = -1
        collon = -1
        colsort = ncmax
        tarrShore = .false.
        tmaxShore = .false.

          do i = 1,nc
          icol(i) = i
            if (verify) print *,'i=',i,' ',names(i)(1:length(names(i)))
          call toleft (names(i))
            if (names(i)(1:4).eq.'* ID') names(i)='id'
            if (names(i)(1:2).eq.'ID') names(i)='id'
            if (names(i)(1:3).eq.'lat') names(i)='lat'
            if (names(i)(1:3).eq.'lon') names(i)='lon'
            if (names(i).eq.'$namePlace') names(i)='place'
            if (names(i).eq.'$name') names(i)='place'
            if (names(i)(1:1).eq.'$') names(i)=names(i)(2:)

            if (names(i).eq.'tarrShore') then
            tarrShore=.true.; names(i)='ArrTime'
            endif
            if (names(i).eq.'tzmaxShore') then
            tmaxShore=.true. ; names(i)='tMaxHei'
            endif

            if (names(i).eq.'simMaxHeight') names(i)='MaxHei'

! repeat this block for SMALL window
            if (names(i).eq.'id') colid = i
            if (names(i).eq.'MaxHei') colzmax = i
            if (names(i).eq.'tMaxHei') coltzmax = i
            if (names(i).eq.'ArrTime') coltarr = i
            if (names(i).eq.'ArrivalTS') coldatearr = i
            if (names(i).eq.'dateMaxHei') coldatezmax = i
            if (names(i).eq.'place') colplace = i
            if (names(i).eq.'lon') collon = i
            if (names(i).eq.'lat') collat = i
            if (names(i).eq.sortedby) colsort = i
! end repeat
            if (verify) print *,'i=',i,' ',names(i)(1:length(names(i)))
          enddo

        if (colid.lt.1) then        
        print *,'colid=',colid
        call exit(2)
        endif

         nval = 0
         do l = 1,nc
           do k = 1,nname
             if (names(l).eq.nameval(k)) then
             nval = nval+1
             ival(nval) = l
             kval(nval) = k
             endif
           enddo
         enddo

        NULLIFY(first)
        nv = 0

        loop0: do while (.true.) 
        read (itab,'(a)',end=10) header
        call nocr(header)
        call proctab (header,values1,ncmax,nc)
        zmax1 = valget(values1(colzmax)) 

*          if (eqmv(zmax1)) cycle
*          if (zmax1.lt.maxHeightmin.or.zmax1.ge.maxHeightmax) cycle

        values = 'NaN'
          do l = 1,nc
*          call toleft (values1(l))
            if (values1(l).eq.'-1'.or.values1(l).eq.' '.or.values1(l).eq.'-') values1(l) = 'NaN'
          enddo
        values(1:nc) = values1(1:nc)

        if (tarrShore) call sec2hhmm (values(coltarr))
        if (tmaxShore) call sec2hhmm (values(coltzmax))

*        call newdate (dateStartCalc,values(coltarr),values(coldatearr),verify)


          do k = 1,nval
          call newval (values(ival(k)),ndig(kval(k)))
          enddo

        id = valget(values(colid))
      
        allocate(ip)
        nv = nv+1

        ip%valcol = values
        ip%valcol(colfile)=largeLoc

        ip%id = id
        ip%from = largeLoc

        ip%place = values(colplace)
        ip%lon = values(collon)
        ip%lat = values(collat)
        ip%zmax = valget(values(colzmax))
        ip%sortval = values(colsort)
        ip%next => first
        first => ip
        enddo loop0

10     continue
        close (itab)

        if (verify) print *,'done ',largeLoc(1:length(largeLoc)),' nv=',nv
11     continue

c---------------------------------------------------
c get small location files and update large location file according to id
c---------------------------------------------------
* ID      country      place      MaxHei      ArrTime      tMaxHei      ArrivalTS      lon      lat      popest      cityclass      Th1m
*  ID lat   long	zmaxSea		zmaxShore	simInundation	simRunup	simDistRunup	tarrShore	tzmaxShore	simMaxHeight	$namePlace  


      do i = 1,narg
      call getarg (i,ch)
        if (ch.ne.'-s') cycle
      j = i+1
      call getarg (j,ch)
c       ----------------------------------------------------
        do while (ch(1:1).ne.'-'.and.ch.ne.' '.and.j.le.narg) 
        smallLoc = ch
c         ---------------------------------------------------- 
        if (.not.existFile(smallLoc)) goto 300

      tarrShore=.false.
      tmaxShore=.false.

        if (verify) print *,'-----------------------------------------'
        if (verify) print *,'processing ',smallLoc(1:length(smallLoc))

          open (file=smallLoc,unit=itab,action='read')
          read (itab,'(a)') header
        if (verify) print *,'header= ',header(1:length(header))
          call nocr(header)

        colid1=-1
        colzmax1=-1
        colsort1=ncmax
        colplace1=-1

        call proctab (header,names1,ncmax,nc)
          do l = 1,nc
          call toleft (names1(l))
            if (names1(l)(1:4).eq.'* ID') names1(l)='id'
            if (names1(l)(1:2).eq.'ID') names1(l)='id'
            if (names1(l)(1:3).eq.'lat') names1(l)='lat'
            if (names1(l)(1:3).eq.'lon') names1(l)='lon'
            if (names1(l).eq.'$namePlace') names1(l)='place'
            if (names1(l)(1:1).eq.'$') names1(l)=names1(l)(2:)

            if (names1(l).eq.'tarrShore') then
            tarrShore=.true.; names1(l)='ArrTime'
            endif
            if (names1(l).eq.'tzmaxShore') then
            tmaxShore=.true. ; names1(l)='tMaxHei'
            endif

            if (names1(l).eq.'simMaxHeight') names1(l)='MaxHei'
! not repeating
            if (names1(l).eq.'id') colid1 = l
            if (names1(l).eq.'MaxHei') colzmax1 = l
            if (names1(l).eq.sortedby) colsort1 = l
            if (names1(l).eq.'place') colplace1 = l
!
            if (verify) print *,'l=',l,' ',names1(l)(1:length(names1(l)))

          ok = .false.
            do k = 1,ncc
              if (names1(l).eq.names(k)) then
              icol(l) = k
              ok = .true.
              exit
              endif
            enddo

            if (.not.ok) then
            ncc = ncc+1
              if (ncc.gt.ncmax) stop 'ncc > ncmax'
            icol(l) = ncc
            names(ncc) = names1(l)
            endif
          enddo

         if (verify) print *,'icol=',(icol(l),l=1,nc)

         nval = 0
         do l = 1,ncc
           do k = 1,nname
             if (names(l).eq.nameval(k)) then
             nval = nval+1
             ival(nval) = l
             kval(nval) = k
             endif
           enddo
         enddo
         
         if (verify) then
         print *,'colplace1=',colplace1
           do l = 1,nc
           print *,'l=',l,' icol=',icol(l),' names1=',names1(l)(1:12),' names=',names(icol(l))(1:12)
           enddo
           print *
           do l = 1,ncc
           print *,'l=',l,' ',names(l)(1:length(names(l)))
           enddo
         endif

         do l = 1,ncc
! repeat this block for LARGE window
            if (names(l).eq.'id') colid = l
            if (names(l).eq.'MaxHei') colzmax = l
            if (names(l).eq.'tMaxHei') coltzmax = l
            if (names(l).eq.'ArrTime') coltarr = l
            if (names(l).eq.'ArrivalTS') coldatearr = l
            if (names(l).eq.'dateMaxHei') coldatezmax = l
            if (names(l).eq.'place') colplace = l
            if (names(l).eq.'lon') collon = l
            if (names(l).eq.'lat') collat = l
            if (names(l).eq.sortedby) colsort = l
! end repeat
         enddo
         

* ID      country      place      MaxHei      ArrTime      tMaxHei      ArrivalTS      lon      lat

        loop1: do while (.true.) 
        read (itab,'(a)',end=20) header
          if (header.eq.' ') exit loop1
        call nocr(header)
        call proctab (header,values1,ncmax,nc)

          do l = 1,nc
*          call toleft (values1(l))
            if (values1(l).eq.'-1'.or.values1(l).eq.' '.or.values1(l).eq.'-') values1(l) = 'NaN'
          enddo

        if (colzmax1.gt.0) then
        zmax1 = valget(values1(colzmax1)) 
         if (eqmv(zmax1)) cycle
         if (zmax1.lt.maxHeightmin.or.zmax1.ge.maxHeightmax) cycle
        endif


        id = valget(values1(colid1))

        if (colsort1.gt.0) then
        sortval1 = values1(colsort1)
          if (sortval1.eq.'NaN') cycle
        else
        sortval1='NaN'
        endif
c--------------------------------------------------
c add/replace list
        ip => first
        search1: do   ! search in the list
          if (.not.associated(ip)) then  ! new location
          allocate (ip)
          values = 'NaN'
          nv = nv+1
          ip%next => first
          first => ip
            if (verify) print *,'new id=',id,' place=',values1(colplace1)(1:length(values1(colplace1))) 
          exit search1
          endif

*          if (verify) print *,'ip%id=',ip%id,' id=',id 

          if (ip%id.eq.id) then ! old location
          values = ip%valcol
            if (verify) print *,'old id=',id,' place=',values(colplace)(1:length(values(colplace)))  
          exit  search1
          endif
        ip => ip%next
        enddo search1
c-------------------------------------------------

          do l = 1,nc
            if (values1(l).eq.'-1') values1(l) = 'NaN'
            if (values1(l).ne.'NaN') values(icol(l)) = values1(l)
          enddo

        if (tarrShore) call sec2hhmm (values(coltarr))
        if (tmaxShore) call sec2hhmm (values(coltzmax))

          do k = 1,nval
          call newval (values(ival(k)),ndig(kval(k)))
          enddo

        ip%valcol = values
        ip%valcol(colfile)=smallLoc

        ip%id = id
        ip%from = smallLoc

        ip%place = values(colplace)
        ip%lon = values(collon)
        ip%lat = values(collat)          
        ip%zmax = zmax1
        ip%sortval = sortval1
        enddo loop1

20      continue
        close (itab)

        if (verify) print *,'done ',smallLoc(1:length(smallLoc)),' nv=',nv
c      ----------------------------------------------------
300     continue
        j = j+1
        call getarg (j,ch)
        enddo
      exit
      enddo
c------------------------------------------------------------
c define order according to sortedby and calculate maxzmax
c------------------------------------------------------------
c       if (nv.eq.0) goto 400

*----------------------------
c compress
*----------------------------
        if (verify) then
        print *,'--------------------------------------------------------'
        print *,'coltarr=',coltarr,' codatearr=',coldatearr,' coltzmax=',coltzmax,' coldatezmax=',coldatezmax
        endif

        NULLIFY (first0)

        ip => first
        i = 0
          search2: do while (associated(ip)) 
          zmax1 = ip%zmax
          sortval1 = ip%sortval
*          call toleft (sortval1)
          switch = eqmv(zmax1)
          switch = switch .or. (zmax1.lt.maxHeightmin.or.zmax1.ge.maxHeightmax)
          switch = switch .or. sortval1.eq.'NaN'

            if (verify) print *,'switch=',switch,' id=',ip%id,' place=',ip%place(1:length(ip%place))
     &                    ,' sortval1=',sortval1(1:3),' zmax1=',zmax1

            if (.not.switch) then
            allocate(ip0)
            ip0%id = ip%id
            ip0%from = ip%from
            ip0%valcol = ip%valcol
            ip0%place = ip%place  
            ip0%lon = ip%lon
            ip0%lat = ip%lat          
            ip0%zmax = ip%zmax
            ip0%sortval = ip%sortval
            ip0%next => first0
            first0 => ip0
            i = i+1
            endif

            if (.not.associated(ip)) exit search2

          back => ip
          ip => ip%next
          deallocate(back)
          enddo search2

        nv = i
*----------------------------
c additional (or missing) variable 
*----------------------------

        nc = ncc
        ip => first0
          do j = 1,nv
          ncc = nc
c       ---------------------
            ncc = ncc+1
              if (ncc.gt.ncmax) stop 'ncc > ncmax'
            names(ncc)='plot'

          plot = 'NaN'
            do l = 1,nw         
            plot0 = windows(l)(1:lwin(l))//bs//'zsurf.'//ip%place(1:length(ip%place))//'.txt.gif'
            inquire (file=plot0,exist=exist)
              if (exist) then
              plot=plot0
              else
              plot0 = windows(l)(1:lwin(l))//bs//'zsurf.'//ip%place(1:length(ip%place))//'.txt.png'
              inquire (file=plot0,exist=exist)
                if (exist) plot=plot0
              endif
            enddo

            if (plot.ne.'NaN') then              
              if (verify) print *,'exist plot ',plot(1:length(plot))
            ip%valcol(ncc)= plot(1:length(plot))
            ncc = ncc+1
            names(ncc)='tabwidth'
            ip%valcol(ncc)='850'

            else
              if (verify) print *,'not exist plot from file ',ip%from(1:length(ip%from))
     &           ,' place=',ip%place(1:length(ip%place))
            ip%valcol(ncc)= 'NaN'
            ncc = ncc+1
            names(ncc)='tabwidth'
            ip%valcol(ncc)='250'
            endif
c       ---------------------
          if (coltarr.gt.0) then
          ncc = ncc+1
              if (ncc.gt.ncmax) stop 'ncc > ncmax'
          coldatearr = ncc
          names(ncc) = 'ArrivalTS'
              if (verify) print *,'ArrTime=',ip%valcol(coltarr)(1:length(ip%valcol(coltarr)))
          call newdate (dateStartCalc,ip%valcol(coltarr),ip%valcol(coldatearr),.false.)
              if (verify) print *,'ArrivalTS=',ip%valcol(ncc)(1:length(ip%valcol(ncc)))
          endif
c       ---------------------
          if (coltzmax.gt.0) then
          ncc = ncc+1
              if (ncc.gt.ncmax) stop 'ncc > ncmax'
          coldatezmax = ncc
          names(ncc) = 'dateMaxHei'          
              if (verify) print *,'tMaxHei=',ip%valcol(coltzmax)(1:length(ip%valcol(coltzmax)))
          call newdate (dateStartCalc,ip%valcol(coltzmax),ip%valcol(coldatezmax),.false.)
              if (verify) print *,'dateMaxHei=',ip%valcol(ncc)(1:length(ip%valcol(ncc)))
          endif
c       ---------------------
          ncc = ncc+1
              if (ncc.gt.ncmax) stop 'ncc > ncmax'
          names(ncc)='Height'
          ip%valcol(ncc)=ip%valcol(colzmax)
c       ---------------------
         zmax = ip%zmax
          ncc = ncc+1
              if (ncc.gt.ncmax) stop 'ncc > ncmax'
          names(ncc)='color'
        if (zmax.lt.1) then
          ip%valcol(ncc) = "ff00aa00" ! verde     0-1 m
        else if (zmax.lt.2) then      
          ip%valcol(ncc) = "ff00ffff" ! giallo    1-2 m
        else if (zmax.lt.3) then
          ip%valcol(ncc) = "ff00aaff" ! arancio chiaro  2-3 m
        else if (zmax.lt.4) then
          ip%valcol(ncc) = "ff0055ff" ! arancio rosso  3-4 m
        else      
          ip%valcol(ncc) = "ff0000ff" ! rosso     >4m
          endif
              if (verify) print *,'color=',ip%valcol(ncc)(1:length(ip%valcol(ncc)))
c       ---------------------
            ip => ip%next
            enddo
c------------------------------------------------------------

        print *,'merged locations=',nv,'  with height between ', maxHeightmin,maxHeightmax,' no of columns =',ncc
        allocate (order(nv),sortval(nv),iplist(nv))

        maxzmax = 0

        ip0 => first0
        do j = 1,nv
        ip => iplist(j)
        ip%id = ip0%id
        ip%from = ip0%from
        ip%valcol = ip0%valcol
        ip%place = ip0%place
        ip%lon = ip0%lon
        ip%lat = ip0%lat          
        ip%zmax = ip0%zmax
        ip%sortval = ip0%sortval
          if (coltarr.gt.0) call newdate (dateStartCalc,ip%valcol(coltarr),ip%valcol(coldatearr),verify)
          if (coltzmax.gt.0) call newdate (dateStartCalc,ip%valcol(coltzmax),ip%valcol(coldatezmax),verify)

        order(j) = j

        zmax = ip%zmax
          if (sortedby.eq.'ArrTime'.or.sortedby.eq.'tMaxHei') then
          sortval(j) = hhmm2val (ip%sortval,hh,mm)
          else if (sortedby.eq.'none') then   
          sortval(j) = -1    
          else
          sortval(j) = valget(ip%sortval)
          endif

          if (eqmv(sortval(j))) sortval(j) = -1

          if (.not.eqmv(zmax).and.zmax.gt.maxzmax) then
          maxzmax = zmax
          placezmax = ip%place
          latzmax=ip%lat
          lonzmax=ip%lon
          endif
        back => ip0
        ip0 => ip0%next
        deallocate(back)
        enddo 

c--------------------------------------------------
        if (sortedby.ne.'none') then
        sortval = sortval*sortsign
        call sort_list (sortval,order,nv)
        endif

        if (verify) print *,'done sortedby'
c------------------------------------------------------------
c assign maxzmax to the global variable list
c------------------------------------------------------------
         nvar = nvar+1
         var(nvar) = 'maxzmax' ; write (cvar(nvar),'(f6.2)') maxzmax
         print*, maxzmax
         nvar = nvar+1
         var(nvar) = 'placezmax'; cvar(nvar) = placezmax
         nvar = nvar+1
         var(nvar) = 'latzmax'; cvar(nvar) = latzmax
         nvar = nvar+1
         var(nvar) = 'lonzmax'; cvar(nvar) = lonzmax

c:::::::::::::::::::::::::::
c       OUTPUT SECTION
c:::::::::::::::::::::::::::

        if (verify) print *
        if (verify) print *,'start output section'

c------------------------------------------------------------
c put locations.txt
c------------------------------------------------------------
400     continue
        if (verify) print *
        if (verify) print *,'start outLoc'
c with nc+1 also the plot column is written in the output
        nc1=nc+1
        if (outLoc.ne.' ') then
        open (iout,file=outLoc)        
        call name2str (names,nc1,header,lh,'	')      
        write (iout,'(a)') header(1:lh)
          do i = 1,nv
          ip => iplist(order(i))
          call name2str (ip%valcol,nc1,header,lh,'	')
          write (iout,'(a)') header(1:lh)
          enddo
        close (iout)
        if (verify) print *,'done outLoc=',outLoc(1:length(outLoc)),' nv=',nv
        endif

c------------------------------------------------------------
c get templates and put kml or xml files
c------------------------------------------------------------
        do ni=1,no

        if (verify) print *
        if (verify) print *,'start procedure ',inpkml(ni)(1:length(inpkml))
c         ---------------------------------------------------- 
          inquire (file=inpkml(ni), exist=exist)
          if (.not.exist) Then
          print *,'The file ',inpkml(ni)(1:length(inpkml)),' does not exist'
          call exit(2)
          endif        
        open (iun,file=inpkml(ni),action='read')
        open (iout,file=outkml(ni))

c------------------------------------------------------------
c get & put template section before locations/image
c------------------------------------------------------------
500     continue

        do while (.true.)
        read (iun,'(a)',end=2000) header
          if (header(1:1).eq.'!') cycle

          if (header.eq.'<Images>') goto 100
          if (header.eq.'<Locations>') goto 200

        call replaceVar (header,lh,var,cvar,nvar)
        write (iout,'(a)') header(1:lh)
        enddo

c------------------------------------------------------------
c get image section
c------------------------------------------------------------
100     continue
        if (verify) print *
        if (verify) print *,'start image'
        l = 0
        loopImage: do while (.true.)
        read (iun,'(a)') header
        call nocr(header)
          if (header(1:1).eq.'!') cycle
          if (header.eq.'</Images>') exit loopImage
        l = l+1
        procline(l) = header
        enddo loopImage
        np = l

c---------------------------------------------------------------
c get imgFile
c---------------------------------------------------------------
* nameImage	north	south	west	east	nameRasterMap 
 
      if (imgFile.eq.' ') goto 500

      inquire (file=imgFile, exist=exist)
        if (.not.exist) Then
        print *,'The file ',imgFile(1:length(imgFile)),' does not exist'
        call exit(2)
        endif
        open (file=imgFile,unit=itab,action='read')
        read (itab,'(a)') header
        call nocr(header)

      call proctab (header,names2,ncmax,nc2)        
         if (verify) then
           do l = 1,nc2
           print *,'l=',l,' names2=',names2(l)(1:12)
           enddo
         endif

      k1 = indstring (names2,nc2,'nameRasterMap')
      k2 = indstring (names2,nc2,'outRasterMap')

        loop3: do while (.true.) 
        read (itab,'(a)',end=30) header
        call nocr(header)
          if (verify) print *,header(1:length(header))
        call proctab (header,values2,ncmax,nc2)
*--------------------------------------------------------------------
          if (.not.existFile(values2(k1))) cycle loop3
          if (k2.gt.0.and.values2(k2).ne.'none'.and.values2(k2).ne.'NaN') then
          call system ('cp '//values2(k1)//values2(k2) )
          values2(k1) = values2(k2)
          endif
*--------------------------------------------------------------------
c replace image data in the template section and put in the output procedure
*--------------------------------------------------------------------
        nvar1=nvar+nc2
        var(nvar+1:nvar1) = names2(1:nc2)
            do l = 1,np   ! start of location template
            header = procline(l) 
            cvar(nvar+1:nvar1) = values2(1:nc2)
            call replaceVar (header,lh,var,cvar,nvar1)
            write (iout,'(a)') header(1:lh)
            enddo ! end of location template
        enddo loop3

30     continue
       close (itab)
        if (verify) print *,'Done image section'
        goto 500

c------------------------------------------------------------
c get locations section
c------------------------------------------------------------
200     continue
        if (verify) print *,'start location section'
        l = 0
        loop2: do while (.true.)
        read (iun,'(a)') header
        call nocr(header)
          if (header(1:1).eq.'!') cycle
          if (header.eq.'</Locations>') exit loop2
        l = l+1
        procline(l) = header
        enddo loop2
        np = l

c get location data

        if (verify) then
        print *,'variables from input deck'
          do l=1,nvar
          print *,var(l)(1:12),' ',cvar(l)(1:64)
          enddo

        print *,'variables from locations'
          do l=1,ncc
          print *,names(l)(1:12)
          enddo
        endif

*--------------------------------------------------------------------
c replace location data in the template section and put in the output procedure
*--------------------------------------------------------------------
        nvar1=nvar+ncc
        var(nvar+1:nvar1) = names(1:ncc)
          do i = 1,nv    ! loop over locations
          ip => iplist(order(i))
            do l = 1,np   ! start of location template
            header = procline(l) 
            cvar(nvar+1:nvar1) = ip%valcol(1:ncc)
            call replaceVar (header,lh,var,cvar,nvar1)
            write (iout,'(a)') header(1:lh)
            enddo ! end of location template
         enddo ! end of locations loop
  
        close (itab)
          if (verify) print *,'done location section'
        goto 500

c------------------------------------------------------------
2000    continue
        close (iun)
        close (iout)
          if (verify) then
          print *,'done ',inpKml(ni)(1:length(inpKml(ni)))
          print *,'done ',outKml(ni)(1:length(outKml(ni)))
          endif
       enddo
c------------------------------------------------------------
         if (verify) print *,'time spent [min] = ', (time()-it0)/60.
       stop
       end
c---------------------------------------------------------------
        function hhmm2val (string,hh,mm)
        implicit none
        character string*(*), msg*3
        real*4 hhmm2val
        integer length, hh,mm

        if (string.eq.'NaN') goto 100

        if (length(string).eq.5) then
        read (string,'(i2,1x,i2)',err=100) hh,mm
        else if (length(string).eq.6) then
        read (string,'(i3,1x,i2)',err=100) hh,mm
        else
        goto 100
        endif
        
        hhmm2val = hh+mm/60.
        return

100     call getmv(hhmm2val)
        return
        end
c---------------------------------------------------------------
        subroutine sec2hhmm (string)
        implicit none
        character string*(*), string1*6
        integer hh,mm, length
        real*4 valget, tsec
        logical eqmv

          if (string.eq.'NaN') return
          if (string(4:4).eq.':') return
        tsec = valget(string)
          if (eqmv(tsec)) then
          string = 'NaN'
          return
          endif

        hh = tsec/3600
        mm = (tsec-hh*3600)/60
        write (string1,'(i3,":",i2)') hh,mm
        call fillzero(string1,6)
*          if (string1(1:1).eq.'0') string1(1:1)=' '
          if (string1.eq.'***:**') then
          print *,'string=',string(1:length(string))
          print *,'tsec=',tsec,' hh=',hh,' mm=',mm
          string = 'NaN'
          else
          string = string1
          endif
        return
        end
c---------------------------------------------------------------
        subroutine newdate (old,shift,new,verify)
        implicit none
        character*(*) old,shift,new
        character command*254
        real*4 tdummy, hhmm2val
        integer*4 hh,mm, length, lh
        logical verify

          if (verify) print *
          if (verify) print *,'base=',old(1:length(old)),' shift=',shift(1:length(shift)),' old=',new(1:length(new)) 

          if (shift.eq.'NaN'.or.old.eq.'NaN') then
          new = 'NaN'
          return
          endif
        
        tdummy= hhmm2val(shift,hh,mm)
        command = ' '
        write (command,'(a,i3,a,i2,a)') 'date --date=" '//old(1:length(old)+1),hh,' hour ',mm,' min " "+%d %b %Y %H:%M:%S" '
        lh = length(command)
        call sendcomm (command(1:lh),new)
          if (verify) print *,'command=',command(1:length(command))
          if (verify) print *,'new=',new(1:length(new))
          
        return
        end
