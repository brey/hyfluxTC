#include <modules.F>
#undef PUTBARRIER
#define DEB c
*================================================================================
      program HyFlux2
*================================================================================
      use parameter_data
      use statistics_data
      use window_data
      use geometry_data
      use hydro_data
      use map_data
      use parallel_data
      use slope_data
      use boundary_data
      use watch_data
      implicit none
c------------------------------------
      logical(i4)  eqmv, existFile, doTransient, okOut, exist, outCpu
      character message*1000, msg*3, char*1, fmtInt*20, fmtRel*20
      real(r4) numget
      integer(i4) length, typeoutmap, l, nvalmax, i, tag, err, ichar, len
c------------------------------------
      call cputime (cpu0)
      call elapstime (elaps0)
c------------------------------------
	code = 'HyFlux2'
c------------------------------------
      CALL SET_DEFAULTS
      CALL GET_PARAMETERS (.true.)
c------------------------------------
#ifdef PARALLEL
      call mpi_init (mpi_erro)
      call mpi_comm_rank (mpi_comm_world,icore,mpi_erro)
      call mpi_comm_size (mpi_comm_world,ncore,mpi_erro)
#else
      icore = 0
      ncore = 1
#endif
      ncore1 = ncore-1
      MASTER = icore.eq.0
        if (postProc.and.ncore.gt.1) then
        ncore = ncore1
        else
        postProc = .false.
        endif
      POST   = icore.eq.ncore
      SLAVE  = icore.gt.0.and.icore.lt.ncore
      CALC = MASTER.or.SLAVE
        if (postProc) then
        PUTMAP = POST
        CALCWATCH = POST
        else
        PUTMAP = MASTER
        CALCWATCH = .true.
        endif
c------------------------------------
      CALL DERIVED_PARAMETERS
      CALL SHOW_PARAMETERS
      
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      if (MASTER) then  ! start only master
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      if (.not.(existFile(demNodemap).or.existFile(demmap).or.existFile(batmap).or.existFile(batnode))
     &    .and.srtmpath.ne.' '.and.srtmpath.ne.'none') then
      write (iout,*)'=========================================================='
      write (iout,*)' srtm2map: srtm30+ => demNode '
      write (iout,*)'=========================================================='
      call srtm2map
      write (iout,*)' done srtm2map'
      write (iout,*)
      endif
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      write (iout,*)'=========================================================='
      write (iout,*)' getBottom0: demNode => zv, dzdk '
      write (iout,*)'=========================================================='
      call getBottom0
      write (iout,*)' done getBottom0'
      write (iout,*)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        if (onlyDem) goto 100
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      write (iout,*)'=========================================================='
      write (iout,*)' volume and junction pointers'
      write (iout,*)'=========================================================='
      call pointers
      write (iout,*)' done pointers'
      write (iout,*)

      write (iout,*)'=========================================================='
      write (iout,*)'getDepth:   => wetfr, case, depth, zsurf  '
      write (iout,*)'=========================================================='
      call getDepth (.true.)
      write (iout,*)' done getDepth'
      write (iout,*)

      write (iout,*)'=========================================================='
      write (iout,*)'getJunction:   => zj, maxstep  '
      write (iout,*)'=========================================================='
      call getJunction
      write (iout,*)' done getJunction'
      write (iout,*)

      if (.not.existFile(boundaryFile)) then
      write (iout,*)'=========================================================='
      write (iout,*)' createBoundary'
      write (iout,*)'=========================================================='
      call createBoundary
      write (iout,*)' done createBoundary'
      write (iout,*)
      endif
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      write (iout,*)
      write (iout,*) '========================================================'
      write (iout,*) ' Start getOtherMaps'
      write (iout,*) '========================================================'
      CALL getOtherMaps
      write (iout,*) ' Done getOtherMaps'

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      if (.not.existFile(watchfile).and.(existFile(shorefile).or.existFile(placefile))) then
      write (iout,*)
      write (iout,*)'=========================================================='
      write (iout,*)' shorefile and/or watchfile'
      write (iout,*)'=========================================================='
      call shoreLine
      write (iout,*)' done shoreline'
      write (iout,*)
      endif 
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      if (verify) then
      write (iout,*)
      write (iout,*) '========================================================'
      write (iout,*) ' verify maps '
      write (iout,*) '========================================================'
      call verifyMaps
      endif
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c deallocate not necessary arrays
      if (allocated(mask)) deallocate (mask)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c  val, ival1, ival4 are used only for map output (see map2val and outanymap)
      if (.not.allocated(outval)) allocate (outval(nv))
      if (.not.allocated(val))    allocate (val(nval))
      if (.not.allocated(ival1))  allocate (ival1(nval))
      if (.not.allocated(ival4))  allocate (ival4(nval))
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      endif  ! end only MASTER

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
100  	continue
      call cputime (cpu1)
      call elapstime (elaps1)
      outline(1) = ' '
      write (outline(2),*) 'preProcessing'
      write (outline(3),*) 'cpu time =', cpu1-cpu0,' sec'
      write (outline(4),*) 'elaps time =', elaps1-elaps0,' sec'
      noutline=4
      call writeoutline

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      if (onlyDem) call logout(0,'STATUS=onlyDem')
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL INITFILES 
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#ifdef PARALLEL
c------------------------------------------------------
c  assign pointers to each core
      write (iout,*)
      write (iout,*) '========================================================'
      write (iout,*) ' Start assignCore '
      write (iout,*) '========================================================'
        if (CALC) call assignCore
c-------------------------------------------------
      call mpi_barrier (mpi_comm_world, mpi_erro)
c-------------------------------------------------
      write (iout,*) 'icore=',icore,' Done assignCore'
#else
      iv1 = 1
      iv2 = nv
      ij1 = 1
      ij2 = nj
      ivL1 = 1
      ivL2 = nv
      ivM1 = 1
      ivM2 = nv
#endif
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      if (nv.eq.0) call logout(100,'STATUS=notDone because nv=0')
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        sizv = iv2 - iv1 + 1
        sizj = ij2 - ij1 + 1
        sizvL = ivL2 - ivL1 + 1
DEB      print *,'icore=',icore,' sizv=',sizv,' sizj=',sizj,' sizvL=',sizvL,' difSiz=',sizvL-sizv
DEB      print *,'icore=',icore,' ivL1=',ivL1,' iv1=',iv1,' iv2=',iv2,' ivL2=',ivL2
      write (iout,'(12(a,i8,3x))') 'nv1=',nv1(icore),'iv1=',iv1,'nv3=',nv3(icore),'ivL1=',ivL1,'nv2=',nv2(icore),'iv2=',iv2
     &           ,'nv4=',nv4(icore)
     &          ,'ivL2=',ivL2,'difL=',iv1-ivL1,' difR=',ivL2-iv2
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      write (iout,*)
      write (iout,*) '========================================================'
      write (iout,*) ' Start Initialize '
      write (iout,*) '========================================================'
      if (CALC) then
      CALL INITIALIZE
      write (iout,*) ' Done Initialize icore=',icore
      write (iout,*)
      endif

      write (iout,*) ' Done Initialize'
      write (iout,*)
c-------------------------------------------------
      ntags=9
      tags(1:ntags) = (/9,10,12,13,23,30,31,tagBound,tagPost/)
      cpuCOM = 0
      cpuBAR = 0
      cpuCAL = 0
      cpuIO = 0
      cpuTOT = 0
      block = ' '
c-------------------------------------------------
#ifdef PARALLEL
      if (postProc) then
      tag = 92
      call V_integer4_master_to_post (nv1(0), ncore, tag)
      call V_integer4_master_to_post (nv2(0), ncore, tag)
      call V_integer4_master_to_post (nvv(0), ncore, tag)
 
      call S_integer4_master_to_post (nval,  tag)
      call S_integer4_master_to_post (nvx,  tag)
      call S_integer4_master_to_post (nvy,  tag)
      call S_real8_master_to_post (xstart,  tag)
      call S_real8_master_to_post (ystart,  tag)
      call S_real8_master_to_post (cellsize,  tag)
      call S_integer4_master_to_post (nv,  tag)
        if (POST) then
        allocate (tarr(nv),zwmax(nv),tzwmax(nv))
        allocate (val(nval),val1(nval),ival1(nval),ival4(nval))
        maps = outdir(1:lout)//bs//'bathymetry.tif'//null
        call OPENGDAL (maps,nvx,nvy,typemap, xstart, ystart, cellsize,sizey,driver,nband,error)
        driver = driver(1:length(driver))//null
        call CLOSEGDAL ()
        allocate (kl(nv),zwv(nv),vv(2,nv),outval(nv),hv(nv),lk(nval))
        endif
      call V_integer4_master_to_post (kl, nv, tag)
      call V_integer4_master_to_post (lk, nval, tag)
 
      call S_integer4_master_to_post (imap,  tag)
      call S_real8_master_to_post (time,tag)
      call S_real8_master_to_post (timestart,tag)

      call V_real4_master_to_post(tarr,nv,tag) 
      call V_real4_master_to_post(zwmax,nv,tag) 
      call V_real4_master_to_post(tzwmax,nv,tag) 

      endif
#endif
c-------------------------------------------------
        outnow=imap.eq.0
        restnow=.true.
        statnow=.false.
 
        write (ext1,'(f7.3)') float(imap)/1000.
        call fillzero (ext1,7)
 
        write (ext2,'(i8)') int(time+0.5)
        call fillzero (ext2,8)
c-------------------------------------------------
      watch=existFile(watchfile).and.CALCWATCH
DEB      print *, 'postProc=',postProc,' POST=',POST,' CALC=',CALC,' CALCWATCH=',CALCWATCH,' watch=',watch,' PUTMAP=',PUTMAP
        if (watch) then
        write (iout,*)
        write (iout,*) '========================================================'
        write (iout,*) ' Start initWatch'
        write (iout,*) '========================================================'
        call initWatch
        write (iout,*) ' Done initWatch'
        write (iout,*)
        endif
c---------------------------------------
        if (CALC) then
        write (iout,*)
        write (iout,*) '========================================================'
        write (iout,*) ' Start boundary'
        write (iout,*) '========================================================'
c  get boundary data from boundaryDir
           if (nested) call setTimeBound
c set boundary conditions according to boundary file/directory
        call setBound
        write (iout,*) ' Done boundary: nested=',nested
        write (iout,*)
        else
        write (iout,*) ' Not done boundary'
        endif
c-------------------------------------------------
        if (CALC) then
        write (iout,*)
        write (iout,*) '========================================================'
        fmtInt='(5(a,i12))'
        fmtRel='(4(a,f12.2))'
        ncase0 = count(case(iv1:iv2).le.0.and.zv(iv1:iv2).gt.seaLevel)
        ncase1 = count(case(iv1:iv2).eq.1)
        ncase2 = count(case(iv1:iv2).eq.2)
        ncase3 = count(case(iv1:iv2).eq.3)
        ncase4 = count(case(iv1:iv2).le.0.and.zv(iv1:iv2).lt.seaLevel)
 
DEB        print *, 'ncore=',ncore,' postProc=',postProc,' nested=',nested
        write (iout,fmtRel) 'timestart[sec]=',timestart,' timestart[h]=',timestart/3600
        write (iout,fmtRel) 'tmax[sec]=',tmax,' tmax[h]=',tmax/3600
        write (iout,fmtInt) 'nvx=',nvx,' nvy=',nvy,' nval=',nval,' nv=',nv
        write (iout,fmtInt) 'nvb=',nvb,' nplace=',nplace,' narrived=',narrived
        write (iout,fmtInt) 'sizv=',sizv,' sizj=',sizj,' ndeepV=',count(deepV(iv1:iv2)),' ndeepJ=',count(deepJ(ij1:ij2))
        write (iout,fmtInt) 'ncaseLand=',ncase0,' ncase1=',ncase1,' ncase2=',ncase2,' ncase3=',ncase3,' ncaseSea=',ncase4
        write (iout,fmtRel) 'dtmax=',dtmax,' dt=',dt
        write (iout,fmtInt) 'no output steps=',int((tmax-timestart)/dtout+0.5)
        write (iout,fmtInt) 'no of hydro steps per output steps=',int(dtout/dt+0.5)
        write (iout,fmtInt) 'tot no hydrodynamic steps=',int((tmax-timestart)/dt+0.5)
        write (iout,*) '========================================================'
        endif
c-------------------------------------------------
#ifdef PARALLEL
        call mpi_barrier (mpi_comm_world, mpi_erro)
#endif
        write (iout,*) 'write imap =',imap,' at time =',time,'[sec]'
        call makePost (.false.)
c-------------------------------------------------
      write (iout,*)
      write (iout,*) '========================================================'
      write (iout,*) ' Start Transient '
      write (iout,*) '========================================================'
 
      doTransient=.true.
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      transloop: do while (doTransient)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#ifdef PARALLEL
      tag = tagPost
        call cputime(cpust)
#ifdef PUTBARRIER
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpuen)
        cpuBAR(tag) = cpuBAR(tag)+cpuen-cpust
        call cputime(cpust)
#else
        call mpi_barrier (mpi_comm_world, mpi_erro)
#endif
#endif
 
        if (CALC) CALL TRANSIENT (err)
 
      call elapstime (elaps1)
      elaps = (elaps1-elaps0)/60.
 
      call cputime (cpu1)
      cpu = (cpu1-cpu0)/60.
 
c-------------------------------------------------
c  manage cpu time of the individual cores
c-------------------------------------------------
#ifdef PARALLEL
      tag = tagPost
        call cputime(cpust)
 
#ifdef PUTBARRIER
        call mpi_barrier (mpi_comm_world, mpi_erro)
	call cputime (cpuen)
        cpuBAR(tag) = cpuBAR(tag)+cpuen-cpust
        call cputime(cpust)
#endif
 
      if (SLAVE.or.POST) then
      call mpi_send (cpu,1, mpi_real4, 0, tag, mpi_comm_world, mpi_erro)
      endif
 
      if (MASTER) then
      cpuCore(0) = cpu
          do i = 1, ncore1
          call mpi_recv (cpuCore(i),1, mpi_real4, i, tag, mpi_comm_world, mpi_stato, mpi_erro)
          cpu = cpu + cpuCore(i)
          enddo
      write (icpu,'(i13,128(f13.3))') istep, time, (cpuCore(i),i=0,ncore1)
      endif
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 	call cputime (cpuen)
        cpuCOM(tag) = cpuCOM(tag)+cpuen-cpust
        call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#endif
 
c-------------------------------------------------
      if (MASTER) then

      if (err.eq.3) then
      message = ' '
      write (message,*) 'STATUS=FAILED ; : from boundaryDir:  max time of the maps < current time '
      endif 

      if (istep.gt.istepmax) then
      message = ' '
      write (message,*) 'STATUS=FAILED ;  no of calc. steps ',istep, ' > ',istepmax
      err = 2
      endif
 
      if (stepdtmin.gt.maxstepdtmin) then
      message = ' '
      write (message,*) 'STATUS=FAILED ;  no of step at dtmin ',stepdtmin, ' > ',maxstepdtmin
      err = 2
      endif
 
      if (dhmax.gt.maxdhmax.and.ilim.ne.0) then
      message = ' '
      write (message,*) 'STATUS=FAILED ;  at time ',time,' on junction ',ldhmax,' dhmax = ',dhmax,' > maxdhmax = ',maxdhmax
      err = 2
      endif
 
      endif
 
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      if (err.eq.0.and.MASTER) call procLogin (err,message)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
#ifdef PARALLEL
      tag = tagPost
      call cputime(cpust)
 
      call S_integer4_master_to_slave (err,  tag)
      call S_integer4_master_to_slave (imap,  tag)
      call S_integer4_master_to_slave (istep,  tag)
      call S_real8_master_to_slave (time,tag)
        if (postProc) then
        call S_integer4_master_to_post (err,  tag)
        call S_integer4_master_to_post (imap,  tag)
        call S_integer4_master_to_post (istep,  tag)
        call S_real8_master_to_post (time,tag)
        endif
#endif
 
      doTransient=time.lt.tmax.and.istep.lt.istepmax.and.err.eq.0
 
      write (ext1,'(f7.3)') float(imap)/1000.
      call fillzero (ext1,7)
 
      write (ext2,'(i8)') int(time+0.5)
      call fillzero (ext2,8)
 
      call makePost (.not.doTransient)
 
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      enddo transloop
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
c-------------------------------------------------
      write (outline(1),*)
      write (outline(2),*) '========================================================'
      write (outline(3),*) ' End Transient '
      write (outline(4),*) '========================================================'
      write (outline(5),*)
      noutline=5
      call writeoutline
 
c========================================================
      outCpu = .false.
       if (err.eq.0.and.CALC.and.outCpu) then
c write cputime on a file
       tag = tagPost
       call getenv ('HOME',home)
       fileout = home(1:length(home))//bs//'cpuHyFlux.txt'
       okOut = MASTER
#ifdef PARALLEL
         if (.not.MASTER) call mpi_recv (okOut,1, mpi_logical4, icore-1, tag, mpi_comm_world, mpi_stato, mpi_erro)
#endif
         if (okOut) then
         exist = existFile(fileout)
         open (ifree,file=fileout,access='append')
           if (.not.exist) then
           write (ifree,'(3(a,"	"),8(a,"	"))') 'cpuTot','cpuJun','cpuBound'
     &     , 'notdeepV', 'nvb', 'nplace', 'narrived', 'sizj', 'sizv', 'icore', 'ncore'
           endif
         write (ifree,'(3(g14.6,"	"),8(i8,"	"))') sum(cpuCal(1:tagBound))/istep,cpuCal(tagJun)/istep
     &                   ,cpuCal(tagBound)/istep
     &                   , count(.not.deepV(iv1:iv2)), nvb, nplace
     &                   , narrived, sizj, sizv, icore, ncore
         close (ifree)
#ifdef PARALLEL
           if (icore.lt.ncore-1) then
           call mpi_send (okOut,1, mpi_logical4, icore+1, tag, mpi_comm_world, mpi_erro)
           endif
#endif
         endif
       endif
c========================================================
#ifdef PARALLEL
      block(100)='TOTT'
      outline(1) = ' '
      write (outline(2),*) 'core = ',icore
      write (outline(3),*) 'block		     tag	CAL     	BAR     	COM     	IO      	Tot'
      noutline=3
      call writeoutline
        do i = 1,ntags
        l = tags(i)
        cpuTOT(l) =  cpuCOM(l)+cpuCAL(l)+ cpuIO(l)+cpuBAR(l)
        cpuCAL(100) = cpuCAL(100) + cpuCAL(l)
        cpuCOM(100) = cpuCOM(100) + cpuCOM(l)
        cpuBAR(100) = cpuBAR(100) + cpuBAR(l)
        cpuIO(100) = cpuIO(100) + cpuIO(l)
        cpuTOT(100) = cpuTOT(100) + cpuTOT(l)
        write (outline(1),'(a,"	",i3,5("	",f12.3))') block(l),l,cpuCAL(l),cpuBAR(l),cpuCOM(l),cpuIO(l),cpuTOT(l)
        noutline=1
        call writeoutline
        enddo
      l=100
        write (outline(1),'(a,"	",i3,5("	",f12.3))') block(l),l,cpuCAL(l),cpuBAR(l),cpuCOM(l),cpuIO(l),cpuTOT(l)
        noutline=1
        call writeoutline
c-------------------------------------------------
 
      cpuCAL(100)=0; cpuCOM(100)=0; cpuIO(100)=0; cpuTOT=0; cpuBar(100)=0
 
      tag = 94
      call V_real4_slave_to_master (cpuCOM, 100, tag,Fsum)
      call V_real4_slave_to_master (cpuBAR, 100, tag,Fsum)
      call V_real4_slave_to_master (cpuCAL, 100, tag,Fsum)
      call V_real4_slave_to_master (cpuIO, 100, tag,Fsum)
        if (postProc) then
        call V_real4_post_to_master (cpuCOM, 100, tag,Fsum)
        call V_real4_post_to_master (cpuBAR, 100, tag,Fsum)
        call V_real4_post_to_master (cpuCAL, 100, tag,Fsum)
        call V_real4_post_to_master (cpuIO, 100, tag,Fsum)
        endif
#endif
      if (MASTER) then
      block(100)='TOTT'
      outline(1) = ' '
      write (outline(2),*) 'core total'
      write (outline(3),*) 'block		     tag	CAL     	BAR     	COM     	IO      	Tot'
      noutline=3
      call writeoutline
        do i = 1,ntags
        l = tags(i)
        cpuTOT(l) =  cpuCOM(l)+cpuCAL(l)+ cpuIO(l)+cpuBAR(l)
        cpuCAL(100) = cpuCAL(100) + cpuCAL(l)
        cpuCOM(100) = cpuCOM(100) + cpuCOM(l)
        cpuBAR(100) = cpuBAR(100) + cpuBAR(l)
        cpuIO(100) = cpuIO(100) + cpuIO(l)
        cpuTOT(100) = cpuTOT(100) + cpuTOT(l)
        write (outline(1),'(a,"	",i3,5("	",f12.3))') block(l),l,cpuCAL(l),cpuBAR(l),cpuCOM(l),cpuIO(l),cpuTOT(l)
        noutline=1
        call writeoutline
        enddo
      l=100
        write (outline(1),'(a,"	",i3,5("	",f12.3))') block(l),l,cpuCAL(l),cpuBAR(l),cpuCOM(l),cpuIO(l),cpuTOT(l)
        noutline=1
        call writeoutline
 
      endif
 
c========================================================
        write (ext1,'(f7.3)') float(imap)/1000.
        call fillzero (ext1,7)
 
        write (ext2,'(f7.3)') time/1000.
        call fillzero (ext2,7)
 
        outnow=.not.outnow
        restnow=.not.restnow
        statnow=.not.statnow
 
        call makePost (.true.)
c========================================================
        if (err.eq.0) then
	message = ' '

	write (message,'((a,a),(a,i8),(a,g12.5),10(a,g10.5))') 
     &      'STATUS=complete ; outdir=',outdir(1:lout) ,'; nv=',nv ,'; zvmin=',minval(zv) 
     &     ,'; avg.dt(sec)= ',(time-timestart)/istep,'; tend(min)= ',tmax/60
     &     ,'; elaps(min)=',elaps,'; cputime(min)=',cpu,'; maxZwmax=',maxZwMax,'; maxVelmax=',maxVnMax
     &     ,'; cellsize(m)=',cellsizem
        endif
c========================================================
      call logout(err,message)
      end
 
*================================================================================
      subroutine procLogin (err,message)
*================================================================================
      use parameter_data
      use statistics_data
      implicit none
c------------------------------------
      logical(i4) wait
      integer err
      character message*(*), msg*3
      real(r4) numget
c------------------------------------
	wait = .false.
 
	call login (message)
 
!  master start
 
	logloop: do while ((wait.or.message.ne.' ').and.err.eq.0)
      call cputime (cpu1)
	cpu = cpu1-cpu0
 
	if (message(1:6).eq.'STATUS') then
	wait = .false.	
	message = ' '
	write (message,'(4(a,g12.5))') 'STATUS: running time = ',time,' dtout = ',dtout,' tend = ',tmax,' cputime = ',cpu
	call logout(-1,message)
 
	else if (message(1:4).eq.'WAIT') then
	wait = .true.
	message = ' '
	write (message,'(4(a,g12.5))') 'STATUS: waiting time = ',time,' dtout = ',dtout,' tend = ',tmax,' cputime = ',cpu
	call logout(-1,message)
	
	else if (message(1:5).eq.'DTOUT') then
	wait = .true.
	dtout=numget(message(6:),msg)
	message = ' '
	write (message,'(4(a,g12.5))') 'STATUS: waiting time = ',time,' dtout = ',dtout,' tend = ',tmax,' cputime = ',cpu
	call logout(-1,message)
	
	else if (message(1:5).eq.'TEND') then
	tmax=numget(message(6:),msg)
	  if (time.ge.tmax) then
	  wait = .false.
	  message = ' '
	  write (message,'(4(a,g12.5))') 'STATUS=TEND time = ',time,' dtout = ',dtout,' tend = ',tmax,' cputime = ',cpu
	  call logout(0,message)
	  else
	  wait = .true.
	  message = ' '
	  write (message,'(4(a,g12.5))') 'STATUS: waiting time = ',time,' dtout = ',dtout,' tend = ',tmax,' cputime = ',cpu
	  call logout(-1,message)
	  endif
 
	else if (message(1:8).eq.'CONTINUE') then
	wait = .false.
	message = ' '
	write (message,'(4(a,g12.5))') 'STATUS: continuing time = ',time,' dtout = ',dtout,' tend = ',tmax,' cputime = ',cpu
	call logout(-1,message)
 
	else if (message(1:4).eq.'STOP') then
	wait = .false.
	message = ' '
	write (message,'(4(a,g12.5))') 'STATUS=STOP stop time = ',time,' dtout = ',dtout,' tend = ',tmax,' cputime = ',cpu
	call logout(0,message)
 
	else if (message.ne.' ') then
	wait = .false.
	message = 'not recognized message: '//message
	call logout(-1,message)
	endif
 
	call login (message)
 
	enddo logloop
 
 
c-------------------------------------------------
      return
      end
