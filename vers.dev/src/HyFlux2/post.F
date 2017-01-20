#undef PUTBARRIER
#define DEB c  
#define DEC c  
c============================================================================
      subroutine makePost(lastOut)
c============================================================================
      use parameter_data
      use statistics_data
      use geometry_data
      use parallel_data
      use hydro_data
      use slope_data
      implicit none
 
      character name*20, outlist*20, maplist(3)*20, commands(6)*120, command*120
      integer(i4) i,l, typeoutmap, length, lnam, tag, lcomm, iun
      real (r4) noDataZsurf, cpupost1,cpupost0
      logical lastOut, eqmv
c-------------------------------------------------------------
      noDataZsurf = 0.
      maplist(1:3) = (/'TIF_H_00000000.tif','TIF_U_00000000.tif','TIF_V_00000000.tif'/)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      tag = tagPost
      cpumap=0
      cpupost=0
      cpubarpost=0
      call cputime(cpust)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ifdef PARALLEL
      if (postProc) then
      call S_logical4_master_to_post (outnow,tag)
      call S_logical4_master_to_post (restnow,tag)
      call S_logical4_master_to_post (statnow,tag)
      endif
#endif
c-------------------------------------------------------------
      if (restnow) then
c-------------------------------------------------------------
        do i = 1,nrestmaps
        name = restmaps(i)
        if (.not.existHolland.and.name.eq.'spu') cycle
        if (.not.existHolland.and.name.eq.'u10') cycle
        if (.not.existHolland.and.name.eq.'depr') cycle
        if (.not.existHolland.and.name.eq.'u10max') cycle
        if (.not.existHolland.and.name.eq.'deprmax') cycle
        if (.not.existHolland.and.name.eq.'spumax') cycle
        call map2val (name,typeoutmap,outlist,mv)
          if (PUTMAP) then
          call fillzero (name,5)
          lnam = length(name)
          maps = workdir(1:lw)//bs//name(1:lnam)//ext1//mapext
          call outanymap (typeoutmap, val,nval,mapfmt)
DEB     print *, 'icore=',icore,' done maps',maps(1:length(1:maps))
          endif   
        enddo
DEB      print *, 'icore=',icore,' done restnow'
      endif
c-------------------------------------------------------------
      if (outnow) then
c-------------------------------------------------------------
        do i = 1,noutmaps
        name = outmaps(i)
        if (.not.existHolland.and.name.eq.'spu') cycle
        if (.not.existHolland.and.name.eq.'u10') cycle
        if (.not.existHolland.and.name.eq.'depr') cycle
        if (.not.existHolland.and.name.eq.'u10max') cycle
        if (.not.existHolland.and.name.eq.'deprmax') cycle
        if (.not.existHolland.and.name.eq.'spumax') cycle
        call map2val (name,typeoutmap,outlist,noDataZsurf)
          call fillzero (outlist,6)
          lnam = length(outlist)
          maps = outdir(1:lout)//bs//outlist(1:lnam)//ext2//outext
        maplist(i) = outlist(1:lnam)//ext2//outext(1:4)

          if (PUTMAP) then
          call outanymap (typeoutmap, val,nval,outfmt)
DEB     print *, 'icore=',icore,' done maps',maps(1:length(1:maps))
          endif   
        enddo

*        do i = noutmaps+1,3
*        maplist(i)(7:14) = maplist(1)(7:14)
*        enddo

      iband=0
        if (MASTER) then
        write (imon1,'(g14.7,3(1x,a20),i3)') time,maplist,iband
        call opnfree(iun,outdir(1:lout)//bs//'statusCalcTime.txt')
        write (iun,'(a,f15.3)') 'time=',time
        close (iun)
        endif
DEB      print *,'icore=',icore,' done outnow'
      endif
c-------------------------------------------------------------
      if (statnow) then
c-------------------------------------------------------------
        do i = 1,nstatmaps
        name = statmaps(i)
          if (.not.existHolland.and.name.eq.'spu') cycle
          if (.not.existHolland.and.name.eq.'u10') cycle
          if (.not.existHolland.and.name.eq.'depr') cycle
          if (.not.existHolland.and.name.eq.'u10max') cycle
          if (.not.existHolland.and.name.eq.'deprmax') cycle
          if (.not.existHolland.and.name.eq.'spumax') cycle
        call map2val (name,typeoutmap,outlist,mv)

          if (PUTMAP) then
          lnam=length(name)
            if (lastOut) then
            maps = workdir(1:lw)//bs//name(1:lnam)//mapext
            else
            maps = workdir(1:lw)//bs//name(1:lnam)//ext2//mapext
            endif
          call outanymap (typeoutmap, val,nval,mapfmt)
DEB     print *, 'icore=',icore,' done maps',maps(1:length(1:maps))
          endif   
        enddo
c-------------------------------------------------------------
        call map2val ('zwmax',typeoutmap,outlist,mv)
          if (PUTMAP) then
            if (lastOut) then
            maps = outdir(1:lout)//bs//'TIF_MAXHEIGHT_END'//outext
            else
            maps = outdir(1:lout)//bs//'TIF_MAXHEIGHT_END'//ext2//outext
            endif
          call outanymap (typeoutmap, val,nval,outfmt)
          endif

        if (outTTT) then
        call map2val ('tarr',typeoutmap,outlist,mv)
          if (PUTMAP) then
            do l = 1,nval
              if (.not.eqmv(val(l))) val(l) = val(l)/3600
            enddo
            if (lastOut) then
            maps = outdir(1:lout)//bs//'TIF_Arrival_Time'//outext
            else
            maps = outdir(1:lout)//bs//'TIF_Arrival_Time'//ext2//outext
            endif
          call outanymap (typeoutmap, val,nval,outfmt)

          commands = (/
     &      'gdal_contour -a Tarrival -nln Tarrival -i 0.08333 $outdir./TIF_Arrival_Time.tif $outdir./ArrTime_5min  '
     &     ,'ogr2ogr -f KML $outdir./Tarrival_5min.kml $outdir./ArrTime_5min/Tarrival.shp                           '
     &     ,'gdal_contour -a Tarrival -nln Tarrival -i 0.250000 $outdir./TIF_Arrival_Time.tif $outdir./ArrTime_15min'
     &     ,'ogr2ogr -f KML $outdir./Tarrival_15min.kml $outdir./ArrTime_15min/Tarrival.shp                         '
     &     ,'gdal_contour -a Tarrival -nln Tarrival -i 1.000000 $outdir./TIF_Arrival_Time.tif $outdir./ArrTime_60min'
     &     ,'ogr2ogr -f KML $outdir./Tarrival_60min.kml $outdir./ArrTime_60min/Tarrival.shp                         '
     &     /)
            if (lastOut) then
              do i = 1,6
              command=commands(i)
              call replaceVar(command,lcomm,'outdir',outdir(1:lout),1)
              call system (command(1:lcomm))
              enddo
            endif ! lastout  
          endif   ! putmap

        endif ! outTTT   

DEB      print *,'icore=',icore,' done statnow'
      endif

c-------------------------------------------------------------
      if (watch.and.outnow) then
      call putWatch
DEB      print *,'icore=',icore,' done putwatch'
      endif
c-------------------------------------------------------------

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        block(tag) = 'POST'
	call cputime (cpuen)
        cpuCAL(tag) = cpuCAL(tag)+cpuen-cpust-cpumap-cpupost-cpubarpost
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       cpuCOM(tag) = cpuCOM(tag)+cpupost
       cpuBAR(tag) = cpuBAR(tag)+cpubarpost
       cpuIO(tag) = cpuIO(tag)+cpumap
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


      return
      end

*================================================================================
      subroutine map2val (outmap,typeoutmap,outlist,noDataZsurf)
*================================================================================
      use parameter_data
      use geometry_data
      use hydro_data
      use slope_data
      use parallel_data
      use statistics_data
      implicit none
c------------------------------------
      character outmap*(*), outlist*(*), message*1000
      integer(i4) l, typeoutmap, tag
      real(r4) footShore, cpupost0, cpupost1, noDataZsurf
      logical eqmv
c------------------------------------
DEB      print *,'icore=',icore,' outmap=',outmap

      tag = tagPost

      if (MASTER) val = mv
      outval = mv
   
      typeoutmap = 4
      outlist = outmap

      if (CALC) then
      
      if (outmap.eq.'dem') then
      outval(iv1:iv2) = zv(iv1:iv2) 
 
      else if (outmap.eq.'depth') then
      outval(iv1:iv2) = hv(iv1:iv2) 
 
      else if (outmap.eq.'hx') then
      outval(iv1:iv2) = hk(1,iv1:iv2) 
 
      else if (outmap.eq.'hy') then
      outval(iv1:iv2) = hk(2,iv1:iv2) 
 
      else if (outmap.eq.'sfx') then
      outval(iv1:iv2) = sf(1,iv1:iv2) 
 
      else if (outmap.eq.'sfy') then
      outval(iv1:iv2) = sf(2,iv1:iv2) 
 
      else if (outmap.eq.'dzwdx') then
      outval(iv1:iv2) = dzwdk(1,iv1:iv2) 
 
      else if (outmap.eq.'dzwdy') then
      outval(iv1:iv2) = dzwdk(2,iv1:iv2) 
  
      else if (outmap.eq.'dzdxm') then
      outval(iv1:iv2) = dzdkm(1,iv1:iv2) 
 
      else if (outmap.eq.'dzdym') then
      outval(iv1:iv2) = dzdkm(2,iv1:iv2) 

      else if (outmap.eq.'dvxdx') then
      outval(iv1:iv2) = dvdk(1,1,iv1:iv2) 
 
      else if (outmap.eq.'dvxdy') then
      outval(iv1:iv2) = dvdk(2,1,iv1:iv2) 
 
      else if (outmap.eq.'dvydx') then
      outval(iv1:iv2) = dvdk(1,2,iv1:iv2) 
 
      else if (outmap.eq.'dvydy') then
      outval(iv1:iv2) = dvdk(2,2,iv1:iv2) 
 
      else if (outmap.eq.'vx'.or.outmap.eq.'vx000') then
      outval(iv1:iv2) = vv(1,iv1:iv2)
        do l = iv1,iv2
          if (abs(outval(l)).lt.voutmin) outval(l) = 0.
        enddo 
      outlist = 'TIF_U_'
 
      else if (outmap.eq.'vy'.or.outmap.eq.'vy000') then
      outval(iv1:iv2) = vv(2,iv1:iv2) 
        do l = iv1,iv2
          if (abs(outval(l)).lt.voutmin) outval(l) = 0.
        enddo 
      outlist = 'TIF_V_'
 
      else if (outmap.eq.'v') then
      outval(iv1:iv2) = vn(iv1:iv2)  
        do l = iv1,iv2
          if (abs(outval(l)).lt.voutmin) outval(l) = 0.
        enddo 
 
      else if (outmap.eq.'wetfr') then
      outval(iv1:iv2) = wetfrac(iv1:iv2)  
 
      else if (outmap.eq.'shore') then
   	  do l = iv1,iv2
	    if (wetfrac(l).gt.0..and.wetfrac(l).lt.1.) then
            outval(l) = 5
	    endif
	  enddo
      typeoutmap = 6
 
      else if (outmap.eq.'zsurf') then
        do l = iv1,iv2
            if ((abs(wetfrac(l)-wetfracIn(l)).gt.wout.or.wetfrac(l).ge.one)
     &         .and.maxval(hk(1:2,l)).gt.hout   !  pozzanghere, case=1 & no water flux in the junctions
     &         ) then
            outval(l) = zwv(l)
            endif
          if (eqmv(outval(l))) then
          outval(l) = noDataZsurf
          else if (abs(outval(l)).lt.houtmin) then
          outval(l) = 0.
          endif
        enddo
      outlist = 'TIF_H_'
 
      else if (outmap.eq.'flood') then
        do l = iv1,iv2
            if (abs(wetfrac(l)-wetfracIn(l)).gt.wout
     &         .and.maxval(hk(1:2,l)).gt.hout   !  pozzanghere, case=1 & no water flux in the junctions
     &         ) then
            outval(l) = wetfrac(l)-wetfracIn(l)
            endif
        enddo
 
      else if (outmap.eq.'frno') then
        do l = iv1,iv2
            if (maxval(hk(1:2,l)).gt.hout) then
            outval(l) = vn(l)/sqrt(hv(l)*g)
            endif
        enddo

      else if (outmap.eq.'update') then
        do l = iv1,iv2
          if (upvol(l)) outval(l) = 1
        enddo
      typeoutmap=1

      else if (outmap.eq.'sfx') then
      outval(iv1:iv2) = sf(1,iv1:iv2)

      else if (outmap.eq.'sfy') then
      outval(iv1:iv2) = sf(2,iv1:iv2)

      else if (outmap.eq.'spu') then
      outval(iv1:iv2) = spu(iv1:iv2)

      else if (outmap.eq.'u10') then
      outval(iv1:iv2) = u10(iv1:iv2)

      else if (outmap.eq.'depr') then
      outval(iv1:iv2) = depr(iv1:iv2)

      else if (outmap.eq.'limvals') then
      outval(iv1:iv2) = limvals(iv1:iv2)  

      else if (outmap.eq.'smalls') then
      outval(iv1:iv2) = smalls(iv1:iv2)  

      else if (outmap.eq.'wetfrac') then
      outval(iv1:iv2) = wetfrac(iv1:iv2)  

      else if (outmap.eq.'case') then
        do l = iv1,iv2
          if (case(l).ge.0) outval(l) = case(l)
        enddo
	typeoutmap=2

      else if (outmap.eq.'courn') then
      outval(iv1:iv2) = courn(iv1:iv2)  

      else if (outmap.eq.'dhjv') then
      outval(iv1:iv2) = dhjv(iv1:iv2)  

      else if (outmap.eq.'tarr') then
      outval(iv1:iv2) = tarr(iv1:iv2)  
 
      else if (outmap.eq.'zsurfmax') then
      outval(iv1:iv2) = zsurfmax(iv1:iv2)  

      else if (outmap.eq.'tzsurfmax') then
      outval(iv1:iv2) = tzsurfmax(iv1:iv2)  

      else if (outmap.eq.'hzsurfmax') then
      outval(iv1:iv2) = hzsurfmax(iv1:iv2)  

      else if (outmap.eq.'ztotmax') then
      outval(iv1:iv2) = ztotmax(iv1:iv2)  

      else if (outmap.eq.'fhdmax') then
      outval(iv1:iv2) = fhdmax(iv1:iv2)  

      else if (outmap.eq.'zwmax') then
      outval(iv1:iv2) = zwmax(iv1:iv2)  

      else if (outmap.eq.'tzwmax') then
      outval(iv1:iv2) = tzwmax(iv1:iv2)  

      else if (outmap.eq.'hzwmax') then
      outval(iv1:iv2) = hzwmax(iv1:iv2)  

      else if (outmap.eq.'wzwmax') then
      outval(iv1:iv2) = wzwmax(iv1:iv2)  

      else if (existHolland.and.outmap.eq.'u10max') then
      outval(iv1:iv2) = u10max(iv1:iv2)  

      else if (existHolland.and.outmap.eq.'deprmax') then
      outval(iv1:iv2) = deprmax(iv1:iv2)  

      else if (existHolland.and.outmap.eq.'spumax') then
      outval(iv1:iv2) = spumax(iv1:iv2)  

      else if (outmap.eq.'vnmax') then
      outval(iv1:iv2) = vnmax(iv1:iv2)  

      else if (outmap.eq.'tvnmax') then
      outval(iv1:iv2) = tvnmax(iv1:iv2)  

      else if (outmap.eq.'zwmin') then
      outval(iv1:iv2) = zwmin(iv1:iv2)  

      else if (outmap.eq.'tzwmin') then
      outval(iv1:iv2) = tzwmin(iv1:iv2)  

      else
      write (message,*) 'outmap ',outmap,' not exists'
      call logout(2,message)
      endif

      endif
DEB      print *,'icore=',icore,' done outval'

#ifdef PARALLEL

#ifdef PUTBARRIER
        call cputime(cpupost0)
        call mpi_barrier (mpi_comm_world, mpi_erro)
        call cputime(cpupost1)
        cpubarpost = cpubarpost+cpupost1-cpupost0
#endif

        call cputime(cpupost0)

        if (postProc) then 
	call real4_calc_to_post(outval,1,tag) 
DEB        print *,'icore=',icore,' done calc_to_post'
	   if(POST) then
           val(kl) = outval 
             if (outmap.eq.'zsurf') then
             zwv = outval
             outlist = 'TIF_H_'
             endif

             if (outmap.eq.'vx000') then
             vv(1,1:nv) = outval
             outlist = 'TIF_U_'
             endif

             if (outmap.eq.'vy000') then
             vv(2,1:nv) = outval
             outlist = 'TIF_V_'
             endif
           endif

        else
	call real4_slave_to_master(outval,1,tag) 
	   if(MASTER) val(kl) = outval 
DEB        print *,'icore=',icore,' done slave_to_master'
	endif

        call cputime(cpupost1)
        cpupost = cpupost+cpupost1-cpupost0

#else
      val(kl)=outval
#endif
DEB      print *,'icore=',icore,' done val'

      return
      end
c==========================================================================================
	subroutine outanymap (typeoutmap,val4, nval4,anyfmt)
        use parameter_data
        use statistics_data
        use map_data
        implicit none
        character*(*) anyfmt
        integer(i4) typeoutmap, l, nval4, length
        real(r4) val4(nval4), cpumap0, cpumap1
        logical eqmv
c==========================================================================================
        call cputime(cpumap0)
c==========================================================================================
        if (typeoutmap.eq.4) then
          call OUTGDAL (maps, nvx,nvy,typeoutmap, xstart, ystart, cellsize,val4,nval4
     &           , sizey, anyfmt, nband,error)

	else if (typeoutmap.eq.2) then
	ival4(1:nval4) = val4
          do l = 1,nval4
            if (eqmv(val4(l))) ival4(l) = mv4
          enddo
          call OUTGDAL (maps, nvx,nvy,typeoutmap, xstart, ystart, cellsize,ival4,nval4
     &           , sizey, anyfmt, nband,error)

	else if (typeoutmap.eq.1.or.typeoutmap.eq.6) then
	ival1(1:nval4) = val4
          do l = 1,nval4
            if (eqmv(val4(l))) ival1(l) = mv1
          enddo
          call OUTGDAL (maps, nvx,nvy,typeoutmap, xstart, ystart, cellsize,ival1,nval4
     &           , sizey, anyfmt, nband,error)
        else
        write (iout,*) 'for maps=',maps(1:length(maps)),' type=',typeoutmap,' not exist'
	endif
c==========================================================================================
        call cputime(cpumap1)
        cpumap = cpumap + cpumap1-cpumap0
c==========================================================================================
       return
       end

c============================================================================
      subroutine makeStat
c============================================================================
      use parameter_data
      use statistics_data
      use geometry_data
      use parallel_data
      use hydro_data
      use slope_data
      implicit none

      real(r4) footShore, ztot, fhd
      integer(i4) l
      logical eqmv
c--------------------------------------------------------------
c  statistics and output
c--------------------------------------------------------------
        do l = iv1, iv2
c--------------------------------------------------------------
          if (vtype(l).ne.1) cycle
c--------------------------------------------------------------
          if (eqmv(tarr(l))
     &     .and.(abs(wetfrac(l)-wetfracIn(l)).gt.wout.or.wetfrac(l).ge.one)
     &     .and.abs(zwv(l)-zwvIn(l)).gt.harr
     &     .and.maxval(hk(1:2,l)).gt.hout   !  pozzanghera, when case=1 & no water flux in the junctions
     &        ) tarr(l) = time 
c--------------------------------------------------------------
           if ((abs(wetfrac(l)-wetfracIn(l)).gt.wout.or.wetfrac(l).ge.one)
     &     .and.maxval(hk(1:2,l)).gt.hout   !  pozzanghera, when case=1 & no water flux in the junctions
     &        ) then

          if (eqmv(zwmin(l))) then
          zwmin(l) = zwv(l)
          tzwmin(l) = time
          else if (zwv(l).lt.zwmin(l)) then
          zwmin(l) = zwv(l)
          tzwmin(l) = time
          endif
 
          if (eqmv(zwmax(l))) then
          zwmax(l) = zwv(l)
          hzwmax(l) = hv(l)
          wzwmax(l) = wetfrac(l)
          tzwmax(l) = time
          else if (zwv(l).gt.zwmax(l)) then
          zwmax(l) = zwv(l)
          hzwmax(l) = hv(l)
          wzwmax(l) = wetfrac(l)
          tzwmax(l) = time
          endif
 
          if (eqmv(vnmax(l))) then
          vnmax(l) = vn(l)
          tvnmax(l) = time
          else if (vn(l).gt.vnmax(l)) then
          vnmax(l) = vn(l)
          tvnmax(l) = time
          endif

        endif
c--------------------------------------------------------------
        if (existHolland) then
          if (eqmv(u10max(l))) then
          u10max(l) = u10(l)
          else if (u10(l).gt.u10max(l)) then
          u10max(l) = u10(l)
          endif

          if (eqmv(deprmax(l))) then
          deprmax(l) = depr(l)
          else if (depr(l).gt.deprmax(l)) then
          deprmax(l) = depr(l)
          endif

          if (eqmv(spumax(l))) then
          spumax(l) = spu(l)
          else if (spu(l).gt.spumax(l)) then
          spumax(l) = spu(l)
          endif
        endif

        enddo
DEB      print *,'icore=',icore,' done makestat'

        return
        end
