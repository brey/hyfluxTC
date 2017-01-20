#include <modules.F>
*================================================================================
      program prova
*================================================================================
      use parameter_data
      use statistics_data
      use window_data
      use geometry_data
      use hydro_data
      use map_data
      use parallel_data
      implicit none
c------------------------------------
      integer(i4) nvalmax, i,j,l,k,i0,j0, tag, length
      character ext*3
      logical(i4) large
c------------------------------------
#ifdef PARALLEL
      call mpi_init (mpi_erro)
      call mpi_comm_rank (mpi_comm_world,icore,mpi_erro)
      call mpi_comm_size (mpi_comm_world,ncore,mpi_erro)
#else
      ncore = 1
      icore = 0      
#endif
      MASTER = icore.eq.0
      SLAVE  = icore.ne.0
c------------------------------------
c from inout
      iout = iout0 + icore
      write (ext(1:3),'(i3)') icore
        do l = 1,3
          if (ext(l:l).eq.' ') ext(l:l) = '0'
        enddo
      workdir='outprova'
      lw = length(workdir)
      outdir='outprova'
      lout = length(outdir)
      consolefile='console.txt'

      open (unit=iout,file=outdir(1:lout)//bs//ext(1:3)//consolefile)
      console = .true.
      logoutfile = 'logout.txt'
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        if (MASTER) then  ! start only master
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      dx = 1
      invdx = 1
      halfdx = dx*0.5
      xstart = 0
      ystart = 0
      cellsize = dx

      nvx = 11
      nvy = 21

      nval = nvx*nvy
      nvalmax = nval
      allocate (mask(nvx,nvy),val(nval))

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c from getBottom etc
      l= 0 ; k= 0
      val = mv
      i0 = (nvx-1)/2 + 1
      j0 = (nvy-1)/2 + 1
      do j = 1,nvy
        do i = 1,nvx
        k = k+1
        mask(i,j) = (i-i0)**2+(j-j0)**2.gt.10
          if (mask(i,j)) then
          l = l +1    
          val(k) = j+float(i)/100
          endif
c        print *,'i=',i,' j=',j,' val=',val(k),mask(i,j)
        enddo
      enddo
      nv = l
      print *,'done val: nv=',nv
      
      call createPointVol
      print *,'done volume pointer'
      
      allocate (zv(nv),dzdk(2,nv),invarea(nv),vn(nv))
      zv = val(np)
      vn = zv
      dzdk = 0
      invarea = invdx

      print *,'done zv'
       
      call getJunction
      print *,'done junction'

      val = mv
      val(np) = zv
      maps = workdir(1:lw)//bs//'zv'//'.txt'
      call NEWMAPTYPE2 
       
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c deallocate not necessary arrays
      if (allocated(mask)) deallocate (mask)
      if (allocated(ip)) deallocate (ip)
      if (allocated(jpx)) deallocate (jpx)
      if (allocated(jpy)) deallocate (jpy)
      if (allocated(ipx)) deallocate (ipx)
      if (allocated(ipy)) deallocate (ipy)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      endif  ! end only MASTER
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#ifdef PARALLEL
c------------------------------------------------------
c  assign pointers to each core
      call assignCore
#else
      iv1 = 1
      iv2 = nv
      ij1 = 1
      ij2 = nj
      ivL1 = 1
      ivL2 = nv
#endif
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        sizv = iv2 - iv1 + 1
        sizj = ij2 - ij1 + 1
        sizvL = ivL2 - ivL1 + 1
      print *,'icore=',icore,' sizv=',sizv,' sizj=',sizj,' sizvL=',sizvL,' sizb=',sizb
c      print *,'icore=',icore,' ivL1=',ivL1,' iv1=',iv1,' iv2=',iv2,' ivL2=',ivL2
c------------------------------
#ifdef PARALLEL
c-------------------------------------------------

c from initialize

c defined in getBottom
      if (SLAVE) then
      allocate (zv(ivL1:ivL2))
      allocate (vn(ivL1:ivL2))
      allocate (invarea(ivL1:ivL2))
      allocate (dzdk(2,ivL1:ivL2))
      allocate (val(nval))
      endif

      call mpi_barrier (mpi_comm_world, mpi_erro)

      tag = 1

      call C_master_to_slave (invarea,8,tag)
      call C_master_to_slave (zv,8,tag)
      call C_master_to_slave (vn,4,tag)
      call C_master_to_slave (dzdk,16,tag)

      write (iout,*) ' Initialize/createPointVol: icore=',icore

c defined in createPointVol
      if (SLAVE) allocate (np(ivL1:ivL2))

      call P_integer4_master_to_slave (np,1,tag)

c-------------------------------------------------

      if (MASTER) then
      val = mv
      val(np) = zv(1:nv)
      maps = workdir(1:lw)//bs//'zvBefore'//'.txt'
      call NEWMAPTYPE2 
      endif           

      val = mv
      val(np(ivL1:ivL2)) = zv(ivL1:ivL2)
      maps = workdir(1:lw)//bs//'zv'//ext//'.txt'
      call NEWMAPTYPE2 

/*
      val = mv
      val(np(ivL1:ivL2)) = vn(ivL1:ivL2)
      maps = workdir(1:lw)//bs//'vn'//ext//'.txt'
      call NEWMAPTYPE2 

      tag = 2          
      call real4_getLR (vn,1,tag)

      val = mv
      val(np(ivL1:ivL2)) = vn(ivL1:ivL2)
      maps = workdir(1:lw)//bs//'vnA'//ext//'.txt'
      call NEWMAPTYPE2 
*/


      tag = 2          
      call real4_getLR (vn,1,tag)
      call real8_getLR (zv,1,tag)
      call real8_getLR (invarea,1,tag)
      call real8_getLR (dzdk,2,tag)

      call mpi_barrier (mpi_comm_world, mpi_erro)

      val = mv
      val(np(ivL1:ivL2)) = zv(ivL1:ivL2)
      maps = workdir(1:lw)//bs//'zvA'//ext//'.txt'
      call NEWMAPTYPE2 

      if (LEFT) then
      zv(ivL1:iv1) = -1
      endif

      if (RIGHT) then
      zv(iv2:ivL2) = +1
      endif

      call real8_putR (zv,1,tag,Fsum)
      

      val = mv
      val(np(ivL1:ivL2)) = zv(ivL1:ivL2)
      maps = workdir(1:lw)//bs//'zvB'//ext//'.txt'
      call NEWMAPTYPE2 



      call mpi_barrier (mpi_comm_world, mpi_erro)

      tag = 3          

      call C_slave_to_master  (zv, 8, tag)


      if (MASTER) then
      print *,'nzero=',count(zv.eq.0.d0)
      val = mv
      val(np) = zv(1:nv)
      maps = workdir(1:lw)//bs//'zvAfter'//'.txt'
      call NEWMAPTYPE2 
      endif           
#endif
      call logout(2,'Stop after P_real8_slave_to_master')
 
      stop
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
	  if (logoutfile.eq.' ') logoutfile = 'logout.txt'
	inquire (file=logoutfile,exist=exist)
	  if (exist) then
          open (ierr,file=logoutfile)
          close (ierr,status='delete')
	  endif
          
        open (ierr,file=logoutfile)
        write (ierr,*) message(1:length(message))
        close (ierr)

	  write (iout,*) 'logout '//message(1:length(message))
         endif

          if (err.gt.0) then
#ifdef PARALLEL
          call mpi_finalize (local_err)
          stop
#else
          call exit (err)
#endif
          endif

	  return
        end
c========================================================================
	function length (arg)
c========================================================================
	implicit none
	character arg*(*)
	integer*4 len, length, l, i
 
	l=len(arg)
 
	do i = l,1,-1
	  if (arg(i:i).ne.' ') go to 100
	enddo
100	continue
	length = i
	return
        end
	Subroutine checkmv (val,mv,mask,nval)
	implicit none
	integer*4 nval, mv, i
	integer*4 val(nval)
c caution: mask is logical*1 !!!
	logical*1 mask(nval)
 
 
	do i = 1,nval
	mask(i) = val(i).eq.mv
	enddo
 
	return
	end
c
	function  eqmv (val)
	implicit none
	integer*4 val, mv /z'ffffffff'/

	logical*4 eqmv

        eqmv = val.eq.mv
 
	return
	end

      subroutine NEWMAPTYPE2 
      use constants
      use map_data
      use geometry_data
      implicit none
/*
      character maps*(*)
      integer*4 nvx,nvy,nval,maptype
      real*8 xstart,ystart,cellsize
      real*4 val(nval)
*/
      integer*4 i,j,l, length
      real*4 riga(100)
      logical eqmv
      print *,'maps=',maps(1:length(maps)),' nvx=',nvx,' nvy=',nvy

      open (imon,file=maps(1:length(maps)))

      write (imon,'(6x,100i6)') (i,i=1,nvx)
      l = 0
      do j = 1,nvy
        do i = 1,nvx
        l = l + 1
        riga(i) = val(l)
        enddo
      write (imon,'(i6,100f6.2)') j,(riga(i),i=1,nvx)
      enddo        
      
      close (imon)

      return
      end

      subroutine fillzero (str,len)
	  implicit none
	  character str*(*)
	  integer*4 len, i
 
	  do i = 1,len
	    if (str(i:i).eq.' ') str(i:i)='0'
	  enddo
	  return
	  end

