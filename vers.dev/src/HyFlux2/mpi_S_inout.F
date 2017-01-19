#define DEB c

*exec master_to_slave real4 real(r4) mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_real4_master_to_slave (val, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        implicit none
        integer(i4) siz/1/,tag, i
        real(r4) val
 
 
        if (MASTER) then
          do i = 1, ncore-1
*	  print *,'send: i=',i,' siz=',siz,' tag=',tag
          call mpi_send (val,siz, mpi_real4, i, tag
     &               , mpi_comm_world, mpi_erro)
          enddo
        endif
 
        if (SLAVE) then
DEB	print *,'real4_recv: icore=',icore,' siz=',siz
        call mpi_recv (val,siz, mpi_real4, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
*exec master_to_slave real8 real(r8) mpi_real8
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_real8_master_to_slave (val, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        implicit none
        integer(i4) siz/1/,tag, i
        real(r8) val
 
 
        if (MASTER) then
          do i = 1, ncore-1
*	  print *,'send: i=',i,' siz=',siz,' tag=',tag
          call mpi_send (val,siz, mpi_real8, i, tag
     &               , mpi_comm_world, mpi_erro)
          enddo
        endif
 
        if (SLAVE) then
DEB	print *,'real8_recv: icore=',icore,' siz=',siz
        call mpi_recv (val,siz, mpi_real8, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
*exec master_to_slave integer4 integer(i4) mpi_integer4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_integer4_master_to_slave (val, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        implicit none
        integer(i4) siz/1/,tag, i
        integer(i4) val
 
 
        if (MASTER) then
          do i = 1, ncore-1
*	  print *,'send: i=',i,' siz=',siz,' tag=',tag
          call mpi_send (val,siz, mpi_integer4, i, tag
     &               , mpi_comm_world, mpi_erro)
          enddo
        endif
 
        if (SLAVE) then
DEB	print *,'integer4_recv: icore=',icore,' siz=',siz
        call mpi_recv (val,siz, mpi_integer4, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
*exec master_to_slave integer1 integer*1 mpi_integer1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_integer1_master_to_slave (val, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        implicit none
        integer(i4) siz/1/,tag, i
        integer*1 val
 
 
        if (MASTER) then
          do i = 1, ncore-1
*	  print *,'send: i=',i,' siz=',siz,' tag=',tag
          call mpi_send (val,siz, mpi_integer1, i, tag
     &               , mpi_comm_world, mpi_erro)
          enddo
        endif
 
        if (SLAVE) then
DEB	print *,'integer1_recv: icore=',icore,' siz=',siz
        call mpi_recv (val,siz, mpi_integer1, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
*exec master_to_slave logical1 logical*1 mpi_logical1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_logical1_master_to_slave (val, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        implicit none
        integer(i4) siz/1/,tag, i
        logical*1 val
 
 
        if (MASTER) then
          do i = 1, ncore-1
*	  print *,'send: i=',i,' siz=',siz,' tag=',tag
          call mpi_send (val,siz, mpi_logical1, i, tag
     &               , mpi_comm_world, mpi_erro)
          enddo
        endif
 
        if (SLAVE) then
DEB	print *,'logical1_recv: icore=',icore,' siz=',siz
        call mpi_recv (val,siz, mpi_logical1, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
*exec master_to_slave logical4 logical(i4) mpi_logical4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_logical4_master_to_slave (val, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        implicit none
        integer(i4) siz/1/,tag, i
        logical(i4) val
 
 
        if (MASTER) then
          do i = 1, ncore-1
*	  print *,'send: i=',i,' siz=',siz,' tag=',tag
          call mpi_send (val,siz, mpi_logical4, i, tag
     &               , mpi_comm_world, mpi_erro)
          enddo
        endif
 
        if (SLAVE) then
DEB	print *,'logical4_recv: icore=',icore,' siz=',siz
        call mpi_recv (val,siz, mpi_logical4, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end

*exec master_to_post real4 real(r4) mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_real4_master_to_post (val, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        implicit none
        integer(i4) siz/1/,tag, i
        real(r4) val
 
 
        if (MASTER) then
        i = ncore
* 	  print *,'real4_send: i=',i,' siz=',siz,' tag=',tag
          call mpi_send (val,siz, mpi_real4, i, tag
     &               , mpi_comm_world, mpi_erro)
        endif
 
        if (POST) then
* 	print *,'real4_recv: icore=',icore,' siz=',siz
        call mpi_recv (val,siz, mpi_real4, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec master_to_post real8 real(r8) mpi_real8
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_real8_master_to_post (val, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        implicit none
        integer(i4) siz/1/,tag, i
        real(r8) val
 
 
        if (MASTER) then
        i = ncore
* 	  print *,'real8_send: i=',i,' siz=',siz,' tag=',tag
          call mpi_send (val,siz, mpi_real8, i, tag
     &               , mpi_comm_world, mpi_erro)
        endif
 
        if (POST) then
* 	print *,'real8_recv: icore=',icore,' siz=',siz
        call mpi_recv (val,siz, mpi_real8, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec master_to_post integer4 integer(i4) mpi_integer4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_integer4_master_to_post (val, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        implicit none
        integer(i4) siz/1/,tag, i
        integer(i4) val
 
 
        if (MASTER) then
        i = ncore
* 	  print *,'integer4_send: i=',i,' siz=',siz,' tag=',tag
          call mpi_send (val,siz, mpi_integer4, i, tag
     &               , mpi_comm_world, mpi_erro)
        endif
 
        if (POST) then
* 	print *,'integer4_recv: icore=',icore,' siz=',siz
        call mpi_recv (val,siz, mpi_integer4, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec master_to_post logical4 logical(i4) mpi_logical4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_logical4_master_to_post (val, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        implicit none
        integer(i4) siz/1/,tag, i
        logical(i4) val
 
 
        if (MASTER) then
        i = ncore
* 	  print *,'logical4_send: i=',i,' siz=',siz,' tag=',tag
          call mpi_send (val,siz, mpi_logical4, i, tag
     &               , mpi_comm_world, mpi_erro)
        endif
 
        if (POST) then
* 	print *,'logical4_recv: icore=',icore,' siz=',siz
        call mpi_recv (val,siz, mpi_logical4, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 

*exec slave_to_master real4 real(r4) mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_real4_slave_to_master (val, tag,ifun)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) siz/1/,tag, i, j, ifun
        real(r4) val,val1
 
        if (SLAVE) then
        call mpi_send (val,siz, mpi_real4, 0, tag
     &               , mpi_comm_world, mpi_erro)
        endif
 
        if (MASTER) then
          do i = 1, ncore-1
DEB	print *,'real4_recv: i=',i,' siz=',siz
          call mpi_recv (val1,siz, mpi_real4, i, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
 
            if (ifun.eq.Fmov) then
            val = val1
            else if (ifun.eq.Fsum) then
            val = val + val1
            else if (ifun.eq.Fdif) then
            val = val - val1
            else if (ifun.eq.Fmin) then
            val = min(val,val1)
            else if (ifun.eq.Fmax) then
            val = max(val,val1)
            else
            write (iout,*) 'ifun=',ifun
            call logout(2,'Error in S_real4_slave_to_master')
            endif
 
          enddo
        endif
 
        return
        end
 
*exec slave_to_master real8 real(r8) mpi_real8
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_real8_slave_to_master (val, tag,ifun)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) siz/1/,tag, i, j, ifun
        real(r8) val,val1
 
        if (SLAVE) then
        call mpi_send (val,siz, mpi_real8, 0, tag
     &               , mpi_comm_world, mpi_erro)
        endif
 
        if (MASTER) then
          do i = 1, ncore-1
DEB	print *,'real8_recv: i=',i,' siz=',siz
          call mpi_recv (val1,siz, mpi_real8, i, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
 
            if (ifun.eq.Fmov) then
            val = val1
            else if (ifun.eq.Fsum) then
            val = val + val1
            else if (ifun.eq.Fdif) then
            val = val - val1
            else if (ifun.eq.Fmin) then
            val = min(val,val1)
            else if (ifun.eq.Fmax) then
            val = max(val,val1)
            else
            write (iout,*) 'ifun=',ifun
            call logout(2,'Error in S_real8_slave_to_master')
            endif
 
          enddo
        endif
 
        return
        end
 
*exec slave_to_master integer4 integer(i4) mpi_integer4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_integer4_slave_to_master (val, tag,ifun)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) siz/1/,tag, i, j, ifun
        integer(i4) val,val1
 
        if (SLAVE) then
        call mpi_send (val,siz, mpi_integer4, 0, tag
     &               , mpi_comm_world, mpi_erro)
        endif
 
        if (MASTER) then
          do i = 1, ncore-1
DEB	print *,'integer4_recv: i=',i,' siz=',siz
          call mpi_recv (val1,siz, mpi_integer4, i, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
 
            if (ifun.eq.Fmov) then
            val = val1
            else if (ifun.eq.Fsum) then
            val = val + val1
            else if (ifun.eq.Fdif) then
            val = val - val1
            else if (ifun.eq.Fmin) then
            val = min(val,val1)
            else if (ifun.eq.Fmax) then
            val = max(val,val1)
            else
            write (iout,*) 'ifun=',ifun
            call logout(2,'Error in S_integer4_slave_to_master')
            endif
 
          enddo
        endif
 
        return
        end
 

*exec post_to_master real4 real(r4) mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine S_real4_post_to_master (val, tag,ifun)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) siz/1/,tag, i, j, ifun
        real(r4) val, val1
 
        if (POST) then
        call mpi_send (val,siz, mpi_real4, 0, tag
     &               , mpi_comm_world, mpi_erro)
        endif
 
        if (MASTER) then
        i = ncore
          call mpi_recv (val1,siz, mpi_real4, i, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
DEB	print *,'real4_recv: i=',i,' siz=',siz
 
            if (ifun.eq.Fmov) then
            val = val1
            else if (ifun.eq.Fsum) then
            val = val + val1
            else if (ifun.eq.Fdif) then
            val = val - val1
            else if (ifun.eq.Fmin) then
            val = min(val,val1)
            else if (ifun.eq.Fmax) then
            val = max(val,val1)
            else
            write (iout,*) 'ifun=',ifun
            call logout(2,'Error in S_real4_slave_to_master')
            endif
        endif
 
 
        return
        end
 
