#define DEB c



*exec sendCore logical1 logical*1 mpi_logical1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_sendCore (val, idim, i,jcore, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1, io(2), jcore
        logical*1 val(*)
 
*        print *,'logical1_send ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_send (val(i1),sc(i)*idim, mpi_logical1, jcore, tag
     &               , mpi_comm_world, mpi_erro)
 
*     ! NO!     call mpi_barrier (mpi_comm_world, mpi_erro)
 
 
        return
        end
 
*exec sendCore integer1 integer*1 mpi_integer1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine integer1_sendCore (val, idim, i,jcore, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1, io(2), jcore
        integer*1 val(*)
 
*        print *,'integer1_send ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_send (val(i1),sc(i)*idim, mpi_integer1, jcore, tag
     &               , mpi_comm_world, mpi_erro)
 
*     ! NO!     call mpi_barrier (mpi_comm_world, mpi_erro)
 
 
        return
        end
 
*exec sendCore real4 real(r4) mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_sendCore (val, idim, i,jcore, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1, io(2), jcore
        real(r4) val(*)
 
*        print *,'real4_send ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_send (val(i1),sc(i)*idim, mpi_real4, jcore, tag
     &               , mpi_comm_world, mpi_erro)
 
*     ! NO!     call mpi_barrier (mpi_comm_world, mpi_erro)
 
 
        return
        end
 
*exec sendCore real8 real(r8) mpi_real8
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_sendCore (val, idim, i,jcore, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1, io(2), jcore
        real(r8) val(*)
 
*        print *,'real8_send ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_send (val(i1),sc(i)*idim, mpi_real8, jcore, tag
     &               , mpi_comm_world, mpi_erro)
 
*     ! NO!     call mpi_barrier (mpi_comm_world, mpi_erro)
 
 
        return
        end
 

*exec recvCore logical1 logical*1 mpi_logical1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_recvCore (val, idim, i,jcore, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1, io(2), jcore
        logical*1 val(*)
 
*        print *,'logical1_recv ',i,' icore=',icore,' from =',jcore
 
        i1 = pf(i)*idim + 1
        call mpi_recv (val(i1),sc(i)*idim, mpi_logical1, jcore, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
 
*     ! NO!     call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec recvCore integer1 integer*1 mpi_integer1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine integer1_recvCore (val, idim, i,jcore, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1, io(2), jcore
        integer*1 val(*)
 
*        print *,'integer1_recv ',i,' icore=',icore,' from =',jcore
 
        i1 = pf(i)*idim + 1
        call mpi_recv (val(i1),sc(i)*idim, mpi_integer1, jcore, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
 
*     ! NO!     call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec recvCore real4 real(r4) mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_recvCore (val, idim, i,jcore, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1, io(2), jcore
        real(r4) val(*)
 
*        print *,'real4_recv ',i,' icore=',icore,' from =',jcore
 
        i1 = pf(i)*idim + 1
        call mpi_recv (val(i1),sc(i)*idim, mpi_real4, jcore, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
 
*     ! NO!     call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec recvCore real8 real(r8) mpi_real8
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_recvCore (val, idim, i,jcore, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1, io(2), jcore
        real(r8) val(*)
 
*        print *,'real8_recv ',i,' icore=',icore,' from =',jcore
 
        i1 = pf(i)*idim + 1
        call mpi_recv (val(i1),sc(i)*idim, mpi_real8, jcore, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
 
*     ! NO!     call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 

*exec getLR logical1 logical*1  mpi_logical1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_getLR (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim
        logical*1 val(*)
c left
        if (RIGHT) call logical1_sendCore(val, idim, 3,icore+1, tag)
 
        if (LEFT)  call logical1_recvCore(val, idim, 1,icore-1, tag)
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
c right
        if (LEFT)  call logical1_sendCore(val, idim, 2,icore-1, tag)
 
        if (RIGHT) call logical1_recvCore(val, idim, 4,icore+1, tag)
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec getLR integer1 integer*1  mpi_integer1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine integer1_getLR (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim
        integer*1 val(*)
c left
        if (RIGHT) call integer1_sendCore(val, idim, 3,icore+1, tag)
 
        if (LEFT)  call integer1_recvCore(val, idim, 1,icore-1, tag)
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
c right
        if (LEFT)  call integer1_sendCore(val, idim, 2,icore-1, tag)
 
        if (RIGHT) call integer1_recvCore(val, idim, 4,icore+1, tag)
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec getLR real4 real(r4) mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_getLR (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim
        real(r4) val(*)
c left
        if (RIGHT) call real4_sendCore(val, idim, 3,icore+1, tag)
 
        if (LEFT)  call real4_recvCore(val, idim, 1,icore-1, tag)
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
c right
        if (LEFT)  call real4_sendCore(val, idim, 2,icore-1, tag)
 
        if (RIGHT) call real4_recvCore(val, idim, 4,icore+1, tag)
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec getLR real8 real(r8) mpi_real8
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_getLR (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim
        real(r8) val(*)
c left
        if (RIGHT) call real8_sendCore(val, idim, 3,icore+1, tag)
 
        if (LEFT)  call real8_recvCore(val, idim, 1,icore-1, tag)
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
c right
        if (LEFT)  call real8_sendCore(val, idim, 2,icore-1, tag)
 
        if (RIGHT) call real8_recvCore(val, idim, 4,icore+1, tag)
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 

*exec getR logical1 logical*1  mpi_logical1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_getR (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim
        logical*1 val(*)
 
*        print *,'logical1_getR  icore =',icore
        if (LEFT)  call logical1_sendCore(val, idim, 2,icore-1, tag)
 
        if (RIGHT) call logical1_recvCore(val, idim, 4,icore+1, tag)
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec getR real4 real(r4) mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_getR (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim
        real(r4) val(*)
 
*        print *,'real4_getR  icore =',icore
        if (LEFT)  call real4_sendCore(val, idim, 2,icore-1, tag)
 
        if (RIGHT) call real4_recvCore(val, idim, 4,icore+1, tag)
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec getR real8 real(r8) mpi_real8
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_getR (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim
        real(r8) val(*)
 
*        print *,'real8_getR  icore =',icore
        if (LEFT)  call real8_sendCore(val, idim, 2,icore-1, tag)
 
        if (RIGHT) call real8_recvCore(val, idim, 4,icore+1, tag)
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 

*exec slave_to_master real4 real(r4)  mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_slave_to_master (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1
        real(r4) val(*)
 
        if (SLAVE) then
        i1  = (nv1(icore)-nv3(icore))*idim + 1
*        print *,'icore=',icore,' i1=',i1
        call mpi_send (val(i1),nvv(icore)*idim, mpi_real4, 0, tag
     &               , mpi_comm_world,  mpi_erro)
        endif
 
        if (MASTER) then
        do i = 1, ncore-1
        i1  = (nv1(i)-1)*idim + 1
*        print *,'i=',i,' i1=',i1
        call mpi_recv (val(i1),nvv(i)*idim, mpi_real4, i, tag
     &                ,  mpi_comm_world, mpi_stato, mpi_erro)
        enddo
        endif
 
        return
        end
 
 

*exec calc_to_post real4 real(r4)  mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_calc_to_post (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1
        real(r4) val(*)
 
        if (CALC) then
        i1  = (nv1(icore)-nv3(icore))*idim + 1
*        print *,'icore=',icore,' i1=',i1
        call mpi_send (val(i1),nvv(icore)*idim, mpi_real4, ncore, tag
     &               , mpi_comm_world,  mpi_erro)
        endif
 
        if (POST) then
        do i = 0, ncore-1
        i1  = (nv1(i)-1)*idim + 1
*        print *,'i=',i,' i1=',i1
        call mpi_recv (val(i1),nvv(i)*idim, mpi_real4, i, tag
     &                ,  mpi_comm_world, mpi_stato, mpi_erro)
        enddo
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec calc_to_post real8 real(r8)  mpi_real8
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_calc_to_post (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1
        real(r8) val(*)
 
        if (CALC) then
        i1  = (nv1(icore)-nv3(icore))*idim + 1
*        print *,'icore=',icore,' i1=',i1
        call mpi_send (val(i1),nvv(icore)*idim, mpi_real8, ncore, tag
     &               , mpi_comm_world,  mpi_erro)
        endif
 
        if (POST) then
        do i = 0, ncore-1
        i1  = (nv1(i)-1)*idim + 1
*        print *,'i=',i,' i1=',i1
        call mpi_recv (val(i1),nvv(i)*idim, mpi_real8, i, tag
     &                ,  mpi_comm_world, mpi_stato, mpi_erro)
        enddo
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 

*exec master_to_slave real4 real(r4)  mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_master_to_slave (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1
        real(r4) val(*)
 
        if (MASTER) then
        do i = 1, ncore-1
        i1  = (nv3(i)-1)*idim + 1
        call mpi_send (val(i1),nvvL(i)*idim, mpi_real4, i, tag
     &               , mpi_comm_world, mpi_erro)
        enddo
        endif
 
        if (SLAVE) then
DEB        print '(2(a,i6)))',' real4_master_to_slave icore=',icore
DEB     &                   ,' size=',nvvL(icore)
        call mpi_recv (val,nvvL(icore)*idim, mpi_real4, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
        return
        end
 
*exec master_to_slave real8 real(r8)  mpi_real8
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_master_to_slave (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1
        real(r8) val(*)
 
        if (MASTER) then
        do i = 1, ncore-1
        i1  = (nv3(i)-1)*idim + 1
        call mpi_send (val(i1),nvvL(i)*idim, mpi_real8, i, tag
     &               , mpi_comm_world, mpi_erro)
        enddo
        endif
 
        if (SLAVE) then
DEB        print '(2(a,i6)))',' real8_master_to_slave icore=',icore
DEB     &                   ,' size=',nvvL(icore)
        call mpi_recv (val,nvvL(icore)*idim, mpi_real8, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
        return
        end
 
*exec master_to_slave integer1 integer*1  mpi_integer1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine integer1_master_to_slave (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1
        integer*1 val(*)
 
        if (MASTER) then
        do i = 1, ncore-1
        i1  = (nv3(i)-1)*idim + 1
        call mpi_send (val(i1),nvvL(i)*idim, mpi_integer1, i, tag
     &               , mpi_comm_world, mpi_erro)
        enddo
        endif
 
        if (SLAVE) then
DEB        print '(2(a,i6)))',' integer1_master_to_slave icore=',icore
DEB     &                   ,' size=',nvvL(icore)
        call mpi_recv (val,nvvL(icore)*idim, mpi_integer1, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
        return
        end
 
*exec master_to_slave integer4 integer*4  mpi_integer4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine integer4_master_to_slave (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1
        integer*4 val(*)
 
        if (MASTER) then
        do i = 1, ncore-1
        i1  = (nv3(i)-1)*idim + 1
        call mpi_send (val(i1),nvvL(i)*idim, mpi_integer4, i, tag
     &               , mpi_comm_world, mpi_erro)
        enddo
        endif
 
        if (SLAVE) then
DEB        print '(2(a,i6)))',' integer4_master_to_slave icore=',icore
DEB     &                   ,' size=',nvvL(icore)
        call mpi_recv (val,nvvL(icore)*idim, mpi_integer4, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
        return
        end
 
*exec master_to_slave logical1 logical*1  mpi_logical1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_master_to_slave (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, i, idim, i1
        logical*1 val(*)
 
        if (MASTER) then
        do i = 1, ncore-1
        i1  = (nv3(i)-1)*idim + 1
        call mpi_send (val(i1),nvvL(i)*idim, mpi_logical1, i, tag
     &               , mpi_comm_world, mpi_erro)
        enddo
        endif
 
        if (SLAVE) then
DEB        print '(2(a,i6)))',' logical1_master_to_slave icore=',icore
DEB     &                   ,' size=',nvvL(icore)
        call mpi_recv (val,nvvL(icore)*idim, mpi_logical1, 0, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
        return
        end
 


#define FLOAT
*exec putR integer1 integer*1  mpi_logical1 my_ival1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine integer1_putR (val, idim, tag,ifun)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim, siz, i1, i2, i, j, k, ifun
        integer*1 val(*)
 
*        print *,'integer1_putR icore =',icore
 
        if (RIGHT) then
        i = 4
        i1 = pf(i)*idim + 1
        siz = sc(i)*idim
        call mpi_send (val(i1),siz, mpi_logical1, icore+1, tag
     &               , mpi_comm_world, mpi_erro)
         endif
 
        if (LEFT) then
        i = 2
        i1 = pf(i)*idim
        siz = sc(i)*idim
        i2 = i1 + siz
        i1 = i1 + 1
        call mpi_recv (my_ival1 ,siz, mpi_logical1, icore-1, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
            if (ifun.eq.Fmov) then
            val(i1:i2) = my_ival1(1:siz)
 
#ifdef FLOAT then
            else if (ifun.eq.Fsum) then
            val(i1:i2) = val(i1:i2) + my_ival1(1:siz)
 
            else if (ifun.eq.Fdif) then
            val(i1:i2) = val(i1:i2) - my_ival1(1:siz)
 
            else if (ifun.eq.Fmin) then
            k = i1
              do j = 1,siz
              val(k) = min(val(k),my_ival1(j))
              k = k + 1
              enddo
 
            else if (ifun.eq.Fmax) then
            k = i1
              do j = 1,siz
              val(k) = max(val(k),my_ival1(j))
              k = k + 1
              enddo
#else ifdef LOGICAL then
            else if (ifun.eq.For) then
            k = i1
              do j = 1,siz
              val(k) = val(k) .or. my_ival1(j)
              k = k + 1
              enddo
 
            else if (ifun.eq.Fand) then
            k = i1
              do j = 1,siz
              val(k) = val(k) .and. my_ival1(j)
              k = k + 1
              enddo
#endif
            else
            write (iout,*) 'ifun=',ifun
            call logout(2,'Error in integer1_putR')
            endif
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec putR real4 real(r4) mpi_real4 my_sval
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_putR (val, idim, tag,ifun)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim, siz, i1, i2, i, j, k, ifun
        real(r4) val(*)
 
*        print *,'real4_putR icore =',icore
 
        if (RIGHT) then
        i = 4
        i1 = pf(i)*idim + 1
        siz = sc(i)*idim
        call mpi_send (val(i1),siz, mpi_real4, icore+1, tag
     &               , mpi_comm_world, mpi_erro)
         endif
 
        if (LEFT) then
        i = 2
        i1 = pf(i)*idim
        siz = sc(i)*idim
        i2 = i1 + siz
        i1 = i1 + 1
        call mpi_recv (my_sval ,siz, mpi_real4, icore-1, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
            if (ifun.eq.Fmov) then
            val(i1:i2) = my_sval(1:siz)
 
#ifdef FLOAT then
            else if (ifun.eq.Fsum) then
            val(i1:i2) = val(i1:i2) + my_sval(1:siz)
 
            else if (ifun.eq.Fdif) then
            val(i1:i2) = val(i1:i2) - my_sval(1:siz)
 
            else if (ifun.eq.Fmin) then
            k = i1
              do j = 1,siz
              val(k) = min(val(k),my_sval(j))
              k = k + 1
              enddo
 
            else if (ifun.eq.Fmax) then
            k = i1
              do j = 1,siz
              val(k) = max(val(k),my_sval(j))
              k = k + 1
              enddo
#else ifdef LOGICAL then
            else if (ifun.eq.For) then
            k = i1
              do j = 1,siz
              val(k) = val(k) .or. my_sval(j)
              k = k + 1
              enddo
 
            else if (ifun.eq.Fand) then
            k = i1
              do j = 1,siz
              val(k) = val(k) .and. my_sval(j)
              k = k + 1
              enddo
#endif
            else
            write (iout,*) 'ifun=',ifun
            call logout(2,'Error in real4_putR')
            endif
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec putR real8 real(r8) mpi_real8 my_dval
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_putR (val, idim, tag,ifun)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim, siz, i1, i2, i, j, k, ifun
        real(r8) val(*)
 
*        print *,'real8_putR icore =',icore
 
        if (RIGHT) then
        i = 4
        i1 = pf(i)*idim + 1
        siz = sc(i)*idim
        call mpi_send (val(i1),siz, mpi_real8, icore+1, tag
     &               , mpi_comm_world, mpi_erro)
         endif
 
        if (LEFT) then
        i = 2
        i1 = pf(i)*idim
        siz = sc(i)*idim
        i2 = i1 + siz
        i1 = i1 + 1
        call mpi_recv (my_dval ,siz, mpi_real8, icore-1, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
            if (ifun.eq.Fmov) then
            val(i1:i2) = my_dval(1:siz)
 
#ifdef FLOAT then
            else if (ifun.eq.Fsum) then
            val(i1:i2) = val(i1:i2) + my_dval(1:siz)
 
            else if (ifun.eq.Fdif) then
            val(i1:i2) = val(i1:i2) - my_dval(1:siz)
 
            else if (ifun.eq.Fmin) then
            k = i1
              do j = 1,siz
              val(k) = min(val(k),my_dval(j))
              k = k + 1
              enddo
 
            else if (ifun.eq.Fmax) then
            k = i1
              do j = 1,siz
              val(k) = max(val(k),my_dval(j))
              k = k + 1
              enddo
#else ifdef LOGICAL then
            else if (ifun.eq.For) then
            k = i1
              do j = 1,siz
              val(k) = val(k) .or. my_dval(j)
              k = k + 1
              enddo
 
            else if (ifun.eq.Fand) then
            k = i1
              do j = 1,siz
              val(k) = val(k) .and. my_dval(j)
              k = k + 1
              enddo
#endif
            else
            write (iout,*) 'ifun=',ifun
            call logout(2,'Error in real8_putR')
            endif
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 

#undef FLOAT
#define LOGICAL
*exec putL logical1 logical*1  mpi_logical1 my_lval1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_putL (val, idim, tag,ifun)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim, siz, i1, i2, i, j, k, ifun
        logical*1 val(*)
 
*        print *,'logical1_putR icore =',icore
 
        if (LEFT) then
        i = 1
        i1 = pf(i)*idim + 1
        siz = sc(i)*idim
        call mpi_send (val(i1),siz, mpi_logical1, icore-1, tag
     &               , mpi_comm_world, mpi_erro)
        endif
 
        if (RIGHT) then
        i = 3
        i1 = pf(i)*idim
        siz = sc(i)*idim
        i2 = i1 + siz
        i1 = i1 + 1
        call mpi_recv (my_lval1 ,siz, mpi_logical1, icore+1, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
            if (ifun.eq.Fmov) then
            val(i1:i2) = my_lval1(1:siz)
 
#ifdef FLOAT then
            else if (ifun.eq.Fsum) then
            val(i1:i2) = val(i1:i2) + my_lval1(1:siz)
 
            else if (ifun.eq.Fdif) then
            val(i1:i2) = val(i1:i2) - my_lval1(1:siz)
 
            else if (ifun.eq.Fmin) then
            k = i1
              do j = 1,siz
              val(k) = min(val(k),my_lval1(j))
              k = k + 1
              enddo
 
            else if (ifun.eq.Fmax) then
            k = i1
              do j = 1,siz
              val(k) = max(val(k),my_lval1(j))
              k = k + 1
              enddo
#else ifdef LOGICAL then
            else if (ifun.eq.For) then
            k = i1
              do j = 1,siz
              val(k) = val(k) .or. my_lval1(j)
              k = k + 1
              enddo
 
            else if (ifun.eq.Fand) then
            k = i1
              do j = 1,siz
              val(k) = val(k) .and. my_lval1(j)
              k = k + 1
              enddo
#endif
            else
            write (iout,*) 'ifun=',ifun
            call logout(2,'Error in logical1_putR')
            endif
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
*exec putR logical1 logical*1  mpi_logical1 my_lval1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_putR (val, idim, tag,ifun)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer tag, idim, siz, i1, i2, i, j, k, ifun
        logical*1 val(*)
 
*        print *,'logical1_putR icore =',icore
 
        if (RIGHT) then
        i = 4
        i1 = pf(i)*idim + 1
        siz = sc(i)*idim
        call mpi_send (val(i1),siz, mpi_logical1, icore+1, tag
     &               , mpi_comm_world, mpi_erro)
         endif
 
        if (LEFT) then
        i = 2
        i1 = pf(i)*idim
        siz = sc(i)*idim
        i2 = i1 + siz
        i1 = i1 + 1
        call mpi_recv (my_lval1 ,siz, mpi_logical1, icore-1, tag
     &               , mpi_comm_world, mpi_stato, mpi_erro)
            if (ifun.eq.Fmov) then
            val(i1:i2) = my_lval1(1:siz)
 
#ifdef FLOAT then
            else if (ifun.eq.Fsum) then
            val(i1:i2) = val(i1:i2) + my_lval1(1:siz)
 
            else if (ifun.eq.Fdif) then
            val(i1:i2) = val(i1:i2) - my_lval1(1:siz)
 
            else if (ifun.eq.Fmin) then
            k = i1
              do j = 1,siz
              val(k) = min(val(k),my_lval1(j))
              k = k + 1
              enddo
 
            else if (ifun.eq.Fmax) then
            k = i1
              do j = 1,siz
              val(k) = max(val(k),my_lval1(j))
              k = k + 1
              enddo
#else ifdef LOGICAL then
            else if (ifun.eq.For) then
            k = i1
              do j = 1,siz
              val(k) = val(k) .or. my_lval1(j)
              k = k + 1
              enddo
 
            else if (ifun.eq.Fand) then
            k = i1
              do j = 1,siz
              val(k) = val(k) .and. my_lval1(j)
              k = k + 1
              enddo
#endif
            else
            write (iout,*) 'ifun=',ifun
            call logout(2,'Error in logical1_putR')
            endif
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
 
