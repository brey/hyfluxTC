
*exec master_to_slave real8 real(r8) mpi_real8
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine PJ_real8_master_to_slave (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) siz,tag, i, idim, i1
        real(r8) val(*)
 
        if (MASTER) then
        do i = 1, ncore-1
        siz = (nj2(i)-nj1(i) + 1)*idim
        i1  = (nj1(i)-1)*idim + 1
        call mpi_send (val(i1),siz, mpi_real8, i, tag
     &                 , mpi_comm_world, mpi_erro)
        enddo
        endif
 
        if (SLAVE) then
        siz = sizj*idim
        call mpi_recv (val,siz, mpi_real8, 0, tag
     &                 , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
*exec master_to_slave real4 real(r4) mpi_real4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine PJ_real4_master_to_slave (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) siz,tag, i, idim, i1
        real(r4) val(*)
 
        if (MASTER) then
        do i = 1, ncore-1
        siz = (nj2(i)-nj1(i) + 1)*idim
        i1  = (nj1(i)-1)*idim + 1
        call mpi_send (val(i1),siz, mpi_real4, i, tag
     &                 , mpi_comm_world, mpi_erro)
        enddo
        endif
 
        if (SLAVE) then
        siz = sizj*idim
        call mpi_recv (val,siz, mpi_real4, 0, tag
     &                 , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
*exec master_to_slave integer4 integer(i4) mpi_integer4
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine PJ_integer4_master_to_slave (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) siz,tag, i, idim, i1
        integer(i4) val(*)
 
        if (MASTER) then
        do i = 1, ncore-1
        siz = (nj2(i)-nj1(i) + 1)*idim
        i1  = (nj1(i)-1)*idim + 1
        call mpi_send (val(i1),siz, mpi_integer4, i, tag
     &                 , mpi_comm_world, mpi_erro)
        enddo
        endif
 
        if (SLAVE) then
        siz = sizj*idim
        call mpi_recv (val,siz, mpi_integer4, 0, tag
     &                 , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end
*exec master_to_slave logical1 logical*1 mpi_logical1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine PJ_logical1_master_to_slave (val, idim, tag)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) siz,tag, i, idim, i1
        logical*1 val(*)
 
        if (MASTER) then
        do i = 1, ncore-1
        siz = (nj2(i)-nj1(i) + 1)*idim
        i1  = (nj1(i)-1)*idim + 1
        call mpi_send (val(i1),siz, mpi_logical1, i, tag
     &                 , mpi_comm_world, mpi_erro)
        enddo
        endif
 
        if (SLAVE) then
        siz = sizj*idim
        call mpi_recv (val,siz, mpi_logical1, 0, tag
     &                 , mpi_comm_world, mpi_stato, mpi_erro)
        endif
 
*        call mpi_barrier (mpi_comm_world, mpi_erro)
 
        return
        end

