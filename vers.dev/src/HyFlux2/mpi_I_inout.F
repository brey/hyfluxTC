#define DEB c


c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine wCore (i,jcore,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) i, jcore, req,ivar

        req = request(ivar,i,jcore)
DEB      print '(10(a,i3))','W: icore=',icore,' jcore=',jcore
DEB     & ,' i=',i,' ivar=',ivar,' req=',req
        call mpi_wait (req,mpi_stato,mpi_erro)
        return
        end

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine wgetL (ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) ivar
c left
        if (RIGHT) call wCore( 3,icore+1,ivar)
        if (LEFT)  call wCore( 1,icore-1,ivar)

        return
        end


c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine wgetR (ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) ivar

c right
        if (LEFT)  call wCore( 2,icore-1,ivar)
        if (RIGHT) call wCore( 4,icore+1,ivar)

        return
        end

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine wgetLR (ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) ivar

        call wgetL (ivar)
        call wgetR (ivar)

        return
        end


*exec isendCore logical1 logical*1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_isendCore (val, idim, i,jcore,tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        use constants
        implicit none
        integer(i4) tag, i, idim, i1, jcore, req,ivar
        logical*1 val(*)
 
*        print *,'isend ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_isend (val(i1),sc(i)*idim, mpi_logical1, jcore, tag
     &               , mpi_comm_world, req, mpi_erro)
DEB      print '(10(a,i3))','S: icore=',icore,' jcore=',jcore
DEB     & ,' i=',i,' ivar=',ivar,' req=',req
        request(ivar,i,jcore) = req
        return
        end
 
*exec isendCore real4 real(r4)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_isendCore (val, idim, i,jcore,tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        use constants
        implicit none
        integer(i4) tag, i, idim, i1, jcore, req,ivar
        real(r4) val(*)
 
*        print *,'isend ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_isend (val(i1),sc(i)*idim, mpi_real4, jcore, tag
     &               , mpi_comm_world, req, mpi_erro)
DEB      print '(10(a,i3))','S: icore=',icore,' jcore=',jcore
DEB     & ,' i=',i,' ivar=',ivar,' req=',req
        request(ivar,i,jcore) = req
        return
        end
 
*exec isendCore real8 real(r8)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_isendCore (val, idim, i,jcore,tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        use constants
        implicit none
        integer(i4) tag, i, idim, i1, jcore, req,ivar
        real(r8) val(*)
 
*        print *,'isend ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_isend (val(i1),sc(i)*idim, mpi_real8, jcore, tag
     &               , mpi_comm_world, req, mpi_erro)
DEB      print '(10(a,i3))','S: icore=',icore,' jcore=',jcore
DEB     & ,' i=',i,' ivar=',ivar,' req=',req
        request(ivar,i,jcore) = req
        return
        end
 
*exec isendCore complex8 complex(r8)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine complex8_isendCore (val, idim, i,jcore,tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
        use constants
        implicit none
        integer(i4) tag, i, idim, i1, jcore, req,ivar
        complex(r8) val(*)
 
*        print *,'isend ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_isend (val(i1),sc(i)*idim, mpi_complex8, jcore, tag
     &               , mpi_comm_world, req, mpi_erro)
DEB      print '(10(a,i3))','S: icore=',icore,' jcore=',jcore
DEB     & ,' i=',i,' ivar=',ivar,' req=',req
        request(ivar,i,jcore) = req
        return
        end
 

*exec irecvCore logical1 logical*1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_irecvCore (val, idim, i,jcore, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, i, idim, i1, jcore, req,ivar
        logical*1 val(*)
 
*        print *,'isend ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_irecv (val(i1),sc(i)*idim, mpi_logical1, jcore, tag
     &               , mpi_comm_world, req, mpi_erro)
DEB      print '(10(a,i3))','R: icore=',icore,' jcore=',jcore
DEB     & ,' i=',i,' ivar=',ivar,' req=',req
        request(ivar,i,jcore) = req
 
        return
        end
 
*exec irecvCore real4 real(r4)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_irecvCore (val, idim, i,jcore, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, i, idim, i1, jcore, req,ivar
        real(r4) val(*)
 
*        print *,'isend ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_irecv (val(i1),sc(i)*idim, mpi_real4, jcore, tag
     &               , mpi_comm_world, req, mpi_erro)
DEB      print '(10(a,i3))','R: icore=',icore,' jcore=',jcore
DEB     & ,' i=',i,' ivar=',ivar,' req=',req
        request(ivar,i,jcore) = req
 
        return
        end
 
*exec irecvCore real8 real(r8)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_irecvCore (val, idim, i,jcore, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, i, idim, i1, jcore, req,ivar
        real(r8) val(*)
 
*        print *,'isend ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_irecv (val(i1),sc(i)*idim, mpi_real8, jcore, tag
     &               , mpi_comm_world, req, mpi_erro)
DEB      print '(10(a,i3))','R: icore=',icore,' jcore=',jcore
DEB     & ,' i=',i,' ivar=',ivar,' req=',req
        request(ivar,i,jcore) = req
 
        return
        end
 
*exec irecvCore complex8 complex(r8)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine complex8_irecvCore (val, idim, i,jcore, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, i, idim, i1, jcore, req,ivar
        complex(r8) val(*)
 
*        print *,'isend ',i,' icore =',icore,' to =',jcore
 
        i1 = pf(i)*idim+1
        call mpi_irecv (val(i1),sc(i)*idim, mpi_complex8, jcore, tag
     &               , mpi_comm_world, req, mpi_erro)
DEB      print '(10(a,i3))','R: icore=',icore,' jcore=',jcore
DEB     & ,' i=',i,' ivar=',ivar,' req=',req
        request(ivar,i,jcore) = req
 
        return
        end
 

*exec igetL logical1 logical*1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_igetL (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        logical*1 val(*)
c left
        if (RIGHT) call logical1_isendCore(val, idim, 3,icore+1, tag,iva
     &r)
        if (LEFT)  call logical1_irecvCore(val, idim, 1,icore-1, tag,iva
     &r)
 
        return
        end
 
*exec igetL real4 real(r4)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_igetL (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        real(r4) val(*)
c left
        if (RIGHT) call real4_isendCore(val, idim, 3,icore+1, tag,ivar)
        if (LEFT)  call real4_irecvCore(val, idim, 1,icore-1, tag,ivar)
 
        return
        end
 
*exec igetL real8 real(r8)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_igetL (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        real(r8) val(*)
c left
        if (RIGHT) call real8_isendCore(val, idim, 3,icore+1, tag,ivar)
        if (LEFT)  call real8_irecvCore(val, idim, 1,icore-1, tag,ivar)
 
        return
        end
 
*exec igetL complex8 real(r8)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine complex8_igetL (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        real(r8) val(*)
c left
        if (RIGHT) call complex8_isendCore(val, idim, 3,icore+1, tag,iva
     &r)
        if (LEFT)  call complex8_irecvCore(val, idim, 1,icore-1, tag,iva
     &r)
 
        return
        end
 

*exec igetR logical1 logical*1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_igetR (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        logical*1 val(*)
 
c right
        if (LEFT)  call logical1_isendCore(val, idim, 2,icore-1, tag,iva
     &r)
        if (RIGHT) call logical1_irecvCore(val, idim, 4,icore+1, tag,iva
     &r)
 
        return
        end
 
*exec igetR real4 real(r4)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_igetR (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        real(r4) val(*)
 
c right
        if (LEFT)  call real4_isendCore(val, idim, 2,icore-1, tag,ivar)
        if (RIGHT) call real4_irecvCore(val, idim, 4,icore+1, tag,ivar)
 
        return
        end
 
*exec igetR real8 real(r8)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_igetR (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        real(r8) val(*)
 
c right
        if (LEFT)  call real8_isendCore(val, idim, 2,icore-1, tag,ivar)
        if (RIGHT) call real8_irecvCore(val, idim, 4,icore+1, tag,ivar)
 
        return
        end
 
*exec igetR complex8 complex(r8)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine complex8_igetR (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        complex(r8) val(*)
 
c right
        if (LEFT)  call complex8_isendCore(val, idim, 2,icore-1, tag,iva
     &r)
        if (RIGHT) call complex8_irecvCore(val, idim, 4,icore+1, tag,iva
     &r)
 
        return
        end
 


*exec igetLR logical1 logical*1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_igetLR (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        logical*1 val(*)
 
        call logical1_igetL (val, idim, tag,ivar)
        call logical1_igetR (val, idim, tag,ivar)
 
        return
        end
*exec igetLR real4 real(r4)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_igetLR (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        real(r4) val(*)
 
        call real4_igetL (val, idim, tag,ivar)
        call real4_igetR (val, idim, tag,ivar)
 
        return
        end
*exec igetLR real8 real(r8)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_igetLR (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        real(r8) val(*)
 
        call real8_igetL (val, idim, tag,ivar)
        call real8_igetR (val, idim, tag,ivar)
 
        return
        end
*exec igetLR complex8 real(r8)
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine complex8_igetLR (val, idim, tag,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim,ivar
        real(r8) val(*)
 
        call complex8_igetL (val, idim, tag,ivar)
        call complex8_igetR (val, idim, tag,ivar)
 
        return
        end

#define FLOAT

*exec iputR integer1 integer*1 my_ival1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine integer1_iputR (val, idim, tag,ifun,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim, siz, i1, i2, i, j, k, ifun, ivar
        integer*1 val(*)
 
*        print *,'integer1_putR icore =',icore
 
        if (RIGHT) call wCore( 4,icore+1,ivar)
 
        if (LEFT) then
        i = 2
        i1 = pf(i)*idim
        siz = sc(i)*idim
        i2 = i1 + siz
        i1 = i1 + 1
        call mpi_recv (my_ival1 ,siz, mpi_integer1, icore-1, tag
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
 
*exec iputR real4 real(r4) my_sval
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real4_iputR (val, idim, tag,ifun,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim, siz, i1, i2, i, j, k, ifun, ivar
        real(r4) val(*)
 
*        print *,'real4_putR icore =',icore
 
        if (RIGHT) call wCore( 4,icore+1,ivar)
 
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
 
*exec iputR real8 real(r8) my_dval
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine real8_iputR (val, idim, tag,ifun,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim, siz, i1, i2, i, j, k, ifun, ivar
        real(r8) val(*)
 
*        print *,'real8_putR icore =',icore
 
        if (RIGHT) call wCore( 4,icore+1,ivar)
 
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
*exec iputR logical1 logical*1 my_lval1
c    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        subroutine logical1_iputR (val, idim, tag,ifun,ivar)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        use parallel_data
       use constants
        implicit none
        integer(i4) tag, idim, siz, i1, i2, i, j, k, ifun, ivar
        logical*1 val(*)
 
*        print *,'logical1_putR icore =',icore
 
        if (RIGHT) call wCore( 4,icore+1,ivar)
 
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
 

