*================================================================================
      module deck_data
*================================================================================
      implicit none

      integer ncard_max, lname, ldata
      parameter (ncard_max=1000,lname=32,ldata=256)
      character name_var(ncard_max)*(lname),data_var(ncard_max)*(ldata) 
      logical used(ncard_max)
      integer ncard /0/
      logical, save :: nocase
      end module deck_data
c========================================================================
      subroutine setncard 
c========================================================================
      use deck_data
      implicit none
c----------------------------------------------------------
      ncard = 0
      end      
c========================================================================
      subroutine proc_deck (filename,nocase0)
c========================================================================
      use deck_data
      implicit none
c----------------------------------------------------------
      character filename*(*)
      logical exist, first, second, nocase0, verify /.false./
      integer*4 i, j, k, length, nchar, iinp, posvar, ipos
      character str1*(ldata), str2*(ldata), var*(lname), data*(ldata) 
c----------------------------------------------------------
      nocase = nocase0
      used = .false.
      call iunfree(iinp)
      inquire (file=filename,exist=exist)
        if (.not.exist) then
        print *,'proc_deck: not exist ',filename(1:length(filename))
      return
        endif

        open (iinp,file=filename)

100     continue
        read (iinp,'(a)',end=300) str1
        call nocr(str1)
          if (verify) print *,'str1:',str1(1:length(str1))

        nchar = length(str1)

        if (str1(1:4).eq.'----') goto 100


        str2 = ' '
        k = 0
        j = 0
        first = .true.
        second = .false.
          do i = 1,nchar
            if (str1(i:i).eq.'*') goto 200
            if (str1(i:i).eq.'	') str1(i:i)=' '
            if (first.and.str1(i:i).ne.' ') first = .false.

            if (.not.first) then
            k = k+1
            str2(k:k) = str1(i:i)
              if (str2(k:k).eq.'=') then
              str2(k:k)=' '
              j = k
              endif
            endif     

            if (.not.second.and.k.gt.2.and.str2(k-1:k-1).eq.' '.and.str2(k:k).ne.' ') then
            second = .true.
            j = k
            endif
                 
          enddo
200     continue
c-----------------------------------------------------------
        if (str2.eq.' ') goto 100
          if (verify) print *,'str2:',str2(1:length(str2))
          if (verify) print *,'ncard=',ncard,' second=',second,' j-1=',j-1,' strj=',str2(j-1:j-1)
c-----------------------------------------------------------
c      character name_var(ncard_max)*(lname),data_var(ncard_max)*(ldata) 

c done str2
        if (second) then
        ipos = posvar(str2(1:j-1))
        name_var(ipos) = str2(1:j-1)        
          if (str2(j:j).eq.'$') then
          call getenv (str2(j+1:),data_var(ipos)) 
          else 
          data_var(ipos) = str2(j:)
          endif

        else if (str2.ne.' ') then
        ipos = posvar(str2)
        name_var(ipos) = str2
        data_var(ipos) = ' '
        
        else
        print *,'proc_deck: not recognized card ',str1(1:length(str1))
        endif

        if (nocase) call lowstr(name_var(ipos))

        if (verify) print *,ipos,') ',name_var(ipos)(1:12),'=',data_var(ipos)(1:64)
        goto 100

300     continue
        close (iinp)

      return
      end

c========================================================================
      function posvar (var0)
c========================================================================
      use deck_data
      implicit none
c----------------------------------------------------------
      integer*4 posvar
      integer i
      character var0*(*), msg*3,var*(lname)
c----------------------------------------------------------
      var = var0
        if (nocase) call lowstr(var)     
c----------------------------------------------------------
      do i = 1,ncard
        if (var.eq.name_var(i)) then
        posvar = i
        return
        endif
      enddo
      ncard = ncard+1
      posvar = ncard
      return
      end   
c========================================================================
      function getnumvar (var0,default)
c========================================================================
      use deck_data
      implicit none
c----------------------------------------------------------
      real*4 getnumvar, default, numget
      integer i
      character var0*(*), msg*3,var*(lname)
c----------------------------------------------------------
      var = var0
        if (nocase) call lowstr(var)     
c----------------------------------------------------------
      do i = 1,ncard
        if (var.eq.name_var(i)) then
          if (data_var(i).eq.' ') then
          print *,'getnumvar: var ',var,' not defined'
          goto 10
          endif          
        getnumvar = numget(data_var(i),msg)
        used(i) = .true.
          if (msg.eq.' ') goto 100
        getnumvar = default
        goto 100
        endif
      enddo

      print *,var,' not exist: set to default'          
10    continue
      getnumvar = default
100   continue
*      print *,var,' = ',getnumvar
      return
      end   
c========================================================================
      function getcharvar (var0,default)
c========================================================================
      use deck_data
      implicit none
c----------------------------------------------------------
      character*(ldata) getcharvar
      integer i, length
      character var0*(*), var*(lname),  default*(*)
c----------------------------------------------------------
      var = var0
        if (nocase) call lowstr(var)     
c----------------------------------------------------------
      do i = 1,ncard
        if (var.eq.name_var(i)) then
          if (data_var(i).eq.' ') then
          print *,'getcharvar: var ',var,' not defined'
          goto 10
          endif          
        getcharvar = data_var(i)
        used(i) = .true.
        goto 100
        endif
      enddo
          
      print *,var,' not exist: set to default'          
10    continue          
      getcharvar = default
100   continue
*      print *,var,' = ',getcharvar(1:length(getcharvar))
      return
      end   

c========================================================================
      function existvar (var0)
c========================================================================
      use deck_data
      implicit none
c----------------------------------------------------------
      integer i
      logical existvar
c----------------------------------------------------------
      character var0*(*), var*(lname)
c----------------------------------------------------------
      var = var0
        if (nocase) call lowstr(var)     
c----------------------------------------------------------
      do i = 1,ncard
        if (var.eq.name_var(i)) then
          if (data_var(i).eq.' ') then
          print *,'existvar: var ',var,' not defined'
          goto 10
          endif          
        existvar = .true.
        used(i) = .true.
        goto 100
        endif
      enddo

*      print *,var,' not exist: set to .false.'
10    continue          
      existvar = .false.          
100   continue
*      print *,'exist ',var,' = ',existvar
      return
      end   

c========================================================================
      subroutine listvar (iout)
c========================================================================
      use deck_data
      implicit none
      integer i,length,iout, lcard
c----------------------------------------------------------      

      write (iout,*) 'List of used variables'
      write (iout,*) 'No case sensitive = ',nocase
      write (iout,*)
      
      do i = 1,ncard
        if (used(i)) then
        lcard= length(data_var(i))
        write(iout,*) name_var(i),' =  ',data_var(i)(1:lcard)
        endif
      enddo
      write (iout,*)

      return
      end

c========================================================================
      subroutine getvars (var,cvar,nvar)
c========================================================================
      use deck_data
      implicit none
      integer i, nvar
      character var(nvar)*(*), cvar(nvar)*(*)
c----------------------------------------------------------      
      nvar = ncard
      do i = 1,ncard
      var(i) = name_var(i)
      cvar(i) = data_var(i)
      enddo

      return
      end

c========================================================================
      subroutine replaceVar(header,lh,var,cvar,nvar)
c========================================================================
c replace simbolc variables (identified by $ or @ or #) with his content
      implicit none
      integer nvar, lh, length, i1, i2, i, lc, isymb
      character header*(*), var(nvar)*(*), cvar(nvar)*(*), svar*64, fvar*64

      do while (.true.)
      lh = length(header)

      i = 1
      do while (header(i:i).ne.'$'.and.header(i:i).ne.'@'.and.header(i:i).ne.'#'.and.i.le.lh)
      i = i+1
      enddo
      
      isymb = 0
        if (header(i:i).eq.'$') isymb = 1  ! character
        if (header(i:i).eq.'@') isymb = 2  ! integer
        if (header(i:i).eq.'#') isymb = 3  ! fixed float

      		if (i.ge.lh) return

        if (i.gt.1.and.header(i-1:i-1).eq.'\') then
        header(i-1:)=header(i:)
        return
        endif          

      i1 = i+1
      do while (header(i:i).ne.' '.and.i.le.lh)
        if (header(i:i).eq.'.') exit
      i = i+1
      enddo
      
      i2 = i-1

      svar = header(i1:i2)
      i = 1
      do while (svar.ne.var(i).and.i.le.nvar)
      i = i+1
      enddo

*      print *,'svar=',svar(1:length(svar)),' var=',var(i)(1:length(var(i))),' cvar=',cvar(i)(1:length(cvar(i)))

      if (svar.eq.var(i)) then
      fvar=cvar(i)
      else
      call getenv(svar,fvar)
        if (fvar.eq.' ') fvar='NaN'
      endif

        if (fvar.eq.'NaN') then
        header= ' '
        return

        else
        call cleanvar(fvar,isymb,lc)

          if (header(i2+1:i2+1).eq.'.') i2=i2+1      
        header = header(:i1-2)//fvar(1:lc)//header(i2+1:)
        endif
      enddo

      end

      subroutine cleanvar (var,isymb,lc)
      implicit none
      character var*(*)
      integer isymb,lc,length, i, j
      logical dot

      lc = length(var)
        if (isymb.eq.1) return
      
        do i=1,lc
        dot = var(i:i).eq.'.'
          if (dot) exit
        enddo

      if (.not.dot) return  ! is yet integer
      
        if (isymb.eq.2) then
          if(dot) lc = i-1      
        return
        endif

        do j=min(lc,i+7),i,-1
          if (var(j:j).eq.' ') cycle
          if (var(j:j).ne.'0') exit
        enddo
      lc =j

      return
      end
      
c---------------------------------------------------------------
        subroutine newval (cval,ndig)
        implicit none
        character cval*(*), fmt*8, cval1*24 
        real*4 val, valget, fac
        logical eqmv
        integer l,len, ndig, ival

        if (cval.eq.'NaN') return

        val = valget(cval)
          if (eqmv(val)) then
          cval='NaN'
          return

          else if (ndig.ge.0) then
          cval = '  '
          write (fmt,'(a,i2,a)')'(f24.',ndig,')'
          write (cval1,fmt) val

          else
          cval = '  '
          fac =  10.**ndig
          ival = int(val*fac)/fac
          write (cval1,'(i24)') ival

          endif

        do l = 1,24
           if (cval1(l:l).ne.' ') exit
        enddo
        cval = cval1(l:)

        return
        end
c---------------------------------------------------------------
        subroutine toleft (cval)
        implicit none
        character*(*) cval
        integer l, ll

        ll = len(cval)
        l=1
           do while(cval(l:l).eq.' '.and.l.lt.ll)
           l = l+1
           enddo
        cval = cval(l:)

        return
        end





