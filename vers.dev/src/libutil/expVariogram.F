        function varerror (var,varcalc)
        implicit none
        real*4 varerror,var,varcalc
*       varerror =  abs(var-varcalc)/varcalc
        varerror =  abs(var-varcalc)
        return
        end
c
        subroutine expVariogram(cleanfac,typlag0,nlag, tfit
     &             ,Lavail,typlagAvail,ccAvail,rangeAvail,sdevAvail
     &             ,navail,typlagtss
     &             ,nmax,nmaxpun,nradius
     &             ,maskval,nval)
 
c recalc values and distance using inverse distance method
c called if filtval or cross kriging is requested
        use findind_mod
        use expvar_mod
        implicit none
c---------------------------------------------------------
        integer nlag,nmax, nmaxpun, nradius,nval,navail
        real*4 typlagAvail(navail),ccAvail(1:nvar,navail),rangeAvail(navail)
     &         ,sdevAvail(navail),typlag0(nlag)
        real*4 tfit, cleanfac
        integer*4 Lavail(navail),typlagtss(nlag)
        logical*1 maskval(nval)
 
c local variables
        logical cleaned, eqmv
        integer i,j,m,n,L,L1,L2, it0, it1
     &         ,nokrig
        real*4 sdev, var
c functions
        real*4 distance, varfun, variance
c---------------------------------------------------------
c start of loop over requested pixels to cover
c---------------------------------------------------------
#if defined linux
        call clockm(it0)
#endif
        nokrig = 0
        pt = -1
 
	do 300 i = 1,navail
c---------------------------------------------------------
        L  = Lavail(i)
        ir = Ireq(L)
c---------------------------------------------------------
        n = 0
          if (typvarRec) then
            do j = 1,npt(ir)
            L1 = pt1(pt2(ir)+j)
              if (maskval(L1).and. .not.eqmv(valrec(L1))) then
              n = n+1
              pt(n) = L1
              dist(n) = distance(L1,L)
                if (dist(n).gt.float(nradius).or.n.ge.nmaxpun) go to 10
              endif
            enddo
          else
            do j = 1,npt(ir)
            L1 = pt1(pt2(ir)+j)
              if (maskval(L1)) then
              n = n+1
              pt(n) = L1
              dist(n) = distance(L1,L)
                if (dist(n).gt.float(nradius).or.n.ge.nmaxpun) go to 10
              endif
            enddo
          endif
*------------------------------------------------
10        continue
          if (n.le.0) then
*         print *,'L=',L,' i=',i,' ir=',ir,' npt=',npt(ir)
          go to 50
          endif
*------------------------------------------------
c pt are the original pointers, to be used for every methods
 
          np = n
 
          rangemin = dist(min(np,nmax))*0.5
          rangemax = dist(min(np,nmax))*1.5
*         rangemin = dist( max(3, min(np,nmax)/4) )
*         rangemax = dist(min(np,nmax))
*------------------------------------------------
 
c fit experimental variogram
 
 
        if (typvarRec) then
        call fitexpvarRec(L,typlag0,nlag,sdev)
        else
        call fitexpvar(L,typlag0,nlag,sdev)
        endif
 
*------------------------------------------------
c clean if requested and fit again exp. variograms
        if (cleanfac.lt.10.) then
        call cleanPoint (L,sdev*cleanfac,cleaned)
          if (cleaned) then
            if (typvarRec) then
            call fitexpvarRec(L,typlag0,nlag,sdev)
            else
            call fitexpvar(L,typlag0,nlag,sdev)
            endif
          endif
        endif
*------------------------------------------------
 
200     continue
 
c..........................
c krig evaluation is OK
c..........................
        typlagAvail(i) = typlags(2)
        ccAvail(1:nvar,i) = cc(1:nvar)
        rangeAvail(i) = range
        sdevAvail(i) = sdev
*       print *,'iav=',i,' typlag=',typlags(2),' cc=',cc(1:2),' range=',range
          do j = 1,nlag
            if (typlags(2).eq.typlag0(j)) typlagtss(j) = typlagtss(j) + 1
          enddo
        go to 300
 
c..........................
c krig evaluation is not OK => aver calculation
c..........................
100     continue
        nokrig = nokrig+1
50      typlagAvail(i) = -1.
c---------------------------------------------------------
c end of loop over requested pixels to cover
300   continue
#if defined linux
        call clockm(it1)
        tfit = tfit + (it1-it0)/1000.
#endif
      print *,'points to fit exp. variogram =',navail,' not fitted =',nokrig
      return
      end
*----------------------------------------------------
	subroutine fitexpvar (L,typlag0,nlag,sdevmin)
        use expvar_mod
	implicit none
 
        real*4 disfun, variance, distance,varfun
        real*4 varmax, var, varcalc, sdev, sdevmin, varerror
        real*8 dis1, ccmin(10)
	integer*4 i,j,k, m,n, ier,nlag
	real*4 typlag0(nlag),typlagmin, mincc/1.d-6/
	integer*4 L,L1,L2
        logical*4 allpos, ok
        real*8 dble
 
        real*4     mv /z'ffffffff'/
 
C------------------------------------------------
c evaluate varmax and range
 
        varmax = -1e30
        n = 0
        do j = 1,np-1
        L1 = pt(j)
          do m = j+1,np
          n = n+1
          L2 = pt(m)
          var= variance(L1,L2)
          y(n) = var
          x(n,1) = 1.
          b(n) = distance(L1,L2)
          w(n) = 1
            if (var.gt.varmax) then
            varmax = var
            range = min(max(b(n)*facvariogram,rangemin),rangemax)
            endif
          enddo
        enddo
 
#if defined DEBUGLAG
        print *,'L=',L,' np=',np,' range=',range,' varmax=',varmax
#endif
c------------------------------------------------------------
c variogram is zero => default lag model
       if (varmax.lt.varmin) go to 3000
 
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c loop over typlag
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        ok = .false.
        sdevmin=1e30
 
        do 1000 i = 1,nlag
 
        typlags(2) = typlag0(i)
 
c evaluate disfun
 
        do j = 1,n
        x(j,2) = disfun(b(j),typlags(2))
        enddo
 
c evaluate experimental variogram
 
        if (n.gt.nril) then
        print *,'program error in fitexp'
        print *,'n=',n,' nril=',nril
        stop
        endif
 
        call cormul (x,y,w,n,nril,bb,nvar,ier)
          if (ier.ne.0) go to 100
 
*       print *,'bb=',bb
 
        cc = 0.d0
        cc(1:nvar) = bb(1:nvar)
 
c---------------------------------------------
c verify that all coefficients are positive
 
        allpos = cc(1).ge.0.d0
 
          do j = 2,nvar
            if (cc(j).le.mincc) allpos = .false.
          enddo
 
          if (allpos) go to 200
 
c---------------------------------------------
c some coefficient < 0 or error from cormul
100     continue
c---------------------------------------------
c manage negative coefficients
c set nugget to 0.
 
*       print *,'L=',L,' cc=',cc(1:nvar)
 
        cc(1) = disfun(range,typlags(2))
        cc(2) = varmax
 
          do j = 1,n
          cc(1) = cc(1) + x(j,2)
          cc(2) = cc(2) + y(j)
          enddo
 
        cc(2) = cc(2)/cc(1)
        cc(1) = 0.d0
 
*       print *,'after: cc=',cc(1:nvar)
 
*----------------------------------------
c  coefficint are ok
200     continue
*----------------------------------------
c evaluate deviation  on variance
 
        sdev = 0.
 
          do j = 1,n
          var = y(j)
          varcalc = varfun(b(j))
          sdev = sdev + varerror(var,varcalc)**2
          enddo
 
        sdev = sqrt(sdev/n)
 
          if (sdev.lt.sdevmin) then
          ok = .true.
          sdevmin = sdev
          ccmin = cc
          typlagmin = typlags(2)
          endif
 
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
1000    continue
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        if (.not.ok) go to 3000
 
        cc = ccmin
        typlags(2) = typlagmin
*       print *,'L=',L,' typlagmin=',typlagmin,' cc=',cc(1:nvar)
        return
 
c--------------------------------------------------------------
3000    continue
        typlags(2) = typlag0(1)
        cc(1) = 0
        cc(2) = varmin/disfun(range,typlags(2))
        sdevmin = 0.
        return
        end
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	subroutine fitexpvarRec (L,typlag0,nlag,sdevmin)
        use expvar_mod
	implicit none
 
        real*4 disfun, variance, distance,varfun
        real*4 varmax, var, sdev, sdevmin, varcalc, varerror
        real*8 dis1, ccmin(ncoef)
	integer*4 i,j,k, m, ier,nlag
	real*4 typlag0(nlag),typlagmin, mincc/1d-6/
	integer*4 L,L1,L2
        logical*4 allpos, ok
 
        real*4     mv /z'ffffffff'/
 
C------------------------------------------------
c evaluate varmax and range
 
        varmax = -1e30
          do j = 1,np
          L1 = pt(j)
          var = variance(L1,L2)
          y(j) = var
          x(j,1) = 1.
          w(j) = 1
          b(j) = distrec(L1)
            if (var.gt.varmax) then
            varmax = var
            range = min(max(b(j)*facvariogram,rangemin),rangemax)
            endif
          enddo
 
#if defined DEBUGLAG
        print *,'L=',L,' np=',np,' range=',range,' varmax=',varmax
#endif
c------------------------------------------------------------
c variogram is zero => default lag model
        if (varmax.lt.varmin) go to 3000
 
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c loop over typlag
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        ok = .false.
        sdev = 1e30
        sdevmin = 1e30
 
        do 1000 i = 1,nlag
 
        typlags(2) = typlag0(i)
 
c evaluate disfun
 
        do j = 1,np
        x(j,2) = disfun(b(j),typlags(2))
        enddo
 
c evaluate experimental variogram
 
        call cormul (x,y,w,np,nril,bb,nvar,ier)
          if (ier.ne.0) then
#if defined DEBUGLAG
          print *,'L=',L,' np=',np,' range=',range,' varmax=',varmax
          print *,'typlag=',typlags(2)
            do j = 1,np
            L1 = pt(j)
            print *,j,'L1=',L1,' distrec=',distrec(L1),' x1=',x(j,1),' x2=',x(j,2),' y=',y(j)
            enddo
#endif
          go to 100
          endif
 
#if defined DEBUGLAG
        print *,'L=',L,' typlags=',typlags(2),'bb=',bb(1:nvar)
#endif
 
        cc = 0.d0
        cc(1:nvar) = bb(1:nvar)
 
c---------------------------------------------
c verify that all coefficients are positive
 
 
        allpos = cc(1).ge.0.d0
 
          do j = 2,nvar
            if (cc(j).le.mincc) allpos = .false.
          enddo
 
          if (allpos) go to 200
 
c---------------------------------------------
c some coefficient < 0 or error from cormul
100     continue
c---------------------------------------------
c manage negative coefficients
c set nugget to 0.
 
*       print *,'L=',L,' cc=',cc(1:nvar)
 
 
        cc(1) = disfun(range,typlags(2))
        cc(2) = varmax
 
          do j = 1,np
          cc(1) = cc(1) + x(j,2)
          cc(2) = cc(2) + y(j)
          enddo
 
        cc(2) = cc(2)/cc(1)
        cc(1) = 0.d0
 
#if defined DEBUGLAG
        print *,'after: cc=',cc(1:nvar)
#endif
 
*----------------------------------------
c  coefficint are ok
200     continue
 
*----------------------------------------
c evaluate deviation  on variance
 
        sdev = 0.
          do j = 1,np
          var = y(j)
          varcalc = varfun(b(j))
          sdev = sdev + varerror(var,varcalc)**2
          enddo
 
        sdev = sqrt(sdev/np)
 
          if (sdev.lt.sdevmin) then
          ok = .true.
          sdevmin = sdev
          ccmin = cc
          typlagmin = typlags(2)
          endif
 
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
1000    continue
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
        if (.not.ok) go to 3000
 
        cc = ccmin
        typlags(2) = typlagmin
        return
 
c------------------------------------------------
3000    continue
        typlags(2) = typlag0(1)
        cc(1) = 0
        cc(2) = varmin/disfun(range,typlags(2))
        sdevmin = 0.
        return
        end
c..........................................
        subroutine cleanPoint (L,sdev,cleaned)
        use findind_mod
        use expvar_mod
        implicit none
c---------------------------------------------------------
        integer*4, allocatable :: order(:)
        real*4 sdev, var, varcalc, varerror
        integer j,m, np0, L, L1,L2, npclean, npkrigmin /4/
        real*4 distance, varfun, variance
        logical cleaned
c---------------------------------------------------------
 
c verify points
c-------------------------------------------
 
        ip = .true.
        np0 = np
        pt0 = pt
 
c-------------------------------------------
        if (typvarRec) then
        do j = 1,np
        L1 = pt(j)
        var = variance(L1,L2)
        varcalc = varfun(distrec(L1))
        vardif(j) = varerror(var,varcalc)
        enddo
c-------------------------------------------
        else
        vardif = 0
        do j = 1,np
        L1 = pt(j)
          do m = 1,np
          L2 = pt(m)
          var = variance(L1,L2)
          varcalc = varfun(a(j,m))
          vardif(j) = max(varerror(var,varcalc),vardif(j))
          enddo
        enddo
        endif
c-------------------------------------------
        allocate (order(np))
        call sort_list(vardif,order,np)
          if (vardif(order(np)).lt.sdev) go to 200
 
*       print *,'L=',L,' sdev=',sdev
        npclean = min(np/2,np-npkrigmin)
          do j = 1,npclean
          m = order(np-j+1)
          ip(m) = vardif(m).lt.sdev
*         print *,'m=',m,' vardif=',vardif(m),' ip=',ip(m)
          enddo
c-------------------------------------------
 
        np = count(ip(1:np0))
 
*       print *,'np=',np,' ip=',ip
 
        if (np.eq.np0) go to 200
 
*       print *,'sdev=',sdev,' np=',np,' np0=',np0
 
c clean points
        m = 0
        do j = 1,np0
          if (ip(j)) then
          m = m+1
          pt(m) = pt0(j)
          endif
        enddo
 
        deallocate (order)
        cleaned = .true.
        return
 
c not cleaned
200     continue
        deallocate (order)
        cleaned = .false.
        return
        end
c..........................................
        subroutine distMatrix (L)
        use expvar_mod
        implicit none
        integer i,j,m,L,L1,L2
        real*4 distance
 
c evaluate distance matrix
 
          do j = 1,np-1
          L1 = pt(j)
          b(j) = distance(L1,L)
            do m = j+1,np
            L2 = pt(m)
            a(j,m) =distance(L1,L2)
            a(m,j) = a(j,m)
            enddo
          enddo
 
        b(np) = distance(pt(np),L)
 
          do j = 1,np+1
          a(j,j) = 0.d0
          enddo
 
        return
        end
