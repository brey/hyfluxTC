**#defien SymSolver
**#define DEBUGSOLVER
**#define DEBUGLAG
*--------------------------------------------
        module functions_mod
        implicit none
        real*4,    allocatable :: valold(:),valrec(:)
        real*8,    allocatable :: distrec(:)
        integer*4 ncoef
        parameter (ncoef=10)
        real*8 range, cc(ncoef)
c typlags(1) not used
        integer*4 typvar
        real*4 typlags(2:ncoef)
c nvar <= ncoef
        integer*4 nvar
        logical cleanabs, typvarlog, typvarRec
        end module functions_mod
*--------------------------------------------
        module expvar_mod
        use functions_mod
        implicit none
        real*8, allocatable :: a(:,:),b(:),AA(:),WS(:), a0(:,:),b0(:)
        real*4,    allocatable :: w(:),y(:),x(:,:)
 
	integer*4, allocatable :: pt(:), pt0(:), varord(:)
	real*4,    allocatable :: weight(:),weight0(:), dist(:),vardif(:)
	logical*1, allocatable :: ip(:)
 
	integer*4 nril, np, nmaxx
        real*4 facvariogram
        real*8 varmin/1.d-8/, bb(ncoef), rangemin, rangemax
        logical aver
	save nril, nmaxx
        end module expvar_mod
*--------------------------------------------
        module findind_mod
        implicit none
	integer*4, allocatable :: Lreq(:), Ireq(:), Lnear(:)
     &                , pt1(:),pt2(:),npt(:)
        logical*1, allocatable :: maskreq(:)
        logical nearest
        integer nmaxpt, nreq, ir
	save Lreq,Ireq,Lnear,  pt1,pt2, npt,nreq
        end module findind_mod
*--------------------------------------------
         module distance_mod
         implicit none
         integer*4,    allocatable :: ix(:),iy(:)
         real*4,    allocatable :: idist(:,:)
         save ix,iy,idist
         end module distance_mod
*--------------------------------------------
          function disfun (dis,typlag)
          use functions_mod
          implicit none
          real*4 disfun, ratio, d1,d2, ratio3, ratio4, esp
          real*4 ratiomin /1e-3/, typlag
          real*8 dis
 
          ratio = dis/range
c linear
             if (typlag.eq.1.) then
             disfun =  ratio
 
 
c power
             else if (typlag.lt.2.) then
             disfun =  ratio**typlag
 
c lin-log
             else if (typlag.eq.11.) then
               if (dis.le.1.d0) then
               disfun = dis
               else
               disfun = 1+log(dis)
               endif
             disfun = disfun / (1+log(range))
 
c spherical
             else if (typlag.eq.2.) then
               if (ratio.ge.1.) then
               disfun = 1.
               else
               disfun =  ratio*(1.5 - 0.5*ratio*ratio)
               endif
 
c linear & exponential
             else if (int(typlag).eq.3) then
c multiply by 1.5 in order to be consistent with spherical model
c (same derivative for ratio = 0 )
             ratio = 1.5*ratio
             ratio3 = typlag-3.
               if (ratio.le.ratio3) then
               disfun = ratio
               else
               d1 = 1-ratio3
               d2 = ratio-ratio3
               disfun = ratio3+d1*(1-exp(-d2/d1))
               endif
 
c linear & quasi-exponential
             else if (int(typlag).eq.4) then
c multiply by 1.5 in order to be consistent with spherical model
c (same derivative for ratio = 0 )
             ratio = 1.5*ratio
             ratio4 = typlag-4.
               if (ratio.le.ratio4) then
               disfun = ratio
               else
               d1 = 1-ratio4
               d2 = ratio-ratio4
               disfun = ratio4+d1*d2/(d1+d2)
               endif
 
c gaussian
             else if (typlag.eq.5.) then
c multiply by 1.5 in order to be consistent with spherical model
c (same derivative for ratio = 0 )
             ratio = 1.5*ratio
             disfun =  1.-exp(-ratio*ratio)
 
             else
             print *,'typlag=',typlag,' not considered'
#if defined linux
*             call error ('Error from disfun')
#endif
             call exit(1)
             endif
 
          end function disfun
*--------------------------------------------
          function varfun (dis)
          use functions_mod
          implicit none
          real*4 disfun, varfun
          real*8 dis
          integer i
 
          if (dis.eq.0.d0) then
          varfun = 0.d0
          endif
 
 
          varfun = cc(1)
            do i = 2,nvar
            varfun = varfun + disfun(dis,typlags(i))*cc(i)
            enddo
 
            if (typvarlog .and. varfun.ge.1.) then
            varfun = exp(varfun-1.)
            endif
 
          end function varfun
*--------------------------------------------
         function linexp(var)
         use functions_mod
         implicit none
         real*4 var,linexp
           if (typvarlog .and. var.gt.1.) then
           linexp = exp(var-1.)
           else
           linexp = var
           endif
         end function linexp
*--------------------------------------------
         function linlog(var)
         use functions_mod
         implicit none
         real*4 var,linlog
           if (typvarlog .and. var.gt.1.) then
           linlog = log(var) + 1.
           else
           linlog = var
           endif
         end function linlog
*--------------------------------------------
          function variance (L1,L2)
          use functions_mod
          implicit none
          real*4 variance, val1, val2
          real*4 minvalue /1./
          integer*4 L1,L2
c L1 is the pointer to the reference value
c L2 is the pointer to the simulated value
 
          if (typvarRec) then
          variance = 0.5*(valrec(L1)-valold(L1))**2
          else
          variance = 0.5*(valold(L2)-valold(L1))**2
          endif
 
              if (typvarlog.and.variance.gt.1.) then
              variance = 1 + log(variance)
              endif
 
          end function variance
*--------------------------------------------
        function distance (L1,L2)
        use distance_mod
        implicit none
        real*4 distance,dx,dy
        integer*4 L1,L2,idx,idy
        dx = ix(L1)-ix(L2)
        dy = iy(L1)-iy(L2)
        distance = sqrt(dx*dx+dy*dy)
*/*
*        idx = ix(L1)-ix(L2)
*        idy = iy(L1)-iy(L2)
*        distance = idist(idx,idy)
**/
 
        end function distance
*--------------------------------------------
	subroutine covervalstat (mask,val,beach,findbeach,nmax,nmaxexp
     &            ,nradius,idp,nval,ncol,nrow,recalc,method,nv
     &            ,cleanfac,filter,naver,ncover,typlag0,nlag,typvar0,typfilt
     &            ,param,existweight,weightval,dist1,pointer
     &            ,typlagval,typlagtss
     &            ,filtered,varval,tfit,tsolve)
        use expvar_mod
        use distance_mod
        use findind_mod
	implicit none
        real*4 distance,varfun, variance
        character message*20
 
	integer*4 nval,nmax,nradius,ncol,nrow, nv, method,nlag, bestlag
	logical*1 mask(nval)
        integer*1 filtered(nval)
	real*4 val(nval),weightval(nval), dist1(nval), typlagval(nval)
     &        ,varval(nval)
        real*4 idp, param(10),typlag0(nlag)
	integer*4 beach(nval),typlagtss(nlag),BoundVal
        character pointer*(*)
        logical findbeach, recalc, test,fit, existweight, krig
 
c local dynamic variables
 
	logical*1, allocatable :: maskval(:), maskvalold(:)
        integer*4, allocatable :: Lavail(:),Iavail(:)
        real*4, allocatable :: typlagAvail(:),ccAvail(:,:),rangeAvail(:),sdevAvail(:)
 
c local variables
 
	logical*4 first /.true./, ok
        logical multifit, globalexpvar
	real*4 vaL1, sumweight, tfit,tsolve
	integer*4 ivaL1, i, j, m,n, ier, np0, nmaxexp, nmaxpun, iav, navail, rmax
	integer*4 L,L1,L2,m1,j1, nreq0, nradius0,it0,it1, typvar0
     &           ,iun, length, naver, ncover, typfilt, npkrigmin /3/, negb
 
	equivalence (vaL1,ivaL1)
 
	real*4     mv /z'ffffffff'/
	integer*4  mv4, imv /z'ffffffff'/
	integer*1  mv1
 
c---------------------------------------------
c krig variables and functions
c
        real*4  cleanfac , filter
        real*8 minvar/0.001/
        logical clean ,exist, filtval, linfilt, cleaned
c---------------------------------------------
	save first, nmaxpun, navail
c---------------------------------------------
 
        facvariogram = 0.95
 
        fit  = method .eq. 1
        krig = method.eq.2 .or. method.eq.3
        typvarRec = method.eq.3
        clean = cleanfac .lt. 10.
        filtval = filter.lt.10
 
c-------------------------------------------------------
        linfilt = .true.
        cleanabs = .true.
        BoundVal = 2
 
        bestlag = 0
        multifit = bestlag .ge. 1
        bestlag = max(bestlag,1)
        nearest = .true.
c-------------------------------------------------------
 
        nvar = 2
 
        typvar = typvar0
        typvarlog = typvar.eq.2
 
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	if (.not.first) goto 400
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
	call getmv1(mv1)
	call getmv4(mv4)
	allocate (maskval(nval),maskvalold(nval),maskreq(nval)
     &           ,valrec(nval),valold(nval),distrec(nval)
     &           ,ix(nval),iy(nval))
 
 
        L=0
          do j = 1,nrow
            do i = 1,ncol
            L = L+1
            ix(L) = i
            iy(L) = j
            enddo
          enddo
 
c define mask of the available values: mask is true for TRUE VALUES
 
	  do L = 1,nval
	  vaL1 = val(L)
	  maskval(L) = ivaL1 .ne. imv
            if (existweight.and.weightval(L).eq.0.) maskval(L) = .false.
	  enddo
 
 
c-------------------------------------------------------------------
c count no. of requested and available values
 
        maskreq = mask.or.maskval
 
	nreq = count(maskreq)
        navail = count(maskval)
 
        print *,'nmask=',count(mask),' navail=',navail,' nreq=',nreq,' nval=',nval
 
c assign indeces
 
	allocate (Lreq(nreq),Ireq(nval))
 
        Lreq = -1
        Ireq = -1
 
	i = 0
	do L = 1,nval
	  if (maskreq(L)) then
	  i = i+1
	  Lreq(i)=L
          Ireq(L)=i
          endif
	enddo
 
c------------------------------------------------------------------
c read pointers to neighboroud (if exist)
 
      exist = .false.
 
      if (pointer.ne.' ' ) then
      inquire (file=pointer,exist=exist)
        if (exist) then
        call iunfree (iun)
        open (iun,file=pointer,form='unformatted')
        print *,'Read file ',pointer(1:length(pointer))
        nreq0=0
        read (iun,err=99) nreq0,nmaxpt,nradius0
99        if (nreq0.ne.nreq.or.nradius.ne.nradius0) then
          exist = .false.
          print *,'nreq=',nreq,' nreq0=',nreq0
          print *,'file ',pointer(1:length(pointer)), ' first record is wrong'
          else
          allocate (pt1(nmaxpt),pt2(nreq),npt(nreq))
          read (iun,err=97) pt1,pt2,npt
          endif
        close (iun)
        else
        print *,'pointer file ',pointer(1:length(pointer)),' not exist'
        print *,'I will create for you'
        endif
      endif
 
      go to 98
 
c error in reading pointer file
97    continue
      deallocate (pt1,pt2,npt)
      exist = .false.
      close (iun)
      print *,'file ',pointer(1:length(pointer)), ' second record is wrong'
 
98    continue
c construct pointers to neighboroud
      if (.not.exist) then
      call findindexstat (maskval,nval,nradius,ncol,nrow,nmax)
 
c write pointers to neighboroud (if requested)
        if (pointer.ne.' ') then
        call iunfree (iun)
        open (iun,file=pointer,form='unformatted')
        write (iun) nreq,nmaxpt,nradius
        write (iun) pt1(1:nmaxpt),pt2,npt
        close (iun)
        print *,'Created file ',pointer(1:length(pointer))
        endif
      endif
 
c-------------------------------
c find nearest , verify pointers , find nmaxexp, find max radius
 
	allocate (Lnear(nval))
        Lnear = -1
        nmaxexp = 0
 
        rmax = 0
	do i = 1,nreq
c find neareset
        L = Lreq(i)
        L1 = pt1(pt2(i)+1)
        Lnear(L) = L1
        nmaxexp = max(nmaxexp,npt(i))
 
c verify pointers
          do j = 1,npt(i)
          L1 = pt1(pt2(i)+j)
          rmax = max(abs(ix(L1)-ix(L)),abs(iy(L1)-iy(L)),rmax)
            if (.not.maskval(L1)) then
            print *,'The pointers are wrong'
            exist = .false.
            deallocate (pt1,pt2,npt,Lnear)
            go to 98
            endif
          enddo
        enddo
c----------------------------------------
      print *,'From pointer: rmax=',rmax
      allocate (idist(-rmax:rmax,-rmax:rmax))
        do i = -rmax, rmax
          do j = -rmax, rmax
          idist(i,j) = sqrt(float(i*i + j*j))
          enddo
        enddo
c----------------------------------------
      if (method.eq.2) then
      nril = min((nmaxexp*(nmaxexp-1))/2 + 1,300)
      nril = max(nril,nmaxexp)
      nmaxpun = (1.+sqrt(1.+8.*nril))*0.5+1
      nril = (nmaxpun*(nmaxpun-1))/2 + 1
 
      else if (method.eq.3) then
      nmaxpun = max(min(nmaxexp,300),nmax)
      nril = nmaxpun+1
 
      else
      nmaxpun = nmax
      nril = nmax+1
      endif
 
      allocate (x(nril,max(nv,nvar,bestlag+1)),w(nril))
      allocate (y(nril),b(nril),b0(nril))
 
          if (krig) then
          nmaxx = nmaxpun+1
          allocate (a(nmaxx,nmaxx),ip(nmaxx),a0(nmaxx,nmaxx)
     &             ,AA(3+nmaxx*(nmaxx+1)/2),WS(nmaxx) )
          endif
c------------------------------------------------------------------
        if (krig) then
        allocate (Lavail(navail),Iavail(nval))
        allocate (typlagAvail(navail),ccAvail(nvar,navail),rangeAvail(navail)
     &           ,sdevAvail(navail))
 
        Lavail = -1
        Iavail = -1
 
        i = 0
        do L = 1,nval
          if (maskval(L)) then
          i = i + 1
          Lavail(i) = L
          Iavail(L) = i
*         print *,'L=',L,' iavail=',i
          endif
        enddo
        endif
c------------------------------------------------------------------
      print *,'nmax=',nmax,' nmaxexp=',nmaxexp
      print *,'nril=',nril,' nmaxpun=',nmaxpun,' nmaxx=',nmaxx
c------------------------------------------------------------------
      allocate (pt(nmaxpun),pt0(nmaxpun),weight(nmaxpun),weight0(nmaxpun)
     &         ,dist(nmaxpun),vardif(nmaxpun),varord(nmaxpun))
 
      first = .false.
 
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c   jump here after the first call
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
400	continue
        tfit =0
        tsolve = 0
        typlagtss = 0
 
        valold = val
 
c---------------------------------------------------------
c define mask of the available values: mask is true for TRUE VALUES
 
	  do L = 1,nval
	  vaL1 = val(L)
	  maskval(L) = ivaL1 .ne. imv
            if (existweight.and.weightval(L).eq.0.) maskval(L) = .false.
            if (maskval(L)) filtered(L) = 0
	  enddo
 
          maskvalold = maskval
 
 
c---------------------------------------------------------
        aver = method .eq. 0
        fit  = method .eq. 1
        krig = method .ge. 2
 
        ncover = 0
        naver = 0
 
*       print *,'recalc=',recalc,' aver=',aver,' fit=',fit,' krig=',krig
 
c---------------------------------------------------------
c call if filtering or cross kriging is requested
          if (typvarRec.or.filtval) then
          call recalcValues (valold,maskvalold,distrec,valrec,nval
     &                      ,weightval,existweight
     &                      ,pt,dist,weight,nmax,nradius,idp)
          endif
c---------------------------------------------------------
c call if filtering is requested
          if (filtval) then
          call newmaskval (valold,valrec,nval
     &                    ,filter,maskvalold,maskval
     &                    ,filtered,typfilt,linfilt)
          endif
c---------------------------------------------------------
c call if kriging is requested
c evaluate cc, typlags, range  for each nearested requested point
 
        if (krig) then
        call expVariogram(cleanfac,typlag0,nlag, tfit
     &             ,Lavail,typlagAvail,ccAvail,rangeAvail,sdevAvail
     &             ,navail,typlagtss
     &             ,nmax,nmaxpun,nradius
     &             ,maskval,nval)
        endif
c---------------------------------------------------------
c start of loop over requested pixels to cover
c---------------------------------------------------------
 
	do 300 i = 1,nreq
*       if (i.le.2) print *,'i=',i
c---------------------------------------------------------
	L = Lreq(i)
c---------------------------------------------------------
            if (.not.mask(L)) go to 300
        aver = method .eq. 0
c---------------------------------------------------------
        cc = mv
        nvar = 2
c---------------------------------------------------------
        n = 0
 
          if (.not.maskval(L).and..not.recalc) then
            do j = 1,npt(i)
            L1 = pt1(pt2(i)+j)
              if (maskval(L1)) then
              n = n+1
              pt0(n) = L1
              dist(n) = distance(L1,L)
                if (dist(n).gt.float(nradius).or.n.ge.nmax) go to 10
              endif
            enddo
 
          else if(maskvalold(L).and.recalc) then
            do j = 2,npt(i)
            L1 = pt1(pt2(i)+j)
              if (maskval(L1)) then
              n = n+1
              pt0(n) = L1
              dist(n) = distance(L1,L)
                if (dist(n).gt.float(nradius).or.n.ge.nmax) go to 10
              endif
            enddo
 
 
          else
            if(maskvalold(L)) then
            varval(L) = 0.
            dist1(L) = 0.
            typlagval(L) = 0
            endif
          go to 300
          endif
 
*------------------------------------------------
10        continue
 
 
c pt0 are the original pointers, to be used for every methods
c pt are the current pointers, that can be cleaned (or not cleaned)
 
          np0 = n
          np = np0
          pt = pt0
 
c          print *,'L=',L,' maskval=',maskval(L),' npt=',npt(i),' np=',np
 
 
*------------------------------------------------
          if (np.le.0) then
          findbeach = .true.
          beach(L) = beach(L) + 1
          varval(L) = mv
          dist1(L) = mv
          typlagval(L) = mv
          go to 300
          endif
*------------------------------------------------
          rangemin = dist(min(np,nmax))*0.75
          rangemax = dist(min(np,nmax))
#if defined DEBUGDIST
         print *,'L=',L,' np=',np,' rangemin=',rangemin,' rangemax=',rangemax
#endif
*------------------------------------------------
          ncover = ncover + 1
*------------------------------------------------
          dist1(L) = 0.
          sumweight = 0.
 
            do 20 j = 1,np
            weight(j) = 1/max(dist(j),0.01)**idp
            L1 = pt(j)
              if (existweight) weight(j)=weight(j)*weightval(L1)
            dist1(L) = dist1(L) + dist(j)*weight(j)
            sumweight = sumweight + weight(j)
20          continue
 
         dist1(L) = dist1(L)/sumweight
 
         weight0 = weight
c---------------------------------------------------------
        if (fit.and.(np.le.nv)) aver = .true.
c---------------------------------------------------------
        if (method.ge.2.and.(np.le.npkrigmin)) aver = .true.
c---------------------------------------------------------
        if ((.not.aver).and.fit) then
        np = min(np,nmax)
          do j = 1,np
          L1 = pt(j)
          x(j,1) = 1.
          x(j,2) = ix(L1) - ix(L)
          x(j,3) = iy(L1) - iy(L)
           if (nv.eq.6) then
           x(j,4) = x(j,2)*x(j,3)
           x(j,5) = x(j,2)*x(j,2)
           x(j,6) = x(j,3)*x(j,3)
           endif
          y(j) = valold(L1)
          w(j) = weight(j)
          enddo
        call cormul (x,y,w,np,nril,bb,nv,ier)
          if (ier.eq.0) then
          val(L) = bb(1)
*/*
*          val(L) = min(max(val(L)
*     &            ,minval(y(1:np)) ),maxval(y(1:np)) )
**/
          aver = (val(L).lt.minval(y(1:np))
     &    .or.val(L).gt.maxval(y(1:np)) )
 
          else
          print *,'FIT: error for cell ',l
          aver = .true.
          endif
 
        endif
c---------------------------------------------------------
c ordinary & cross kriging
c---------------------------------------------------------
        if ((.not.aver).and.krig) then
c---------------------------------------------------------
        np = min(np,nmax)
 
c load variogram function
 
        L1 = Lnear(L)
        iav = Iavail(L1)
*       print *,'L=',L,' L1=',L1,' iav=',iav
 
        typlags(2) = typlagAvail(iav)
        cc(1:nvar) = ccAvail(1:nvar,iav)
        range = rangeAvail(iav)
 
*       print *,'typlag=',typlags(2),' cc=',cc(1:2),' range=',range
 
          if (typlags(2).eq.-1.) then
          message = 'in fitting exp. var.'
          go to 100
          endif
 
c evaluate distance matrix
 
        call distMatrix (L)
 
c---------------------------------------------------------
          if (clean) then
          call cleanPoint (L,sdevAvail(iav)*cleanfac,cleaned)
            if (cleaned) then
c evaluate again distance matrix
            call distMatrix (L)
            endif
          endif
c---------------------------------------------------------
 
#if defined DEBUGSOLVER
        print *
        print *,'L=',L,' typlags=',typlags(2),' cc=',cc(1:2)
     &         ,' range=',range
        print *
        do j = 1,np
        print '(i3,a,g9.2,a,100(g9.2))',j,' b=',b(j),' a=',(a(j,m),m=1,np)
        enddo
#endif
c..........................................
c update matrix with exp variogram
 
        do j = 1,np
        b(j) = varfun(b(j))
          do m = j,np
          a(j,m) = varfun(a(j,m))
          a(m,j) = a(j,m)
          enddo
        enddo
 
c..........................................
c set last column and row
        do j =1,np
        a(j,np+1) = 1.d0
        a(np+1,j) = 1.d0
        enddo
 
        a(np+1,np+1) = 0.d0
        b(np+1) = 1.d0
 
#if defined DEBUGSOLVER
        print *
        do j = 1,np+1
        print '(i3,a,g9.2,a,100(g9.2))',j,' b=',b(j),' a=',(a(j,m),m=1,np+1)
        enddo
#endif
c-----------------------------------------------------------
#if defined linux
        call clockm(it0)
#endif
c..........................................
 
c evaluate Krig weights
 
        y = b
c-----------------------------------------
#if defined SymSolver
c load compact matrix
        n = 0
        do j = 1,np+1
          do m = j,np+1
          n = n+1
          AA(n) = a(j,m)
          enddo
        enddo
c factor the symmetric matrix
        call sfact (AA,np+1,WS)
c solve the factored matrix
        WS = b
        call ssolve (WS,AA,b)
        b = WS
c-----------------------------------------
#else
        a0=a
        b0=b
        call simeq (a,b,np+1,nmaxx,ier)
        message ='from simeq'
          if (ier.ne.0) then
          print *
        print *,'The varigram are flat'
        print *,'typlag=',typlags(2),' cc=',cc(1:2),' range=',range
            do j = 1,np+1
            print '(i3,a,g9.2,a,100(g9.2))'
     &              ,j,' b=',b(j),' a=',(a(j,m),m=1,np+1)
            enddo
          print *,'No problem: I use the inverse distance method'
          go to 100
          endif
#endif
c..........................................
        negb = count(b(1:np).lt.0.d0)
#if defined DEBUGSOLVER
        if (negb.gt.0) then
*       print '(a,100(g10.2))',' b=',(b(j),j=1,np+1)
        print *,'L=',L,' Negb=',negb,' np=',np
        endif
#endif
c..........................................
 
#if defined linux
        call clockm(it1)
        tsolve = tsolve + (it1-it0)/1000.
#endif
c..........................................
c Manage negative b: set to 0 the negative values and recalculate weights
c-----------------------------------------
          if (BoundVal.eq.2) then
          if (negb.gt.0) then
          b(1:np) = max(b(1:np),0.d0)
          sumweight = 0.d0
            do j = 1,np
            sumweight = sumweight + b(j)
            enddo
          sumweight = 1.d0/sumweight
            do j = 1,np
            b(j) = b(j) * sumweight
            enddo
          endif
          endif
 
c evaluate Interpolated value and associateted variance
	  val(L) = 0.
          varval(L) = 0.
	    do j = 1,np
            L1 = pt(j)
	    val(L) = val(L) + valold(L1)*b(j)
            varval(L) = varval(L) + y(j)*b(j)
            y(j) = valold(L1)
	    enddo
 
          varval(L) = sqrt(max(varval(L) + b(np+1),0.d0))
 
          if (BoundVal.eq.0) then
            if (val(L).lt.minval(y(1:np))
     &      .or.val(L).gt.maxval(y(1:np)) ) go to 100
 
          else if (BoundVal.eq.1) then
          val(L) = min(max(val(L)
     &            ,minval(y(1:np)) ),maxval(y(1:np)) )
          endif
 
c..........................
c krig evaluation is OK
c..........................
        typlagval(L) = typlags(2)
        go to 300
 
c..........................
c krig evaluation is not OK => aver calculation
c..........................
100     continue
*       print *,'L=',L,' error ',message
        np = np0
        pt = pt0
        weight = weight0
        cc = mv
        aver = .true.
        endif
c---------------------------------------------------------
        if (aver) then
        naver = naver + 1
        np = min(np,nmax)
 
       sumweight = 0
 
	val(L) = 0.
	  do j = 1,np
          L1 = pt(j)
	  val(L) = val(L) + valold(L1)*weight(j)
          sumweight = sumweight + weight(j)
	  enddo
 
        val(L) = val(L) / sumweight
        varval(L) = 0.
        typlagval(L) = 0
        endif
c---------------------------------------------------------
c end of loop over requested pixels to cover
300   continue
c---------------------------------------------------------
	do i = 1,nval
	  if (.not.mask(i)) then
*         val(i) = mv
          dist1(i) = mv
          varval(i) = mv
          typlagval(i) = mv
          endif
	enddo
c/*
c        if (method.ge.2) print *,'naver=',naver,' ncover=',ncover
c     &                ,' perc. ', naver/float(ncover)*100.
c*/
	return
	end
*-------------------------------------------------------------
        subroutine newmaskval (valold,valrec,nval,filter
     &            ,maskvalold,maskval,filtered,typfilt,linfilt)
        use findind_mod
        implicit none
        integer nval, typfilt
        logical linfilt
        real*4 valold(nval),valrec(nval),filter
        logical*1 maskvalold(nval),maskval(nval)
        integer*1 filtered(nval)
c
c  local variables
c
        integer*4,    allocatable :: ip(:),iavail(:)
        real*4 valmin0/2./, valmin, rmse, avg, err, trans1, trans2
        integer n,i, L, L1, n1, j, m
 
c---------------------------------
c evaluate avg and pointers of masked values
 
        allocate (ip(nreq),iavail(nreq))
 
c-----------------------------------------
c evaluate pointers
        n = 0
          do i = 1,nreq
          L = Lreq(i)
            if (maskvalold(L)) then
            n = n+1
            ip(n) = L
            iavail(n) = i
            endif
          enddo
c---------------------------------------
c evaluate err and filter values
 
          do j = 1,n
          L = ip(j)
          i = iavail(j)
 
c evaluate local rmse
          n1 = 0
          rmse = 0
          avg = 0
            loopnp: do m = 1,npt(i)
            L1 = pt1(pt2(i)+m)
              if (maskvalold(L1)) then
              n1 = n1+1
              rmse = rmse + (valold(L1)-valrec(L1))**2
              avg = avg + valold(L)
              endif
            enddo loopnp
 
          avg = abs(avg)/n1
          valmin = max(avg/100,valmin0)
 
c evaluate local trans
          trans2 = max(rmse/n1,valmin*valmin)
          trans1 =  sqrt(trans2)
 
c evaluate err and make filter
            if (valold(L)*valrec(L).le.trans2) then
            err = (valrec(L)-valold(L))/trans1
            else
              if (linfilt) then
                if (abs(valrec(L)).gt.abs(valold(L))) then
                err = (valrec(L)-valold(L))/valrec(L)
                else
                err = (valrec(L)-valold(L))/valold(L)
                endif
              else
              err = (log(valrec(L)/valold(L)))
              endif
            endif
 
c typfilt is 0
          if (typfilt.eq.0) then
            if (abs(err).gt.filter) then
*           print *,'L=',L,' trans1=',trans1,' err=',err,' valold=',valold(L)
*    &              ,' valrec=',valrec(L)
            filtered(L) = 1
            maskval(L) = .false.
            else
            filtered(L) = 0
            endif
 
c typfilt is -1 => remove under recalculated stations
          else if (typfilt.lt.0) then
            if (err.lt.-filter) then
*           print *,'L=',L,' trans1=',trans1,' err=',err,' valold=',valold(L)
*    &              ,' valrec=',valrec(L)
            filtered(L) = 1
            maskval(L) = .false.
            else
            filtered(L) = 0
            endif
 
c typfilt is 1 => remove over recalculated stations
          else if (typfilt.gt.0) then
            if (err.gt.filter) then
*           print *,'L=',L,' trans1=',trans1,' err=',err,' valold=',valold(L)
*    &              ,' valrec=',valrec(L)
            filtered(L) = 1
            maskval(L) = .false.
            else
            filtered(L) = 0
            endif
 
          endif
 
 
          enddo
 
        deallocate (ip,iavail)
 
        return
        end
