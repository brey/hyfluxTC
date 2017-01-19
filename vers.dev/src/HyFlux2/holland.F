*================================================================================
        module holland_data
*================================================================================
        use kind
        use parameter_data
        real(r8) xhc, yhc, b, rmax, deltap, vtx, vty, k, x, bb0 /1.2/, rrmax0 /20e3/
     &          ,maxR /500e3/, expratio0, hsurgeMin /1.d0/  
        real(r4), allocatable :: timeHol(:), xhcHol(:), yhcHol(:), bHol(:), rmaxHol(:), deltapHol(:)
     &          ,kHol(:), vtxHol(:), vtyHol(:)
        integer(i4) nvalue
        end module holland_data

*================================================================================
        subroutine holland(xcell,ycell,coslat,fcor,vx,vy,hv,man2,sx,sy,u10,deltapn)
*================================================================================
c INPUT: xcell, ycell, coslat, fcor, hv, vx,vy !  (vx,vy) == water velocity
c OUTPUT: sx, sy    
c---------------------------------------------------------------------
c indice hc --> centro dell hurricane
c indice cell --> punto ad una distanza r dal centro --> cella (xcell,ycell)
c--------------------------------------------------------------------
c LAT LONG CONVERSION: dlong (°) and dlat (°) converted in m
c --> dlat(m) = dlat(°)*pigr*radius/180 = dlat(°)*deg2m
c --> dlong(m) = (dlong(°)*pigr*radius(m)/180)*cos(latm)= dlong(°)*deg2m*cos(latm))
c wehre: deg2rad = pi/180
c        deg2m = deg2rad*radius0
c        latm = (lat1+lat2)/2
c-------------------------------------------------------------------
        use holland_data
 
        implicit none
        real(r8) xcell, ycell, vx, vy, hv, sx, sy, coslat
        real(r4) fcor, u10, deltapn, man2

        real(r4) km, beta, cdrag, cu, cw, sinb, cosb 
        real(r4) coscellm, costeta, sinteta
        real(r4) dycell, dxcell,rcell 
        real(r4) pc, ratio, expratio, pr, dpr, dpx, dpy
        real(r4) vg10, vg10x, vg10y, u10x, u10y, vtr
        real(r4) spx, spy, sux, suy
        integer(i4) l, ideb /0/
*        real(r4) pn/1010e2/, mindp/0./, maxrcell /40 000e3/, minexpratio/0./
        real(r4) pn/1010e2/, mindp/0.01/, maxrcell /5 000e3/, minexpratio/1e-4/
         
c-------------------------------------------------------------------
c RADIUS: distance between hurricane CENTRE(hc) and CELL (rcell)
c-------------------------------------------------------------------
        coscellm = cos(deg2rad*(yhc+ycell)/2)
        dycell = (ycell-yhc)*deg2m
        dxcell = (xcell-xhc)*deg2m*coscellm
        rcell = sqrt((dxcell)**2+(dycell)**2)
        rcell = max(rcell,1000.d0)  
        costeta = dxcell/rcell
        sinteta = dycell/rcell 
        
c-------------------------------------------------------------------
c SURFACE PRESSURE GRADIENT: dpx and dpy   
c-------------------------------------------------------------------
        ratio=(rcell-rmax)/(maxR-rmax)
        x=0.5 + min(max(ratio,0.),1.)*k

        ratio = (rmax/rcell)**b  
        expratio = exp(-ratio)     
        expratio0 = 1-exp(-(rrmax0/rcell)**bb0)     
        pc = pn - deltap         
        pr = pc + deltap*expratio
        deltapn = (pn-pr)/100
          if (deltapn.lt.mindp.or.rcell.gt.maxrcell.or.expratio.lt.minexpratio) then
          sx = 0
          sy = 0
          u10 = 0
          deltapn = 0
          return
          endif
        dpr = deltap*b*ratio*expratio/rcell  
        dpx = dpr*costeta
        dpy = dpr*sinteta

c-------------------------------------------------------------------
c 10 WIND FIELD:  10m=km*vg+vt      
c-------------------------------------------------------------------
c holland gradient velocity                     
        vg10 = ((b/rhoa)*ratio*deltap*expratio+((rcell*abs(fcor)/2)**(1/x)))**x-(rcell*abs(fcor)/2)

         
*************************************************
*c v10m x,y component: CON INFLOW ANGLE CORRECTION     
*c NH:
*c vx = v * cos(teta+90+beta) = - v*(sin(teta)*cos(beta)+cos(teta)*sin(beta))
*c    = - v*(dy/r*cos(beta)+dx/r*sin(beta))
*c vy = v * sin(teta+90+beta) = v*(cos(teta)*cos(beta)-sin(teta)*sin(teta)) 
*c    = v*(dx/r*cos(beta)-dy/r*sin(beta))
*c SH:
*c vx = v * cos(teta+270-beta) = v*(sin(teta)*cos(beta)-cos(teta)*sin(beta)
*c    = v*(dy/r*cos(beta)-dx/r*sin(beta))
*c vy = v * sin(teta+270-beta) = - v*(cos(teta)*cos(beta)+sin(beta)*sin(teta))
*c    = -v*(dx/r*cos(beta)+dy/r*sin(beta))
*
*c inflow angle correction 
        if (rcell.gt.(rmax*1.2)) then
           beta = 25 
        else if (rcell.gt.rmax) then
           beta = 85 * rcell/rmax - 65
        else 
           beta = 10 * rcell/rmax   
        endif 

      beta = beta*deg2rad
      cosb = cos(beta)
      sinb = sin(beta)
        if (yhc.gt.0) then
            vg10x = - vg10*(sinteta*cosb+costeta*sinb)
            vg10y = + vg10*(costeta*cosb-sinteta*sinb)    
        else
            vg10x = + vg10*(sinteta*cosb-costeta*sinb)   
            vg10y = - vg10*(costeta*cosb+sinteta*sinb)          
        endif  
*************************************************        
                            
c 10m wind field  -->  u10 = k * vg + vt
c with vt*exp(-ratio) the translational effect desappear for high distance from the cyclone center                  
        u10x = vg10x + vtx*expratio0 - vx
        u10y = vg10y + vty*expratio0 - vy
        u10 = sqrt((u10x)**2+(u10y)**2)
        vtr = expratio0*sqrt(vtx**2+vty**2)

        if (abs(vg10-u10).gt.10+vtr.and.ideb.lt.100.or.u10.le.0.or.deltapn.le.0) then
        ideb=ideb+1
        write (iout,*)'xcell=',xcell,' ycell=',ycell,' rcell=',rcell,' expratio=',expratio
        write (iout,*)'vg10=',vg10,' u10=',u10,' hv=',hv,' deltapn=',deltapn
        write (iout,*)'vg10x=',vg10x,' vtx=',vtx,' vx=',vx,' u10x=',u10x 
        write (iout,*)'vg10y=',vg10y,' vty=',vty,' vy=',vy,' u10y=',u10y 
        endif

c-------------------------------------------------------------------
c SOURCE TERMS   S = Sp - Su -->!!!senza la g
c-------------------------------------------------------------------
c Pressure Surge
        spx=dpx/rhow
        spy=dpy/rhow
                  
c Wind Friction (drag coeff: Garratt, 1977,  Donelan 2004)
*        cdrag=min(0.75+0.067*u10,2.5)*0.001
        cdrag=min(0.75+0.067*u10,cdmax,max(4.34-0.061*u10,0.5))*0.001

        cu = rhoa*cdrag/rhow*hv/max(hv,hsurgeMin)
        cw = g*man2/max(hv,hvel0)**onethird

c wind frinction must be lower than bottom friction 

        cu = min(cu,cw) 

        sux=cu*u10*u10x        
        suy=cu*u10*u10y

c Source Term multiplied by g*h
        sx=hv*spx-sux
        sy=hv*spy-suy
*        sx=hv*spx-sux*min(hv/min(hv,100.d0),10.d0)
*        sy=hv*spy-suy*min(hv/min(hv,100.d0),10.d0)
*        sx=hv*spx-sux*min(hv/min(hv,50.d0),100.d0)
*        sy=hv*spy-suy*min(hv/min(hv,50.d0),100.d0)
           
        return
        end

*================================================================================
        subroutine readHolland
*================================================================================
        use holland_data
        use statistics_data
        implicit none

        integer(i4) nmax, nh, nvalue0, l,  length
        integer(i4) itime, ixhc, iyhc, ib, irmax, ideltap, ivtx, ivty, ik
        parameter (nmax=30)
        character buff*1024, header*1024, names(nmax)*30, message*80, msg*3
        real(r4) numget


        open(iinp,file=hollandFile)
        read (iinp,'(a)') header
        call nocr(header)

	call proctab (header,names,nmax,nh)

* time xhc yhc b                k             rmax           deltap            vmax0      vmaxknvtnco              vtr             vtrx             vtry             bias             rmse 

        itime=0; ixhc=0; iyhc=0; ib=0; irmax=0; ideltap=0; ik=0; ivtx=0; ivty=0 

        do l =1,nh
           if (names(l).eq.'time') then
           itime = l
           else if (names(l).eq.'xhc') then
           ixhc = l
           else if (names(l).eq.'yhc') then
           iyhc = l
           else if (names(l).eq.'k') then
           ik = l
           else if (names(l).eq.'vtrx') then
           ivtx = l
           else if (names(l).eq.'vtry') then
           ivty = l
           else if (names(l).eq.'b') then
           ib = l
           else if (names(l).eq.'rmax') then
           irmax = l           
           else if (names(l).eq.'deltap') then
           ideltap = l           
           endif
        enddo

        if (itime*ixhc*iyhc*ib*irmax*ideltap*ivtx*ivty*ik.eq.0) then
           write (iout,*) 'header=',header(1:length(header))
           write (iout,*) (names(l)(1:length(names(l))),l=1,nh)
           write (errmsg,*)'from hollandFile: nh=',nh,' itime=',itime,' ixhc=',ixhc,' iyhc=',iyhc,' ib=',ib
     &       ,' irmax=',irmax,' ideltap=',ideltap,' ivtx=',ivtx,' ivty=',ivty,' ik=',ik
           call logout (2,errmsg)
        endif

	      nvalue = 0
        do while (.true.)
           read (iinp,'(a)',end=101)  buff
           call nocr(buff)
           nvalue = nvalue+1
        enddo
 
101     continue
        write(iout,*) 'from ',hollandFile(1:length(hollandFile)),' nvalue=',nvalue
        if (nvalue.le.0) then
           message= 'nvalue must be > 1'
           call logout(2,message)
        endif

        allocate (timeHol(nvalue), xhcHol(nvalue), yhcHol(nvalue), bHol(nvalue), rmaxHol(nvalue), deltapHol(nvalue)
     &           ,kHol(nvalue),vtxHol(nvalue),vtyHol(nvalue))

        rewind (iinp)
        read (iinp,'(a)') header

        do l = 1,nvalue
           read (iinp,'(a)')  buff
           call nocr(buff)
           call proctab (buff,names,nmax,nh)
           timeHol(l) = numget(names(itime),msg)*3600
           xhcHol(l) = numget(names(ixhc),msg)
           yhcHol(l) = numget(names(iyhc),msg)
           bHol(l) = numget(names(ib),msg)
           rmaxHol(l) = numget(names(irmax),msg)
           deltapHol(l) = numget(names(ideltap),msg)
           kHol(l) = numget(names(ik),msg)
           vtxHol(l) = numget(names(ivtx),msg)
           vtyHol(l) = numget(names(ivty),msg)
        enddo

        close(iinp)

        return
        end

*================================================================================
        subroutine setHolland
*================================================================================
        use holland_data
        use statistics_data
        implicit none
        real(r4), save :: t0, t1, xhc0, xhc1, yhc0, yhc1, b0,b1, rmax0, rmax1, deltap0, deltap1, k0, k1
        real(r4), save :: vtx0, vtx1, vty0, vty1, icurr
        real(r4), save ::  fact
        logical first /.true./

c------------------------------------------------------------------------
c legge i valori della prima scheda
        if (first) then
            icurr = 1
            t1 = timeHol(1)
            t0 = t1-1        
            xhc1 = xhcHol(1)
            yhc1 = yhcHol(1)
            b1 = bHol(1)
            rmax1 = rmaxHol(1)
            deltap1 = deltapHol(1)
            k1 = kHol(1)
            vtx1 = vtxHol(1)
            vty1 = vtyHol(1)
      
            first = .false.
        endif
c------------------------------------------------------------------------
c get next
        do while (time.gt.t1.and.icurr.lt.nvalue)
c time 0
           t0 = t1
           xhc0 = xhc1   
           yhc0 = yhc1
           b0 = b1
           rmax0 = rmax1
           deltap0 = deltap1
           vtx0 = vtx1
           vty0 = vty1
           k0 = k1
           
c time 1
           icurr = icurr+1             
           t1 = timeHol(icurr)        
           xhc1 = xhcHol(icurr)
           yhc1 = yhcHol(icurr)
           b1 = bHol(icurr)
           rmax1 = rmaxHol(icurr)
           deltap1 = deltapHol(icurr)
           vtx1 = vtxHol(icurr)
           vty1 = vtyHol(icurr)
           k1 = kHol(icurr)

        if (t1.lt.timestart) cycle

        write (iout,'(10(a,g13.7))' ) 't0=',t0,' t1=',t1,' xhc1=',xhc1,' yhc1=',yhc1,' b1=',b1,' rmax1=',rmax1,' deltap1=',deltap1
     &            ,' vtx1=',vtx1,' vty1=',vty1,' k1=',k1

        enddo
c------------------------------------------------------------------------
c interpolate       
        if (time.le.t1) then
           fact = (time-t0)/(t1-t0)
        else
           fact = 1
        endif
        xhc = xhc0 + (xhc1-xhc0)*fact
        yhc = yhc0 + (yhc1-yhc0)*fact
        b = b0 + (b1-b0)*fact
        rmax = rmax0 + (rmax1-rmax0)*fact
        deltap = deltap0 + (deltap1-deltap0)*fact
        vtx = vtx0 + (vtx1-vtx0)*fact
        vty = vty0 + (vty1-vty0)*fact
        k = k0 + (k1-k0)*fact
        return
        end

