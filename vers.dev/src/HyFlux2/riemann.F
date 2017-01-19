*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine riemann (PrimL,PrimR,FJ,VarJ,type,juncalc)
      use Riemann_data
      use parameter_data
      implicit none
c
c local variables
      real(r8) PrimL(nvec),PrimR(nvec),FJ(nvec),Fdry(nvec)
 
      real(r8) fac,sgn(nvec),Var(nvec),Lam(nvec),VarJ(nvec)
     &       ,Dfout(nvec),Dv(nvec),Sv(nvec),DF(nvec),Sf(nvec),DU(2)
     &       ,dp(nvec),sp(nvec), LamL(nvec), LamR(nvec), bp, bm
      integer(i4) type, i, juncalc
      logical(i4) rarefL, rarefR
*-------------------------------------------------
      call primToVar (PrimL,VarL)
      call primToVar (PrimR,VarR)
      sv = half*(VarL+VarR)
*-------------------------------------------------
      call primToFlux (PrimL,FL)
      call primToFlux (PrimR,FR)
      sf = half*(FL+FR)
*-------------------------------------------------
      dp = half*(PrimL-PrimR)
        if (maxval(abs(dp)).lt.errprim) then
        FJ = sf
        VarJ = sv
        go to 400
        endif
*-------------------------------------------------
c Sonic & dray variables
      call testSonicDry
*-------------------------------------------------
c volume eigenvalues
      rarefL = DryR.and.(VarL(ivn).lt.zero)
      rarefR = DryL.and.(VarR(ivn).gt.zero)
      call varToLam (VarL,LamL,rarefL)
      call varToLam (VarR,LamR,rarefR)
*--------------------------------------------------------
c Var is arithmetic average
      sgn = 0
      type = 0
 
c Celerity & velocity average

        if (juncalc.eq.0) then
        Var = VarJ

        else if (varjun.eq.0) then
        Var = sv
 
c Roe's sheme (Toro, pag. 188)
        else if (varjun.eq.1) then
        Var(ih) = sqrt(half*g*(PrimL(ih)+PrimR(ih)))
        Var(ivn) = (VarL(ivn)*VarL(ih) + VarR(ivn)*VarR(ih) )/(VarL(ih)+VarR(ih))
        Var(ivt) = (VarL(ivt)*VarL(ih) + VarR(ivt)*VarR(ih) )/(VarL(ih)+VarR(ih))
 
c Cons. var. average  .or.  HLLE scheme
        else if (varjun.eq.2) then
        Var(ih) = sqrt(half*g*(PrimL(ih)+PrimR(ih)))
        Var(ivn) = (VarL(ivn)*PrimL(ih) + VarR(ivn)*PrimR(ih) )/(PrimL(ih)+PrimR(ih))
        Var(ivt) = (VarL(ivt)*PrimL(ih) + VarR(ivt)*PrimR(ih) )/(PrimL(ih)+PrimR(ih))

c average eigenvalues
        else if (varjun.eq.3) then
        Lam = half*(LamL+LamR)
        Var(ih)  = half*(-Lam(1)+Lam(2))
        Var(ivn) = half*(Lam(1)+Lam(2))
        Var(ivt) = sv(ivt)
        endif
*--------------------------------------------------------
c junction eigenvalues
        call varToLam (Var,Lam,.false.)
c recalculate eigenvalues according to Xin Wen (majour effects in wet/dry conditions)
          if (varHLLE.eq.1) then
          Lam(1) = min(Lam(1),LamL(1))
          Lam(2) = max(Lam(2),LamR(2))
          Var(ih)  = half*(-Lam(1)+Lam(2))
          Var(ivn) = half*(Lam(1)+Lam(2))
          Lam(3)   = Var(ivn)
          endif
*--------------------------------------------------------
c evaluate eigenvalues sign
      Call SignLam(lamtrans,Lam,sgn,type,strans)
*--------------------------------------------------------
c  calculation of Var in Star region
*-------------------------------------------------------
      dv = half*(VarL-VarR)
      call riemannVar (dv,sgn,type,dfout)
      VarJ = sv + dfout
*--------------------------------------------------------
c HLLE Solver: see Xin Wen 2006, Appendix A
        if (solver.eq.0) then
        bp = max(Lam(2),zero)
        bm = min(Lam(1),zero)
        DU(1) = PrimR(ih)-PrimL(ih)
        DU(2) = FR(1)-FL(1)
        FJ(1:2) = (bp*FL(1:2)-bm*FR(1:2)+bp*bm*DU)/(bp-bm)
          if (Lam(3).lt.zero) then
          FJ(3) = FR(3)
          else if (Lam(3).gt.zero) then
          FJ(3) = FL(3)
          else
          FJ(3) = half*(FL(3)+FR(3))
          endif
        type = 0
        return
        endif
*--------------------------------------------------------
*  tests
*--------------------------------------------------------
c entropy fix
      FJ = sf
      call sonic (VarJ,FJ,*100,*200)
*--------------------------------------------------------
c Only Var solver
c  two rarefraction wave solver
        if (solver.eq.1) then
        call varToFlux(VarJ,FJ)
        type = 0
        return
        endif
*--------------------------------------------------------
* Flux Vector Splitting 
        if (solver.eq.2) then
        df = half*(FL-FR)
        call riemannFlux (df,Lam,sgn,type,dfout)
        FJ = sf + dfout
        endif
c-------------------------------------------
c completly wet
        if (Var(ih).ge.ccrit) return
c-------------------------------------------
c from wet to dry
      call varToFlux(VarJ,Fdry)
      fac = log(max(Var(ih),ccrit0)/ccrit0)*invcrit
      fac = min(max(fac,zero),one)
      FJ = FJ*fac + Fdry*(one-fac)
      go to 300
c-------------------------------------------
 
c entropy fix
100   continue
      type = -1
      return
 
c DryL or DryR
200   continue
      type = -2
      return
 
c from depthPositive: Var(ih) < ccrit
300   continue
      type = -3
      return
 
c dp < errprim
400   continue
      type = -4
      return
      end
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine riemannDeep (PrimL,PrimR,FJ,typeRiem)
      use Riemann_data
      use parameter_data
      implicit none
c
c local variables
      real(r8) PrimL(nvec),PrimR(nvec),FJ(nvec), DP(nvec)
 
      real(r8) sgn(nvec),Var(nvec),Lam(nvec)
     &       ,Dfout(nvec),Df(nvec),Sf(nvec)
      integer type, typeRiem
*------------------------------------------------- 
      call primToFlux (PrimL,FL)
      call primToFlux (PrimR,FR)
      sf = half*(FL+FR)
*-------------------------------------------------
      dp = half*(PrimL-PrimR)
        if (maxval(abs(dp)).lt.errprim) then
        FJ = sf
        typeRiem = -4
        return
        endif
*-------------------------------------------------
      call primToVar (PrimL,VarL)
      call primToVar (PrimR,VarR)

      Var = half*(VarL+VarR)

      Lam = (/ Var(ivn)-Var(ih), Var(ivn)+Var(ih), Var(ivn) /)
      sgn = (/-1.d0,1.d0,sign(1.d0,Lam(3))/)
      type = 1
*-------------------------------------------------
      df = half*(FL-FR)
      call riemannFlux (df,Lam,sgn,type,dfout)
      FJ = sf + dfout
      return
      end
*--------------------------------------------------------
c test based on left & right only
*--------------------------------------------------------
      subroutine testSonicDry
      use parameter_data
      use Riemann_data
      use statistics_data
      implicit none
 
      frL = abs(VarL(ivn))/max(VarL(ih),cmin0)
      frR = abs(VarR(ivn))/max(VarR(ih),cmin0)
      testL = frL.le.one.and.frR.ge.one.and.VarL(ih).gt.VarR(ih)
     &       .and.VarL(ivn).gt.zero.and.VarR(ivn).gt.zero
      testR = frR.le.one.and.frL.ge.one.and.VarR(ih).gt.VarL(ih)
     &       .and.VarL(ivn).lt.zero.and.VarR(ivn).lt.zero
      testLR = (testL.or.testR)
      DryL = VarL(ih).le.cmin*1.1
      DryR = VarR(ih).le.cmin*1.1
        if (DryL) ndry = ndry+1
        if (DryR) ndry = ndry+1
      return
      end
c=====================================================
c additional test based on star region
c=====================================================
      subroutine sonic (VarJ,FJ,*,*)
      use Riemann_data
      use parameter_data
      implicit none
      real(r8) VarJ(nvec), FJ(nvec)
 
c it works very well for dry/inclined: don't remove
c start dry L&R
      if (DryL.and.DryR) return
      if (DryL.or.DryR) go to 200
c end dry L&R
 
        if (testLR.and.entrfix) goto 100
 
      RETURN
 
100   continue
 
      call chockVar (VarJ,2)
      call varToFlux (VarJ,FJ)
      return 1
 
c DryL or DryR
200   continue
      if (DryR) testL = frL.lt.one.and.VarL(ivn).gt.zero
      if (DryL) testR = frR.lt.one.and.VarR(ivn).lt.zero
      if (testL.or.testR) call chockVar (VarJ,3)  ! this is  correct

      call varToFlux (VarJ,FJ)
      return 2
      end
c=====================================================
      subroutine chockVar (Var,type)
      use Riemann_data
      implicit none
      real(r8) Var(nvec)
      integer(i4) type
 
c energy conservation (but the energy should not conserve)
      if (type.eq.1) then
      Var(ih) = sqrt((Var(ivn)*Var(ivn)+2*Var(ih)*Var(ih))/3.)
 
c momentum conservation
      else if (type.eq.2) then
      Var(ih) = (abs(Var(ivn))*Var(ih)*Var(ih))**onethird
 
c from Toro (dry bed state)
      else if (type.eq.3) then
      Var(ih) = ((abs(Var(ivn))+2*Var(ih))/3.)
      endif
 
      Var(ivn) = sign(Var(ih),Var(ivn))
        if (Var(ivn).gt.0.) then
        Var(ivt) = VarL(ivt)
        else
        Var(ivt) = VarR(ivt)
        endif
      return
      end
 
c============================================================================
      subroutine fluxBound (h,vv,sgn,FJ,VarJ)
c============================================================================
      use constants
      implicit none
      real(r8) PrimL(nvec),PrimR(nvec),h,vv(ndim),FJ(nvec)
     &      ,hL,hR,vL(ndim),vR(ndim),VarJ(nvec)
      integer(i4) sgn(2),k,typeRiem
      logical(i4) symmetric/.false./
 
      hL = h
      hR = h

      if (symmetric) then
        do k = 1,ndim
        vL(k) = sgn(1)*vv(k)
        vR(k) = sgn(2)*vv(k)
        enddo
      vL(it) = vv(it)
      vR(it) = vv(it)
      else
      vL(in) = sgn(1)*vv(in)
      vR(in) = sgn(2)*vv(in)
      vL(it) = vv(it)
      vR(it) = vv(it)
      endif
 
      PrimL = (/vL(in),hL,vL(it)/)
      PrimR = (/vR(in),hR,vR(it)/)
      call riemann (PrimL,PrimR,FJ,VarJ,typeRiem,1)
      return
      end
c=====================================================
      subroutine primToVar (Prim,Var)
      use parameter_data
      implicit none
      real(r8) Var(nvec),Prim(nvec)
 
        Var = (/Prim(ivn),max(sqrt(Prim(ih)*g),zero),Prim(ivt)/)
      return
      end
c=====================================================
      subroutine varToLam (Var,Lam,dryrarefaction)
      use constants
      implicit none
      logical (i4) dryrarefaction
      real(r8) Var(nvec),Lam(nvec)

        if (dryrarefaction) then
        Lam(1) = Var(ivn)-2*Var(ih)
        Lam(2) = Var(ivn)+2*Var(ih)
        else
        Lam(1) = Var(ivn)-Var(ih)
        Lam(2) = Var(ivn)+Var(ih)
        endif
      Lam(3) = Var(ivn)
      return
      end
c=====================================================
      subroutine varToPrim (Var,Prim)
      use parameter_data
      implicit none
      real(r8) Var(nvec),Prim(nvec)
*       PrimL = (/vL(in),hL,vL(it)/)
 
      Prim(ih)  = max(Var(ih)*Var(ih)/g,hmin)
      Prim(ivn) = Var(ivn)
      Prim(ivt) = Var(ivt)
      return
      end
c=====================================================
      subroutine primToFlux (Prim,flux)
      use parameter_data
      implicit none
      real(r8) flux(nvec),Prim(nvec), hvn
      hvn = Prim(ih)*Prim(ivn)
        flux   = (/hvn
     &            ,hvn*Prim(ivn) + half*g*Prim(ih)*Prim(ih)
     &            ,hvn*Prim(ivt)/)
      return
      end
c=====================================================
      subroutine varToFlux (Var,Flux)
      use parameter_data
      implicit none
      real(r8) Var(nvec),Flux(nvec),h,v,sgh,gh,hvn
 
      v  = Var(ivn)
      sgh = max(var(ih),zero)
      gh = sgh*sgh
      h  = gh/g
      hvn = h*Var(ivn)
 
      Flux(1) =  hvn
      Flux(2) =  hvn*Var(ivn)+half*gh*h
      Flux(3) =  hvn*Var(ivt)
      return
      end
c=====================================================
      subroutine riemannVar (df,sgn,type,dfout)
      use parameter_data
      implicit none
      real(r8) df(nvec),sgn(nvec),dfout(nvec),R(2,2)
      integer(i4) type
* math start
c
c file outvar generated by matrix/channel/primvar
c
c  transonic
      if (type.eq.0) then
c
      R(1,1) = (sgn(1) + sgn(2))/2.
      R(1,2) = -sgn(1) + sgn(2)
      R(2,1) = (-sgn(1) + sgn(2))/4.
      R(2,2) = (sgn(1) + sgn(2))/2.
c
      Dfout(1) = R(1,1)*df(1) + R(1,2)*df(2)
      Dfout(2) = R(2,1)*df(1) + R(2,2)*df(2)
c
c  subsonic
      else if (type.eq.1) then
c
      Dfout(1) = -2*df(2)*sgn(1)
      Dfout(2) = -(df(1)*sgn(1))/2.
c
c  supersonic
      else if (type.eq.2) then
c
      Dfout(1) = df(1)*sgn(1)
      Dfout(2) = df(2)*sgn(1)
      endif
c
 
* math end
      Dfout(3) = df(3)*sgn(3)
      return
      end
c=====================================================
      subroutine riemannFlux (df,Lam,sgn,type,dfout)
      use parameter_data
      implicit none
      real(r8) df(nvec),Lam(nvec),sgn(nvec),dfout(nvec),R(2,2), fac
      integer(i4) type
* math start
c
c file out generated by matrix/channel/stven1
c
c  transition
      if (type.eq.0) then
c
      fac =1/max(cmin0,-Lam(1) + Lam(2))
c
      R(1,1) = Lam(2)*sgn(1) - Lam(1)*sgn(2)
      R(1,2) = -sgn(1) + sgn(2)
      R(2,1) = Lam(1)*Lam(2)*(sgn(1) - sgn(2))
      R(2,2) = -(Lam(1)*sgn(1)) + Lam(2)*sgn(2)
c
      Dfout(1) = fac*(R(1,1)*df(1) + R(1,2)*df(2))
      Dfout(2) = fac*(R(2,1)*df(1) + R(2,2)*df(2))
c
c  subsonic
      else if (type.eq.1) then
c
       fac =sgn(1)/max(cmin0,-Lam(1) + Lam(2))
c
       R(1,1) = Lam(1) + Lam(2)
       R(1,2) = -2
       R(2,1) = 2*Lam(1)*Lam(2)
       R(2,2) = -Lam(1) - Lam(2)
 
       Dfout(1) = fac*(R(1,1)*df(1) - 2*df(2))
       Dfout(2) = fac*(R(2,1)*df(1) + R(2,2)*df(2))
c
c  supersonic
      else if (type.eq.2) then
      Dfout(1) = df(1)*sgn(1)
      Dfout(2) = df(2)*sgn(1)
      endif
 
      Dfout(3) = df(3)*sgn(3)
 
* math end
      return
      end
c=====================================================
      Subroutine SignLam(lam1,Lam,flam,type,strans)
      use constants
      implicit none
      real(r8) lam(nvec),flam(nvec),lam1,lam2,f,xx,x1,flam0, esp
      integer(i4) type, i, strans
      parameter (esp=5)
      f(xx) = (3. - xx*xx)*0.5
 
 
      do i = 1,nvec
          if (abs(lam(i)).gt.lam1) then
          flam(i) = sign(1.d0,lam(i))
 
          else if (strans.eq.0) then
*         flam(i) = sign((abs(lam(i))/lam1)**esp,lam(i))
          flam(i) = 0.
 
          else if (strans.eq.1) then
          flam(i) = lam(i)/lam1
 
          else if (strans.eq.2) then
          lam2 = 1./lam1
          x1 = lam(i)*lam2
          flam0 = f(x1)
          flam(i)    = flam0*x1
 
          else
          write (iout,*) 'program error: strans ',strans,' not allowed'
          call logout(0,'STATUS=programError from Riemann')
          endif
      enddo
 
      if (flam(1)*flam(2).eq.-1.) then
      type = 1
      else if (flam(1)*flam(2).eq.1.) then
      type = 2
      else
      type = 0
      endif
 
*     write (iout,*) ' '
*     write (iout,*) 'lam=',lam
*     write (iout,*) 'flam=',flam
*     write (iout,*) 'type=',type
 
      return
      end
 
 
c=====================================================
      FUNCTION MINMOD(a,b)
      use kind
      real(r8) MINMOD,a,b, fi
c
      if(a*b.le.0.) then
      MINMOD = 0.
 
      else  if(ABS(a).lt.ABS(b)) then
      MINMOD = a
 
*     else  if(ABS(b).lt.ABS(a)) then
      else
      MINMOD = b
      end if
 
      RETURN
      END
c=====================================
      FUNCTION SUPERBE(a,b)
      use kind
      real(r8) SUPERBE,a,b,zero,one,two,
     &                 fi,teta
c
      DATA zero,one,two/0.,1.,2./
        if(ABS(a) .gt. 0. ) then
        teta = b/(a+1.0e-10)
        else
        teta = b/(a-1.0e-10)
        end if
      fi = MAX(zero,MIN(one,two*teta),MIN(two,teta))
      SUPERBE = fi*a
      RETURN
      END
