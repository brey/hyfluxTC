*================================================================================
      module kind
*================================================================================
      integer*4 r4,r8,rr8
      integer*4 i4,l4
      parameter (r4=4,r8=8,rr8=8,i4=4,l4=4)
      end module kind

*================================================================================
      module constants
*================================================================================
* constant values which are not changed in the program
      use kind
      implicit none
      character execution*30,executable*80
#include <version.inc>
      integer(i4) ndim, nvec
      parameter (ndim=2,nvec=ndim+1)
 
      integer(i4) iinp/10/, imon/11/, imon1/13/, icpu/14/, ierr /15/, ifree/16/, iout, ipar
     &           , ipar0/1000/, iout0/2000/,iwatch /3000/, iwatch1/3001/, icpudeb /4000/, isplit /5000/
 
      integer(i4) ih, ivn,ivt
      parameter (ih=2,ivn=1,ivt=3)

      integer(i4) jn(2)/1,2/
      integer(i4) jt(2)/2,1/
 
      integer(i4), save :: in, it, maxiter2, maxiter3
 
      real(r8) half , one, zero, onethird, fourthird
      parameter (half=0.5d0)
      parameter (zero=0.d0,one=1.d0)
      parameter (onethird=1.d0/3.d0,fourthird=4.d0/3.d0)
      real(r4) g, rhoa, rhow, minDistBound, minDistShore, minDistSlope, runUpLim
      parameter (minDistBound=2.,minDistShore=2.,minDistSlope=2)

      real(r8) epsvar 
      real(r8) pi, deg2rad, rad2deg, deg2m

      parameter (rhoa=1.15, rhow=1000)

*      parameter (fdhmin=1e-4, fdhmed=4e-2, fdhmax=4e-2) 
*      parameter (fdhmin=1e-5, fdhmed=1e-1, fdhmax=1e-1) ! 2010.09.13 8:13
*      parameter (fdhmin=1e-4, fdhmed=1e-2, fdhmax=4e-2)  ! 2010.09.13 13:30
*      parameter (fdhmin=1e-4, fdhmed=1e-2, fdhmax=0.5)  ! 2010.09.14 9:00

      real(r4) fdhmin /1e-5/, fdhmed/1e-2/, fdhmax/1e-2/   ! 2010.09.14 13:00

       logical(l4) sumslope /.true./

	character null*1 
        integer*1 inull /z'00'/
        equivalence (null,inull)
 
	character tab*1/'	'/
 
	real(r4) mv /z'ffffffff'/
	integer(i4) imv /z'ffffffff'/, mv4
	integer*1 mv1
        logical*1 lmv1
        equivalence (mv1,lmv1)
 
#if defined WIN32
      character bs*1 /'\'/
#else
      character bs*1 /'/'/
#endif
      end module constants

*================================================================================
      module parameter_data
*================================================================================
      use kind
      use constants

      implicit none
c parameters
      integer(i4) nout_max
      parameter (nout_max=20)
c input data
      logical(l4), save :: verify, limnoise, debug, entrfix, console, watch, faultDem, faultZsurf
     &           , mapdeg, nocase, existHolland, nested, noSpot, check
      logical(i4) outnow,restnow, statnow, useMinDem, onlyDem, outTTT
 
      integer(i4) , save ::strans, ilim, varjun, solver, imap, istepmax,varHLLE
     &           , noutmaps,nrestmaps,nstatmaps, intsrtm, ldeb, freqmon
 
      real(r8) , save :: lamtrans, seaLevel, velmin, courmax, fluxnum
      real(r8), save :: manval, man0, manslopeval, expslope, manReef, demReef
 
      real(r4), save :: limval, limV, limZW, errstep, facstep
     &        , maxdhmax, minDem, maxDem, maxDist, maxDistLand, minDistLand, minDistSea, maxDepth

      integer(i4) :: nlimcoeff
      parameter (nlimcoeff=4)
      real(r4), save :: limCoeff(nlimcoeff)
 
      character*256, save :: outdir,workdir, demmap, dzdxmap, dzdymap, batmap, batnode
     &         ,paramfile, consolefile, loginfile, logoutfile
     &         ,placefile, watchfile, shorefile, watchdir
     &         ,maskmap, demnodemap
     &         ,vxmap, vymap, depthmap,  rainmap , zsurfmap, monitor
     &         ,manmap,  manslopemap, faultmap , fcormap
     &        ,outmaps(nout_max),restmaps(nout_max),statmaps(nout_max)
     &        ,code, noSlopemap, fileout, home
     &        ,inpdeck, confdeck, errmsg, hollandFile, outline(10)
      
      integer(i4) noutline
 
      real(r8), save :: tmax, maxstepdtmin,dtmin,dtout, dtrest, dtstat, dthydro
     &       , hmin, hmin2, cmin, cmin0, hvel0, hvel00, hcrit, hprim
     &       , hout, wout, harr
     &       , ccrit, ccrit0, hlim, hlim0, hshore, hshore0, invhshore, invsf
     &       , invhlim,invcrit,invhvel0, halfg, inv2g, hprim0, invhprim
     &       ,errprim, houtmin, voutmin,cminDem

      real(r4), save :: smallArg, smallNew , smallH, smallV, smallHV
      real(r4), save :: cdmax  ! max wind friction coefficient (holland.F)

      real(r4) :: vmaxShore
      parameter (vmaxShore=2)
 
      real (r4), save :: radius0

      integer(i4), save :: lout, lw,loll, retcode, lwatch
 
      end module parameter_data

*================================================================================
      module Riemann_data
*================================================================================
      use kind
      use constants
      implicit none
      real(r8), save :: frL, frR
      real(r8), save :: FL(nvec),FR(nvec),VarL(nvec),VarR(nvec)
      logical(l4), save :: testS, testL, testR, testLR, DryL, DryR
      end module Riemann_data
*================================================================================
      module map_data
*================================================================================
      use kind
      implicit none
 
      integer(i4), save ::  typemap

      real(rr8), save :: xstart,ystart,cellsize  ! [deg]
      character, save :: maps*1000, ext1*7, ext2*8 
c--------------------------------------------
* gdal additional variables
        integer(i4) nband /1/, iband /1/, error, nbyte/4/
        character driver*64, datatype*64, mapfmt*64, outfmt*64, mapext*5, outext*5
        real(rr8) sizey
c--------------------------------------------
      integer(i4), save :: nvx,nvy,nval
      real(r4), save, allocatable :: val(:),outval(:),val1(:)
      integer(i4), save, allocatable :: ival4(:)
      integer*1, save, allocatable :: ival1(:)
      logical*1, save, allocatable :: mask(:,:)
      end module map_data
*================================================================================
      module geometry_data
*================================================================================
      use kind
      use map_data
      implicit none
 
      integer(i4), save :: nv0, nj, nv, nn
 
      integer*1, save, allocatable :: c_lev(:)	! (nv0), temporary ? 

      integer(i4), save, allocatable ::
     &        il(:),jl(:)   			! (nv0), temporary: pointers, geom
     &       ,lij(:,:)  			! (nvx0, nvy0), temporary ? used in: pointers, init
     &       ,kl(:)	          		! (nv0), permanent 
     &       ,vtol0(:,:),vtor0(:,:)    		! (nv0), temporary
     &       ,lk(:)	          		! (nval), permanent 
     &       ,jtype(:),jtol(:),jtor(:)  	! (nj), permanent 
     &       ,vtype(:),vtol(:,:),vtor(:,:)	! (nv), permanent
     &       ,kbound(:)				! (nv), permanent
     &       ,lev(:)    			! (nv), permanent
 
      real(r8), save, allocatable ::
     &        zv(:),dzdk(:,:),dzdkm(:,:), tanLatv(:),coslatv(:)		! (nv), permanent

      real(r4), save, allocatable :: pixShore(:)  		! (nv), permanent
 
      real(r8), save, allocatable ::
     &        areav(:)			! (nv), temporary

      real(r8), save, allocatable :: zj(:), facL(:), facR(:)	! (nj), permanent !
      logical*1, save, allocatable :: deepJ(:)	! (nj), permanent !

      real(r4), save, allocatable :: xv(:),yv(:)  		! (nv), normalized coord of cell center, permanent

      real(r4), save, allocatable :: xj(:),yj(:)  		! (nj), normalized coord of junctions, temporary ?

c--------------------------------

! nsize is half size of the finest cell => for the finest cell nsize = 1
! cellsizeN is the size of coarse cell


c  see getBottom for the definitions

      integer(i4) cellsizeN		! coarse cell size [-]
      real(r8), save :: cellsizeD	! coarse cell size [deg]
      real(r8), save :: cellratio	! cellratio=cellsizeD/cellsizeN [deg]
      real(r8), save :: cellsizeM	! coarse cell size [m]

c      cellsizeN = 2*baseS**maxlev	
c            xv(l) = (i-0.5)*cellsizeN
C            yv(l) = (j-0.5)*cellsizeN
c
c     xcell = xstart + (i-0.5)*cellsizeD = xstart + (i-0.5)**cellsizeN * cellsizeD ; cellsizeN = xstart + xv * cellratio
c     ycell = ystart - yv * cellratio
c  

      integer(i4) maxlev, maxmaxlev, regularize
      real(r4) baseS 
      parameter (maxmaxlev=10)

      integer(i4) Ms(0:maxmaxlev)

      real(r8)    nsize(0:maxmaxlev)
      real(r8)    dsize(0:maxmaxlev)
      real(r8)    msize(0:maxmaxlev)

c--------------------------------
      TYPE QuadStruct
      INTEGER(i4), pointer :: f_ind(:,:)
      END TYPE QuadStruct

      TYPE (QuadStruct), pointer :: c_ind(:)  
c--------------------------------
      TYPE QuadLevel
      real*8 :: cellsize
      INTEGER(i4) :: nvx, nvy, nval, nv,l,k
      integer(i4),  pointer :: kl(:),ll(:) 
      END TYPE QuadLevel

      TYPE(QuadLevel), pointer :: ql, qlev(:) 
c---------------------------------
     

      end module geometry_data
 
*================================================================================
      module parallel_data
*================================================================================
      use kind
      implicit none
#ifdef PARALLEL
#include <mpif.h>

#else
      integer mpi_status_size
      parameter (mpi_status_size=1)
#endif

      integer Fmov,Fsum, Fdif, Fmin, Fmax, For, Fand  
      parameter (Fmov=0,Fsum=1,Fdif=2,Fmin=3,Fmax=4,For=5,Fand=6)
      integer ncore_max, nvar_max,npack
      logical*1, save :: MASTER, SLAVE, LEFT, RIGHT, POST, CALC, postProc, PUTMAP, CALCWATCH
      parameter (ncore_max=256,nvar_max=15,npack=4)
      integer, save ::   icore, ncore, ncore1, nvmax, position(2), nvmaxx
     &           , mpi_erro, mpi_stato(mpi_status_size), reqs(2)
     &           ,nv1(0:ncore_max), nv2(0:ncore_max) ! cell pointers for each core
     &           , nv3(0:ncore_max), nv4(0:ncore_max) ! enlarged cell pointers for each core
     &           , nvv(0:ncore_max), njj(0:ncore_max), nvvL(0:ncore_max) 
     &           , nj1(0:ncore_max), nj2(0:ncore_max) ! junction pointers for each core 
      integer ntags
      real(r4)  cpuCore(0:ncore_max), cpuCOM(100), cpuCAL(100), cpuIO(100)
     &          , cpuBAR(100),tags(10), cpuTOT(100)
      character*12 block(100)
      integer(i4), save, allocatable :: corev(:)
      character chcore*3

      integer(i4), save :: sizv, sizvL, iv1, iv2, ivL1, ivL2, ivM1, ivM2, ij1, ij2, sizj
     &          ,pf(4),sc(4), sizb, sizbmax

      real(r8), save, allocatable ::  my_dval(:)
      real(r8), save, allocatable :: dval(:)
      real(r4), save, allocatable ::  my_sval(:)
      integer(i4), save, allocatable :: my_ival(:)
      logical*1, save, allocatable :: my_lval1(:)
      integer*1, save, allocatable :: my_ival1(:)
      integer*1, save, allocatable :: my_ibuf(:,:)
      integer(i4) request(nvar_max,npack,ncore_max)

      end module parallel_data
*================================================================================
      module hydro_data
*================================================================================
      use kind
      implicit none
c Note: cvv is cons. variable
      real(r8), save, allocatable ::
     &        hv(:),cvv(:,:)
     &       ,fhv(:),fvv(:,:)
      real(r8), save, allocatable :: vv(:,:)
 
 
      real(r8), save, allocatable ::
     &        zwv(:), hk(:,:),hj(:,:,:),zminv(:)
 
      real(r4), save, allocatable ::
     &        vn(:),wetfrac(:),man(:),manh(:),manslope(:)
     &       ,wetfracIn(:),sf(:,:), zwvIn(:), hvIn(:)

      real(r4), allocatable :: limvals(:), smalls(:),courn(:)
     &   ,dhjv(:),spu(:),u10(:),depr(:)
 
      integer*1, save, allocatable :: case(:)

      real(r8) hj0(2,2), hk0(2), zeros(2) /0.d0,0.d0/
      real(r4) wetfrac0
      integer*1 case0
 
      real(r4), save, allocatable ::
     &    rain(:),addrain(:) ,fcor(:)
     &   ,zwmin(:),tzwmin(:)
     &   ,zsurfmax(:),tzsurfmax(:),hzsurfmax(:)
     &  ,zwmax(:),tzwmax(:),hzwmax(:),wzwmax(:), ztotmax(:), fhdmax(:)
     &  ,tarr(:),vnmax(:),tvnmax(:),u10max(:),deprmax(:), spumax(:)

      logical*1, allocatable :: upvol(:),upflux(:)
      logical*1, allocatable :: noSlope(:)
      end module hydro_data

*================================================================================
      module watch_data
*================================================================================
      use kind
      implicit none
      logical*1, save, allocatable :: arrived(:)
      integer(i4), save, allocatable :: idW(:),cellW(:),iunW(:),karr(:),lpW(:)
      real(r4),save,allocatable :: tarrW(:),zmaxW(:),tzmaxW(:),fmaxW(:),vmaxW(:)
     &     ,latW(:),longW(:)
      character*255, allocatable :: namePlace(:), filePlace(:)
      real(rr8), allocatable :: dxW(:),dyW(:), demW(:)
      integer(i4) nplace, narrived, tagWatch /200/
      end module watch_data
*================================================================================
      module slope_data
*================================================================================
      use kind
      implicit none
      real(r8), save, allocatable ::
     &        dzwdk0(:,:), dzwdk(:,:),dcvdk(:,:,:), dhdk(:,:)
      real(r8), save, allocatable :: dvdk(:,:,:)
      logical*4, save, allocatable :: slopeV(:), deepV(:)
      end module slope_data
*================================================================================
      module window_data
*================================================================================
      use kind
      use constants
      implicit none
c     ---------------------------
! window: longmax0, latmin0 are derived as follows
! longmax0 = longmin0 + dlong0*nlong0
! latmin0 = latmax0 - dlat0*nlat0

! long = longmin0 + (i-0.5)*dlong0
! lat  = latmax0  - (j-0.5)*dlat0
 
      real (rr8), save :: longmin0,longmax0,latmin0,latmax0 ! window of interest
      integer(i4), save :: nlong0, nlat0, nval0
      real (rr8), save :: dlat0,dlong0
      real(r4), save, allocatable :: val0(:)
 
      character*1000, save :: srtmpath
c-----------------------------------------------------------
      type window
      real(rr8)  xstart, ystart, cellsize,cellsizey
      integer(i4) nvx,nvy,nval,typemap,  nband 
      real(r4), pointer :: val(:)
*      real(r4), allocatable :: val(:)  ! compile error in gnu
      character driver*64
      end type window
c     ---------------------------
      end module window_data
*================================================================================
      module boundary_data
*================================================================================
      use kind
      use window_data
      implicit none
      integer*1, save, allocatable :: nupsh(:),nupsv(:)
      real(r4), save, allocatable :: zbound(:),vxbound(:),vybound(:)
     &          ,timeBound(:), xbound(:,:),ybound(:,:)

      integer(i4), save, allocatable :: cellbound(:),codebound(:),ibandBound(:)
     &             ,lpB(:),kB(:,:),lkb(:,:),nmapok(:),ivbound(:)

      character*64, save, allocatable :: zsurfBoundMap(:), vxBoundMap(:), vyBoundMap(:)

      integer(i4), save :: nbound,imapMax,imapCurr, boundaryCode, lbdir, nkb(3), nvb, boundaryCode0
      real(r4) boundaryFac
      character, save :: boundaryFile*1000, boundaryDir*1000

      type(window), save, pointer :: ipB
      

      end module boundary_data
*================================================================================
      module statistics_data
*================================================================================
      use kind
      implicit none
 
      integer(i4), save :: ncorr, istep, stepdtmin, nstep, nsteprem, ndry, ldhmax
     &           ,ncase0,ncase1,ncase2,ncase3,ncase4
 
      real(r8), save :: time, dt,dtv,dtmax, dtold, dtjun, timestart
     &      ,timeold,timeout, timerest, dtrem, timestat
 
      real(r4), save :: cpu0, elaps0,cpu1, cpu2, elaps1,cpu, elaps, cpust, cpuen, cpumap, cpupost
     &        ,cpubarpost,cpuComBound,cpuIoBound, cpuBarBound, cpuShore(0:3)

      integer(i4) tagPost/33/, tagBound/32/, tagJun/23/

      real(r4), save :: avgstep, maxstep, haver, haverIn, maxfr, dhmax, maxvn, maxzw, maxu10, maxdepr
     &         ,setdh, maxzwmax, maxvnmax

      integer(i4) ntimeL, ntimeMax, itimeL
      parameter (ntimeMax=100) 
      real(r4) toutL(ntimeMax), dtoutL(ntimeMax), dtrestL(ntimeMax), dtstatL(ntimeMax)     
 
      end module statistics_data
