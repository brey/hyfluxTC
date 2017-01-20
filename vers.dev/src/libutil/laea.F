c----------------------------------------------------------------------
c  phi (latitude) and lambda (longitude) are in radiants
c  radius, x,y are in meters
c----------------------------------------------------------------------
       module laea_mod
       implicit none
       real*8, save :: phi0, lambda0, radius0, sinphi0, cosphi0, invradius
       end module laea_mod
c----------------------------------------------------------------------
       subroutine laea (phi,lambda,radius)
       use laea_mod
       implicit none
       real*4 phi,lambda,radius
 
        phi0 = phi
        lambda0 = lambda
        sinphi0 = sin(phi)
        cosphi0 = cos(phi)
        radius0 = radius
        invradius = 1.d0/radius0
        return
        end
c----------------------------------------------------------------------
        subroutine geo2laeaFun (phi,lambda,x,y)
       use laea_mod
       implicit none
       real*4 phi,lambda, x, y
       real*8 sinphi, cosphi, coslambda,k1
c The Lambert azimuthal equal-area projection is a map projection having transformation equations
 
        sinphi = sin(phi)
        cosphi = cos(phi)
        coslambda = cos(lambda-lambda0)
	k1 = radius0*sqrt(2.d0/(1.d0+sinphi0*sinphi+cosphi0*cosphi*coslambda))
 
	x  = k1 * cosphi*sin(lambda-lambda0) 	
	y  = k1 * (cosphi0*sinphi-sinphi0*cosphi*coslambda)
        return
        end
c--------------------------------------------------------------------
        subroutine  laea2geoFun (x0,y0,phi,lambda)
       use laea_mod
       implicit none
       real*4 phi,lambda, x, y, x0,y0
       real*8 rho,c, sinc,cosc

c The inverse formulas are
 
        x = x0*invradius
        y = y0*invradius
	rho = sqrt(x*x+y*y)

	if (abs(rho).gt.2) then
	print *,'x=',x,' y=',y,' rho=',rho
	rho = 2
	endif

	if (rho.le.0.) then
	phi = phi0
	lambda = lambda0
	else 	
	c   = 2*asin(0.5*rho)
	sinc  = sin(c)
        cosc  = cos(c)
	phi 	= 	asin(cosc*sinphi0+(y*sinc*cosphi0)/rho)
	lambda 	= 	lambda0+atan((x*sinc)/(rho*cosphi0*cosc-y*sinphi0*sinc))
	endif
        return
        end
