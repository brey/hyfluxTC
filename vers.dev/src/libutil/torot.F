      subroutine togeo(lamr,phir,lamn,phin,lamg,phig)
c input  lamr, phir, lamn, phin
c output: lamg, phig
	implicit none
	real*4 lamr,phir,lamn,phin,lamg,phig
     &       , slam,clam, sphi, cphi, sphin, cphin, arg1, arg2
     &       , slamn, clamn
 
	slam = sin(lamr)
	clam = cos(lamr)
 
	slamn = sin(lamn)
	clamn = cos(lamn)
 
	sphi = sin(phir)
	cphi = cos(phir)
 
	sphin = sin(phin)
	cphin = cos(phin)
 
 
	arg1 = slamn*(-sphin*clam*cphi+cphin*sphi) - clamn*slam*cphi
	arg2 = clamn*(-sphin*clam*cphi+cphin*sphi) + slamn*slam*cphi
 
	lamg = atan2(arg1,arg2)
 
	phig = asin(sphi*sphin+cphi*clam*cphin)
 
	return
	end
 
c---------------------------------------------
 
	subroutine torot (lamg,phig, lamn, phin, lamr, phir)
	implicit none
c input: lamg, phig, lamn, phin
c output: lamr, phir
	real*4 lamr,phir,lamn,phin,lamg,phig
     &       , slam,clam, sphi, cphi, sphin, cphin, arg1, arg2
 
	slam = sin(lamg-lamn)
	clam = cos(lamg-lamn)
 
	sphi = sin(phig)
	cphi = cos(phig)
 
	sphin = sin(phin)
	cphin = cos(phin)
 
 
	arg1 = -slam*cphi
	arg2 = -sphin*cphi*clam+cphin*sphi
 
	lamr = atan2(arg1,arg2)
	phir  = asin(sphi*sphin +cphi*cphin*clam)
 
	return
	end
