      integer*4 nvar, tmax, nfor, nval
     &         ,npar, nmer, latf, longf, latl, longl
     &        ,ic1, iy1, im1, id1, ih1
 
	  common /dbcom1/ nvar, tmax, nfor, nval
     &         ,npar, nmer, latf, longf, latl, longl
     &        ,ic1, iy1, im1, id1, ih1
 
	  integer*4 head(50)
	  equivalence (head,nvar)
 
      character var(50)*4, typ*8
	  common /dbcom2/ var, typ
 
	  integer*4  lirec, nrec, ptxyz
      common /dbcom3/ lirec, nrec, ptxyz
 
