       subroutine lddind (ldd,i,j,nrow,ncol,ito,jto,il,jl,ir,jr)
	   implicit none
	   integer*1 ldd
	   integer*4 i,j,ll,lr,nrow,ncol,ito,jto,il,jl,ir,jr
     &  , imap(3,9), lrmap(3,9)
	   data imap /
     &  1, -1, 1,
     &  2,  0, 1,
     &  3,  1, 1,
     &  4, -1, 0,
     &  5,  0, 0,
     &  6,  1, 0,
     &  7, -1,-1,
     &  8,  0,-1,
     &  9,  1,-1/
	   data lrmap /
     &  1, 3, 7,
     &  2, 6, 4,
     &  3, 9, 1,
     &  4, 2, 8,
     &  5, 0, 0,
     &  6, 8, 2,
     &  7, 1, 9,
     &  8, 4, 6,
     &  9, 7, 3/
 
	   ito = min(max(i+imap(2,ldd),1),ncol)
	   jto = min(max(j+imap(3,ldd),1),nrow)
 
	   ll = lrmap(2,ldd)
	   il = min(max(i+imap(2,ll),1),ncol)
	   jl = min(max(j+imap(3,ll),1),nrow)
 
	   lr = lrmap(3,ldd)
	   ir = min(max(i+imap(2,lr),1),ncol)
	   jr = min(max(j+imap(3,lr),1),nrow)
 
c	   print '(9(a,i4))','ldd=',ldd,' i=',i,' j=',j
c     &,' ll=',ll,' il=',il,' jl=',jl
c     &,' lr=',lr,' ir=',ir,' jr=',jr
 
	   return
           end
