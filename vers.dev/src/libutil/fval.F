      function fval (v1,f1,val,v2,f2)
	implicit none
	real*4 fval,val,v1,f1,v2,f2 
	fval = f1 + (val-v1)*(f2-f1)/(v2-v1)
	fval = max(fval,min(f1,f2))	
	return
	end
