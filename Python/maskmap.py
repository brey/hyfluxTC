from pmap import getmap,puttif
import numpy as np
import sys

batout=sys.argv[1]
maskmap=sys.argv[2]

MV=-3.4028234663852886e+38

out=getmap(batout)
mask=getmap(maskmap)
#landin = (.not.eqmv(mask).and.mask.lt.0)
#landout = (.not.eqmv(out).and.out.lt.0)
w=(mask.data != MV) & (mask.data < 0.)
w1=(out.data != MV) & (out.data < 0.)
#m=(.not.landin.and.landout).or.(.not.landout.and.landin)
#m=landin.or.landout
#m=landout

if np.sum(w1) > 0 : mask.data[w1] = mask.nan

#putmap mask $maskmap
nband=1
puttif(maskmap,mask.data,mask.GeoTr,mask.Proj,nband,mask.nan)

