import numpy as np
from pmap import getmap, putmap
import sys

#setexp start `pwd`


def doMap(filein,fileout):
#---------------------------------------

   try:
     dat=getmap(filein)
     v1=np.empty((dat.NCOLS,dat.NROWS))
     v1.fill(dat.nan)
     #setexp DRIVER PCRaster
     GEO=dat.GeoTr
     TYPE=4
     # putmap 
     putmap(fileout,v1,GEO,TYPE)
  
   except IOError as e:
          print e


if __name__ == "__main__":
    filein=sys.argv[1]
    fileout=sys.argv[2]
    doMap(filein,fileout)

