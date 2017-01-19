import numpy as np
import os
import sys
from pcraster import *
from read import readtxt
#from pmap import getmap



def statF(statFile):

     try:
        sF=readtxt(statFile)
     except:        
	print 'problem in reading {}'.format(statFile)
        return 0

     maxH[sF.idPlace] = sF.zmax

     m=np.isnan(maxH) != True
     if sum(m) == 0 : return

     for lab1 in range(10):
          m = (np.isnan(maxH) != True) & (maxH > minzmax)
          if (minzmax < 0.01):  sys.exit()
          if (minzmax > 1.00):  sys.exit()
          if (np.sum(m) >= minloc ) & (np.sum(m) <= maxloc) : sys.exit()
          if (np.sum(m) > maxloc) :  minzmax=minzmax*2
          if (np.sum(m) < minloc) :  minzmax=minzmax/2

     print minzmax, np.sum(m)

     exLoc=0
     if (sum(m) != 0) :

          ID = ID[m]
          lat = lat[m]
          lon = lon[m]
          maxH = maxH[m]
          typeVal = typeVal[m]
          mesVal = mesVal[m]

#--------------------------------------

          x=lon
          y=lat
          
          ix=((x-XSTART)/CELLSIZE).astype(np.int)+1 
          iy=((YSTART-y)/CELLSIZE).astype(np.int)+1 
          l=(iy-1)*NCOLS+ix
     
          m0=(ix >= 1.) & (ix <= NCOLS) & (iy >= 1.) & (iy <= NROWS)

          exLoc=np.sum(m0)>0




     id1=np.empty(n)
     id1.fill(np.nan)

     if (exLoc) :
          id1[l] = ID[m0]


     try:

       TYPE=2
       mv=nodataType[dataType[TYPE].name]
       mdat=pcraster.numpy2pcr(dataType[TYPE].reshape(NROWS,NCOLS),id1,mv)
       pcraster.report(mdat,'location.map')
       id1=id1[II]

     except:
       return 1

#-----------------------------------

     maskSea=np.ones((NROWS,NCOLS))

     TYPE=4
     mv=nodataType[dataType[TYPE].name]
#    putmap maskSea maskSV.map
     mdat=pcraster.numpy2pcr(dataType[TYPE],maskSea,mv)
     pcraster.report(mdat,'maskSV.map')


     TYPE=1
     mv=nodataType[dataType[TYPE].name]
#    putmap maskSea maskBool.map
     mdat=pcraster.numpy2pcr(dataType[TYPE],maskSea,mv)
     pcraster.report(mdat,'maskBool.map')

     maskSea=None

#-----------------------------------

#    getmap shoreline shoreline.map
     fmap=pcraster.readmap('shoreline.map')
     shoreline=pcraster.pcr_as_numpy(fmap)

     shoreline = shoreline[II]
     shore=shoreline != nodataType[fmap.dataType().name]

     TYPE=4
     mv=nodataType[dataType[TYPE].name]
     m=np.isnan(id1) != True
     i=np.argwhere(np.not_equal(shore,m))

     shoreInd=np.empty(n)
     shoreInd.fill(mv)
     shoreInd[i] = i

#    putmap shoreInd shoreInd.map
     mdat=pcraster.numpy2pcr(dataType[TYPE],shoreInd.reshape(NROWS,NCOLS),mv)
     pcraster.report(mdat,'shoreInd.map')

     os.system('MapInterp -m maskSV.map -i shoreInd.map -radius {} -nmax 1'.format(radius))

#    getmap distShore dist.map
     fmap=pcraster.readmap('distShore.map')
     distShore=pcraster.pcr_as_numpy(fmap)

     distShore=distShore[II]
     distShore=distShore*1000

     lookArea = np.empty(n)
     lookArea.fill(mv)

#-------------------------------------------
     if exLoc == False: return 0
#-------------------------------------------

     nloc=np.size(ID)
     i=np.arange(nloc)

     lloc = l[m0]
     iloc = i[m0]


     if 'maxHeight' in locals() :
          maxHeight0=mesVal
     else:
          maxHeight0 = np.empty(nloc)
          maxHeight0.fill(mv)

     typeMes0=typeVal


     id1=np.empty(n)
     id1.fill(mv)
     id1[lloc] = ID[iloc]

     TYPE=2
     mv=nodataType[dataType[TYPE].namemv]
#    putmap id1 location.map
     mdat=pcraster.numpy2pcr(dataType[TYPE],id1.reshape(NROWS,NCOLS),mv)
     pcraster.report(mdat,'location.map')

#    getmap shoreInd shoreInd.map
     fmap=pcraster.readmap('shoreInd.map')
     shoreInd=pcraster.pcr_as_numpy(fmap)

     j=shoreInd[lloc]
     shoreInd=None
     shoreline = None

     m=np.isnan(j) != True
     exLoc = exLoc & (sum(m) > 0)

#-------------------------------------------
     if exLoc == False : return 0
#-------------------------------------------

     j1=j[m]
     lloc1=lloc[m]
     lookArea[j1] = id1[lloc1]

     id1=None
     
#-----------------------------------
     try:

       TYPE=4
       mv=nodataType[dataType[TYPE].name]
#    putmap lookArea lookArea.map
       mdat=pcraster.numpy2pcr(dataType[TYPE],lookArea.reshape(NROWS,NCOLS),mv)
       pcraster.report(mdat,'lookArea.map')

     except : return 1 

#-----------------------------------
     o.system('MapInterp -m maskSV.map -i lookArea.map -radius {} -nmax 1'.format(radius))

     try:
#    getmap lookArea lookArea.map
       fmap=pcraster.readmap('lookArea.map')
       lookArea=pcraster.pcr_as_numpy(fmap)
       TYPE=2
       mv=nodataType[dataType[TYPE].name]
#    putmap lookArea lookArea.map
       mdat=pcraster.numpy2pcr(dataType[TYPE],lookArea,mv)
       pcraster.report(mdat,'lookArea.map')

     except: return 1

#-------------------------------------------
# next
