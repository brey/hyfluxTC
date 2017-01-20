import numpy as np
import os
import shutil
import sys
from glob import glob
from osgeo import gdal,gdal_array
from read import readtxt
from ask import query_yes_no
from bunch import bunchify
from pmap import puttif, getmap, putmap
#from pcraster import *
import subprocess
import re

dataType={1:'Boolean',2:'Nominal',3:'Ordinal',4:'Scalar',5:'Directional',6:'Ldd'}
nodataType={'Ldd':255,'Boolean':255,'Nominal':-2147483647,'Ordinal':-2147483647,'Scalar':np.nan,'Directional':np.nan}

start=os.getcwd()
popmap=os.getenv('popmap','/home/brey/cycloneSurge/DATA/coastal_pop.tif') 
#---------------------------------------
# variables define externally
# scripts=$cycloneDir, tmpdir, nested, procDir

from project4 import * #exec $scripts./4-project.pr

#---------------------------------------
tmpdir=os.getenv('tmpdir','.') 
outexec = '>> ' +tmpdir+'/outexec.txt' 
print outexec
#---------------------------------------

dir='work'
dir=sys.argv[1]
#setexp dir $1
print dir

folder='1-1-'+start.split('/')[-1]
folder=sys.argv[2]
#setexp folder $2
print folder

#---------------------------------------
if folder == None : folder=dir
#---------------------------------------
nested=os.getenv('nested','yes')
if nested==None:
	print 'variable nested in not defined'
        sys.exit()

calcType=os.getenv('calcType','CYCLONE')
#---------------------------------------

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import map2kml
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#---------------------------------------
if 'maxLookRunup' not in locals() : maxLookRunup=10000
if 'maxLookZsea' not in locals() : maxLookZsea=5000
if 'maxDemPlace' not in locals() : maxDemPlace=50
if 'maxDemSea' not in locals() : maxDemSea=-20

if 'hmax' not in locals() : hmax=2
if 'showvar' not in locals() : showvar=1
if 'wout' not in locals() : wout=0.01 

if 'maxid' not in locals() : maxid = 1e6
if 'minzmax' not in locals() : minzmax=0.1 
if 'minloc' not in locals() : minloc = 10
if 'maxloc' not in locals() : maxloc = 1000

radius=6378388.
fact=radius*np.pi/180
outkml=False

print wout,maxid,minzmax,minloc,maxloc
#--------------------------------------
os.chdir(start)

os.chdir(dir)

idmax=0
#--------------------------------------

# CAUTION: demSea.map = ../demLatLong.map + ../faultLatLong.map 
try:
   dSea=getmap('demSea.map')
   XSTART=dSea.XSTART
   YSTART=dSea.YSTART
   CELLSIZE=dSea.CELLSIZE
   NROWS=dSea.NROWS
   NCOLS=dSea.NCOLS
   MV=dSea.nan
   GEO=dSea.GeoTr
   SP=dSea.Proj
   nband=1
except:
   print 'no demSea.map'

demSea=dSea.data

n=np.size(demSea)
 
#--------------------------------------
ID   = np.zeros(maxid)
lat  = np.empty(maxid)
lat.fill(MV)
lon = np.empty(maxid)
lon.fill(MV)
maxH = np.empty(maxid)
maxH.fill(MV)
typeVal = np.empty(maxid)
typeVal.fill(MV)
mesVal=np.empty(maxid)
mesVal.fill(MV)
#--------------------------------------
II=demSea != MV
N=np.sum(II)
#--------------------------------------
I=np.arange(n)


#demSea=demSea[II]

ulx=np.max([np.int(XSTART)-1,-240])
uly=np.min([np.int(YSTART)+1,70])
lrx=np.min([np.int(XSTART+NCOLS*CELLSIZE)+1,300])
lry=np.max([np.int(YSTART-NROWS*CELLSIZE)-1,-56])
subprocess.call('gdal_translate -projwin {} {} {} {}  -of PCRaster -ot Float32 {} popmap0.map'.format(ulx ,uly ,lrx ,lry, popmap), shell=True)
subprocess.call('resmap -clone demSea.map -i popmap0.map -o popmap.map -int 0 -nodata -32768 -setdata 0.', shell=True)

popd=getmap('popmap.map')
popden=popd.data#[II]

#--------------------------------------

dx=CELLSIZE*fact
ndig=np.int(np.log10(dx))
dx=np.int(dx/10**ndig+0.5)*10.**ndig

if 'runuplim' not in locals() : runuplim=np.min([dx/5.,200.])

print dx,runuplim

cellarea=(CELLSIZE/ (0.5/60)) **2  #  cell area in km^2

print cellarea

y=np.min([dx*10,10000.])
x=np.log(y/100)/np.log(2)
i=np.int(x)
minDem=100.*2**i

#--------------------------------------

radiusSea=maxLookZsea/fact
radiusLand0=maxLookRunup/fact

#--------------------------------------
radiusSea=np.min([np.max([radiusSea,CELLSIZE*5]),CELLSIZE*100])
radiusLand0=np.min([np.max([radiusLand0,CELLSIZE*5]),CELLSIZE*100])
radius=np.max([radiusSea,radiusLand0])

print radius,radiusSea,radiusLand0
#------------------------------------------------

if os.path.isfile(watchFile): 

# idPlace	latPlace	longPlace	zPlace	cellNear	latNear	longNear	zNear	distNear	$namePlace

     try:
        wf=readtxt(watchFile)
     
        idmax=np.max(wf.idPlace)
        ID[wf.idPlace] = wf.idPlace
        lat[wf.idPlace] = wf.latNear
        lon[wf.idPlace] = wf.longNear
        typeVal[wf.idPlace] = -1

     except: pass 

#------------------------------------------------

if os.path.isfile(mesFile) :


     #  from NGDC data base
     #idMes	latMes	longMes	distance	Hrs	Min	maxHeight	$nameMes   
     # from survey
     #idMes	latMes	longMes	mesRunup	mesInundation	mesDistRunup	$nameMes

   try:
     mf=readtxt(mesFile)
     maxHeight=mf.maxHeight

     for i in range(np.size(mf.idMes)):
        id=mf.idMes[i]
        mf.nameMes[id]=wf['$namePlace'][id+idmax]
     
     mf.idMes=mf.idMes+idmax
     idmax=np.max(mf.idMes)

     ID[mf.idMes] = mf.idMes
     lat[mf.idMes] = mf.latMes
     lon[mf.idMes] = mf.longMes

     if maxHeight != None:
       typeVal[idMes] = maxHeight*0 + 2
       mesVal[idMes] = maxHeight
       maxH[idMes] = maxHeight

   except: pass 

#------------------------------------------------

if os.path.isfile(statFile) == False :
  print 'End of post processing because file {} non exist'.format(statFile)
  sys.exit()


# idPlace	        tarr	        zmax	       tzmax	        vmax	  $namePlace

check=0

while True:
     try:
        sF=readtxt(statFile)
     except:        
	print 'problem in reading {}'.format(statFile)
        break 

     maxH[sF.idPlace] = sF.zmax

     m=maxH != MV
     if np.sum(m) == 0 : break  

     for lab1 in range(10):
          print minzmax
          m = (maxH != MV) & (maxH > minzmax)
          if (minzmax < 0.01):  break 
          if (minzmax > 1.00):  break
          if (np.sum(m) >= minloc ) & (np.sum(m) <= maxloc) : break
          if (np.sum(m) > maxloc) :  minzmax=minzmax*2
          if (np.sum(m) < minloc) :  minzmax=minzmax/2

     print minzmax, np.sum(m)

     exLoc=0
     if (np.sum(m) != 0) :

          ID = ID[m]
          lat = lat[m]
          lon = lon[m]
          maxH = maxH[m]
          typeVal = typeVal[m]
          mesVal = mesVal[m]

#--------------------------------------

          x=lon
          y=lat
          
          ix=((x-XSTART)/CELLSIZE).astype(np.int)#+1 
          iy=((YSTART-y)/CELLSIZE).astype(np.int)#+1 
         #l=(iy-1)*NCOLS+ix
          l=iy*NCOLS+ix
     
         #m0=(ix >= 1.) & (ix <= NCOLS) & (iy >= 1.) & (iy <= NROWS)
          m0=(ix >= 0.) & (ix <  NCOLS) & (iy >= 0.) & (iy <  NROWS)

          exLoc=np.sum(m0)>0




     id1=np.empty(n)
     id1.fill(MV)

     if (exLoc) :
          id1[l] = ID[m0]

     id1=id1.reshape(NROWS,NCOLS)

     try:

       TYPE=2
       putmap('location.map',id1,GEO,TYPE)
#      id1=id1[II]

     except:
       check = 1
       break    

#-----------------------------------

     maskSea=np.ones((NROWS,NCOLS))

     try:

#----------------------------------------------------------------------------------------
          TYPE=4
     #    putmap maskSea maskSV.map
          putmap('maskSV.map',maskSea,GEO,TYPE)
#----------------------------------------------------------------------------------------

     except:
          print 'maskSea problem'


     try:
#         TYPE=1
#    #    putmap maskSea maskBool.map
#         putmap('maskBool.map',maskSea,GEO,TYPE)

          subprocess.call('gdal_translate -of PCRaster -ot Byte maskSV.map maskBool.map', shell=True)

     except:
          print 'maskBool problem'

     maskSea=None

#-----------------------------------

#    getmap shoreline shoreline.map
     shorel=getmap('shoreline.map')
     shoreline=shorel.data

  #  shoreline = shoreline[II]
     shore=shoreline != shorel.nan

     TYPE=4
     m=(id1 != MV) | shore
     
     i=np.argwhere(m.flatten()==True)

     shoreInd=np.empty(n)
     shoreInd.fill(MV)
     shoreInd[i] = i

     shoreInd=shoreInd.reshape(NROWS,NCOLS)

#----------------------------------------------------------------------------------------
#    putmap shoreInd shoreInd.map
     putmap('shoreInd.map',shoreInd,GEO, TYPE)
#----------------------------------------------------------------------------------------
     subprocess.call('MapInterp -m maskSV.map -i shoreInd.map -radius {} -nmax 1'.format(radius), shell=True)

#    getmap distShore dist.map
     dis=getmap('dist.map')
     distShore=dis.data

#    distShore=distShore[II]
     w=distShore != dis.nan
     distShore[w]=distShore[w]*1000

     lookArea = np.empty(n)
     lookArea.fill(MV)

#-------------------------------------------
     if exLoc == False: break    
#-------------------------------------------

     nloc=np.size(ID)
     i=np.arange(nloc)

     lloc = l[m0]
     iloc = i[m0]


     if 'maxHeight' in locals() :
          maxHeight0=mesVal
     else:
          maxHeight0 = np.empty(nloc)
          maxHeight0.fill(MV)

     typeMes0=typeVal


     id1=np.empty(n)
     id1.fill(MV)
     id1[lloc] = ID[iloc]

     TYPE=2
#    putmap id1 location.map
     putmap('location.map',id1.reshape(NROWS,NCOLS),GEO,TYPE)

#    getmap shoreInd shoreInd.map
     shoreI=getmap('shoreInd.map')
     shoreInd=shoreI.data

     j=shoreInd.flatten()[lloc]
     shoreInd=None
     shoreline = None

     m=j != shoreI.nan
     exLoc = exLoc & (np.sum(m) > 0)


#-------------------------------------------
     if exLoc == False : break    
#-------------------------------------------

     j1=j[m].astype(np.int)
     lloc1=lloc[m]
     lookArea[j1] = id1[lloc1]

     id1=None
     
#-----------------------------------
     try:

       TYPE=4
#----------------------------------------------------------------------------------------
#    putmap lookArea lookArea.map
       putmap('lookArea.map',lookArea.reshape(NROWS,NCOLS),GEO,TYPE)
#----------------------------------------------------------------------------------------
     except:
          print 'problem with lookArea.map'
          check = 1
          break
          
#-----------------------------------
     subprocess.call('MapInterp -m maskSV.map -i lookArea.map -radius {} -nmax 1'.format(radius), shell=True)

     try:
#    getmap lookArea lookArea.map
       lookA=getmap('lookArea.map')
       lookArea=lookA.data
       TYPE=2
#    putmap lookArea lookArea.map
       putmap('lookArea.map',lookArea,GEO,TYPE)

#      lookArea = lookArea[II]
     except:          
          check = 1
          break

     break
#-------------------------------------------
# next
if check == 1 : sys.exit()
#-------------------------------------------

#--------------------------------------
while True:
  try:
   # getmap zwmax zwmax.map
     zwm=getmap('zwmax.map')
     zwmax=zwm.data
#    zwmax=zwmax[II]
  except:
     print 'no zwmap.map'

  m=((distShore != MV) & (zwmax != MV) & (demSea != MV )) & (demSea > 0. )
  radiusLand=CELLSIZE
  if (np.sum(m) > 0) : radiusLand=radiusLand+np.max(distShore[m])
  radiusLand=np.min([np.max([radiusLand,2*CELLSIZE]),radiusLand0])

  print radiusLand, radiusSea

  land= ((shore != True) & (demSea != MV)) & (demSea >= 0.) & (demSea < maxDemPlace) & ( (distShore <= radiusLand) &  (distShore != MV) )
  sea= ((shore != True) & (land != True) & (demSea != MV)) & (demSea < maxDemSea) & ( (distShore <= radiusSea) &  (distShore != MV) ) & (distShore >= CELLSIZE)

  runup = list(land+shore)
  runup = np.array(runup)

  print np.sum(shore),np.sum(land),np.sum(sea),np.sum(runup)

  TYPE=2
# mv=nodataType[dataType[TYPE]]

  areaShore=np.empty((NROWS,NCOLS))
  areaShore.fill(MV)
  if (np.sum(shore) > 0) :
#   q=np.argwhere(shore.flatten() == II.flatten())[1:-1]
#   areaShore[q]=lookArea[shore]
    areaShore[shore]=lookArea[shore]
  # putmap areaShore areaShore.map
    putmap('areaShore.map',areaShore,GEO,TYPE)
# areaShore=areaShore[II.flatten()]


  areaRunup=np.empty((NROWS,NCOLS))
  areaRunup.fill(MV)
  if (np.sum(runup) > 0) :
#   q=np.argwhere(runup.flatten() == II.flatten())[1:-1]
#   areaRunup[q] = lookArea[runup]
    areaRunup[runup] = lookArea[runup]
  # putmap areaRunup areaRunup.map
    putmap('areaRunup.map',areaRunup, GEO, TYPE)
# areaRunup=areaRunup[II.flatten()]

# runup = None

  areaSea=np.empty((NROWS,NCOLS))
  areaSea.fill(MV)
  if (np.sum(sea) > 0) :
#   q=np.argwhere(sea.flatten() == II.flatten())[1:-1]
#   areaSea[q] = lookArea[sea]
    areaSea[sea] = lookArea[sea]
  # putmap areaSea areaSea.map
    putmap('areaSea.map',areaSea,GEO,TYPE)
# areaSea=areaSea[II.flatten()]
# sea=None

#--------------------------------------

  try :
     #getmap depth0 depth.map
       depth=getmap('depth.map')
       depth0=depth.data
  except:  
     # getmap depth0 depth000.000
       depth=getmap('depth000.000')
       depth0=depth.data

  #getmap depth hzwmax.map
  hzwm=getmap('hzwmax.map')
  depth=hzwm.data

  Hmax = depth-depth0

  depth == None
  depth0  == None
# Hmax=Hmax[II]

#--------------------------------------
# wet fraction: MV replaced by 0

  #getmap wfr0 wetfrac.map
  wetf=getmap('wetfrac.map')
  wfr0=wetf.data
# wfr0 = wfr0[II]
  #getmap wfr wzwmax.map
  wzwm=getmap('wzwmax.map')
  wfr=wzwm.data
# wfr=wfr[II]

  if (np.sum(land) > 0) :
#   wfr0[I[land.flatten()]] = 0
    wfr0[land] = 0


  wetfrac=wfr-wfr0

  m= wetfrac == MV
  if (np.sum(m) > 0) :
#    wetfrac.flatten()[I[m.flatten()]] = 0
     wetfrac[m] = 0

#--------------------------------------

  distWet=np.empty((NROWS,NCOLS))
  distWet.fill(MV)

  m1=((distShore != MV) & (wfr != MV ) & (wfr0 != 1) & (demSea != MV)) & (distShore >= 0) & (zwmax != MV)
     
  if (np.sum(m1) > 0) :
    distWet[m1] = (distShore[m1] + (wfr[m1]-0.5)*CELLSIZE)*fact

  TYPE=4
  buff = np.empty((NROWS,NCOLS))
  buff.fill(MV)
# buff[II.flatten()] = distWet
  buff = distWet
#----------------------------------------------------------------------------------------
  #putmap buff landWet0.map
  putmap('landWet0.map',buff,GEO,TYPE)
#----------------------------------------------------------------------------------------

  subprocess.call('mapsum -m maskBool.map -i landWet0.map -radius {} -idist idistWet.map'.format(CELLSIZE*2), shell=True)
  #getmap idist idistWet.map
  idistW=getmap('idistWet.map')
  idist=idistW.data
# idist=idist.flatten()[II.flatten()]

  m2=(np.isnan(idist) != True)  & (idist <= 1.5) & (shore == True)
  if (np.sum(m2) > 0) :
     distWet[m2] = (wfr[m2]-wfr0[m2])*CELLSIZE*fact


#--------------------------------------
  land,m1,m2,idist,distShore,shore = ([] for i in range(6))
#------------------------------------
#setexp DRIVER GTiff

  m=(distWet == MV ) & (wetfrac <= wout)
#--------------------------------------
  #distWet[I[m]]=np.nan
  buff = np.empty((NROWS,NCOLS))
  buff.fill(99999.)
  buff = distWet
  buff[buff==MV]=99999.
  #putmap buff inundationDist.tif
  puttif('inundationDist.tif', buff,GEO,SP,nband,99999.)
#--------------------------------------

  Hmax[m]=99999.
  buff = np.empty((NROWS,NCOLS))
  buff.fill(99999.)
  buff = Hmax
  #  putmap buff inundationDepth.tif
  puttif('inundationDepth.tif', buff, GEO, SP, nband,99999.)

  Hmax = None

#--------------------------------------
     
  zwmax0=np.copy(zwmax)

  zwmax0[m]=99999.
  buff = np.empty((NROWS,NCOLS))
  buff.fill(99999.)
  buff = zwmax0
  #  putmap buff inundationHeight.tif
  puttif('inundationHeight.tif', buff, GEO, SP, nband,99999.)
     
  zwmax0=None

#--------------------------------------
  wm=wetfrac!=MV
  popWet=np.empty((NROWS,NCOLS))
  popWet.fill(MV)
  popWet[wm] = popden[wm] * wetfrac[wm]  
  buff = np.empty((NROWS,NCOLS))
  buff.fill(MV)
  buff = popWet
  buff[0,0]= 99999.
  buff[-1,-1]= 99999.
  #  putmap buff inundationPop.tif
  puttif('inundationPop.tif', buff, GEO, SP, nband,99999.)

     
  #  DRIVER=PCRaster
  TYPE=4
  buff[0,0]= MV
  buff[-1,-1]= MV
  #  putmap buff inundationPop.map
  putmap('inundationPop.map',buff,GEO, TYPE)

#--------------------------------------
  #  DRIVER=GTiff

  wetfrac[m] = 99999.
  buff = np.empty((NROWS,NCOLS))
  buff.fill(99999.)
  buff = wetfrac
   # putmap buff inundationFrac.tif
  puttif('inundationFrac.tif', buff, GEO, SP, nband,99999.)

   # DRIVER=PCRaster
  TYPE=4
   # putmap buff inundationFrac.map
  putmap('inundationFrac.map', buff, GEO, TYPE)

  wfr0=None
  wfr=None

#--------------------------------------

     #getmap vnmax vnmax.map
  vnm=getmap('vnmax.map')
  vnmax=vnm.data
# vnmax=vnmax[II]


   # DRIVER=GTiff

  vnmax[m]=99999.
  buff = np.empty((NROWS,NCOLS))
  buff.fill(99999.)
  buff = vnmax
   # putmap buff inundationVel.tif
  puttif('inundationVel.tif', buff, GEO, SP, nband,99999.)
     
  vnmax=None

#--------------------------------------
  buff=None

#--------------------------------------
  if exLoc :
#--------------------------------------

          ncalc=np.size(ID)

          zmaxSea=np.empty(ncalc)
          zmaxSea.fill(MV)
          zmaxShore=np.empty(ncalc)
          zmaxShore.fill(MV)
          
          tzmaxShore=np.empty(ncalc)
          tzmaxShore.fill(MV)
          hmaxShore=np.empty(ncalc)
          hmaxShore.fill(MV)

          tarrShore=np.empty(ncalc)
          tarrShore.fill(MV)

          simInundation=np.empty(ncalc)
          simInundation.fill(MV)

          simRunup=np.empty(ncalc)
          simRunup.fill(MV)
          simDistRunup=np.empty(ncalc)
          simDistRunup.fill(MV)
          
          maxHeight = np.empty(ncalc)
          maxHeight.fill(MV)
          typeMes = np.empty(ncalc)
          typeMes.fill(MV)

          popInundated = np.empty(ncalc)
          popInundated.fill(MV)
          areaInundated = np.empty(ncalc)
          areaInundated.fill(MV)

#---------------------------------------
          if os.path.isfile('tarr.map') :
          #getmap tarr tarr.map
             tar=getmap('tarr.map')
             tarr=tar.data
            #tarr=tarr[II]

          if os.path.isfile('tzwmax.map') :
          #getmap tzwmax tzwmax.map
             tzwm=getmap('tzwmax.map')
             tzwmax=tzwm.data
           # tzwmax=tzwmax[II]

#---------------------------------------
          j=-1

          for i in range(ncalc):
               id=ID[i]
          
               if ( not  m0[i] ): continue
               m=(lookArea != MV) & (lookArea == id)
               if (np.sum(m) == 0) : continue
          
               j=j+1

               ID[j] = ID[i]
               lat[j] = lat[i]
               lon[j] = lon[i]
          

#---------------------------------------
# areaSea

               m=(areaSea != MV) & (zwmax != MV) & (areaSea == id)
               if (np.sum(m) != 0) :
                  zmaxSea[j]=np.max(zwmax[m])

#---------------------------------------
# areaRunup

               maxHeight[j] = maxHeight0[i]
               typeMes[j] = typeMes0[i]

               m=(areaRunup != MV) & (zwmax != MV) & (distWet != MV ) & (areaRunup == id) 

               if (np.sum(m) != 0):  

# -------------- simInundation

                    simInundation[j]=np.max(zwmax[m])

# ------------- popInundated

                    m1=(areaRunup != MV ) & (popWet != MV ) & (areaRunup  == id)
          
                    if (np.sum(m1) != 0) :

                       popInundated[j] = cellarea*np.sum(popWet[m1])
                       if (popInundated[j] < 10.) :
                          popInundated[j]=MV
                       else:
                          ndig=np.max([np.int(np.log10(popInundated[j]))-1,1])
                          popInundated[j]=np.int(popInundated[j]/(10.**ndig))*(10.**ndig)
   

# ------------- areaInundated

                    m1=(areaRunup != MV) & (wetfrac != MV ) & (areaRunup  == id)

                    if (np.sum(m1) != 0) :
                       end5=0

                       areaInundated[j] = cellarea*np.sum(wetfrac[m1])
                       if (areaInundated[j] < 0.01 or popInundated[j] == MV) :
                          popInundated[j]=MV
                          areaInundated[j]=MV
                          end5=1

                    if(end5 != 1) : 
                       ndig=np.max([np.int(np.log10(areaInundated[j]))-1,-2])
                       if (ndig >= 0) :
                          areaInundated[j]=np.int(areaInundated[j]/(10.**ndig))*(10.**ndig)
                       else:
                          ndig=-ndig
                          areaInundated[j]=np.int(areaInundated[j]*(10.**ndig))/(10.**ndig)

#-------------- simDistRunup, simRunup  !!!!!! CHECK THIS ONE

                    simDistRunup[j] = np.max(distWet[m])

                    mm=(simDistRunup[j] == distWet)
     
                    if (np.sum(mm) != 0) :
                       simRunup[j] = zwmax[mm][0]


                       if (simDistRunup[j] < runuplim) :
                          simDistRunup[j]=MV
                          simRunup[j]=MV

                       else:
                          ndig=np.max([np.int(np.log10(simDistRunup[j]))-1,1])
                          simDistRunup[j]=np.int(simDistRunup[j]/(10.**ndig))*(10.**ndig)

#------------------------------

       #       $$$$ end2

#------------------------------

# areaShore

                    m=(areaShore != MV) & (zwmax != MV ) & (areaShore == id)

                    if (np.sum(m) == 0):  continue 
     
                    mzwmax=np.max(zwmax[m])
                    zmaxShore[j]=mzwmax

                    try:
                       tarrShore[j]=np.min(tarr[m])
                    except:
                       print 'no tarr.map ? '
                    try:
                       mm=mzwmax == zwmax
                       tzmaxShore[j]=np.min(tzwmax[mm])
                    except:
                       print 'no tzwmax.map ? '




#-----------------------------------------------------------------------

          if j<0 : 
                j=0
          else:
                j=j+1

          ncalc=j

          l=np.arange(ncalc)


          ID = ID[:j]
          lat = lat[:j]
          lon = lon[:j]

          zmaxSea=zmaxSea[:j]
          zmaxShore=zmaxShore[:j]
          
          tzmaxShore=tzmaxShore[:j]
          hmaxShore=hmaxShore[:j]

          tarrShore=tarrShore[:j]
          
          simInundation=simInundation[:j]

          simRunup=simRunup[:j]
          simDistRunup=simDistRunup[:j]

          maxHeight = maxHeight[:j]
          typeMes = typeMes[:j]

          popInundated = popInundated[:j]
          areaInundated = areaInundated[:j]


#-----------------------------------------------------------------------
          mesInundation=np.empty(ncalc)
          mesInundation.fill(MV)
          mesRunup=np.empty(ncalc)
          mesRunup.fill(MV)
          mesDistRunup=np.empty(ncalc)
          mesDistRunup.fill(MV)

#-----------------------------------------------------------------------
          m=tarrShore == MV 
          if (np.sum(m) > 0) :
               j=l[m]
               tarrShore[j] = tzmaxShore[j]

#-----------------------------------------------------------------------

          simMaxHeight=np.zeros(ncalc)

          var=zmaxSea

          m=(var != MV )
          if (np.sum(m) > 0) :
               simMaxHeight[m]=np.maximum(simMaxHeight[m],var[m])


          var=zmaxShore

          m=(var != MV  )
          if (np.sum(m) > 0) :
               simMaxHeight[m]=np.maximum(simMaxHeight[m],var[m])


          var=simInundation

          m=(var != MV  )
          if (np.sum(m) > 0) :
               simMaxHeight[m]=np.maximum(simMaxHeight[m],var[m])
#-----------------------------------------------------------------------

          simlab=np.column_stack([ID, lat, lon, zmaxSea, zmaxShore, simInundation, simRunup, simDistRunup, tarrShore, tzmaxShore,  simMaxHeight, popInundated, areaInundated])
          he=('ID', 'lat', 'lon', 'zmaxSea', 'zmaxShore', 'simInundation', 'simRunup', 'simDistRunup', 'tarrShore', 'tzmaxShore',  'simMaxHeight', 'popInundated', 'areaInundated')

          simlab[simlab<-180.]='NaN'  # in order to write NaN in the inundation.txt file below'

          if  np.sum( maxHeight != MV) != 0  :
               simlab=np.column_stack([simlab,maxHeight]) 
               he=he+('maxHeight',)

          simdic=dict(zip(he,simlab.T))

          fmt="\t".join(['%10s']*(np.shape(simlab)[1]))
          nameP=[]
          for ii in range(ID.size):
             ip=list(sF['idPlace']).index(ID[ii].astype(int))
             nameP.append(sF['$namePlace'][ip])
          simlab=np.column_stack([simlab,nameP]) 


     #    write inundation.txt tab $simlab  $meslab \$namePlace
          he=he+(' $namePlace',)
          he="\t".join(he)
          fmt=fmt+'\t%20s'


          simlab[simlab=='nan']='NaN'  # to adapt to mergeLoc format


          np.savetxt('inundation.txt',simlab,header=he, fmt=fmt, comments='')

          simdic['namePlace']=nameP



#         $$$$ inundationEnd

# lookArea=None
# wetfrac=None
# areaShore=None
# wmax=None
# areaRunup=None
# areaSea=None
# distWet=None


#    check

#--------------------------------------
# image setup
#--------------------------------------
  try:

     #siz=15e6
     #prod=8

     siz=4.e6
     prod=4.


     xshift=120

     pixsize0=1040./NCOLS
     pixsize=np.min([np.max([np.int(pixsize0),1.]),2.])

     scale0=np.sqrt(siz/((NCOLS+xshift/pixsize)*NROWS))/pixsize
     scale=np.min([np.max([np.int(scale0),1]),prod/pixsize])

     bulletsize=1

     print pixsize0, scale0, pixsize, scale

#     scale=?scale
#     pixsize=?pixsize

     f=open('imageData.txt','w')
     f.writelines('pixsize0    ={}\n'.format(pixsize0))
     f.writelines('scale0    ={}\n'.format(scale0))
     f.writelines('pixsize    ={}\n'.format(pixsize))
     f.writelines('scale    ={}\n\n'.format(scale))
     f.writelines('NCOLS    ={}\n'.format(NCOLS))
     f.writelines('NROWS    ={}\n'.format(NROWS))
     f.writelines('xshift/pixsize    ={}\n'.format(xshift/pixsize))
     f.writelines('(NCOLS+xshift/pixsize)*NROWS/1e6    ={}\n\n'.format((NCOLS+xshift/pixsize)*NROWS/1e6))
     f.writelines('(scale*pixsize)**2*(NCOLS+xshift/pixsize)*NROWS/1e6    ={}\n'.format((scale*pixsize)**2*(NCOLS+xshift/pixsize)*NROWS/1e6))
     f.close()


#--------------------------------------------
# input for kml image

     north=YSTART
     south=YSTART-CELLSIZE*NROWS
     west0=XSTART
     west=west0
     westl=XSTART-CELLSIZE*(xshift/pixsize)
     east=XSTART+CELLSIZE*NCOLS

     f=open('imageData.sh','w')
     f.writelines('export scale={}\n'.format(scale))
     f.writelines('export pixsize={}\n'.format(pixsize))
     f.writelines('export cellsize={:<10.4f}  # sec\n'.format(CELLSIZE*3600))
     f.writelines('export xshift={}\n'.format(xshift))
     f.writelines('export north={:<8.4f}\n'.format(north))
     f.writelines('export south={:<8.4f}\n'.format(south))
     f.writelines('export westl={:<8.4f}\n'.format(westl))
     f.writelines('export west={:<8.4f}\n'.format(west))
     f.writelines('export east={:<8.4f}\n'.format(east))
     f.close()

#--------------------------------------------
# kml file
#--------------------------------------------

#    echo off
#    echo ?on/off
     outkml=True
     
     if os.path.isfile(folder+'.kml' ) : os.remove(folder+'.kml')
          
     output=folder+'.kml'
     f=open(output,'w')
          
     map2kml.startkml(f,project,folder)
          
     f.writelines('<Folder>\n')
     f.writelines('<name>Raster maps</name>\n')
     

   # if (maxcc) close all
     

     palette='-palette gyor -palout WM'
     
   # do setdolegend
     if (nested == 'yes') :
          dolegend='-mapshift 0 0 -doLegend no no' 
          #resize='-resize yes'
          resize='-resize no'
          west=west0
     else:
          dolegend=' -mapshift {} 0 -doLegend yes yes'.format(xshift)
          resize='-resize no'
          west=westl
   # end setdolegend

     
     legendZmax=' -Slegend 0.2 3.2 5 -leglog yes -Smin 0.2 -nstepcol 1 {}'.format(dolegend)
     legendVel=' -Slegend 0.1 1.6 5 -leglog yes -Smin 0.1 -nstepcol 1 {}'.format(dolegend)
  
     paletteFrac='-palette cbB  -palout WB' 
     legendFrac=' -Slegend 0.1 1 10  -Smin 0.2 -nstepcol 3 {}'.format(dolegend)
     
     palettePop='-palette WmM  -palout WM' 
     legendPop=' -Slegend 1 10000 5  -Smin 1 -nstepcol 3 -leglog yes {} '.format(dolegend)
     
     smin=np.min([30,dx*0.5])
     legendWet=' -Slegend 30 480 5 -leglog yes -Smin {} -nstepcol 1 {}'.format(smin,dolegend)
     
     legendLand=' -Slegend 0.3 4.8 5 -leglog yes -Smin 0.3 -nstepcol 1 {}'.format(dolegend)
          
     legendVelLand=' -Slegend 0.05 4.05 5 -leglog yes -Smin 0.05 -nstepcol 1 {}'.format(dolegend)
     legendZ='  -Slegend -{} {} 11 -leglog no -Smin 0.01 -palette bcWyr -palout BM {}'.format(hmax,hmax,dolegend)

     legendDem='-Slegend -{} {} 7 -palette BbcWGO -palout BW -nstepcol 2  -leglog no {}'.format(minDem,minDem/2,dolegend) 
     

  except:
     print 'image problem'
     sys.exit()
#-------------------------------------------

  try:
# enter 1 if you want eps file
   # ext1=query_yes_no('enter no if you want eps file', default='yes')
     ext1=-1

     def doGif(mapn,resize):
          filekml='{}.gif'.format(mapn) 
          out='-o {} {}'.format(filekml,resize)
          if ext1 < 0 : return out,filekml
          out='{} -outps {}.eps'.format(out,mapn)
          return out,filekml
     
     def doPng(mapn,resize):
          filekml='{}.png'.format(mapn) 
          out='-o {}  {}'.format(filekml,resize)
          if ext1 < 0: return out,filekml
          out='{} -outps {}.eps'.format(out,mapn)
          return out,filekml
     
     def doJpeg(mapn,resize):
          filekml='{}.jpeg  {}'.format(mapn,resize)
          out='-o {}'.format(filekml)
          if ext1 < 0 : return out,filekml
          out='{} -outps {}.jpeg'.format(out,mapn)
          return out,filekml
     
     def doOut(mapn,resize):
         return doPng(mapn,resize)
     
#-------------------------------------------
     visibility=1
#-------------------------------------------

     try:
          mapn='inundationFrac.tif'
          out,filekml=doPng(mapn,resize)
     
          subprocess.call('map2image  -i {}   {} -pixsize {} -scale {}  {}   {}  {}'.format(mapn,out,pixsize,scale,paletteFrac,legendFrac,outexec), shell=True)
     
          map2kml.dokml(f,visibility,filekml,north,south,east,west)

     except:
	print 'problem with {}'.format(mapn)
#-------------------------------------------
     try:

          mapn='inundationPop.tif'
          out,filekml=doPng(mapn,resize)

          subprocess.call('map2image  -i {}   {} -pixsize {} -scale {}  {}   {}  {}'.format(mapn,out,pixsize,scale,palettePop,legendPop,outexec), shell=True)
     
          map2kml.dokml(f,visibility,filekml,north,south,east,west)

     except:
	print 'problem with {}'.format(mapn)
#-------------------------------------------

   # ans=query_yes_no('enter no if you want only the previous figure', default='yes')
   # if ans > 0 :

#-------------------------------------------
     try:

          mapn='shoreline.map'
          out,filekml=doPng(mapn,resize)

          subprocess.call('map2image  -i {}   {} -pixsize {} -scale {}  {}   {}  {}'.format(mapn,out,pixsize,scale,palette,dolegend,outexec), shell=True)

     #west=west0
          map2kml.dokml(f,visibility,filekml,north,south,east,west)

     except:
	print 'problem with {}'.format(mapn)
     #exec setdolegend

#-------------------------------------------
     visibility=0
#-------------------------------------------
     try:

          mapn='inundationDist.tif'
          out,filekml=doPng(mapn,resize)
     
          subprocess.call('map2image  -i {}   {} -pixsize {} -scale {}  {}   {}  {}'.format(mapn,out,pixsize,scale,palette,legendWet,outexec), shell=True)

          map2kml.dokml(f,visibility,filekml,north,south,east,west)


     except:
	print 'problem with {}'.format(mapn)

#-------------------------------------------
     try:

          mapn='inundationHeight.tif'
          out,filekml=doPng(mapn,resize)

          subprocess.call('map2image  -i {}   {} -pixsize {} -scale {}  {}   {}  {}'.format(mapn,out,pixsize,scale,palette,legendLand,outexec), shell=True)

          map2kml.dokml(f,visibility,filekml,north,south,east,west)

     except:
	print 'problem with {}'.format(mapn)
#-------------------------------------------
     if (calcType == 'TSUNAMI') :

          mapn='../deformation.tif'
          if (os.path.isfile(mapn)) :
               shutil.copy(mapn,'.')
               mapn='deformation.tif'
          else:
               mapn='fault.map'

          if (os.path.isfile(mapn)) :

               out,filekml=doPng(mapn,resize)

               subprocess.call('map2image  -i {}   {} -pixsize {} -scale {}  {}   -Smin 0.1  {}'.format(mapn,out,pixsize,scale,legendZ,outexec), shell=True)

               map2kml.dokml(f,visibility,filekml,north,south,east,west)
#-------------------------------------------
     else: 

          mapn='u10max.map'
          if (os.path.isfile(mapn)) :
     
            try:

               out,filekml=doPng(mapn,resize)
          
               legend='  -Slegend 18 42 5 -leglog yes -Smin 18 -nstepcol 1  {}'.format(dolegend)
     
               subprocess.call('map2image  -i {}   {} -pixsize {} -scale {}  {}   {}  {}'.format(mapn,out,pixsize,scale,palette,legend,outexec), shell=True)

               map2kml.dokml(f,visibility,filekml,north,south,east,west)

            except:
               print 'problem with {}'.format(mapn)

     if (calcType == 'CYCLONE') :

          mapn='deprmax.map'
          if (os.path.isfile(mapn)) :

            try:

               out,filekml=doPng(mapn,resize)
               # 5.0000   G    10.000   Y    20.000   O    40.000    R   80.000   M
               legend=' -Slegend 5 80 5 -leglog yes -Smin 5 -nstepcol 1   {}'.format(dolegend)

               subprocess.call('map2image  -i {}   {} -pixsize {} -scale {}  {}   {}  {}'.format(mapn,out,pixsize,scale,palette,legend,outexec), shell=True)
     
               map2kml.dokml(f,visibility,filekml,north,south,east,west)

            except:
               print 'problem with {}'.format(mapn)

#-------------------------------------------

# enter goto endkml if you want only the previous figure
   # ans=query_yes_no('enter no if you want only the previous figure', default='yes')
   # if ans > 0 :

#-------------------------------------------
     visibility=0

     try:

          mapn='inundationDepth.tif'
          out,filekml=doOut(mapn,resize)

          subprocess.call('map2image  -i {}   {} -pixsize {} -scale {}  {}   {}  {}'.format(mapn,out,pixsize,scale,palette,legendLand,outexec), shell=True)
          map2kml.dokml(f,visibility,filekml,north,south,east,west)
     
     except:
	print 'problem with {}'.format(mapn)

#-------------------------------------------
     try:

          mapn='inundationVel.tif'
          out,filekml=doOut(mapn,resize)

          subprocess.call('map2image  -i {}   {} -pixsize {} -scale {}  {}   {}  {}'.format(mapn,out,pixsize,scale,palette,legendVelLand,outexec), shell=True)


          map2kml.dokml(f,visibility,filekml,north,south,east,west)

     except:
	print 'problem with {}'.format(mapn)
#-------------------------------------------
     try:

          mapn='zwmax.map'
          out,filekml=doOut(mapn,resize)

          subprocess.call('map2image  -i {}   {} -pixsize {} -scale {}  {}   {}  {}'.format(mapn,out,pixsize,scale,palette,legendZmax,outexec), shell=True)
          map2kml.dokml(f,visibility,filekml,north,south,east,west)
     
     except:
	print 'problem with {}'.format(mapn)

#-------------------------------------------
     try:

          mapn='vnmax.map'
          out,filekml=doOut(mapn,resize)

          subprocess.call('map2image  -i {}  {} -pixsize {}  -scale {}   {}   {}  {}'.format(mapn,out,pixsize,scale,palette,legendVel,outexec), shell=True)

          map2kml.dokml(f,visibility,filekml,north,south,east,west)

     except:
	print 'problem with {}'.format(mapn)
#-------------------------------------------
     try:

          mapn='demSea.map'
          out,filekml=doOut(mapn,resize)

          subprocess.call('map2image  -i {} shoreline.map {} -pixsize {}  -scale {}  {}  {}'.format(mapn,out,pixsize,scale,legendDem,outexec), shell=True)

          map2kml.dokml(f,visibility,filekml,north,south,east,west)

     except:
	print 'problem with {}'.format(mapn)
#-------------------------------------------
     try:

          mapn='popmap.map'
          out,filekml=doOut(mapn,resize)

          subprocess.call('map2image  -i  {}  {} -pixsize {}  -scale {} {} {} {}'.format(mapn,out,pixsize,scale,palettePop,legendPop,outexec), shell=True)
     
          map2kml.dokml(f,visibility,filekml,north,south,east,west)

     except:
	print 'problem with {}'.format(mapn)
#--------------------------------------
     f.writelines(' </Folder>\n')  # close image folder


     if (exLoc) :

#--------------------------------------
     # enter goto endcalc if you do NOT want locations

#----------------------------------
          showvar=1
          simdic['maxHeight']=maxHeight
          simdic['typeMes']=typeMes
          simdic['minzmax']=minzmax
          simdic['showvar']=showvar
          simdic['zmaxSea']=zmaxSea
          simdic['zmaxShore']=zmaxShore
          simdic['hmaxShore']=hmaxShore
          simdic['tarrShore']=tarrShore
          simdic['tzmaxShore']=tzmaxShore
          simdic['simInundation']=simInundation
          simdic['mesInundation']=mesInundation
          simdic['simRunup']=simRunup
          simdic['mesRunup']=mesRunup
          simdic['simDistRunup']=simDistRunup
          simdic['mesDistRunup']=mesDistRunup
          simdic['nan']=MV
          simdic=bunchify(simdic)          


          map2kml.watchMes(f,simdic)
     
#----------------------------------
  except Exception,e:
     print e
     print 'problem with kml'
     break

  break

if (outkml) :
     map2kml.endkml(f)
     f.close() #$folder..kml

#rmfiles=('dist.map','idist.map','maskSV.map','shoreInd.map','idistWet.map','maskBool.map')  
try:
     for f in rmfiles:
          os.remove(f)
except:
     print 'problem with rmfiles'

#delete xml files
for xfile in os.listdir('.'):
  if re.search('.xml',xfile): os.remove(xfile)

zipfiles=glob('./*.png')
zipfiles.extend([folder+'.kml'])
zipfiles.extend(['watch.txt'])
zipfiles.extend(['summary.txt'])
zipfiles.extend(glob('watch/*'))  
zipfiles=' '.join(map(str,zipfiles))

subprocess.call('zip {}.kmz {} {}'.format(folder,zipfiles,outexec), shell=True)

os.chdir(start)
     
print 'done', radiusLand*fact


