import numpy as np
import os
import xml.etree.ElementTree as et
from xmldic import bxml
import sys
from read import readf
from xmlreader import xmlr
from read import readtxt
import matplotlib.pyplot as plt


#*************************************************************************
#             makeLogs
# run the script on the hurricane directory
#*************************************************************************

def makeLogs(bulNo=1):

     lastBul=np.str(bulNo)+'/'
     
     f=open('logs.sh','w')
     
     np.savetxt(f,[])

     tree=et.parse(lastBul+'info.xml')
     info=bxml(lastBul+'info.xml').setexp
     
     if 'typeData' in info.keys() :
      typeData = np.int(info.typeData)
     else:
      typeData = 1
     
#    bulInfo=bxml(lastBul+'bulInfo.xml').data
     bulInfo=readtxt(lastBul+'bulInfo.txt')
     advNo=np.array(bulInfo.advNo).astype(int)
     tShift=np.array(bulInfo.tShift).astype(float)
     land=np.array(bulInfo.land).astype(int)
     notes=np.array(bulInfo.notes)

     nm2m=1852.
     kt2ms=nm2m/3600.  # knots to m/s

     nadv = np.size(advNo)
     first=1

     #lat=np.zeros(nadv)
     #lon=np.zeros(nadv)
     #time=np.zeros(nadv)
     #vmax=np.zeros(nadv)
     labt=[None]*3
     labv=[None]*3
     labx=[None]*3
     laby=[None]*3
     k=0


     for i in range(nadv): 
       bulNumber=advNo[i]

       try:
          inpdat=xmlr(np.str(bulNumber)+'/inpData.xml')
#         inpdat=readtxt(np.str(bulNumber)+'/inpData.txt')
       except IOError as e:
          print e
          outdat=readf(np.str(bulNumber)+'/outData.txt')

          vmaxF=outdat.vmax0[0]
          maxvmax=np.max(outdat.vmax0)
          minvmax=np.min(outdat.vmax0)
          nvel=np.size(outdat.vmax0)
          nwr=nvel*4
          land=0

          with open('longSign.pr', 'w') as text_file:
            text_file.write('export vmax1={} maxvmax={} minvmax={} nvel={} nwr={} land={}'.format(vmaxF,maxvmax,minvmax,nvel,nwr,land))

          sys.exit()


#----------------------------------------------------

       # RSMC
       if (typeData == 2): inpdat.vmax=inpdat.vmax*kt2ms
     
       # NOAA
       if (typeData==3): inpdat.vmax=inpdat.vmax*kt2ms

       time = inpdat.time+tShift[i]
       vmax = inpdat.vmax

#-----------------------
       sig=np.sign(inpdat.lon)
     
       if (first):
             sig1=sig[0]
             first=0
             with open('longSign.pr', 'w') as text_file:
                 text_file.write('sig1={:.0f}'.format(sig1)) 

       m=sig != sig1
       if np.sum(m) != 0  :        
             if (sig1 > 0) : inpdat.lon[m]=inpdat.lon[m]+360
             if (sig1 < 0) : inpdat.lon[m]=inpdat.lon[m]-360
     
#-----------------------

       lat = inpdat.lat
       lon = inpdat.lon

       if (i >= nadv-3) :
           labt[k]=time
           labv[k]=vmax
           labx[k]=lon
           laby[k]=lat
           k+=1

       vmaxF=np.int(inpdat.vmax[0]+0.5)
       maxvmax=np.int(np.max(inpdat.vmax)+0.5)
       minvmax=np.int(np.min(inpdat.vmax)+0.5)
       nvel=np.size(inpdat.vmax)
       nwr=np.sum(np.concatenate((inpdat.ne34,inpdat.se34,inpdat.sw34,inpdat.nw34)) > 0)
      #nwr=np.sum(np.concatenate((inpdat['34ne'],inpdat['34se'],inpdat['34sw'],inpdat['34nw'])) > 0)

       #setexp date " $$date@(advNo_i) "
       f.write('export bulNumber={} vmax1={} maxvmax={} minvmax={} nvel={} nwr={} land={}\n'.format(bulNumber,vmaxF,maxvmax,minvmax,nvel,nwr,land[i]))



     f.close()

#################################
     for i in range(2):
      try:
        labt.remove(None)
        labv.remove(None)
        labx.remove(None)
        laby.remove(None)
      except: pass

     labt=np.array(labt)
     labv=np.array(labv)
     labx=np.array(labx)
     laby=np.array(laby)

     labels=[]
     for i in range(labt.shape[0]):
        labels.append('bul{}'.format(i+1))

     plt.figure()
     for t,d in zip(labt.T,labv.T):
      plt.plot(t,d)
     plt.xlabel('time [h]')
     plt.ylabel('vmax [m/sec]')
     plt.legend(labels, loc=0) 
     plt.savefig('vmax.png')

     plt.figure()
     for t,d in zip(labx.T,laby.T):
      plt.plot(t,d)
     plt.xlabel('lon')
     plt.ylabel('lat')
     plt.legend(labels, loc=0) 
     plt.savefig('tracks.png')



if __name__ == "__main__":
    bul=sys.argv[1]
    makeLogs(bul)
