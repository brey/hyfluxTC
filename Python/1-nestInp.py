import os
import sys
import numpy as np
from shutil import copyfile
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, parse
from xml.etree.ElementTree import SubElement
from xml.dom import minidom
#import pcraster
from pmap import getmap



# variables defined externally 

inputf=os.getenv('input','Calc_input_deck.txt')
outDir=os.getenv('outDir')
boundaryDir=os.getenv('boundaryDir',outDir)
maskmap=os.getenv('maskmap')

batgrid=os.getenv('batgrid')

#---------------------------
# evaluate new window
#---------------------------

# get lat,lon from maskmap

#getmap mask $maskmap
try:
#    fmap=pcraster.readmap(maskmap)
#    mask=pcraster.pcr_as_numpy(fmap)
     dat=getmap(maskmap)
except:
     print 'problem with maskmap'
     sys.exit()

#imap=pcraster.clone()
#XSTART=imap.west()
#YSTART=imap.north()
#CELLSIZE=imap.cellSize()
#NROWS=imap.nrRows()
#NCOLS=imap.nrCols()

XSTART=dat.XSTART
YSTART=dat.YSTART
CELLSIZE=dat.CELLSIZE
NROWS=dat.NROWS
NCOLS=dat.NCOLS
mask=dat.data


#m=np.isnan(mask) != True
m=mask != dat.nan
maxcc=np.sum(m) == 0
if maxcc == True : sys.exit()

i=np.argwhere(m)[:,1]
j=np.argwhere(m)[:,0]

lon=XSTART+CELLSIZE*(i+0.5)
lat=YSTART-CELLSIZE*(j+0.5)


lonmin=np.min(lon)-CELLSIZE/2
lonmax=np.max(lon)+CELLSIZE/2

latmin=np.min(lat)-CELLSIZE/2
latmax=np.max(lat)+CELLSIZE/2

CELLSIZE=np.float(batgrid)/60.

ncols=np.int((lonmax-lonmin)/CELLSIZE+0.5)
nrows=np.int((latmax-latmin)/CELLSIZE+0.5)

lonmax=lonmin+ncols*CELLSIZE
latmax=latmin+nrows*CELLSIZE

f=open(outDir+'/out.pr','r')
x=f.readline().strip('\n').split()
minHeight=np.float(x[1])
nv=np.float(x[4])

#----------------------------------------
# make new input deck

inpdeck=outDir+'/'+inputf

copyfile(boundaryDir+'/'+inputf, inpdeck)

f=open(inpdeck, 'a')


f.writelines('*--------------------------------------*\n')
f.writelines('* revised window *\n') 
f.writelines('* ncols={} nrows={} *\n'.format(ncols,nrows)) 
f.writelines('* estimated nv={:.0f} for minHeight={:e} *  \n'.format(nv,minHeight))
f.writelines('*--------------------------------------*\n') 
f.writelines('batgrid={}\n'.format(batgrid))
f.writelines('lonmin={:.4f}\n'.format(lonmin))
f.writelines('lonmax={:.4f}\n'.format(lonmax))
f.writelines('latmin={:.4f}\n'.format(latmin))
f.writelines('latmax={:.4f}\n'.format(latmax))


f.close()


tree=parse(boundaryDir+'/Calc_input_deck.xml')
root=tree.getroot()

l1=SubElement(root,'batgrid')
l1.text=batgrid
l2=SubElement(root,'lonmin')
l2.text=lonmin.astype(np.str)
l3=SubElement(root,'lonmax')
l3.text=lonmax.astype(np.str)
l4=SubElement(root,'latmin')
l4.text=latmin.astype(np.str)
l5=SubElement(root,'latmax')
l5.text=latmax.astype(np.str)


# beautify xml line
r_string=ElementTree.tostring( root )
rep=minidom.parseString(r_string)

f=open(outDir+'/Calc_input_deck.xml','w')
f.write('\n'.join([line for line in rep.toprettyxml(indent=' '*2).split('\n') if line.strip()]))

f.close()



