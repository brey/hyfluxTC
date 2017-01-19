import numpy as np
import sys
import os
import matplotlib.pyplot as plt
from read import read_header, readf
from bunch import bunchify
from string import Template
#from dicttoxml import dicttoxml
from xmldic import dict_to_xml
from xml.dom.minidom import parseString
from lxml import objectify
from pmap import putmap


try:
     fromBul=np.int(os.getenv('fromBul'))
     dx=np.float(os.getenv('batgrid'))
     inputf=os.getenv('input','Calc_input_deck.txt') 
     lastDir=os.getenv('lastDir')
     workDir=os.getenv('workDir','.')
     dtout=np.float(os.getenv('dtout',3600))
     timestart=np.float(os.getenv('timestart'))
     outDir=os.getenv('outDir')
     lastWin=os.getenv('lastWin',None)
     cycloneDir=os.getenv('cycloneDir','/home/brey/cycloneSurge/TCHome.4') 
     pythonDir=os.getenv('pythonDir','/home/brey/cycloneSurge/Python') 
     RESTART=os.getenv('RESTART')
except KeyError:
     print 'check env variables'
     sys.exit(1)


print RESTART
#-----------------------------------
#setexp yes yes
#restart=compexp(RESTART,yes)

if (RESTART == 'yes'):
     try:
        lastDir=os.getenv('lastDir')
     except:
        print 'check env variables'
        sys.exit(1)
     inpdeck=lastDir+'/'+inputf.strip('txt')+'xml'
     with open(inpdeck) as f:
        ixml = f.read()
     inp=objectify.fromstring( ixml )
     latmin0=np.float(inp.latmin.text)
     latmax0=np.float(inp.latmax.text)
     lonmin0=np.float(inp.lonmin.text)
     lonmax0=np.float(inp.lonmax.text)
#-----------------------------------
infoxml=workDir+'/info.xml'
with open(infoxml) as f:
        xml = f.read()
info = objectify.fromstring( xml )

infile=workDir+'/outData.txt' 
outdat=readf(infile)
#var=read_header(workDir+'/outData.txt')
#data=np.loadtxt(workDir+'/outData.txt',skiprows=1)
#dd=dict(zip(var,data.T))

#info.extend(info.b1.iterchildren())


if info.timeShift == None : info.timeShift=0
if info.InTime == None : info.InTime=0
if info.FinTime == None : info.FinTime=np.max(outdat.time)

Lat=outdat.yhc[(outdat.time==info.timeShift).flatten()][0]
Lon=outdat.xhc[(outdat.time==info.timeShift).flatten()][0]

CELLSIZE=dx/60.
dlat=np.min([120*CELLSIZE,10.])
dlon=np.min([120*CELLSIZE,10.])

lat=np.array(outdat.yhc[(outdat.time >= info.InTime).flatten() & (outdat.time <= info.FinTime).flatten()]) 
lon=np.array(outdat.xhc[(outdat.time >= info.InTime).flatten() & (outdat.time <= info.FinTime).flatten()])

prec=np.min([10.,np.int(1./CELLSIZE)])*CELLSIZE

lonmin=np.min(lon)
lonmax=np.max(lon)
lonmin=lonmin-dlon
lonmax=lonmax+dlon

latmin=np.min(lat)
latmax=np.max(lat)
latmin=latmin-dlat
latmax=latmax+dlat

lonmin=np.int(lonmin/prec)*prec
if (lonmin < 0): lonmin=lonmin-prec
lonmax=np.int(lonmax/prec)*prec
if (lonmax > 0): lonmax=lonmax+prec

latmin=np.int(latmin/prec)*prec
if (latmin < 0): latmin=latmin-prec
latmax=np.int(latmax/prec)*prec
if (latmax > 0): latmax=latmax+prec

ncols=np.int((lonmax-lonmin)/CELLSIZE+0.5)
nrows=np.int((latmax-latmin)/CELLSIZE+0.5)

lonmax=lonmin+ncols*CELLSIZE
latmax=latmin+nrows*CELLSIZE

#--------------------------
if (RESTART == 'yes') :
        lonmin1=lonmin
        lonmax1=lonmax
        latmin1=latmin
        latmax1=latmax
        lonmin=np.min([lonmin,lonmin0])
        lonmax=np.max([lonmax,lonmax0])
        latmin=np.min([latmin,latmin0])
        latmax=np.max([latmax,latmax0])

#--------------------------
if (Lat > 0) :
        latmin=np.max([latmin,0.])
        latmax=np.min([latmax,60.])
else:
        latmin=np.max([latmin,-60.])
        latmax=np.min([latmax,0.])
#--------------------------
ncols=np.int((lonmax-lonmin)/CELLSIZE+0.5)
nrows=np.int((latmax-latmin)/CELLSIZE+0.5)
#--------------------------


Tsave=dtout/60.  # sec to min

XSTART=lonmin
YSTART=latmax
TYPE=4
NCOLS=ncols
NROWS=nrows
nt=np.size(lat)
MV=-3.4028234663852886e+38

ix=np.linspace(lonmin+CELLSIZE/2,lonmax-CELLSIZE/2,ncols)
jy=np.linspace(latmax-CELLSIZE/2,latmin+CELLSIZE/2,nrows)
x,y=np.meshgrid(ix,jy)
Rmax=np.empty((nrows,ncols))
Rmax.fill(MV)
TRmax=np.empty((nrows,ncols))
TRmax.fill(MV)


for i in range(nt):
        x0=lon[i]
        y0=lat[i]
        r=np.sqrt((x-x0)**2+(y-y0)**2)*1e5
        m=r < outdat.rmax[i]
        if (np.sum(np.sum(m)) == 0): break 
        Rmax[m] = outdat.rmax[i]
        TRmax[m] = outdat.time[i]

os.chdir(outDir)

inputdic={'hurName':info.xpath('hurName')[-1],'source':info.source.text.strip(),'bulDate':info.bulDate,'lastBulDate':info.lastBulDate, 'hurId':info.hurId, 'basin':info.basin, 'fromBul':fromBul, 'toBul':info.toBul, 'Lat':Lat, 'Lon':Lon, 'catMaxWind':info.catMaxWind, 'timestart':timestart/3600, 'FinTime':info.FinTime, 'Tsave':Tsave, 'dx':dx, 'lonmin':lonmin, 'lonmax':lonmax, 'latmin':latmin, 'latmax':latmax, 'ncols':ncols, 'nrows':nrows}


##USING XML##
#xml=dicttoxml(inputdic)
xml=dict_to_xml('xml',inputdic)
dom=parseString(xml)

with open('Calc_input_deck.xml', 'w') as f:
    f.write(dom.toprettyxml(indent="  "))

##ALTERNATIVE BELOW##

templ=open(pythonDir+'/Calc_input_deck.pr')
src=Template( templ.read() )
result=src.substitute(inputdic)
with open('Calc_input_deck2.txt', 'w') as f:
    f.write(result)


##ALTERNATIVE ABOVE##

f=open('Calc_input_deck.txt','wb')

f.write('*********************************************\n')
f.write('*       Tropical Cyclone input deck\n')
f.write('*********************************************\n')
f.write('* General data\n')
f.write('Title={} -{}\n'.format(info.xpath('hurName')[-1],info.source))
f.write('*\n')                                        
f.write('DateTsunami={}   * bulDate is the date of the bulletin on which is based the time, i.e. date of fromBul\n'.format(info.bulDate))
f.write('lastBulDate={} * lastBulDate is the date of the current bulletin i.e. date of toBul\n'.format(info.lastBulDate))
f.write('hurName={}\n'.format(info.xpath('hurName')[-1]))
f.write('hurId={}\n'.format(info.hurId))
f.write('basin={}\n'.format(info.basin))
f.write('fromBul={}\n'.format(fromBul))
f.write('toBul={}\n'.format(info.toBul))
f.write('*\n')
f.write('*  Last position at time  *\n')
f.write('Lat=               {}      * degree\n'.format(Lat))                                           
f.write('Lon=               {}      * degree\n'.format(Lon))                                           
f.write('Mag=               {}       * Cyclone s\n'.format(info.catMaxWind))
f.write('*  Calculation parameters  *\n')
f.write('InTime=       {:.0f}.         * h\n'.format(timestart/3600))
f.write('FinTime=      {:.0f}.        * h\n'.format(np.float(info.FinTime)))  
f.write('Tsave=        {:.0f}.       * min\n'.format(Tsave)) 
f.write('fanning=0.015\n')   
f.write('*\n')
f.write('faultform=-1    *  modifica     variab\n')
f.write('*\n')
f.write('*  Grid parameters  *\n')
f.write('batgrid=      {:.0f}.        * min\n'.format(dx)) 
f.write('lonmin={:.0f}.\n'.format(lonmin))
f.write('lonmax={:.0f}.\n'.format(lonmax))
f.write('latmin={:.0f}.\n'.format(latmin))
f.write('latmax={:.0f}.\n'.format(latmax))
f.write('ncols={:.0f}.\n'.format(np.float(ncols)))
f.write('nrows={:.0f}.\n'.format(np.float(nrows)))
f.write('bathymetry=SRTM30+    * GEGCO ETOPO2\n')

f.close()

#--------------------------

Rm=np.ma.masked_values(Rmax,MV)
Rm.fill_value=np.nan
RR=Rm.reshape(nrows,ncols)

TRm=np.ma.masked_values(TRmax,MV)
TRm.fill_value=np.nan
TRR=TRm.reshape(nrows,ncols)

plt.figure()
plt.scatter(x,y,c=RR,cmap=plt.cm.gist_rainbow_r)
plt.colorbar()
plt.savefig('Rmax.png')
plt.figure()
plt.scatter(x,y,c=TRR,cmap=plt.cm.gist_rainbow_r)
plt.colorbar()
plt.savefig('TRmax.png')

#putmap Rmax Rmax.map
#putmap TRmax TRmax.map
GEO=(lonmin, CELLSIZE, 0.0, latmax, 0.0, -CELLSIZE)
putmap('Rmax.map',Rmax,GEO,TYPE)
putmap('TRmax.map',TRmax,GEO,TYPE)

#--------------------------
if (RESTART == 'yes'):
    mask=np.empty((nrows,ncols))
    mask.fill(MV)
    m=(x >= lonmin1) & (x <= lonmax1) & (y >= latmin1) & (y <= latmax1) 
    mask[m]=1
# putmap mask mask.map
    putmap('mask.map',mask,GEO,TYPE)

