import numpy as np
import os
import sys
from lxml import objectify
from datetime import datetime,timedelta
from read import readtxt,read_header
from matplotlib import pyplot as plt
from family import family
import matplotlib.dates as mdates

def expf(t,A,K,C):
        return A*np.exp(K*t)+C


cpwd=os.getcwd()
ok=0
#setexp trange # 0.  180.

#-------------------------------------
# variables define externally
# scripts, folder 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
from project4 import * #exec $scripts./4-project.pr
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#inputf=os.getenv('input','Calc_input_deck.xml')
inputf='Calc_input_deck.xml'

filein=open(inputf)

inp = objectify.fromstring( filein.read() )

#-------------------------------------
calcType=os.getenv('calcType','CYCLONE')

if (calcType == 'TSUNAMI') :
      TITLE=inp.Title 
      forecasts={}
else:
      TITLE='{} forecast on {}'.format(inp.hurName,inp.lastBulDate)

      form='%d %b %Y %H:%M:%S'
#
      forecast=info.timeShift.pyval
      dt=timedelta(hours=forecast)
      date0=inp.bulDate.pyval
      dforecast=datetime.strptime(date0, form)+dt
#
      forecast1=forecast+24
      dt=timedelta(hours=forecast1)
      dforecast1=datetime.strptime(date0, form)+dt
#
      forecast2=forecast+48
      dt=timedelta(hours=forecast2)
      dforecast2=datetime.strptime(date0, form)+dt
#
#-------------------------------------

dir1='work/watch'
#setexp dir1 ?dir1
#dir1=$1
dir1=sys.argv[1]

dir2=cpwd
#setexp dir2 ?dir2
#setexp dir2 $2
dir2=sys.argv[2]

outdir='work'
#setexp outdir ?outdir
#setexp outdir $3

#-------------------------------------

if not os.path.exists(outdir):
    os.makedirs(outdir)


nw=200000

ID=np.arange(nw)
out=np.zeros(nw)

ZmaxS=np.empty(nw)
ZmaxS.fill(np.nan)

TzmaxS=np.empty(nw)
TzmaxS.fill(np.nan)

TarrS=np.empty(nw)
TarrS.fill(np.nan)

VmaxS=np.empty(nw)
VmaxS.fill(np.nan)

Lat=np.empty(nw)
Lat.fill(np.nan)

Lon=np.empty(nw)
Lon.fill(np.nan)

ZmaxR=np.zeros(nw)
TzmaxR=np.empty(nw)
TzmaxR.fill(np.nan)
TarrR=np.empty(nw)
TarrR.fill(np.nan)



#----------------------------------------
#idPlace	latPlace	longPlace	zPlace	cellNear	latNear	longNear	zNear	distNear	$namePlace
try:

	wat=readtxt(dir1+'/../watch.txt')

	id=wat.idPlace
	Lat[id] = wat.latNear
	Lon[id] = wat.longNear

except:
    	print 'error reading watch.txt'
        sys.exit()


#----------------------------------------
#idPlace	        tarr	        zmax	       tzmax	        vmax	  $namePlace

try:
	stat=readtxt(dir1+'/statistics.txt')

	id=stat.idPlace

	out[id]=out[id]+1
	ZmaxS[id]= (stat.zmax*100).astype(int)/100.
	VmaxS[id]= (stat.vmax*100).astype(int)/100.
	TzmaxS[id]=stat.tzmax/3600.
	TarrS[id]=stat.tarr/3600.

except:
    	print 'error reading statistics.txt'
        sys.exit()

id=None
#----------------------------------------
#* ID	country	place	MaxHei	ArrTime	tMaxHei	ArrivalTS	lon	lat	popest	cityclass	Th1m	h1m

try:

	loc=readtxt(dir2+'/locations.txt') 
	id=loc['* ID']

        loc.tMaxHei=np.array([s.replace(":",".") for s in loc.tMaxHei]).astype(np.float)
        loc.ArrTime=np.array([s.replace(":",".") for s in loc.ArrTime]).astype(np.float)

	tMaxHei1=loc.tMaxHei.astype(np.int)+np.mod(loc.tMaxHei,1)/0.6
	ArrTime1=loc.ArrTime.astype(np.int)+np.mod(loc.ArrTime,1)/0.6

	out[id]=out[id] + 1

	Lat[id] = loc.lat
	Lon[id] = loc.lon
	ZmaxR[id] = loc.MaxHei
	TzmaxR[id]=tMaxHei1
	TarrR[id]=ArrTime1
except:
    	print 'error reading locations.txt'
        sys.exit()


#----------------------------------------

maxzmax=np.max(ZmaxR)
if minzmax == [] : minzmax=np.min(ZmaxR)
minzmax=np.min([maxzmax/2,minzmax])

print minzmax, maxzmax 

#----------------------------------------

nz=20
#maxval=gprog(nz,maxzmax,minzmax)
rx=np.arange(nz)
ex=np.log(minzmax/maxzmax)/nz
maxval=expf(rx,maxzmax,ex,0.)


nval=np.zeros(nz)
for i in range(nz):
	nval[i]=np.sum((ZmaxR >= maxval[i]) & (out == 2))

print maxval, nval 


if (nplot < nval[-1]) :
   [fs]=family(nval,maxval)
   minzmax=fs(nplot)

print minzmax, nplot

m=(ZmaxR > minzmax) & (out == 2)
#----------------------------------------

if(np.sum(m) == 0) : 
      print 'isuue with minzmax'
      sys.exit()


ii=ID[m]


ID=ID[ii]
out=out[ii]
Lat=Lat[ii]
Long=Lon[ii]
ZmaxS=ZmaxS[ii]
VmaxS=VmaxS[ii]
TarrS=TarrS[ii]
TzmaxS=TzmaxS[ii]

ZmaxR=ZmaxR[ii]
TarrR=TarrR[ii]
TzmaxR=TzmaxR[ii]


#----------------------------------------
nw=np.size(ii)

for j in range(nw):

     id = ID[j]


     filename= wat['$namePlace'][wat.idPlace==id]

     file1=dir1+'/'+filename[0]+'.txt'
     if os.path.isfile(file1) == False : continue

     file2=dir2+'/WT_'+filename[0].replace(' ','_')+'.txt'
 #   fill   file2 _
     if os.path.isfile(file2) == False : continue


     #time	zsurf	v
     f1=readtxt(file1)
#    zsurf=Shoreline

     header=read_header(file2)
     time2,zsurfmax,zsurfmin,zmax=np.loadtxt(file2,skiprows=1,delimiter=',',unpack=True)
     zsurfminl='Min. at 5 Km Radius'
     zsurfmaxl='Max. at 5 Km Radius'
 
#    labx=[time2/3600, f1.time/3600, time2/3600]
#    laby=[zsurfmax, f1.zsurf, zsurfmin] 

     minzsurfmin=np.min(zsurfmin)
     minzsurfmin=np.int(minzsurfmin*100)/100

     name=wat['$namePlace'][wat.idPlace==id]

     fig = plt.figure()#figsize=(6,4))
     ax = fig.add_axes([0.1,0.2,0.8,0.75])
#-------------------------------------------------------------------------------
     plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%H \n %Y/%m/%d'))
     plt.gca().xaxis.set_major_locator(mdates.HourLocator(byhour=[0,12]))
     ax.xaxis_date()
     xdate=[datetime.strptime(date0,form)+timedelta(seconds=t) for t in time2]
#-------------------------------------------------------------------------------
#    plt.plot(time2/3600.,zsurfmax,'r')
     plt.plot(xdate,zsurfmax,'r')
#    plt.plot(f1.time/3600.,f1.zsurf,'b')
#    plt.plot(time2/3600.,zsurfmin,'g')
     plt.axvline(x=dforecast,color='m')
     plt.axvline(x=dforecast1,color='y')
     plt.axvline(x=dforecast2,color='brown')
#    plt.xlabel('time (h)')
     plt.xlabel('date')
     plt.ylabel('Water surf. level (m)')
#------------------------------------------------------
     ax.text(0.01, 0.9,zsurfmaxl, ha='left', va='center', transform=ax.transAxes, color='r')
     #ax.text(0.01, 0.85,'shoreline', ha='left', va='center', transform=ax.transAxes, color='b')
     #ax.text(0.01, 0.8,zsurfminl, ha='left', va='center', transform=ax.transAxes, color='g')
     ax.text(0.01, 0.85,dforecast, ha='left', va='center', transform=ax.transAxes, color='m')
     ax.text(0.01, 0.8,dforecast1, ha='left', va='center', transform=ax.transAxes, color='y')
     ax.text(0.01, 0.75,dforecast2, ha='left', va='center', transform=ax.transAxes, color='brown')
#------------------------------------------------------
     plt.figtext(.01,.09,'{}  --- location={}  --- id={}'.format(TITLE,name,id),size='x-small')  
     plt.figtext(.01,.05,' At 5 km radius : Min Height(m)={} Max Height(m)={}'.format(minzsurfmin,ZmaxR[j]),size='x-small') 
     plt.figtext(.01,.01,' At the shoreline: Max Height(m)={} Max Velocity(m/s)={}'.format(ZmaxS[j],VmaxS[j]),size='x-small') 
     plt.gcf().autofmt_xdate(bottom=0.2, rotation=0, ha='right')
     ax.tick_params(axis='x', labelsize=8)
     plt.savefig(outdir+'/zsurf.'+filename[0]+'.txt.png')




