# input: dirkml, filekml, north,south east, west
import numpy as np
from read import readtxt
import os
import datetime
from time import gmtime, strftime
from pytz import timezone

ndesc=0

#-------------------------------------------------------------
def startkml(f,project,folder):#,north,south,east,west):
#-------------------------------------------------------------
     global scalePlacemark, roundval, green, gray, yellow, orange, orangeDark, red, magentaDark, blue, pushpin, color



     f.writelines('<?xml version="1.0" encoding="UTF-8"?>\n')
     f.writelines('<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">\n')

     f.writelines('<Document>\n')
     f.writelines('<name>{} - {}</name>\n'.format(project,folder))
     f.writelines('<open>1</open>\n')
     #if (maxcc) close all
#--------------------
     roundval=0.01

#--------------------
     doPlacemark=1
     if ('pushpin' not in locals()) : pushpin='ylw-pushpin'


     gray=0
     green=1
     yellow=2
     orange=3
     orangeDark=4
     red=5
     magentaDark=6
     blue=7

     color=gray

     scalePlacemark=0.8  # the scale could be defined for each placemark

     col=gray
     colNum='ff888888'
     stylePushpin(f,col,colNum)
     styleStar(f,col,colNum)

     col=red
     colNum='ff0000ff'
     stylePushpin(f,col,colNum)
     styleStar(f,col,colNum)

     col=orange
     colNum='ff00aaff'
     stylePushpin(f,col,colNum)
     styleStar(f,col,colNum)

     col=orangeDark
     colNum='ff0055b8'
     stylePushpin(f,col,colNum)
     styleStar(f,col,colNum)

     col=yellow
     colNum='ff00ffff'
     stylePushpin(f,col,colNum)
     styleStar(f,col,colNum)

     col=green
     colNum='ff00ff00'
     stylePushpin(f,col,colNum)
     styleStar(f,col,colNum)

     col=magentaDark
     colNum='ff980098'
     stylePushpin(f,col,colNum)
     styleStar(f,col,colNum)

     col=blue
     colNum='ffffff00' 
     stylePushpin(f,col,colNum)
     styleStar(f,col,colNum)

     #$$$$ endstartkml

     #f.writelines(' <Folder>')
     #f.writelines('    <name>$folder.</name>')
     #f.writelines('  <open>1</open>')

     #end startkml

#------------------------------------------
def endkml(f):
#------------------------------------------
     f.writelines('</Document>\n')
     f.writelines('</kml>\n')

#------------------------------------------
def path(f,namePath,xpath,ypath):
# input: namePath, xpath, ypath
#------------------------------------------
     f.writelines('<Placemark>\n')
     f.writelines('<name> {} </name>\n'.format(namePath))
     f.writelines('<styleUrl>{}</styleUrl>\n'.format(msn_ylw-pushpin))
     f.writelines('<LineString>\n')
     f.writelines('<tessellate>1</tessellate>\n')
     f.writelines('<coordinates>\n')
     npath=np.size(xpath)
     ipath=0
     for labpath in range(npath):
          ipath=ipath+1
          f.writelines('') #(xpath_ipath).,#(ypath_ipath).,0
     f.writeliness('</coordinates>\n')
     f.writelines('</LineString>\n')
     f.writelines('</Placemark>\n')



#-------------------------------------------------------------
def doline (var,cvar,unmes):
#-------------------------------------------------------------
     global ndesc

     if var == MV : return
     ndesc=ndesc+1
     if (var > 0): var=np.int(var/roundval+0.5)*roundval
     if (var < 0): var=-np.int(-var/roundval+0.5)*roundval
     desc[ndesc]='{} = {} {}'.format(cvar,var,unmes) 
#-------------------------------------------------------------
def watchMes(f,dic) :
#-------------------------------------------------------------
     global ndesc,desc,name,x,y,width,styleType,MV, toplace, color

     f.writelines('<Folder>\n')
     f.writelines('<name>Locations</name>\n')

     toplace=False

     #ID          zmaxSea        zmaxShore      distZmaxSea   distInundation    simInundation    mesInundation         simRunup         mesRunup     simDistRunup     mesDistRunup         $namePlace 

#-------------------------
     ID=dic.ID
     lat=dic.lat 
     lon=dic.lon 
     simMaxHeight=dic.simMaxHeight
     maxHeight=dic.maxHeight
     typeMes=dic.typeMes
     minzmax=dic.minzmax
     zmaxSea=dic.zmaxSea
     zmaxShore=dic.zmaxShore
     hmaxShore=dic.hmaxShore
     tarrShore=dic.tarrShore
     tzmaxShore=dic.tzmaxShore
     mesInundation=dic.mesInundation
     simInundation=dic.simInundation
     mesRunup=dic.mesRunup
     simRunup=dic.simRunup
     mesDistRunup=dic.mesDistRunup
     simDistRunup=dic.simDistRunup
     showvar=dic.showvar
     namePlace=dic.namePlace
     MV=dic.nan

#-------------------------

     npp=np.size(ID)

     ii=np.arange(npp)

     outval=simMaxHeight

     m=(np.isnan(outval)) & (np.isnan(maxHeight) != True)
     if (np.sum(m)>0): 
          i=ii[m]
          outval[i] = maxHeight[i]


     m=np.isnan(outval)
     if (np.sum(m) > 0) :
          i=ii[m]
          outval[i] = -1

     J=np.argsort(-outval)
     #=sorted(range(len(outval)), key=lambda k: -x[k])

     for II in range(npp):
          i=J[II]

          if (np.isnan(simMaxHeight[i]) & np.isnan(maxHeight[i])) :  continue

          id=ID[i]
          y=lat[i]
          x=lon[i]


     
          type=typeMes[i]

          # type = -1 for simulation
     
          if (type == MV) :
     
               if (maxHeight[i] != MV) :
                    type=2
               else:
                    type=-1


          if (simMaxHeight[i] != MV) :
               zmax0=simMaxHeight[i]
               zmax1=np.int(zmax0/roundval+0.5)*roundval
               name=zmax1
               zmax=zmax1

          else:

               zmax=MV
               zmax1=MV
               name=None

          if (maxHeight[i] != MV) :
               zmax0=maxHeight[i]
               zmax1=np.int(zmax0/roundval+0.5)*roundval
               name=name #zmax1

          if ( (zmax == MV)  & (zmax1 != MV)) : zmax=zmax1


          if zmax == MV : continue 
          if (zmax < minzmax and type< 0) : continue 


          if ('showvar' not in locals()) :
               name=[]  

          # set colors

          if (zmax > 4.8) :
               color=magentaDark
          elif (zmax > 2.4) :
               color=red
          elif (zmax > 1.2) :
               color=orange
          elif (zmax > 0.6) :
               color=yellow
          else : # if (zmax.gt.0.3) :
               color=green


          ndesc=0
          desc={}
     
          ndesc=ndesc+1
          desc[ndesc]= 'simMaxHeight / mesMaxHeight' 
     
          ndesc=ndesc+1

          desc[ndesc]= namePlace[i] 

          ndesc=ndesc+1
          desc[ndesc]= [y,x]   

          cvar='simMaxHeight' 
          var=simMaxHeight[i]  
          unmes='[m]'
          doline(var,cvar,unmes)
     
          if 'mesMaxHeight' in locals():
            cvar='mesMaxHeight' 
            var=maxHeight[i]   
            unmes='[m]'
            doline(var,cvar,unmes)

          if (type > 0) :
               cvar='typeMes' 
               var=type   
               unmes='-'
               doline(var,cvar,unmes)

          cvar='zmaxSea' 
          var=zmaxSea[i]    
          unmes='[m]'
          doline(var,cvar,unmes)
          cvar='zmaxShore' 
          var=zmaxShore[i]   
          unmes='[m]'
          doline(var,cvar,unmes)

          cvar='hmaxShore' 
          var=hmaxShore[i]    
          unmes='[m]'
          doline(var,cvar,unmes)

          cvar='tarrShore' 
          var=(tarrShore[i]/3600. if tarrShore[i] != MV else tarrShore[i])
          unmes='[h]'  
          doline(var,cvar,unmes)

#----------------------------------
          bulDate=os.getenv('bulDate')
          TZ=strftime("%Z", gmtime()) 
          dbul=datetime.datetime.strptime(bulDate,'%d %b %Y  %H:%M:%S')
          dbulc=timezone(TZ).localize(dbul)
#----------------------------------


          if ((var != MV) & (bulDate != None)) :
               date0=dbulc+datetime.timedelta(hours=var)
               ndesc=ndesc+1
               desc[ndesc]=datetime.datetime.strftime(date0, '%a %b %d  %H:%M:%S %Z %Y')
              #desc[ndesc]=``date --date="$date0 " `

          cvar='tzmaxShore' 
          var=(tzmaxShore[i]/3600. if tzmaxShore[i] != MV else tzmaxShore[i])
          unmes='[h]'

          doline(var,cvar,unmes)
          if (var != MV) & (bulDate != None) :
               date0=dbulc+datetime.timedelta(hours=var)
               ndesc=ndesc+1
               desc[ndesc]=datetime.datetime.strftime(date0, '%a %b %d  %H:%M:%S %Z %Y')
              #desc[ndesc]=``date --date="$date0 " `


          cvar='mesInundation' 
          var=mesInundation[i]    
          unmes='[m]'
          doline(var,cvar,unmes)
          cvar='simInundation' 
          var=simInundation[i]   
          unmes='[m]'
          doline(var,cvar,unmes)


          cvar='mesRunup' 
          var=mesRunup[i]   
          unmes='[m]'
          doline(var,cvar,unmes)
          cvar='simRunup' 
          var=simRunup[i]   
          unmes='[m]'
          doline(var,cvar,unmes)

          cvar='mesDistRunup' 
          var=mesDistRunup[i]   
          unmes='[m]'
          doline(var,cvar,unmes)
          cvar='simDistRunup' 
          var=simDistRunup[i]   
          unmes='[m]'
          doline(var,cvar,unmes)


          width=200
     
          place=namePlace[i]
          file='zsurf.'+place+'.txt.png'
          if not os.path.isfile(file) : file='zsurf.'+place+'.txt.gif'
          if os.path.isfile(file) :
               width=width+600
               ndesc=ndesc+1
               desc[ndesc]=' </td><td> <img src="{}">'.format(file) 


          if (type < 0) :
               styleType=pushpin
          else:
               styleType='ltblu-stars'

          placemark(f)

         #if (maxcc) close all


     f.writelines(' </Folder>\n')
     return


def doline2(var,cvar,unmes):
     global ndesc
     if np.isnan(var) : return
     ndesc=ndesc+1
     if (var > 0): var=np.int(var*100+0.5)/100
     if (var < 0): var=-np.int(-var*100+0.5)/100
     desc[ndesc]='{}  = {} {}'.format(cvar,var,unmes) 


#-------------------------------------------------------------
def watchSurveyed(f):
#-------------------------------------------------------------
     f.writelines(' <Folder>\n')
     f.writelines(' <name>Surveyed points</name>\n')
     if note in locals():  
          if (np.sum(np.isnan(note)!=True) > 0) :
               f.writelines(' <description>\n')
               f.writelines(' Note\n')
               f.writelines(' (1) \n')
               f.writelines(' (2) \n')
               f.writelines(' </description>\n')

     toplace=False

     npp=np.size(ID)

    #iord=order(-maxHeight)
     iord=sorted(range(len(maxHeight)), key=lambda k: -maxHeight[k])

     for ii in range(npp):
          i=iord[ii]
          id=ID[i]
          y=lat[i]
          x=lon[i]

          if (np.isnan(maxHeight[i])) : continue

          zmax0=maxHeight[i]
          zmax=np.int(zmax0*100+0.5)/100

          name= namePlace[i]+'  maxHeight={}'.format(zmax)

          if (note in locals()) : 
               if (np.isnan(note[i]) != True ) :
                    name= name+' ('+np.str(note[i])+')'

          ndesc=0

          if (zmax > 4.8) :
               color=magentaDark
          elif (zmax > 2.4) :
               color=red
          elif (zmax > 1.2) :
               color=orange
          elif (zmax > 0.6) :
               color=yellow
          else: # if (zmax > 0.3) :
               color=green


          cvar=initHeight 
          var=initHeight[i]  
          unmes='m' 
          doline2()

          cvar=maxHeight 
          var=maxHeight[i] 
          unmes='m'
          doline2()

          cvar=tinitHeight 
          var=tinitHeight[i] 
          unmes='min' 
          doline2()

          cvar=tmaxHeight 
          var=tmaxHeight[i] 
          unmes='min'
          doline2()

          placemark(f)

     #     if (maxcc) close all
     


     f.writelines(' </Folder>\n')



#-------------------------------------------------------------
def watchSimulated(f):
#-------------------------------------------------------------
     f.writelines(' <Folder>\n')
     f.writelines('         <name>Simulated points</name>\n')

     toplace=False

     nnp=np.size(idSim)

#    iord=order(-zmaxSim)
     iord=sorted(range(len(zmaxSim)), key=lambda k: -zmaxSim[k])
     ii=0

     for ii in range(npp):
          i=iord[ii]
          id=idSim[i]
          y=latSim[i]
          x=longSim[i]


          zmax0=zmaxSim[i]
          zmax=np.int(zmax0*100+0.5)/100
     
          name=nameSim[id] +'  zmaxSim={}'.format(zmax)
          ndesc=0

          if (np.isnan(zmax0)) :
               name=nameSim[id]
               color=gray
          elif (zmax > 4.8) :
               color=magentaDark
          elif (zmax > 2.4) :
               color=red
          elif (zmax > 1.2) :
               color=orange
          elif (zmax > 0.6) :
               color=yellow
          else : # if (zmax > 0.3) :
               color=green


          styleType=pushpin
          placemark(f)

       #  if (maxcc) close all

     
     f.writelines(' </Folder>\n')

#-------------------------------------------------------------
def watchShore():
#-------------------------------------------------------------
     global ndesc

     maxdis=maxLookRunup
     maxdem=maxDemPlace

     toplace=True

     f.writelines('\n')
     f.writelines(' <Folder>\n')
     f.writelines('         <name>Watch shore</name>\n')

     try:

     # idPlace latPlace longPlace zPlace cellNear latNear longNear xNear yNear zNear distNear $namePlace
          wf=readtxt('../watch.txt')

     # idPlace         tarr        zmax0         zmax        tzmax         vmax          dem   $namePlace
    #setexp LCNEXT M
          sf=readtxt('../watch/statistics.txt') 
    #LCNEXT = None

     except:
         print 'no file'
         exit()

     n=np.max([sf.idPlace,wf.idPlace])
     kk=np.arange(n)

     latShore=np.zeros(n)
     longShore=np.zeros(n)
     latPlace1=np.zeros(n)
     longPlace1=np.zeros(n)
     zmaxShore=np.zeros(n)
     dem=np.zeros(n)
     dis=np.zeros(n,maxdis)
     dem=np.zeros(n,maxdem)
     tzmaxShore=np.zeros(n)
     tarrShore=np.zeros(n)
     id=np.zeros(n)

     latShore[wf.idPlace]=wf.latNear
     longShore[wf.idPlace]=wf.longNear
     latPlace1[wf.idPlace]=wf.latPlace
     longPlace1[wf.idPlace]=wf.longPlace
     dis[wf.idPlace]=wf.distNear
     dem[wf.idPlace]=wf.zPlace

     tzmaxShore[sf.idPlace]=sf.tzmax
     tarrShore[sf.idPlace]=sf.tarr
     zmaxShore[sf.idPlace]=sf.zmax
     
     id[sf.idPlace]=1
     m=(id == 1) & (dis < maxdis) & (dem<maxdem)
     k=kk[m]
    #k=k_order(-zmaxShore_k)
     k=k[sorted(range(len(zmaxShore)), key=lambda k: -zmaxShore[k])]

     idPlace=k
     latShore=latShore[k]
     longShore=longShore[k]
     latPlace1=latPlace1[k]
     longPlace1=longPlace1[k]
     zmaxShore=zmaxShore[k]
     tzmaxShore=tzmaxShore[k]
     tarrShore=tarrShore[k]
     dis=dis[k]
     dem=dem[k]


    #if (maxcc) close all


#.........................................
     npp=size(idPlace)
     
     for i in range(npp):
          id=idPlace[i]
          y=latShore[i]
          x=longShore[i]
          y0=latPlace1[i]
          x0=longPlace1[i]
          distPlace=np.int(dis[i]+0.5)
          demPlace=np.int(dem[i]+0.5)

          name0=namePlace[i]


          zmax=zmaxShore[i]
     
          zmax=np.int(zmax*100+0.5)/100

          if (zmax < minzmax) : continue 


          if (np.isnan(zmax)) :
               color=gray
          elif (zmax>4.8) :
               color=magentaDark
          elif (zmax>2.4) :
               color=red
          elif (zmax>1.2) :
               color=orange
          elif (zmax>0.6) :
               color=yellow
          else : # if (zmax>0.3) :
               color=green

          tarr=np.int(((tarrShore[i])/60)*10+0.5)/10
          tzmax=np.int(((tzmaxShore[i])/60)*10+0.5)/10
     
          name=namePlace[i]+' zmaxShore={}'.format(zmax)
          desc1=dir
          desc2='time arrival ={} min'.format(tarr)
          desc3='time zmax ={} min'.format(tzmax) 
          ndesc=3

          styleType=pushpin
          placemark(f)

       #  if (maxcc) close all


     f.writelines(' </Folder>\n')
     

#-------------------------------------------------------------
def dokml(f,visibility,filekml,north,south,east,west):
#-------------------------------------------------------------

     f.writelines('\n\n')
     f.writelines('<GroundOverlay>\n')
     f.writelines('<name>{}</name>\n'.format(filekml))
     f.writelines('<visibility>{}</visibility>\n'.format(visibility))
     f.writelines('<color>ffffffff</color>\n')

     f.writelines('<Icon>\n')
     f.writelines('<href>{}</href>\n'.format(filekml))
     # f.writelines(' <viewBoundScale>0.75</viewBoundScale>\n')
     f.writelines('</Icon>\n')

     f.writelines('<LatLonBox>\n')
     f.writelines('<north>{}</north>\n'.format(north))
     f.writelines('<south>{}</south>\n'.format(south))
     f.writelines('<east>{}</east>\n'.format(east))
     f.writelines('<west>{}</west>\n'.format(west))
     f.writelines('</LatLonBox>\n')
     f.writelines('</GroundOverlay>\n')


#-------------------------------------------------------------
def dokmlTimeSpan(f):
#-------------------------------------------------------------

     f.writelines('\n')
     f.writelines('<GroundOverlay>\n')
     f.writelines('<name>{}</name>\n'.format(name))
     f.writelines('<visibility>{}</visibility>\n'.format(visibility))
     f.writelines('<color>ffffffff</color>\n')

     f.writelines('<gx:TimeSpan>\n')
     f.writelines('<begin>{}</begin>\n'.format(time1))
     f.writelines('<end>{}</end>\n'.format(time2))
     f.writelines('</gx:TimeSpan>\n')


     f.writelines('<Icon>\n')
     f.writelines('<href>{}</href>\n'.format(href))
     # f.writelines(' <viewBoundScale>0.75</viewBoundScale>\n')
     f.writelines('</Icon>\n')

     f.writelines('<LatLonBox>\n')
     f.writelines('<north>{}</north>\n'.format(north))
     f.writelines('<south>{}</south>\n'.format(south))
     f.writelines('<east>{}</east>\n'.format(east))
     f.writelines('<west>{}</west>\n'.format(west))
     f.writelines('</LatLonBox>\n')

     f.writelines('</GroundOverlay>\n')

#-------------------------------------------------------------
def stylePushpin(f,col,colNum):
#-------------------------------------------------------------
     styleType=pushpin
     f.writelines('\n\n') 
     f.writelines('<Style id="sh_{}{}">\n'.format(styleType,col))
     f.writelines('	<IconStyle>\n')
     f.writelines('	<color>{}</color>\n'.format(colNum))
     f.writelines('		<scale>{}</scale>\n'.format(scalePlacemark))
     f.writelines('	</IconStyle>\n')
     f.writelines('	<LabelStyle>\n')
     f.writelines('		<scale>{}</scale>\n'.format(scalePlacemark))
     f.writelines('	</LabelStyle>\n')
     f.writelines('</Style>\n')
     
     f.writelines('<Style id="sn_{}{}">\n'.format(styleType,col))
     f.writelines('	<IconStyle>\n')
     f.writelines('		<color>{}</color>\n'.format(colNum))
     f.writelines('		<scale>{}</scale>\n'.format(scalePlacemark))
     f.writelines('	</IconStyle>\n')
     f.writelines('	<LabelStyle>\n')
     f.writelines('		<scale>{}</scale>\n'.format(scalePlacemark))
     f.writelines('	</LabelStyle>\n')
     f.writelines('</Style>\n')
     
     f.writelines('<StyleMap id="msn_{}{}">\n'.format(styleType,col))
     f.writelines('	<Pair>\n')
     f.writelines('		<key>normal</key>\n')
     f.writelines('		<styleUrl>#sn_{}{}</styleUrl>\n'.format(styleType,col))
     f.writelines('	</Pair>\n')
     f.writelines('	<Pair>\n')
     f.writelines('		<key>highlight</key>\n')
     f.writelines('		<styleUrl>#sh_{}{}</styleUrl>\n'.format(styleType,col))
     f.writelines('	</Pair>\n')
     f.writelines('</StyleMap>\n')
     

#-------------------------------------------------------------
def styleStar(f,col,colNum):
#-------------------------------------------------------------
     styleType='ltblu-stars'
     f.writelines('<StyleMap id="msn_{}{}">\n'.format(styleType,col))
     f.writelines('	<Pair>\n')
     f.writelines('		<key>normal</key>\n')
     f.writelines('		<styleUrl>#sn_{}{}</styleUrl>\n'.format(styleType,col))
     f.writelines('	</Pair>\n')
     f.writelines('	<Pair>\n')
     f.writelines('		<key>highlight</key>\n')
     f.writelines('		<styleUrl>#sh_{}{}</styleUrl>\n'.format(styleType,col))
     f.writelines('	</Pair>\n')
     f.writelines('</StyleMap>\n')

     f.writelines('<Style id="sh_{}{}">\n'.format(styleType,col))
     f.writelines('	<IconStyle>\n')
     f.writelines('	  <color>{}</color>\n'.format(colNum))
     f.writelines('		<scale>{}</scale>\n'.format(scalePlacemark))
     f.writelines('		<Icon>\n')
     f.writelines('			<href>http://maps.google.com/mapfiles/kml/paddle/ltblu-stars.png</href>\n')
     f.writelines('		</Icon>\n')
     f.writelines('		<hotSpot x="32" y="1" xunits="pixels" yunits="pixels"/>\n')
     f.writelines('	</IconStyle>\n')
     f.writelines('	<LabelStyle>\n')
     f.writelines('		<scale>{}</scale>\n'.format(scalePlacemark))
     f.writelines('	</LabelStyle>\n')
     f.writelines('	<ListStyle>\n')
     f.writelines('		<ItemIcon>\n')
     f.writelines('			<href>http://maps.google.com/mapfiles/kml/paddle/ltblu-stars-lv.png</href>\n')
     f.writelines('		</ItemIcon>\n')
     f.writelines('	</ListStyle>\n')
     f.writelines('</Style>\n')


     f.writelines('<Style id="sn_{}{}">\n'.format(styleType,col))
     f.writelines('	<IconStyle>\n')
     f.writelines('	  <color>{}</color>\n'.format(colNum))
     f.writelines('		<scale>{}</scale>\n'.format(scalePlacemark))
     f.writelines('		<Icon>\n')
     f.writelines('			<href>http://maps.google.com/mapfiles/kml/paddle/ltblu-stars.png</href>\n')
     f.writelines('		</Icon>\n')
     f.writelines('		<hotSpot x="32" y="1" xunits="pixels" yunits="pixels"/>\n')
     f.writelines('	</IconStyle>\n')
     f.writelines('	<LabelStyle>\n')
     f.writelines('		<scale>{}</scale>\n'.format(scalePlacemark))
     f.writelines('	</LabelStyle>\n')
     f.writelines('	<ListStyle>\n')
     f.writelines('		<ItemIcon>\n')
     f.writelines('			<href>http://maps.google.com/mapfiles/kml/paddle/ltblu-stars-lv.png</href>\n')
     f.writelines('		</ItemIcon>\n')
     f.writelines('	</ListStyle>\n')
     f.writelines('</Style>\n')
     
#-------------------------------------------------------------
def placemark(f):
#-------------------------------------------------------------
     global ndesc

     f.writelines('\n\n')
     f.writelines('<Placemark>\n')
     f.writelines('<name>{}</name>\n'.format(name))
     f.writelines('<description> <![CDATA[ \n')

     f.writelines('<table width={}><td valign=top>\n'.format(width))

     for j in range(1,ndesc+1):
          f.writelines('{}  <br />\n'.format(desc[j]))

     f.writelines('</td></table>\n')

     f.writelines(']]> </description>\n')

     f.writelines('		<LookAt>\n')
     f.writelines('			<longitude>{}</longitude>\n'.format(x))
     f.writelines('			<latitude>{}</latitude>\n'.format(y))
     f.writelines('			<altitude>0</altitude>\n')
     f.writelines('			<range>20000</range>\n')
     f.writelines('			<tilt>0</tilt>\n')
     f.writelines('			<heading>-0.05183987880918447</heading>\n')
     f.writelines('			<altitudeMode>relativeToGround</altitudeMode>\n')
     f.writelines('			<gx:altitudeMode>relativeToSeaFloor</gx:altitudeMode>\n')
     f.writelines('		</LookAt>\n')
     
     f.writelines('		<styleUrl>#msn_{}{}</styleUrl>\n'.format(styleType,color))
     f.writelines('<Point>\n')
     f.writelines('<coordinates>\n')
     f.writelines('{},{},0\n'.format(x,y))
     f.writelines('</coordinates>\n')
     f.writelines('</Point>\n')

     f.writelines('</Placemark>\n')


     if (toplace) :

          f.writelines('')
          f.writelines('<Placemark>\n')
          f.writelines('<visibility>0</visibility>\n')
          f.writelines('<name>{}</name>\n'.format(name0))
          f.writelines('<description> \n')
          f.writelines('<![CDATA[ \n')
          f.writelines('distPlace={} m\n'.format(distPlace))
          f.writelines('demPlace={} m\n'.format(demPlace))
          f.writelines(']]> \n')
          f.writelines('</description>\n')

          f.writelines(' 		<LookAt>\n')
          f.writelines(' 			<longitude>{}</longitude>\n'.format(x0))
          f.writelines(' 			<latitude>{}</latitude>\n'.format(y0))
          f.writelines(' 			<altitude>0</altitude>\n')
          f.writelines(' 			<range>20000</range>\n')
          f.writelines(' 			<tilt>0</tilt>\n')
          f.writelines(' 			<heading>-0.05183987880918447</heading>\n')
          f.writelines(' 			<altitudeMode>relativeToGround</altitudeMode>\n')
          f.writelines(' 			<gx:altitudeMode>relativeToSeaFloor</gx:altitudeMode>\n')
          f.writelines(' 		</LookAt>\n')
     


          f.writelines('<Style><LineStyle><color>FF0000FF</color><width>3</width></LineStyle></Style>\n')
          f.writelines('<LineString>\n')
          f.writelines('<altitudeMode>absolute</altitudeMode>\n')
          f.writelines('<tessellate>1</tessellate>\n')
          f.writelines('<coordinates>\n')
          f.writelines('{},{},0\n'.format(x,y))
          f.writelines('{},{},{}\n'.format(x0,y0,demPlace))
          f.writelines('</coordinates>\n')
          f.writelines('</LineString>\n')
     
          f.writelines('</Placemark>\n')
     

def doTime():
     ih=np.int(Time/3600)
     irem=np.mod(Time,3600)
     imin=np.int(irem/60)
     irem=np.mod(irem,60)
     isec=irem


     if (ih < 10) :
      ch='0'+np.str(ih)
     else:
      ch=np.str(ih)
     
     if (imin < 10) :
      cmin='0'+np.str(imin)
     else:
      cmin=np.str(imin)
     
     if (isec < 10) :
      csec='0'+np.str(isec)
     else:
      csec=np.str(isec)

     ctime=ch+':'+cmin+':'+csec
