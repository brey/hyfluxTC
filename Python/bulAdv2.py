import numpy as np
import os
from family import family
from subprocess import call
import sys
from shutil import copy
from pylab import plot,show,xlabel,ylabel,text,savefig,title,axis,figure,figtext,legend,subplots_adjust
from read import cvread, readf
from xmldic import bxml
from bunch import Bunch
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, parse
from xml.etree.ElementTree import SubElement
from xml.dom import minidom



def bulAv(*argv):

     maxcc=True

     latmax=60

     # vpl -i $cycloneDir/bulAdv.pr $fromBul $toBul $outDir

     # the script must run on the TC directory

     fromBul=np.int(sys.argv[1])
     toBul=np.int(sys.argv[2])
     try:
        outDir=sys.argv[3]
     except:
        pass

     velStart=os.getenv('velStart',30.)  # m/s
     velEnd=os.getenv('velEnd',18.)  # m/s
     timeForecast=os.getenv('timeForecast',96.)
     InTime=os.getenv('InTime',0.)  # m/s

     velStart=np.float(velStart)
     velEnd=np.float(velStart)
     timeForecast=np.float(velStart)
     InTime=np.float(InTime)

#------------------------------------

     cath=np.array([ -1 , 0 ,  1 ,  2 ,  3 ,  4 ,  5 ,  6])
     wind=np.array([  0 , 18 , 33 ,  43 ,  50 ,  60 , 70 , 1000])
     [cath]=family(wind,cath)

#------------------------------------
#advNo tShift $date land notes
     advNo, tShift, date, land, notes = cvread(np.str(toBul)+'/bulInfo.txt')
#exec $toBul./info.pr
     info=bxml(np.str(toBul)+'/info.xml').setexp
     lastHurName=info.hurName
#------------------------------------
# copy bulInfo
#------------------------------------

     n=np.size(advNo)
     lastBulNo = 0

     outdat=[]
     outdat.append(Bunch())

     first=True
     for i in range(n):
        adv=advNo[i]
        if (adv < fromBul): break 

        infile=np.str(adv)+'/outData.txt' 
        if os.path.isfile(infile)==False : break

        LCNEXT=adv
        # time   xhc     yhc     b      k     rmax    deltap     vtrx    vtry vmax0
        lst=readf(infile)
        outdat.append(lst)
        #lvar

        if not outdat[adv].vmax0.any() : outdat[adv].vmax0=outdat[adv].vmax

# bul.@adv.=max(bul.@adv.,bulNo)

        vmaxBul=outdat[adv].vmax
        maxvmax=np.max(vmaxBul)
        print adv,first,maxvmax,velStart

        if ((first)  & (maxvmax < velStart)): break
        if ((np.size(vmaxBul) == 1) & (i == n-1)): exit 
        if ((np.size(vmaxBul) == 1) & (vmaxBul[0] != None)):  break

        outdat[adv].rmax =0.001*outdat[adv].rmax
        outdat[adv].deltap =0.01*outdat[adv].deltap


        if (first) :
                forecast=0
                timeShift=0
                advFirst=adv
                bul=adv
                ts=tShift[i]
                time=0
                xhc=outdat[adv].xhc[0]
                yhc=outdat[adv].yhc[0]
                b=outdat[adv].b[0]
                k=outdat[adv].k[0]
                rmax=outdat[adv].rmax[0]
                deltap=outdat[adv].deltap[0]
                vtrx=outdat[adv].vtrx[0]
                vtry=outdat[adv].vtry[0]
                vmax0=outdat[adv].vmax0[0]
                vmax=outdat[adv].vmax[0]
                first=False
        else:
                forecast=np.append(forecast,0)
                timeShift=tShift[i] - ts
                time=np.append(time,timeShift)
                bul=np.append(bul,adv)
                xhc=np.append(xhc,outdat[adv].xhc[0])
                yhc=np.append(yhc,outdat[adv].yhc[0])
                b=np.append(b,outdat[adv].b[0])
                k=np.append(k,outdat[adv].k[0])
                rmax=np.append(rmax,outdat[adv].rmax[0])
                deltap=np.append(deltap,outdat[adv].deltap[0])
                vtrx=np.append(vtrx,outdat[adv].vtrx[0])
                vtry=np.append(vtry,outdat[adv].vtry[0])
                vmax0=np.append(vmax0,outdat[adv].vmax0[0])
                vmax=np.append(vmax,outdat[adv].vmax[0])
        
        bias=outdat[adv].bias
       #nv[adv]=np.sum(outdat[adv].nv != None)

        labt= timeShift+outdat[adv].time
        labv= outdat[adv].vmax0
        labx= outdat[adv].xhc
        laby= outdat[adv].yhc
        labr= outdat[adv].rmax
        labp= outdat[adv].deltap
        labb= outdat[adv].b
        labk= outdat[adv].k
        
        lastBulNo = adv
        lastBulDate=date[adv-1]


#---------------------------------------------------------------------------

     lat = np.abs(outdat[adv].yhc)
     if (np.max(lat) > latmax) :
          ntot=np.argwhere(lat > latmax).flatten()[0]-1
     else:
          ntot=np.size(lat)

     print np.arange(np.size(lat)),lat,ntot

     maxcc= maxcc or (ntot <= 1) or lastBulNo == 0
     print maxcc

#---------------------------------------------------------------------------

     adv=lastBulNo

     time=np.append(time,outdat[adv].time[1:ntot]+timeShift)
     bul=np.append(bul,np.ones(ntot-1)*adv)
     forecast=np.append(forecast,np.ones(ntot-1))
     xhc=np.append(xhc,outdat[adv].xhc[1:ntot])
     yhc=np.append(yhc,outdat[adv].yhc[1:ntot])
     b=np.append(b,outdat[adv].b[1:ntot])
     k=np.append(k,outdat[adv].k[1:ntot])
     rmax=np.append(rmax,outdat[adv].rmax[1:ntot])
     deltap=np.append(deltap,outdat[adv].deltap[1:ntot])
     vtrx=np.append(vtrx,outdat[adv].vtrx[1:ntot])
     vtry=np.append(vtry,outdat[adv].vtry[1:ntot])
     vmax0=np.append(vmax0,outdat[adv].vmax0[1:ntot])
     vmax=np.append(vmax,outdat[adv].vmax[1:ntot])

#------------------------------------

     cat0=cath(vmax)
     catWind=cat0.astype(int)
     catWind[cat0<0]=-1

#------------------------------------
     FinTime=timeShift+np.min([np.max(time)-timeShift,timeForecast])
#------------------------------------

     maxWind=np.max(vmax) 
     bulMaxWind=np.argwhere(bul[maxWind == vmax]).flatten()[0]
     catMaxWind=np.argwhere(catWind[maxWind == vmax]).flatten()[0]
     dateMaxWind=date[bulMaxWind]

     iadv=np.argwhere(bul == toBul).flatten()[0]
     advWind=vmax[iadv]
     catAdvWind=catWind[iadv]

#------------------------------------
# read last bul and evaluate the track in the next timeForecast 


     i=np.argwhere((time > (timeShift-48)) & (time < (timeShift+timeForecast+12))).flatten()
     if (np.max(i)+1 < time.size-1): i=np.append(i,np.max(i)+1)
     rmax1=1+5*np.max(rmax[i])/100e3

     xhc1=xhc[i]
     yhc1=yhc[i]
     lonmin=np.min(xhc1-rmax1/np.cos(np.radians(yhc1)))
     lonmax=np.max(xhc1+rmax1/np.cos(np.radians(yhc1)))
     latmin=np.min(yhc1-rmax1)
     latmax=np.max(yhc1+rmax1)

#------------------------------------

     outDir=np.str(advFirst)+'-'+np.str(lastBulNo)

     if os.path.exists(outDir) == False :  os.makedirs(outDir)

     try:
       copy(np.str(advFirst)+'/info.sh' ,outDir)
     except IOError as e:
       print e
       pass
#call(["chmod +w $outDir./info.sh"])

     if InTime==None : InTime=timeShift
     dtout=3600

     tree=parse(np.str(toBul)+'/info.xml')
     root=tree.getroot()

     bulno = SubElement(root,'bul_update')
     bulno.text='number '+np.str(info.bulNo)
     l1=SubElement(bulno,'lastBulDate')
     l1.text=lastBulDate
     l2=SubElement(bulno, 'hurName')
     l2.text=np.str(lastHurName)
     l3=SubElement(bulno, 'toBul')
     l3.text=np.str(lastBulNo)
     l4=SubElement(bulno, 'InTime')
     l4.text=np.str(InTime)
     l5=SubElement(bulno, 'timeShift')
     l5.text=np.str(timeShift)
     l6=SubElement(bulno, 'FinTime')
     l6.text=np.str(FinTime)
     l7=SubElement(bulno, 'dtout')
     l7.text=np.str(dtout)

     l8=SubElement(bulno, 'maxWind')
     l8.text=np.str(maxWind)
     l9=SubElement(bulno, 'catMaxWind')
     l9.text=np.str(catMaxWind)
     l10=SubElement(bulno, 'dateMaxWind')
     l10.text=dateMaxWind
     l11=SubElement(bulno, 'bulMaxWind')
     l11.text=np.str(bulMaxWind)
     l12=SubElement(bulno, 'advWind')
     l12.text=np.str(advWind)
     l13=SubElement(bulno, 'catAdvWind')
     l13.text=np.str(catAdvWind)


# beautify xml line
     r_string=ElementTree.tostring( root )
     rep=minidom.parseString(r_string)

     f=open(outDir+'/info.xml','w')
     f.write('\n'.join([line for line in rep.toprettyxml(indent=' '*2).split('\n') if line.strip()]))
     f.close()
     
     f=open(outDir+'/info.sh','a')

     f.write("# ------------------  \n")
     f.write("# added by bulAdv.pr  \n")
     f.write("# ------------------  \n")
     f.write("export lastBulDate='{}'\n".format(lastBulDate))
     f.write("export hurName={}\n".format(lastHurName))
     f.write("export toBul={}\n".format(lastBulNo))
     f.write("export InTime={}\n".format(InTime))
     f.write("export timeShift={}\n".format(timeShift))
     f.write("export FinTime={}\n".format(FinTime))
     f.write("export dtout={}\n".format(dtout))
     
     f.write("export maxWind={}\n".format(maxWind))
     f.write("export catMaxWind={}\n".format(catMaxWind))
     f.write("export dateMaxWind='{}'\n".format(dateMaxWind))
     f.write("export bulMaxWind={}\n".format(bulMaxWind))
     f.write("export advWind={}\n".format(advWind))
     f.write("export catAdvWind={}\n".format(catAdvWind))
     f.write("export lonminAdv={}\n".format(lonmin))
     f.write("export lonmaxAdv={}\n".format(lonmax))
     f.write("export latminAdv={}\n".format(latmin))
     f.write("export latmaxAdv={}\n".format(latmax))
     
     f.close()
     
     try:
        workStatus=os.environ.get('workStatus')
     except:
        pass

     if workStatus == None :   workStatus="/home/brey/cycloneSurge/storage/tmp/1000146/STATUS.sh"

     f=open(workStatus,'a')
     
     f.write("# ------------------  \n")
     f.write("# added by bulAdv.pr  \n")
     f.write("# ------------------  \n")
     f.write("export advNo={}\n".format(outDir))
     f.write("export fromBul={}\n".format(advFirst))
     f.write("export toBul={}\n".format(lastBulNo))
     f.write("export InTime={}\n".format(InTime))
     f.write("export timeShift={}\n".format(timeShift))
     f.write("export FinTime={}\n".format(FinTime))
     f.write("# ..................  ")
     
     f.close()
     
     try:
       copy(np.str(toBul)+'/bulInfo.txt' ,outDir)
     except IOError as e:
       print e
       pass

     timemin=time*60
     timesec=time*3600


#let timemin timesec bul forecast xhc  yhc  vmax vmax0  b  k  rmax  deltap vtrx vtry 

     figure()
     plot(labx,laby)
     legend()
     savefig(outDir+'/tracks.png')


     figure()
     plot(labt,labv)
     xlabel('Time (h)')
     ylabel(' Max Velocity (m/s)')
     figtext(.01,.05,'vmax: max velocity as in inpData.txt',size='small')
     figtext(.01,.03,'vmax0: max velocity at 10 min average minus translational minus coriolis effect',size='small')
     legend()
     subplots_adjust(bottom=.15)
     savefig(outDir+'/vmax.png')

     figure()
     plot(labt,labp)
     xlabel('Time (h)')
     ylabel('Pressure deficit (mBar)')
     legend()
     savefig(outDir+'/deltap.png')


     figure()
     plot(labt,labr)
     xlabel('Time (h)')
     ylabel('Radius of Max Velocity (Km)')
     legend()
     savefig(outDir+'/rmax.png')


     figure()
     plot(labt,labk)
     legend()
     savefig(outDir+'/k.png')


     figure()
     plot(labt,labb)
     legend()
     savefig(outDir+'/b.png')

     rmax=rmax*1000
     deltap=deltap*100

     outfile=outDir+'/outData.txt'
     
     var=np.column_stack([time,timemin,timesec,bul,forecast,xhc,yhc,vmax,vmax0,b,k,rmax,deltap,vtrx,vtry,catWind])
     varn=('time','timemin','timesec','bul','forecast','xhc','yhc','vmax','vmax0','b','k','rmax','deltap','vtrx','vtry','catWind')
     he=" ".join(varn)
     fmt=" ".join(["%10.4f"]*(np.shape(var)[1]))
     fmt="{0: ^{1}}".format('%10.4f',10)
     fmt=('%3.0f','%4.0f','%6.0f','%1i','%1i','%6.3f','%6.3f','%8.4f','%8.4f','%8.4f','%8.4f','%6.3f','%6.3f','%8.4f','%8.4f','%1i')
     np.savetxt(outfile,var,header=he, fmt=fmt)


