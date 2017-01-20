import numpy as np
import os
from lxml import objectify


#--------------------------------------
# setup vpl env. variable for cyclone online calc.

hmax=2   # used in 4-mesArea.5.pr 
nplot=500 # used in 4-post3.pr
     
showvar=1
maxid = 1e6
minzmax=0.05 
minloc = 10
maxloc = 1000

#---------------------------------

date0=''  
mesFile=''  

#---------------------------------
watchFile='watch.txt'  # done by HyFlux or pre_Hyflux 
statFile='watch/statistics.txt'   # done by HyFlux 
     
summaryFile='summary.txt' #  done by 4-post2.pr
inundationFile='inundation.txt'  #  done by 4-mesArea.4.pr
     
#--------------------------------
workDir=os.getenv('workDir')
     
inputf=workDir+'/info.xml'
filein=open(inputf)
info = objectify.fromstring( filein.read() )


project=info.xpath('hurName')[-1] 
baseDate=info.bulDate       

# bulDate is the date of the bulletin on which is based the time, i.e. date of fromBul
# lastBulDate is the date of the current bulletin i.e. date of toBul

