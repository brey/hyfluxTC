
if [ $# -ne 2 ] ; then
echo $0 fullpathdir testMin
exit
fi



dir=$1
export testMin=$2




cd $dir

export arrMap=TIF_Arrival_Time_notCleaned.tif
gdaldem slope $arrMap tarrSlope.tif

###########################################################
#  Clean arrival time
###########################################################

cat   << EOF &> clean.pr

testMin=$testMin
getmap tarr $arrMap
getmap bat bathymetry.tif

getmap dtarr tarrSlope.tif

radius=6378388
deg2m=pigr*radius/180

% convert from h/deg -> sec/m
dtarr=dtarr*3600/deg2m
c1 = 1/max(1,dtarr)
c2 = sqrt(max(1,-grav*bat))
n=size(c1)
ii=do(n)

test=c1/c2

i=getm(eqmv(tarr),ii)
test_i = MV

setexp DRIVER PCRaster
TYPE=4
putmap test test.map

%---------------------------------------------
% clean tarr & test

i=getm(eqmv(test).or.test.lt.testMin,ii)
tarr_i = MV
test_i = MV
putmap test $testMin.test.map

setexp DRIVER GTiff
putmap tarr TIF_Arrival_Time.tif

EOF

vpl -i clean.pr &> /dev/null

echo Cleaned Arrival Time  testMin=$testMin

###########################################################
# contour
###########################################################
 
gdal_contour -a Tarrival -nln Tarrival -i 0.08333 "./TIF_Arrival_Time.tif" "./ArrTime_5min"
ogr2ogr -f KML ./Tarrival_5min.kml ./ArrTime_5min/Tarrival.shp
 
gdal_contour -a Tarrival -nln Tarrival -i 0.250000 "./TIF_Arrival_Time.tif" "./ArrTime_15min"
ogr2ogr -f KML ./Tarrival_15min.kml ./ArrTime_15min/Tarrival.shp

gdal_contour -a Tarrival -nln Tarrival -i 1.000000 "./TIF_Arrival_Time.tif" "./ArrTime_60min"
ogr2ogr -f KML ./Tarrival_60min.kml ./ArrTime_60min/Tarrival.shp

zip -f locations.kmz Tarrival*.kml

if [ "$tmpClean" = yes ] ; then rm -rf *test.map Tarrival*.kml ; fi

