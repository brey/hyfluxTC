makedir

obj=`dir obj`

prog=$1
 
library=$lib/commonFiles.a
 

include="-I. "
if [ -d "$gdal/include" ] ; then include=$include" -I$gdal/include " ; fi
if [ -d "$gdalinclude" ] ; then include=$include" -I$gdalinclude " ; fi
if [ -d "$netcdf/include" ] ; then include=$include" -I $netcdf/include " ; fi
if [ -d "$hdf5/include" ] ; then include=$include" -I $hdf5/include " ; fi
echo include=$include

if [ "$NCOLS" = "" ] ; then
echo enter NCOLS
read NCOLS
fi

if [ "$NROWS" = "" ] ; then
echo enter NROWS
read NROWS
fi

echo $CC *.cpp -c  $CFLAGS $include -DNCOLS=$NCOLS -DNROWS=$NROWS 
$CC *.cpp -c  $CFLAGS $include -DNCOLS=$NCOLS -DNROWS=$NROWS
 
 
mv *.o $obj
 
if [ -f $library ]
then rm $library
fi
 
$ar -qv $library $obj/*.o
$ar -ts $library
 

