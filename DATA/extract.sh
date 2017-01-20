
if [ $# -ge 1 ] ; then
export outmap=$1
else
echo enter
echo $0  outmap [ lonmin lonmax latmin latmax ]
exit
fi

if [ $# -eq 5 ] ; then
export lonmin=$2
export lonmax=$3
export latmin=$4
export latmax=$5
fi
if [ "$type" = "" ] ; then type=Int16 ; fi

gdal_translate -ot $type -mo COMPRESSION=LZW -projwin $lonmin $latmax $lonmax $latmin $DATA_HOME/gebco30.tif  $outmap
