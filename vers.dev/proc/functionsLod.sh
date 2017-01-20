
export minFac=0.5 maxFac=20 unitLod=10000

function pixLodFun ()
{
echo " sqrt ( ( $east - $west ) ^ 2 + ( $north - $south ) ^ 2 ) / ( $batgrid / 60 ) " | bc -l | cut -d'.' -f 1
}
export -f  pixLodFun

function exprLod ()
{
echo " $# " | bc -l | cut -d'.' -f 1 
}
export -f  exprLod

function minbatgrid () {
export min=100000000
for val in $batgridS ; do
let dx=`echo " $val * $unitLod " | bc -l | cut -d'.' -f 1 `
#echo $val $dx
if [ $dx -lt $min ] ; then export min=$dx ; fi
done
echo $min
} 
export -f  minbatgrid


function maxbatgrid () {
export max=0
for val in $batgridS ; do
let dx=`echo " $val * $unitLod " | bc -l | cut -d'.' -f 1 `
#echo $val $dx
if [ $dx -gt $max ] ; then export max=$dx ; fi
done
echo $max
} 
export -f  maxbatgrid


function minLodFun () { 
let dx=`echo " $batgrid * $unitLod " | bc -l | cut -d'.' -f 1 `
echo " `pixLodFun` * $minFac / sqrt( $dx / `minbatgrid` ) " | bc -l | cut -d'.' -f 1 
}  
export -f  minLodFun

function maxLodFun () { 
let dx=`echo " $batgrid * $unitLod " | bc -l | cut -d'.' -f 1 `
if [ "$finergrid" = yes ] ; then
echo '-1'
else
echo "  `pixLodFun` * $maxFac  * sqrt( $dx  / `maxbatgrid` ) " | bc -l | cut -d'.' -f 1
fi
} 
export -f  maxLodFun
