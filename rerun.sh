#!/bin/bash
. bash_profile

#----------------------------------------------------------
function item () { echo $1 | cut -d' ' -f $2 ; }
#----------------------------------------------------------
function CMD ()
{
com=`item "$*" 1`
echo `basename $com`
}
#----------------------------------------------------------
function execute ()
{
echo $*
$1
ccexec=$?
echo done  $cmdChain/`CMD $1`
}


# get arguments

command="$0"

if [ $# -eq 0 ] ; then
echo
echo enter: $0 -hurNo 
echo
fi


export hurNo="$1"
echo "hurNo=" $hurNo

WEB=/mnt/web/cycloneSurgeVM

# initialize
#------------------------------------------------ 
if [ ! -d $PROJ_HOME/storage/public/$hurNo ] ; then mkdir -p $PROJ_HOME/storage/public/$hurNo ; fi

# copy input file in public directory

cp -uvr $WEB/$hurNo/input  $PROJ_HOME/storage/public/$hurNo

LOCATION=$PROJ_HOME/storage/public/$hurNo/input

max=-999999999;
min=999999999;

for item in $LOCATION/* #$LOCATION/.*
do
Num=${item: -2}
if [ ${Num:0:1} = "/" ] ; then Num=${Num:1:2};fi

     if [[ $Num -gt $max ]] 
     then 
         max=$Num
     fi
     if [ $Num -lt $min ] 
     then
        min=$Num
     fi
done

if [[ $min -lt $2 ]] ; then min=$2;fi
if [[ $max -gt $3 ]] ; then max=$3;fi


echo Max is: $max
echo Min is: $min
# -----------------------------------------------------
# run the compute script
# -----------------------------------------------------
for i in `seq $min $max`;
        do
            echo 'executing BulNo '$i
            execute "runSurgeModel.sh -hurNo $hurNo -bulNo $i -surgeCalc automatic -testing yes"
        done

