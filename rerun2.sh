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

min=$2
max=$3


echo Max is: $max
echo Min is: $min
# -----------------------------------------------------
# run the compute script
# -----------------------------------------------------
for i in `seq $min $max`;
        do
            echo 'executing BulNo '$i
           #execute "runSurgeModel.sh -hurNo $hurNo -bulNo $i -surgeCalc automatic"
            execute "runSurgeModel.sh -hurNo $hurNo -bulNo $i -surgeCalc automatic -testing yes"
        done

