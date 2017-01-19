#!/bin/bash
#-------------------------------------------- 
function item () { echo $1 | cut -d' ' -f $2 ; }
export -f item
#--------------------------------------------
function float_cond () {
cond=$(echo "$*" | bc -q 2>/dev/null)
echo $cond
}
export float_cond
#--------------------------------------------
function expr () {
echo $(echo "$*" | bc -l 2>/dev/null)
}
export expr
#--------------------------------------------
function min () {
cond=$(echo "$1 < $2 " | bc -q 2>/dev/null)
if [ $cond -eq 1 ] ; then
echo $1
else
echo $2
fi
}
export min
#--------------------------------------------
function max () {
cond=$(echo "$1 > $2 " | bc -q 2>/dev/null)
if [ $cond -eq 1 ] ; then
echo $1
else
echo $2
fi
}
export max
#--------------------------------------------
function int () {
i=`echo $1 | cut -d'.' -f 1 `
if [ "$i" = "" ] ; then i=0 ; fi
echo $i
}
export -f int
#--------------------------------------------
function nlines () {
echo `cat $1 -n | tail -n 1 | cut -f 1 `
}
export -f nlines
#--------------------------------------------
function nitem () {
echo $#
}
export -f nitem

#--------------------------------------------
function giga () { 
let n=`echo $1 | cut -f 1 -d.`  
line=`echo $1 2> /dev/null | grep G`
if [ "$line" = "" ] ; then let n=0 ; fi
echo $n
}
export -f giga

#--------------------------------------------
tmpspace () {
if [ $host = hpc ] ; then
line=`qhost -h $fullhostname -F tmpfree | grep  tmpfree`
export tmpfree=`getField -l "$line" -f hl:tmpfree -d =`
	if [ "$tmpfree" = "" ] ; then gigafree=10000
	else gigafree=`giga $tmpfree`  ; fi
else
gigafree=10000
fi
echo $gigafree
}
export -f tmpspace
#--------------------------------------------
check_free_space()
{
# space in GB
if [ $# -eq 0 ] ; then return 1 ; fi
    location=$1
    real_location=$location
    while [ ! -d "$real_location" ] ; do
        real_location=$(dirname $real_location)
    done
    exist_space=$(df -k $real_location | tail -n1 | tr -s "[:space:]" | cut -d ' ' -f 4)
    let exist_space=exist_space/1024
    let exist_space=exist_space/1024

    echo $exist_space
}
export -f check_free_space

#--------------------------------------------
check_free_mem()
{
# mem in GB
    mem=$(free -g | grep -i mem | tr -s "[:space:]" | cut -d " " -f 4 )
    echo $mem
}
export -f check_free_mem

#--------------------------------------------
check_mkdir()
{
dir=$1
ok=0
count=0
while [ $ok -eq 0 -a $count -lt 10 ] ; do 
cd $dir
maxcc=$?
pwd=`pwd`
let maxcc=$maxcc+$?
if [ $maxcc -eq 0 -a "$dir" = "$pwd" ] ; then ok=1  
else sleep 1s ; echo Sleep 1s because roblems in check_mkdir  $1 >> $stepOut  
fi
let count=count+1
done
chmod +w -R $dir
echo $ok
}
#--------------------------------------------
function cdir () {
echo `basename $1`
}
export -f cdir

#--------------------------------------------
function elaps () {
let dt=$2-$1
let hh=dt/3600
mm=`echo " ( $dt - $hh * 3600 ) / 60 " | bc `
ss=`echo " ( $dt - $hh * 3600 - $mm * 60 )  " | bc `
if [ $mm -le 9 ] ; then mm="0$mm" ; fi
if [ $ss -le 9 ] ; then ss="0$ss" ; fi

echo ${hh}:${mm}:${ss}
}
export -f elaps
#--------------------------------------------
function CMD ()
{
com=`item "$*" 1`
echo `basename $com`
}
export -f CMD 

#--------------------------------------------
function echoStep ()
{ 
date_fun=`date "+%H:%M:%S"`
path_fun=`pwd`
comm_fun="$1"
narg_fun=$#
ncomm_fun=`nitem $comm_fun`
arg0_fun=$0
  if [ $ncomm_fun  -gt 1 ] ; then
  arg_fun=`item "$comm_fun" 2-`
  else
  arg_fun=
  fi
  if [ $narg_fun -eq 2 ] ; then
  out_fun="---> $2"
  else
  out_fun=
  fi
#declare -p arg0 date path comm arg narg ncomm out  ccexec
comm_fun=$cmdChain/`CMD $1`
  if [ $ccexec -eq 0 ] ; then
  res_fun=" ---> done "
  else
  res_fun=" ---> failed "
  fi
echo  $date_fun -\> $path_fun -\> $comm_fun $arg_fun $out_fun  $res_fun >> $stepOut
}
export -f echoStep
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
function CSexecute ()
{
t1=`date +%s` 
echo
echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
echo `pwd`\> $*  
echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
$* 
ccexec=$?
echo
t2=`date +%s` 
#echo free_tmp_space: `check_free_space /tmp/tmpfs`[G]   free_mem: `check_free_mem`G 
echo done  $cmdChain/`CMD $*` in  'elaps_time[hh.mm.ss]' `elaps $t1 $t2`
echo ..............................................................
echo
echoStep  "$*" 
return $ccexec
}
export -f CSexecute


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
function SSexecute ()
{
echo
echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
echo `pwd`\> $*   
echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
$* 
ccexec=$?
echo done  $cmdChain/`CMD $*` 
echo ..............................................................
echo
echoStep  "$*" 
return $ccexec
}
export -f SSexecute

#--------------------------------------------
function NVSexecute ()
{
echo $*
$* &> /dev/null
ccexec=$?
echoStep  "$*" 
return $ccexec
}
export -f NVSexecute
#--------------------------------------------
function FVSexecute ()
{
echo $1
$1 &> $2
ccexec=$?
echoStep  "$1" $2 
return $ccexec
}
export -f FVSexecute


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function htmlOut ()
{
name=`basename $1`
#echo '<a href="'$name'" >' $name '< /a>'
echo $name
}
export -f htmlOut
 
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
function FSexecute ()
{
echo
echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
echo `pwd`\> $1 \&\> `htmlOut $2`
echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
$1 &> $2
ccexec=$?
echo done  $cmdChain/`CMD $1` 
echo ..............................................................
echo
echoStep  "$1" $2
return $ccexec
}
export -f FSexecute
 
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
function TSexecute ()
{
# put output file on tmpdir
if [ "$nout" = "" ] ; then nout=0 ; fi
let nout=nout+1
export nout

#file=$tmpdir/$nout-$win-`basename $1`.html
file=$tmpdir/$nout-$win-`basename $1`.txt


echo `pwd`\> $*   \&\> `htmlOut $file `
$* &> $file 
ccexec=$?
echoStep  "$*" `htmlOut $file`
return $ccexec
}
export -f TSexecute
 