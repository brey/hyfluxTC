#!/bin/bash
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
#----------------------------------------------------------
unset n
while IFS='' read -r line || [[ -n "$line" ]]; do
    echo "executing: $line"
    execute "$line"
    : $[n++]
done < "$1"
#sed -i "1,$n d" "$1"
