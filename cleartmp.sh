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

# get arguments

hurName="$1"

execute "rm -rf /home/critechuser/cycloneSurge/storage/tmp/$hurName*"

