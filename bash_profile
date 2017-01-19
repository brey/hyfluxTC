#! /bin/sh 



########################################
# hostname
########################################

export hostname=`hostname`

########################################
# Get the aliases and functions from standard setting
########################################

if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

ulimit -s unlimited

umask u=rwx,g=rwx,o=rwx

export PS1=`hostname`:'${PWD}'">"

#########################################################################################################################
# machine dependent variables:  hostser, compiler, machine, comp77, comp90, cc, cpusize, gdal, netcdf, hdf5, x11lib, mpi, PATH, LD_LIBRARY_PATH
#########################################################################################################################

export hostser=linux
export comp77=gfortran

# compiler

imach=2

#--------------
export compiler=gnu
export machine=${hostname}_gnu
export comp90=gfortran
export cc=gcc

#--------------

export cpusize=CPU64BIT
export gdal=/usr
export gdalinclude=/usr/include/gdal
export netcdf=/usr
export hdf5=/usr

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:$gdal/lib64:$netcdf/lib64:$hdf5/lib64
export x11lib=-lX11

export OS=`cat /etc/issue`

echo
echo ----------------------------------------------------------------
echo hostname=$hostname LOGNAME=$LOGNAME  hostser=$hostser  OS=$OS
echo ----------------------------------------------------------------


##################################################
#  PROJ_HOME:  folder with the software
##################################################
export PROJ_HOME=$HOME/cycloneSurge  

if [ "$PROJ_HOME" = "" -o ! -d $PROJ_HOME ] ; then
echo CAUTION ! The folder $PROJ_HOME does not exist
fi

##################################################
# version
##################################################

ivers=dev

export ivers

##################################################
# MPL_HOME: folder with the version
##################################################

export MPL_HOME=$PROJ_HOME/vers.$ivers


############################### 
# scripts & help files
############################### 

export hlpHyFlux2=$MPL_HOME/HyFlux2.hlp

export procDir=$MPL_HOME/proc

# path to map, data files, config file etc.
 
export DATA_HOME=$PROJ_HOME/DATA
export srtmpath=$DATA_HOME/srtm30_plus-v5.0/data10x10

export places=$DATA_HOME/places.txt
export popmap=$DATA_HOME/coastal_pop.tif
export configHF=$DATA_HOME/config.txt

#######################################
# set here the binary path
#######################################

export bin=$PROJ_HOME/vers.${ivers}/bin 
export PATH=./:$bin:$PATH:$gdal/bin

#######################################
# mpi
#######################################

export compmpi=mpif90

#if [ -n $netcdf ] ; then
#export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:$netcdf/lib
#export PATH=$PATH:$netcdf/bin
#fi


export more_flag=$more_flag" -DPARALLEL"
export more_flagc=$more_flag" -DPARALLEL"



##################################################
# variables for code development: alias for folder, compilers, flags

##################################################
# relap
##################################################

export r5=$MPL_HOME/r5
export prog5=$r5/prog
export mf=$prog5/mf
export mod1=$prog5/mod1
export env5=$prog5/env
export up=$prog5/up
export eur=$prog5/eur
export str5=$prog5/str
export inclr=$prog5/incl


########################################
# tpm
##################################################

export tpm=$MPL_HOME/tpm
export str=$tpm/str
export hydro=$tpm/hydro
export trans=$tpm/trans
export incls=$str/incl

########################################
# HyFlux & Swan sources
########################################

export src=$MPL_HOME/src
export program=$src/program
export HyFlux2=$src/HyFlux2

export libcsf=$src/libcsf
export libutil=$src/libutil

############################### 
# machine dependent folders
###############################
 
export lib=$MPL_HOME/lib  
export obj=$MPL_HOME/obj
export mod=$MPL_HOME/mod 


########################################
# all platform profile for library path
########################################

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/lib:/usr/lib64

export ar=ar		# for building static library
export opt="-O "
export deb="-g "


##################################################
# COMPILE & LINK flags: isorder, comp, flag, fixcard, opt
##################################################

export isorder=ISORDER  # in sun machine was isorder=noISORDER
export comp=$comp90

export flag="$flag -ffixed-form  -ffixed-line-length-132 -fno-automatic -fno-range-check  -fcray-pointer -w "
export fixcard=" -ffixed-line-length-72 "
export more_flag="-O2 "

##################################################
#  fortran flag
##################################################

export flag="$flag -D$comp -D$compiler -D$hostser -D$isorder "

###################################
# Flags for make (see $dig and $edf)
###################################

export AR=$ar
export ARFLAGS=rv
export ARTS="$ar -ts"
export ARRV="$ar -rv"

export RANLIB=ranlib
export FC=$comp
export CC=$cc

export CFLAGS=" -w  -D$hostser -D$isorder -Daddunderscore $more_flag "
export FFLAGS=" $flag $more_flag "


