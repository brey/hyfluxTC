#! /bin/bash

echo > stepout.txt 
function Cexec () {
echo `pwd` \> $* >> stepout.txt
#echo $*
$*
}

cd program

lib="../../lib"
mod="../../mod"
bin="../../bin"
obj="../../obj"
objutil="../../obj/src/libutil"

for dir in $lib $mod $bin $obj  $objutil; do
if [ ! -d $dir ] ; then Cexec mkdir -p $dir ; fi
done

cd ..


hostser="linux"
cpusize="CPU64BIT"
mpi=$1  # non mpi indicare nullo


if [ "$mpi" = "yes" ] ; then 
	echo "Compilining for MPI"
else
	echo "Compilining for single core"
fi

CC=gcc       # c compiler
CPP="g++"      # c++ compiler
#CPP=gcc         # critech cluster01
FC=gfortran  # Fortran compiler
FCMPI=mpif90 # Fortran compiler for MPI

#UBUNTU PC
#NCOLS=2402
#NROWS=1900

NCOLS=5402
NROWS=3202

comp=$FC
CFLAGS=" -w   -Dlinux -DISORDER   -O2    -Daddunderscore"
           
# gfortran flags
flag="   -ffixed-form  -ffixed-line-length-132 -fno-automatic -fno-range-check  -fcray-pointer -w  -Dgfortran -Dgnu -Dlinux -DISORDER -J$mod -I$mod -I. "
optFlag=" -O2    "

gdalinclude="-I/usr/include -I/usr/include/gdal"    #gdal library folder
netcdfinclude="-I/usr/include"

echo 'Compile Hyflux Libraries ? (Enter y/n)'
read answ
if [ "$answ" = "y" ] ; then 


#-----------------------------------------
#  compile libcsf
#-----------------------------------------
echo "=============================================================="
echo 'start compiling libcsf'
echo "=============================================================="
cd libcsf
if [ -e $lib/libcsf.a ] ; then 
rm $lib/libcsf.a
fi
echo pwd=`pwd`

echo "=============================================================="
echo $CC *.c -c -w  -D$hostser  -D$cpusize
echo "=============================================================="
$CC *.c -c -w  -D$hostser -D$cpusize
 
ar -qv libcsf.a *.o
ar -ts libcsf.a

mv libcsf.a $lib
 
#rm *.o
echo 'end compiling libcsf'
cd ..


#-----------------------------------------
#  compile libutil
#-----------------------------------------
echo "=============================================================="
echo 'start compiling libutil'
echo "=============================================================="
cd libutil
 
if [ -e $lib/libcsf.a ] ; then 
rm $lib/libutil.a
fi

include="-I. -I../libcsf  $gdalinclude"

echo "=============================================================="
Cexec $CC *.c -c  $CFLAGS $include -D$hostser -D$cpusize 
echo "=============================================================="
 
echo "=============================================================="
Cexec "$FC *.F -c -D$FC $flag $optFlag $include"
echo "=============================================================="
 
Cexec ar -qv libutil.a *.o
Cexec ar -ts libutil.a

Cexec mv libutil.a $lib
Cexec mv *.o  $objutil
cd ..

echo 'end compiling libutil'

fi

echo 'Compile supporting programmes for Hyflux ? (Enter y/n)'
read answ
if [ "$answ" = "y" ] ; then


#-----------------------------------------
#  compile program
#-----------------------------------------
echo "=============================================================="
echo 'start compiling programs'
echo "=============================================================="

cd program



library="$lib/libutil.a $lib/libcsf.a -lgdal"
listProgs="cleanSpot endfile endtable getField getMinheight getMinheight.1 getNv inquire ldd2path map2image mapind MapInterp1 MapInterp mapsum mergeLoc mergeMap path2kml resmap stathur" 

for prog in $listProgs; do
	echo
	echo compiling and linking $prog
	if [ -e $bin/$prog ] ; then 
	rm $bin/$prog
	fi

Cexec	$FC $prog.F $flag $optFlag -I./ -o $bin/$prog  $library
	ls -la $bin/$prog
done
echo 'end compiling programs'
cd ..

fi

echo 'Compile Hyflux ? (Enter y/n)'
read answ
if [ "$answ" = "y" ] ; then

#-----------------------------------------
#  compile Hyflux
#-----------------------------------------
echo "=============================================================="
echo 'start compiling HyFlux'
echo "=============================================================="

cd HyFlux2

library="$lib/libutil.a $lib/libcsf.a -lgdal"
listFiles="HyFlux2.1 inout geom pointers riemann init trans boundary post watch shoreline holland mapEnv maplib"

comp=$FC
moreFlagMpi=""
if [ "$mpi" = "yes" ] ; then
	listFiles=$listFiles" mpi_C_inout mpi_I_inout mpi_S_inout mpi_P_inout mpi_V_inout mpi_assignCore " 
	comp=$FCMPI
	echo $listFiles
	moreFlagMpi=" -DPARALLEL -I/usr/include/openmpi-x86_64 "
fi

echo      "      character*30 version/'`date`'/ " > version.inc
echo      "      character*80 pwdsoft /'`pwd`'/ " >> version.inc
echo      "      character*80 more_flag /'$optFlag $moreFlagMpi'/ " >> version.inc

rm *.o


for f in $listFiles; do
	echo
	echo compiling $f
	if [ "$mpi" = "yes" ] ; then
		if [ -e $1.pf -a $f.F -nt $1.pf ] ; then 
			echo precmp_low $1.pf $f.F
			precmp_low $1.pf $f.F
		fi
	fi

Cexec	$comp $f.F -c $flag $path  $moreFlagMpi $optFlag
	
done

if [ "$mpi" = "yes" ] ; then
	if [ -e $bin/mpi.HyFlux2.1 ] ; then 
		rm $bin/mpi.HyFlux2.1
	fi

	Cexec	$comp -o $bin/mpi.HyFlux2.1 $optFlag *.o $library
else
	if [ -e $bin/HyFlux2.1 ] ; then 
		rm $bin/HyFlux2.1
	fi

	Cexec	$comp -o $bin/HyFlux2.1 $optFlag *.o $library
fi


echo 'end compiling Hyflux'
cd ..
fi

echo 'Compile source Common Files programmes ? (Enter y/n)'
read answ
if [ "$answ" = "y" ] ; then

#-----------------------------------------
#  compile Common Files 
#-----------------------------------------
echo "=============================================================="
echo  start compiling source Common Files
echo "=============================================================="

NCOLS=6500
NROWS=3802


library="-lgdal -lnetcdf"

opt=-O3
#opt="-g" # for debugging
echo
echo Compilation with NCOLS=$NCOLS  NROWS=$NROWS

for dir in  sourceCommonFiles postProcessing coastalImpact ; do
	echo
	echo processing $dir
	cd $dir
	rm *.o
	
	
	echo "$CPP -c *.cpp  $gdalinclude  $netcdfinclude -I ../sourceCommonFiles  -Wno-write-strings -w $opt  -DNCOLS=$NCOLS -DNROWS=$NROWS "
	$CPP -c *.cpp  $gdalinclude  $netcdfinclude  -I ../sourceCommonFiles  -Wno-write-strings -w $opt  -DNCOLS=$NCOLS -DNROWS=$NROWS
	if [ $dir != sourceCommonFiles ] ; then 
		if [ -e $bin/$dir ] ; then 
		rm $bin/$dir
		fi

		echo "	$CPP -o $bin/$dir *.o -lm ../sourceCommonFiles/*.o $library $opt"
		$CPP -o $bin/$dir *.o -lm ../sourceCommonFiles/*.o $library $opt $opt
	fi

	cd ..
	echo
	#echo done $dir

done

fi

 
ls -la ../bin/*
