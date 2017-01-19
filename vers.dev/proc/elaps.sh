
t1=`date --date="$date1" +%s` 
t2=`date --date="$date2" +%s` 
let dt=t2-t1
let hh=dt/3600
mm=`echo " ( $dt - $hh * 3600 ) / 60 " | bc `
ss=`echo " ( $dt - $hh * 3600 - $mm * 60 )  " | bc `
if [ $mm -le 9 ] ; then mm="0$mm" ; fi
if [ $ss -le 9 ] ; then ss="0$ss" ; fi

echo ${hh}:${mm}:${ss}