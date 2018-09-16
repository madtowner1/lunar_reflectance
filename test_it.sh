#!/bin/sh

args=("$@") 
year=${args[0]} 
doy=${args[1]}
hour=${args[2]}
minu=${args[3]}

algo_path=$(cd $(dirname "$0"); pwd)
echo $year, $doy, $hour, $minu

 if date -v 1d > /dev/null 2>&1; then
  doy_dum=`expr ${doy} - 1` 
  
   doy_dum=`expr ${doy} - 1` 

  DAY_OBS=$(./shift_date.sh ${year}0101 +${doy_dum})
  month=${DAY_OBS:4:2}
  day=${DAY_OBS:6:2}
else
  month=$(date -d "01/01/${year} +${doy} days -1 day" "+%m")
  day=$(date -d "01/01/${year} +${doy} days -1 day" "+%d")
  
fi
echo $month
echo $day
  
  #cp peate_downloader.sh $algo_path
  
 [ ! -d 'results' ] && mkdir -v -p 'results'

 echo '=>  ', $algo_path
sh -c 'peate_downloader.sh '$year'-'$month'-'$day'+'$hour':'$minu':00 '$year'-'$month'-'$day'+'$hour':'$minu':00 SVDNB  GDNBO '


pwd > $algo_path'/dnb_input'
ls -tr SV*h5 | tail -1 >> $algo_path'/dnb_input'
ls -tr GDNBO*h5 | tail -1 >> $algo_path'/dnb_input'

cd $algo_path

./dnb_start

#rm -f GDNBO* 
#rm -f SVDNB*

lunar_file=`ls -tr results/LUNAR*h5 | tail -1`

# idl -quiet -args $lunar_file << eof
# 
# arg = COMMAND_LINE_ARGS()
# file = arg[0]
# 
# 
# 
# 
# data = h5_parse(file,/read_data)
# 
# window,1,xsize=400,ysize=400,/pixmap
# tvscl,congrid(data.ref_chdnb_lunar._data,400,400)
# 
# 
# 
# write_jpeg,'results/'+file_basename(file,'.h5')+'.jpg',tvrd(/true),true=1
# print
# print,'created h5 file:   ',file_basename(file)
# print,'created jpeg file: ',file_basename(file,'.h5')+'.jpg'
# print,' in results directory'
# print
# eof
# lunar_file_jpg=`ls -tr results/LUNAR*.jpg | tail -1`
# display $lunar_file_jpg&
