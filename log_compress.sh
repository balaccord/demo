#!/bin/sh

# Logs compression utility
# The destination format is 7z, the compression algorithm is FLZMA2 (Fast LZMA)
# For this we use 7-Zip ZSTD: https://github.com/mcmilk/7-Zip-zstd

srcdir=../logs
dstdir=.
runflag=$dstdir/.compressing

mask="$srcdir/*.log.2020-[0-9][0-9]-[0-9][0-9]*"
ext="7z"
arc="/c/Program Files/7-Zip-Zstandard/7z.exe"
arc_opts="-t7z -mmt12 -mqs -m0=FLZMA2 -ms=on -mx=9 -mfb=273 -md=256m -mlc=4"

# check if filelist is empty
for log in $mask; do
  test -e "$log" || exit
  break
done

if [ -f $runflag ]; then
  echo
  echo "Another compress session is running. Press any key to exit"
  read -n1
  exit
fi
touch $runflag

format_time() {
  t=$1
  sec=$(( t % 60 ))
  min=$(( (t-sec) / 60 ))
  printf "$min:%02d" $sec
}

size=`stat -c "%s" $mask | awk '{s+= $1}END{print s}'`
count=`ls -f1 $mask | wc -l`
printf "Files: $count, size: %'d bytes\r\n" $size

curno=0
accumsize=0
# UNIX time (sec)
t1=`date +%s`
for log in $mask; do
  # check archive extension
  test "${log##*.}" = "$ext" && continue
  date=`perl -e "'$log'"' =~ /\.log\.(\d{4}-\d{2}-\d{2})/; print $1'`
  basename=`perl -e "'$log'"' =~ m!^(?:.+[/\\\\])?([^/\\\\]+)\.log\.!; print $1'`
  if test -d $date || mkdir $date; then
    true
  else
    echo Error creation directory '"'$date'"'
    exit
  fi
  cursize=`stat -c "%s" $log`
  curno=$((curno+1))
  printf "`date +'%F %T'` File $curno of $count, %'d bytes: " $cursize
  zipped="$dstdir"/$date/$basename-$date.$ext
  echo $log
  if nice -20 "$arc" a $arc_opts "$zipped" "$log" >/dev/null; then
    rm "$log"
  else
    test -f "$zipped.tmp" && rm "$zipped.tmp"
    exit
  fi
  t=$((`date +%s`-t1))
  accumsize=$((accumsize+cursize))
  estimated=$((t * size / accumsize))
  printf "`date +'%F %T'` Total: `format_time $t`, %d%%, estimated: `format_time $estimated`\r\n" $((100 * accumsize / size))
done

rm $runflag
