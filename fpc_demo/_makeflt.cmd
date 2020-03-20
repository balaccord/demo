@echo off

:: make domain name filter from the source "informer_filter.src.txt"
:: comment is the ";" symbol with the optional whitespaces before it at the line beginning
:: empty lines are ignored
:: sort order is the domain levels 2,1 then Nth,...,4,3

:: also uses the AdAway domain black lists in the "adaway_hosts" folder, update them with _fetch.cmd before use

path D:\Program Files (x86)\UnixUtils

(
type adaway_hosts\*.txt 2>nul | egrep "^^127\.0\.0\.1" | egrep -v localhost | gawk "{print $2}"
type informer_filter.src.txt 2>nul | sed -r -e "s/^^[\x20\t]*;.*//g" -e "s/^^[\x20\t]+//g" -e "s/[\x20\t]+$//g" | egrep -v "^^$"
) |^
gawk -F. "{for(i=NF-1;i<=NF;i++)printf ""%%s%%s"""",$i,OFS;for(i=NF-2;i>0;--i)printf ""%%s%%s"""",$i,OFS;printf ORS}" |^
sort -u |^
gawk "{for(i=NF;i>2;--i)printf ""%%s%%s"""",$i,OFS;for(i=1;i<3;++i)printf ""%%s%%s"""",$i,OFS;printf ""%%s"""",ORS}" |^
sed -r -e "s/[\x20\t]+$//g" -e "s/[\x20\t]/./g" > Debug32\informer_filter.txt
D:\lazarus\tools\lazres.exe informer_filter.lrs Debug32\informer_filter.txt=informer_filter
