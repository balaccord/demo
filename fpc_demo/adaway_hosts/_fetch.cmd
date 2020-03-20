::@echo off
:: info at https://github.com/AdAway/AdAway/wiki/hostssources

call :fetch adaway_hosts.txt https://adaway.org/hosts.txt
call :fetch yoyos_hosts.txt "https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&showintro=0&mimetype=plaintext"
call :fetch ad_servers.txt https://hosts-file.net/ad_servers.txt
goto :EOF

:fetch
wget -T 15 -t 1 -O %1.new %2
if %errorlevel%==0 (
  move %1.new %1
) else (
  del %1.new
)
goto :EOF
