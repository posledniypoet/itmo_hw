call 3.1.bat services.txt
net stop dnscache
timeout /t 10
call 3.1.bat services_upd.txt
call 3.3.bat
net start dnscache