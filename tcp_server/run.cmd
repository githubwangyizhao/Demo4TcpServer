@echo off

cd %~dp0
start werl -args_file etc/vm.args -config etc/sys.config

pause

