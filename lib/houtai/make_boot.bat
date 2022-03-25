set ERL_VM=E:\erlang\erl5.10.4\bin\erl -noshell

set ERL_PATH=-pa ebin application_resource_file

%ERL_VM% %ERL_PATH% -eval "systools:make_script(\"start_houtai\")" -eval "init:stop()"

::pause