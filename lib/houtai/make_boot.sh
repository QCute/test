#!/bin/bash

ERL_VM="erl -noshell"

ERL_PATH="-pa ebin application_resource_file"

$ERL_VM $ERL_PATH -eval "systools:make_script(\"start_houtai\")" -eval "init:stop()"
