#!/bin/bash

ERL_VM="erl -detached -noinput -noshell"

ERL_PATH="-pa ebin"

ERL_NODE_NAME="-name houtai@192.168.1.99"

ERL_BOOT="-boot start_houtai"

ERL_COOKIE="-setcookie houtai"

$ERL_VM $ERL_NODE_NAME $ERL_PATH $ERL_COOKIE $ERL_BOOT
