set ERL_VM=erl

set ERL_PATH=-pa ebin

set ERL_NODE_NAME=-name houtai@192.168.1.99

set ERL_BOOT=-boot start_houtai

set ERL_COOKIE=-setcookie houtai

%ERL_VM% %ERL_NODE_NAME% %ERL_PATH% %ERL_COOKIE% %ERL_BOOT%

pause