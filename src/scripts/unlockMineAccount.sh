#!/bin/bash

server_and_port=$1 # http://127.0.0.1:8546
address=$2 # 0xedde8656c35fcb7126c61fc6e2673734425a72bf
password=$3 # 1234

echo $server_and_port $address $password

curl -X POST \
  "$server_and_port"/ \
  -H 'cache-control: no-cache' \
  -H 'content-type: application/json' \
  -d '{ "jsonrpc": "2.0", "method": "personal_unlockMinerAccount", "params": ["'$address'", "'$password'"], "id": 1}'