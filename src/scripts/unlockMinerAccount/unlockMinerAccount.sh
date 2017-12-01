#!/bin/bash

#
# TODO:
#   - Check command-line args
#

endpoint=$1 # example: http://127.0.0.1:8546
address=$2 # example: edde8656c35fcb7126c61fc6e2673734425a72bf

function jsonval {
    tmp=$(
        echo $1 |
            sed 's/\\\\\//\//g' |
            sed 's/[{}]//g' |
            awk -v k="text" '{n=split($0,a,","); for (i=1; i<=n; i++) print a[i]}' |
            sed 's/\"\:\"/\|/g' |
            sed 's/[\,]/ /g' |
            sed 's/\"//g' |
            grep -w $2 |
            tr -d '[:space:]' |
            cut -d: -f2)
    echo $tmp
}

read -s -p "Enter passphrase: " passphrase

echo ""
echo "Unlocking account $address using RPC endpoint $endpoint"

response=$(curl -s -X POST \
  "$endpoint"/ \
  -H 'cache-control: no-cache' \
  -H 'content-type: application/json' \
  -d '{ "jsonrpc": "2.0", "method": "personal_unlockMinerAccount", "params": ["'$address'", "'$passphrase'", 0], "id": 1 }')

maybeError=$(jsonval "$response" error)
if [[ ! -z "$maybeError" ]]; then
    >&2 echo "Could not unlock account [$maybeError]"
    exit -1
else
    result=$(jsonval "$response" result)

    if [ "$result" = "true" ]; then
        echo "Account successfully unlocked"
    else
        >&2 echo "Could not unlock account [result=$result]"
        exit -1
    fi
fi

exit 0
