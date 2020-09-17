#!/bin/sh

NODE=$1

if [ "" = "$NODE" ]; then
    echo "USAGE: $0 <node_number>@<IP_address>"
    exit 1
fi

COOKIE=u7OhJ1HVUz6659RpbGwJ

erl \
    -pa ebin -pa deps/*/ebin \
    -boot start_sasl \
    -config leader \
    -name $NODE \
    -setcookie $COOKIE \
    -s leader_app start