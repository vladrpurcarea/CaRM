#!/bin/bash

killall "carm"
./carm carm.conf
sleep 3s
./logs.sh
