#!/bin/bash

while true; do
	clear
	./logs.sh | tail -n100
	sleep 60s
done
