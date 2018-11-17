#!/bin/bash

if ipconfig getIfaddr en0;then
	echo "en0 exist"
	sudo sudo /Users/alptugan/ptpd/src/ptpd2 -M -C -V -i en0
else
	echo "Check your connection"
fi
