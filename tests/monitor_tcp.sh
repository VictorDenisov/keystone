#!/bin/bash

sudo tcpdump -i lo -A -s 0 'tcp port 35357 and (((ip[2:2] - ((ip[0]&0xf)<<2)) - ((tcp[12]&0xf0)>>2)) != 0)'
