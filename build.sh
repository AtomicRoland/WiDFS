#!/bin/bash

beebasm -i RAMFS104.asm -v > ramfs104.lst

ts=`date`
echo Assembly completed at $ts

