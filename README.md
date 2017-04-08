# telemon 

## Introduction
This repo contains telemon 2.4 released in 1986 for the Oric Telestrat

The code is done by Fabrice Broche

Assembler : XA
CPU : 6502

## Timeline

### 2017-03-17
* fread/fopen/fclose are working
* fwrite is working at 50%
* build info added (date)

### 2016-10-09
* Working on 3.0 version : adding fread/fopen from usb key
* some minitel routines are removed
* some 65C02 code

### 2016-07-02
2.4 version is finished. Some labels are missing (Page 0, page 2 etc has still the true address). But it's working.
Hash of generated file of this version : 9a432244d9ee4a49e8ddcde64af94e05

### 2016-06-30
2.4 version is almost disassembled. Some labels are missing (Page 0, page 2 etc has still the true address)

##How to build a version ?

###For 2.4
* clone this repo with 2.4 branch
* edit build.bat and change "OSDKB" value to the xa.exe (6502 assembler) path, and "ORICUTRON" value to your own oricutron path if you want to copy the version in oricutron ROM. You have to change the path of the ROM file in oricutron.cfg
 

## Features

### 2.5 version : what's new ?
 * all minitel routines removed
 * all minitel keyboard shortcuts removed (used in minitel mode)
 * all minitel primitives related removed

### 2.4 version
Read books released in 1986 on the telestrat :) !


 

Goal : trying to change some features