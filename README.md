[![Build Status](https://travis-ci.org/jedeoric/telemon.svg?branch=master)](https://travis-ci.org/jedeoric/telemon)

# telemon 

## Introduction
This repo contains telemon 2.4 released in 1986 for the Oric Telestrat and a new version in master branch : telemon 3

Telemon 3 is designed to work with ORICHD. See : http://orix.oric.org

The V2.4 code is done by Fabrice Broche

The V3.0 code is done by Fabrice Broche (90%) and Jede (10%). Anyway, all minitel and FDC routines had been removed

Assembler : XA
CPU : 6502

## Timeline

### 2017-10-27

* Now XMKDIR is available

### 2017-09-24

* corrections de 2 bugs sur la lecture du fichier (l'un pour dans le cas d'une lecture d'une taille vide et donc dépassement de buffer : acceleration de la lecture) (bugs trouvés par Assinie)
* passage de certains headers (telemon.h, 6522_1.h, 6522_2.h) qui étaient dans Oric-common, dans le repo telemon.
* passage des variables telemon dans le repo telemon (avant : dans oric-common)
* nettoyage des appels au FDC (orix devrait démarrer plus vite ainsi que telemon)
* ajout d'une primitive : XVARS qui permet de récupérer certaines adresses de telemon (pour l'instant, il est possible de récupérer que l'adresse du PWD). Ceci permettra d'avoir des binaires compatibles avec différentes versions de telemon.
* gestion dans la fonction XREAD du 65C02 qui devrait légèrement accélérer la lecture quand telemon est assemblé pour le 65C02 avec un 65C02 embarqué.
* correction du bug du XCLOSE qui n'était pas implémenté à la fin du chargement de binaire, ce qui laissait ouvert /bin. Ce bug était connu car XCLOSE faisait planter ORICHD, dès que la primitive était appelée. XCLOSE ne fait plus planter ORICHD

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

Commands

* ls
* man man
* 4kkong
* meminfo
* rm
* vi 
* help
* pwd


 



Sending bytes to RS232
