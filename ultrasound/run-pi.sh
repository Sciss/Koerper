#!/bin/sh
echo "Koerper-Ultrasound"
xset s off
xset -dpms

cd /home/pi/Documents/devel/Koerper/ultrasound/
java -jar koerper-ultrasound.jar --workspace /home/pi/Documents/projects/Koerper/aux/koerper-pi.mllt --start osc,eye

