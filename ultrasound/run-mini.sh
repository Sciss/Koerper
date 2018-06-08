#!/bin/sh
echo "Koerper-Ultrasound"
# xset s off
# xset -dpms

echo "- starting QJackCtl attempt1"
sleep 2
qjackctl &
sleep 8
killall qjackctl
killall jackd
sleep 1
echo "- starting QJackCtl attempt2"
qjackctl &
sleep 8

echo "- starting Mellite"
cd /home/hhrutz/Documents/devel/Koerper/ultrasound/
java -jar koerper-ultrasound.jar --workspace /home/hhrutz/Documents/projects/Koerper/aux/koerper-mini.mllt --start osc,stop,audio,iterate