#!/bin/sh
echo "Koerper-Beta"
xset s off
xset -dpms

echo "- starting QJackCtl"
sleep 2
qjackctl &
#sleep 14
#killall qjackctl
#killall jackd
#sleep 1
#echo "- starting QJackCtl attempt2"
#qjackctl &
sleep 8

echo "- starting Koerper"
cd /home/pi/Documents/devel/Koerper/beta/
java -jar koerper-beta.jar
