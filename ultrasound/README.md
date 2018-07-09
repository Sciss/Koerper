# Körper - Ultrasound

Software for an art installation. (C)opyright 2018 by Hanns Holger Rutz. All rights reserved. This project is released under the
[GNU General Public License](http://github.com/Sciss/Koerper/blob/master/LICENSE) v3+ and comes with absolutely no warranties.
To contact the author, send an email to `contact at sciss.de`.

## building

Builds with sbt against Scala 2.12.

## assembly

    sbt clean test:assembly

## create workspace

    sbt clean "test:runMain de.sciss.koerper.session.SessionPi"
    
    sbt clean "test:runMain de.sciss.koerper.session.SessionMini"

Or:

    java -cp koerper-ultrasound.jar de.sciss.koerper.session.SessionPi
    
    java -cp koerper-ultrasound.jar de.sciss.koerper.session.SessionMini

## run workspaces

    java -jar koerper-ultrasound.jar --workspace /home/pi/Documents/projects/Koerper/aux/koerper-pi.mllt --start osc,eye

    java -jar koerper-ultrasound.jar --workspace /home/hhrutz/Documents/projects/Koerper/aux/koerper-mini.mllt --start osc,stop,audio,iterate,sphere,eye

## notes

- Due to macro compilation units issues, the Mellite workspace creation code `Session` is in the test sources instead of main sources.
