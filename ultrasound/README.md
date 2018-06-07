# Körper - Ultrasound

Software for an art installation. (C)opyright 2018 by Hanns Holger Rutz. All rights reserved. This project is released under the
[GNU General Public License](http://github.com/Sciss/Koerper/blob/master/LICENSE) v3+ and comes with absolutely no warranties.
To contact the author, send an email to `contact at sciss.de`.

## building

Builds with sbt against Scala 2.12.

## sessions

    sbt clean "test:runMain de.sciss.koerper.session.SessionPi"

## notes

- Due to macro compilation units issues, the Mellite workspace creation code `Session` is in the test sources instead of main sources.
