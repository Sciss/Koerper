/*
 *  ConstQConfig.scala
 *  (Körper)
 *
 *  Copyright (c) 2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.koerper

import de.sciss.lucre.expr.{DoubleObj, IntObj, StringObj}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.numbers.Implicits._
import de.sciss.synth.proc.ObjKeys

object ConstQConfig {
  val KeyDbMin    = "db-min"
  val KeyDbMax    = "db-max"
  val KeyFreqMin  = "freq-min"
  val KeyFreqMax  = "freq-max"
  val KeyNumBands = "num-bands"
  val KeyFFTSize  = "fft-size"
  val KeyTimeRes  = "time-res"
  val KeyGainIn   = "gain-in"
  val KeySr       = "sample-rate"

  val ValueName   = "const-q"

  val keys: List[String] = List(
    KeyDbMin, KeyDbMax, KeyFreqMin, KeyFreqMax, KeyNumBands, KeyFFTSize, KeyTimeRes, KeyGainIn, KeySr
  )

  def mkObj[S <: Sys[S]](config: ConstQConfig)(implicit tx: S#Tx): Obj[S] = {
    val obj = StringObj.newConst("const-q")
    val a   = obj.attr
    a.put(KeyDbMin    , DoubleObj .newVar(config.dbMin    ))
    a.put(KeyDbMax    , DoubleObj .newVar(config.dbMax    ))
    a.put(KeyFreqMin  , DoubleObj .newVar(config.freqMin  ))
    a.put(KeyFreqMax  , DoubleObj .newVar(config.freqMax  ))
    a.put(KeyNumBands , IntObj    .newVar(config.numBands ))
    a.put(KeyFFTSize  , IntObj    .newVar(config.fftSize  ))
    a.put(KeyTimeRes  , DoubleObj .newVar(config.timeResMS))
    a.put(KeyGainIn   , DoubleObj .newVar(config.gainIn   ))
    a.put(KeySr       , DoubleObj .newVar(config.sr       ))

    a.put(ObjKeys.attrName, StringObj.newVar(ValueName))
    obj
  }
}
final case class ConstQConfig(
                            dbMin     : Double  = -66.0,
                            dbMax     : Double  = -18.0,
                            freqMin   : Double  = 39500.0,
                            freqMax   : Double  = 40000.0.squared / 39500.0,
                            numBands  : Int     = 384,
                            fftSize   : Int     = 8192,
                            timeResMS : Double  = 4.0,
                            gainIn    : Double  = 40.0,
                            sr        : Double  = 96000.0
                          )
