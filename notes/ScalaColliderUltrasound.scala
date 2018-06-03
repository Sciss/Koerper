val x = play {
  val osc = SinOsc.ar(40000)
  Out.ar(0, Seq.fill(5)(osc))
}

x.free()
