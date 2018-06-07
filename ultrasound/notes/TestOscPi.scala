val t = osc.UDP.Transmitter()
val target = localhost -> 57111

t.send(osc.Message("/pd", "/data/projects/Koerper/aux/pd/pd-180603_104108.aif"), target)

/////////

// scp /data/projects/Koerper/aux/pd/pd-180603_104108.aif pi@192.168.0.27:Documents/projects/Koerper/aux/pd/

val t = osc.UDP.Transmitter()
val target = "192.168.0.27" -> 57111

t.send(osc.Message("/pd", "/home/pi/Documents/projects/Koerper/aux/pd/pd-180603_104108.aif"), target)

t.send(osc.Message("/reboot"), target)
