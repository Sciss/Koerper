val c = osc.UDP.Config()
c.localSocketAddress = "192.168.0.77" -> 7771
c.codec = osc.PacketCodec().doubles()
val r = osc.UDP.Receiver(c)
r.connect()
r.dump()
r.isConnected
r.isOpen
