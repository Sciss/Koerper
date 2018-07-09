import scala.collection.mutable

val sync = new AnyRef
var messages = mutable.Buffer.empty[(Long, osc.Message)]

def add(m: osc.Message) = sync.synchronized(messages += System.currentTimeMillis -> m)
def size(): Int = sync.synchronized(messages.size)

val fmt = new java.text.SimpleDateFormat("'osc'yyMMdd_HHmmss'.bin'", java.util.Locale.US)

// fmt.format(new java.util.Date)

val c = osc.PacketCodec.default

def flush(): Unit = sync.synchronized {
  val name = "beta-osc.bin" // fmt.format(new java.util.Date)
  val f = file("/data/temp") / name
  try {
    val fos = new java.io.FileOutputStream(f, true)
    val dos = new java.io.DataOutputStream(fos)
    try {
      val bb = java.nio.ByteBuffer.allocate(8192)
      messages.foreach { case (time, m) =>
        bb.clear()
        c.encodeMessage(m, bb)
        bb.flip()
        dos.writeLong(time)
        val sz = bb.limit
        val arr = new Array[Byte](sz)
        bb.get(arr)
        dos.writeInt(sz)
        dos.flush()
        fos.write(arr)
      }      
    } finally {
      fos.close()
    }
    messages.clear()
    
  } catch {
    case scala.util.control.NonFatal(ex) =>
      ex.printStackTrace()
  }
}

// flush()

val oscCfg = osc.UDP.Config()
oscCfg.localSocketAddress = new java.net.InetSocketAddress("192.168.0.77", 57112)
val rcv = osc.UDP.Receiver(oscCfg)
rcv.connect()
rcv.dump(osc.Dump.Text)
rcv.dump(osc.Dump.Off)

rcv.action = { 
  case (m: osc.Message, _) => add(m)
  case _ =>
}

size()
flush()

rcv.action = (_, _) => ()
