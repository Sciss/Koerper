# Raspberry Pi support in Mellite

## Notes190418

We'll use [pi4j](https://www.pi4j.com/) I think. How would we integrate? With `Ex`, with `Pattern`, or a new
abstraction?

The Pi4j API allows for inputs to be callback based, so we would probably not want to use a poll based approach,
which in turn implies that we need a push based view on the inputs, which would be incompatible with patterns.
On the other hand, writing to GPIO outputs could easily be represented by a pattern specifying the GPIO state.

Therefore, if inputs are tied to `Ex` and outputs to `Pattern`, can we find a bridge between the two, so that
"GPIO programs" can be written within the same abstraction?

This is kind of the same question as OSC input and OSC output, so ideally we would solve the two together.

Obviously, Akka Stream would solve this as hybrid push/pull, _but_ ... we don't want to write a fourth system,
so that implies that we abstract the operations in `Ex`, `Pattern`, (`FScape`), ... And we have two incompatible
state models (Akka Actors with direct mutable state, and "single threaded" patterns and expressions with
transactional state).

----

Let's think about `FScape` versus the rest as audio signals versus messages in the PD world for a moment. They
also need to communicate with each other. So we have `[sig~]` and `[snapshot~]`, but more importantly most
audio objects use message inlets. This is something we perhaps do _not_ want here, as it would be a regression
for `FScape` which can handle "full rate" signals everywhere.

Do we give up the synchronous pulling of patterns? Or do we make it _optional_ somehow?
Or do we accept the difference in impedance? That patterns are much more "light weight" than FScape DSP processes?
If so, we still need to look for the bridging technology.

It's somehow useful that we can write `Vector.fill(n)(stream.next())`. Let's not forget that we use `stm.Base`,
and so we _can_ actually push a pattern "graph" into a Akka Stream graph stage logic without requiring STM.

Also let's not prematurely say that these elements need to be in the same program; they could be different snippets,
and then the task is to make it more comfortable combining and linking snippets.

Let this be an _advantage_: To assemble heterogeneous abstractions instead of reinventing the
"one abstraction to rule them all". Interestingly, this only enters as a question because there is not "global time"
in FScape, otherwise we could just say "sample the trigger at audio rate", and then the _one audio stream_ takes
the role of the one abstraction to rule them all.

----

## Givens

We have added `Ex` and `Trig` recently, they provide a simple mechanism for "interaction". For example, even if
we did not have GPIO input _values_, but just interrupts, we could define them as a `Trig`, and then all we need
need is to add a `Toggle <: Ex[Boolean]` (hypothetical) that is driven by that trigger. So this "works" already
for us.

Also a given is the _reset_ functionality in patterns, which I believe is a great achievement that helps to
formalise a number of things in the future; (and indeed, FScape UGens should follow that lead and implement a
"state reset" as well, although it is perhaps much more difficult, as we do not have the one-element atomicity).
So a "synchronous" pattern can be used, to a degree, in an asynchronous setting by using its reset function once
input data is available. And we can probably invent a buffering pattern to put in-between if we need more than
single element input (e.g. "sort the past six elements").

## Interface

It puts us back at the question if the interface between abstractions is rich enough? How do we "address" an
OSC input and output, both from the perspectives of a `Widget` / `Ex`, a `Pattern`, an `FScape` object?

I wonder if `Widget` was such a smart decision? Should it be `Control`, and thus allow other stateful objects to
be created, not necessarily with a UI component required? An OSC input node could be easily understood as a
`Control` element. On the other hand, an OSC input does not really constitute a _side effect_, and `IExpr` is already
`Disposable`, so would it not suffice to say that an OSC input is an `Ex[Message]` or `Ex[Packet]`? It is more a
question of where the object is instantiated. We don't want to have an OSC socket created over and over again,
more so if we want to guarantee a particular input port. So the question is more about life cycles of objects.
(Of course, we could cache the OSC socket, so multiple instantiations with the same port will refer to the same
socket/runner). We did centralise the audio system (there is always one); should we do the same with OSC and GPIO,
like we did with the sensors hack? We do already have `Runner.Factory#isSingleton`. Technically, it would be quite
esoteric needing more than one OSC input (per protocol), as we can filter the received messages for multiple
points of reception (?).

----

So we already had a hackish object `OscNode`, as a bidirectional `Obj` with a `send` method and in implied receiver
through an attribute taking an `Action` value. And the latter of course is the main problem, since we would like
to get rid of `Action` as far as possible.

----

## Writing Machine

Let's go back to Writing Machine and Koerper Alpha/Beta, so OSC and not GPIO. Let's assume we don't need an
`OscNode <: Obj[S]` but just an `OscNode <: Control`, i.e. within a "widget" program.

```
case class OscNode(protocol: Ex[String] = "udp", port: Ex[Int] = -1) extends Control {
  def received: Trig
  
  def message: Ex[OscMessage]   // empty message when nothing had been received
  
  // def sender: Ex[...]
}

trait OscMessage {
  def name: Ex[String]
  
  def args: Ex[Seq[Any]]
}

trait Example {
  val osc = OscNode("tcp", 12345)
  val r   = Runner("iterate")
  val it  = osc.received.filter(osc.message.name sig_== "/iterate")     // we could have a shorthand for this
  
  it ---> r.run
}

```

Should we expand `Trig` to `Trig[+A]` and `Trig[A] <: Common, Ex[A] <: Common[A]` or at least allow the same stuff
that happens in `ExOps` to work for triggers as well? As a side effect, `Trig[A]` will allow us to bridge to
`FScape` I guess. We'd have

```
trait OscNode {
  def received: Trig[OscMessage]
}
```

Here is the receive match for the RPi in Koerper alpha:

```
case osc.Message("/pd", path: String) =>
  val locBase = u.root.![ArtifactLocation]("base")
  val eye     = u.root.![de.sciss.koerper.lucre.Eye]("eye")
  val f       = new java.io.File(path)
  val art     = Artifact[S](locBase, f)
  val spec    = io.AudioFile.readSpec(f)
  val cue     = AudioCue.Obj[S](art, spec, offset = LongObj.newConst(0L), gain = DoubleObj.newConst(1.0))
  cue.name    = f.getName
  eye.attr.put("table", cue)

case osc.Message("/shutdown") =>
  tx.afterCommit {
    import sys.process._
    println("SHUTDOWN")
    Seq("sudo", "shutdown", "now").run()
  }

case osc.Message("/reboot") =>
  tx.afterCommit {
    import sys.process._
    println("REBOOT")
    Seq("sudo", "reboot", "now").run()
  }

case other =>
  println(s"Warning: Ignoring unsupported message $other")
```

The second and third cases could be handled through a `Shell <: Control` such as

```
case class Shell(cmd: Ex[String], args: Ex[Seq[String]], cwd: Ex[Option[File]]) {
  def execute: Act
}
```

The fourth case with string format and `Println`.

The interesting case is the first.

```
val tr      = ??? : Trig
val path    = msg.arg[String](0, "?")   : Trig[String]   // t.b.d.
val locBase = ArtifactLocation(":base")
val f       = File(path)                : Trig[File]
val art     = locBase.make(f)           : Trig[Artifact]
val cue     = AudioCue(art)             : Trig[AudioCue]
val eye     = Eye(":eye")   // ?
eye.attr.put("table", cue)

```

--------

Let's assume something along these lines would work. Now to the more involved case of Writing Machine:

- `OscRadioRecSet` - this might be handled directly through an auxiliary function (copying resources)
- `OscSetVolume` - it could be more "imperative", or using an arrow, such as

```
val vol = oscNode.select("/set-volume").argOpt[Double](0).getOrElse(1.0)
vol ---> "scene:volume".attr(1.0)
```
 
Let's assume it is solved similar to this.

- `OscIterate` (`"/iterate", ch: Int, relay: Boolean`)

```
val dur = play()
iterate(dur = dur, relay = relay)
replyToSender()


val sel     = oscNode.select("/iterate")
val ch      = sel.arg[Int    ](0, 0)
val relay   = sel.arg[Boolean](1, false)

...
```

With

```
def play()(implicit tx: Txn): Double = {
  import TxnLike.peer

  val ph0 = phFile()
  if (ph0.numFrames > 4800) {
    val fadeIn  = 0.1f
    import numbers.Implicits._
    val fadeOut = random.nextFloat().linLin(0, 1, 0.1f, 0.5f)
    val start   = 0L
    val stop    = ph0.numFrames
    sscenePlay(ph0, ch = channel, start = start, stop = stop, fadeIn = fadeIn, fadeOut = fadeOut)
    ph0.numFrames / SR
  } else {
    0.0
  }
}
```

which may become

```
val random = Stream[Double]("random")    // or PatAttr ?
val SR     = 48000.0

def play(): Ex[Double] = {
  val numFrames0 = "num-frames".attr(0L)  // or, well, use an AudioCue
  val ph0 = phFile()
  If (numFrames0 > 4800) Then {
    val fadeIn  = 0.1
    val fadeOut = random.next().linLin(0, 1, 0.1, 0.5)
    val start   = 0L
    val stop    = numFrames0
    scenePlay(ph0, ch = channel, start = start, stop = stop, fadeIn = fadeIn, fadeOut = fadeOut)
    numFrames0 / SR
  } Else {
    0.0
  }
}
```

So we're having this weird hybrid of `Ex` and `Trig` again; the `Trig` is actually missing, we're constructing
an expression for the duration, and we miss a trigger going into `scenePlay`? How is `scenePlay`?

```
def scenePlay(ref: AudioFileRef, ch: Int, start: Long, stop: Long, fadeIn: Float, fadeOut: Float)
             (implicit tx: Txn): Unit = {

  val targetOpt: Option[Group] = if (ch == 0) group1() else group2()
  targetOpt.foreach { target =>
    ref.acquire()
    val bus = ch // / 6
    target.freeAll()
    val path    = ref.f.path
    val buf     = Buffer.diskIn(target.server)(path = path, startFrame = start, numChannels = 2)
    val dur     = math.max(0L, stop - start) / SR
    // avoid clicking
    val fdIn1   = if (fadeIn  > 0) fadeIn  else 0.01f
    val fdOut1  = if (fadeOut > 0) fadeOut else 0.01f
    val amp     = /* textAmpLin * */ ampChan(bus)
    val syn     = Synth.play(diskGraph, nameHint = Some("disk"))(target = target, addAction = addToTail,
      args = List("bus" -> bus, "buf" -> buf.id, "dur" -> dur, "fadeIn" -> fdIn1, "fadeOut" -> fdOut1, "amp" -> amp),
      dependencies = buf :: Nil)
    syn.onEndTxn { implicit tx =>
      buf.dispose()
      ref.release()
    }
  }
}

```

The "group" is only used because we can conveniently run `freeAll`. And it's only an option because we can run
this without the server having been booted yet. So this disappears when using SP abstractions. The `ref.acquire`
and `ref.release` bit should also be directly supported by SP. `start` is not used, so we remove it here for
simplicity. `ampChan` is constant one.

The `scenePlay` thus perhaps translates to

```
def scenePlay(ref: AudioCue, ch: Ex[Int], stop: Ex[Long], 
              fadeIn: Ex[Double], fadeOut: Ex[Double]): Unit = {

  val bus     = ch
  val dur     = ((stop /* - start */) / SR).max(0)
  val fdIn1   = fadeIn .max(0.01)
  val fdOut1  = fadeOut.max(0.01)
  val run     = Runner("disk-graph")
  run.runWith("disk" -> ref, "dur" -> dur, "fadeIn" -> fdIn1, "fadeOut" -> fdOut1, "bus" -> bus)
}

```

More interesting, `iterate`.

```
def iterate(dur: Double, relay: Boolean)(implicit tx: InTxn): Future[Unit] = {
  val entryOff  = playEntryMotion.step()
  val dly       = math.max(4.0, dur * entryOff)
  val dlyMS     = (dly * 1000).toLong
  if (relay) client.scheduleTxn(dlyMS) { implicit tx =>
    client.relayIterate(thisCh = channel)
  }

  if (stateRef().isDefined) {
    // note: we don't have to "retry" anything, because the
    // relay keeps going above, independent of rendering
    val msg = "state still busy"
    log(s"playLogic() - WARNING: $msg")
    txFutureFailed(new Exception(msg))

  } else {

    val stateId   = stateCnt.getAndTransform(_ + 1)
    stateRef()    = new State(stateId)

    val futIter   = iterateImpl(stateId)

    val timeoutMS = 150000L
    val taskTimeout = client.scheduleTxn(timeoutMS) { implicit tx =>
      if (stateRef().id == stateId) {
        log(s"playLogic() - WARNING: timeout $stateId")
        stateRef() = NoState
        cancelRendering()
      }
    }

    futIter.onComplete(_ => atomic { implicit tx =>
      if (stateRef().id == stateId) {
        stateRef() = NoState
        taskTimeout.cancel()
      }
    })

    futIter
  }
}
```

The stuff represented by `Task` (cancellable trigger) is probably straight forwarded to implement, 
as is delaying triggers. `stateCnt` would be another stream, and we can "latch" value using the OSC message
trigger.

```
def iterateImpl(iterId: Int)(implicit tx: InTxn): Future[Unit] = {
  log(s"iterate($iterId)")

  for {
    _     <- dbFill()
    instr <- atomic { implicit tx => phSelectOverwrite  ()            }
    mat   <- atomic { implicit tx => dbFindMatch        (instr)       }
    _     <- atomic { implicit tx => performOverwrite   (instr, mat)  }
  } yield {

    log("iterate() - done")
    ()
  }
}
```

with `dbFill` for example consisting of a `queryRadioRec` followed by an FScape rendering. (Threading futures)

-----------

## Matching

I propose something like the following to "match" arguments:


```
val ch    = Var[Int]()
val relay = Var[Boolean]()
val sel   = n.message.select("/iterate", ch, relay)
```

So that arguments to `select` (should it be `route`?) can be one of the following

- `Var[A]` - most specific; open argument that captures a particular type
- `Ex[A]` - determines a constant that must be found here

It could be extended later to include `Rest` for var-args (if needed) and so on.

They would each associate `A` with a type class for OSC atom decoding? Difficult, as we can only define
select as

    def select(name: VarOrEx[String], args: VarOrEx[Any]): Trig

unless we overload select for all arities (up to 22 I guess)...

    def select(name: VarOrEx[String]): Trig
    def select[T1](name: VarOrEx[String], arg1: VarOrEx[T1]): Trig
    def select[T1, T2](name: VarOrEx[String], arg1: VarOrEx[T1], args2: VarOrEx[T2]): Trig

etc. But the particular implementation could be graceful (e.g. `Var[Double]` could accept float input). We might
even introduce a `Var.Any` that could be routed at a later point.

------------

## Notes190424

Let's assume all the OscNode is solved (we will be able to implement some sort of argument matching), to return to
the use of persistent pattern streams, and to the koerper-alpha actions. For the latter, we have auxiliary objects
`RecordAudioChunk` and `RenderProbabilityDistribution`. The former has a prepare and a done action. Prepare:

```
def mkPrepareAction[S <: Sys[S]]()(implicit tx: S#Tx): proc.Action[S] = {
  proc.Action.apply[S] { u =>
    println(s"[${new java.util.Date}] Körper: iteration begin.")
    val locBase   = u.root.![ArtifactLocation]("base")
    val ens       = u.root.![Ensemble]("rec-audio-chunk")
    val countVal  = u.root.![IntObj]("iterations").value  // only used for time out
    ens.stop()
    val pRec      = ens.![Proc]("proc")
    // N.B. we have a race condition when using AIFF: the done action may see
    // the file before the header is flushed, thus reading numFrames == 0.
    // If we use IRCAM, the header does not carry numFrames information.
    val fmt       = new java.text.SimpleDateFormat("'us-'yyMMdd_HHmmss'.irc'", java.util.Locale.US)
    val name      = fmt.format(new java.util.Date)
    val child     = new java.io.File("us", name).getPath
    val art       = Artifact(locBase, Artifact.Child(child))
    pRec.attr.put("out", art)
    ens.play()

    tx.afterCommit { ... }  // detect time out
  }
}
```

With `Runner` we don't actually need the ensemble any longer, we can use the proc directly. Again we a way to
_make_ a new object (an `Artifact`). And a way to put an object into an attribute map (so let's focus on that
and forget about a `Runner#playWith(e: Event)` kind of thing which looks easy but is hard because of the
mess that aural objects are).

The done action of `RecordAudioChunk`:

```
def mkDoneAction[S <: Sys[S]]()(implicit tx: S#Tx): Obj[S] = {
  proc.Action.apply[S] { u =>
    import de.sciss.fscape.lucre.FScape
    import de.sciss.fscape.stream.Control
    import de.sciss.synth.proc.GenContext

    println(s"[${new java.util.Date}] Körper: recording stopped.")

    // store the chunk in the 'us' folder
    val ens       = u.root.![Ensemble]("rec-audio-chunk")
    ens.stop()
    val pRec      = ens.![Proc]("proc")
    val artRec    = pRec.attr.![Artifact]("out")
    val artRecVal = artRec.value
    val specRec   = de.sciss.synth.io.AudioFile.readSpec(artRecVal)
    val cueRec    = AudioCue.Obj(artRec, specRec, offset = 0L, gain = 1.0)
    cueRec.name   = artRecVal.getName

    // invoke pd rendering
    val fsc       = u.root.![FScape]("render-pd")
    val aFsc      = fsc.attr
    aFsc.put("audio-in", cueRec)
    val locBase   = u.root.![ArtifactLocation]("base")
    val fmtTab    = new java.text.SimpleDateFormat("'pd-'yyMMdd_HHmmss'.aif'", java.util.Locale.US)
    val nameTab   = fmtTab.format(new java.util.Date)
    val childTab  = new java.io.File("pd", nameTab).getPath
    val artTab    = Artifact(locBase, Artifact.Child(childTab))
    aFsc.put("table-out", artTab)
    // XXX TODO: "calib-in"

    // XXX TODO --- too much ceremony
    val cfgFsc    = Control.Config()
    import u.{cursor, workspace}
    implicit val gtx: GenContext[S] = GenContext[S]
    /* val r = */ FScape.Rendering(fsc, cfgFsc)

    println(s"[${new java.util.Date}] Körper: FScape rendering started.")
  }
}
```

The annoyance of "recovering" objects in action B that were used in action A will be avoid if we write this
in a single program. The recording proc uses a `Line` and `DoneAction` to "terminate itself"; obviously we can
just use a `Delay` in the control program now.

We now need a way to _make_ and `AudioCue`, and again _put attributes_ (even just the name).
Then we have another runner for FScape, whose graph is defined in `RenderProbabilityDistribution`.
It has again a done action, with the old version and no Raspi being `mkDoneActionNoPi`, and the new one with
dedicated second computer being `mkDoneActionPi`. In the Pi case, we run a shell script for `scp`, copying the
file via network, then confirm by sending an OSC message. If using one computer, there is an `Eye` instance and
we _put an attribute_ for the new probability distribution.

----------

As long as we don't need multiple views of the same model at the same time, we're good just putting values in
attribute maps; otherwise, we need a notion of `playWith(events)` (polyphony). This we already have in place
via `Ex#--->(Attr.Like)`.

## Notes190426

_Case study:_ receive file path via OSC, and consequently import an audio file by that path. Play that file and
ignore messages arriving while the file is being played



