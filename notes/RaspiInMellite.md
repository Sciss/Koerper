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
val path    = msg.arg(0).cast[String]   : Trig[String]   // t.b.d.
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


    val vol = oscNode.select("/set-volume").arg[Double](0).getOrElse(1.0)
    vol ---> "scene:volume".attr(1.0)
    
Let's assume it is solved similar to this.

- `OscIterate` (`"/iterate", ch: Int, relay: Boolean`)


    val dur = play()
    iterate(dur = dur, relay = relay)
    replyToSender()
    

    val sel     = oscNode.select("/iterate")
    val ch      = sel.arg[Int    ](0).getOrElse(0)
    val relay   = sel.arg[Boolean](1).getOrElse(false)
    
    ...
    