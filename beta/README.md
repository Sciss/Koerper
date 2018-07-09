# Körper-beta

Configure OSC on David's computer:

```bash
cd ~/src/rattle_fortran/projects/imperfect/
vim config_koerper_b.txt
```

Run:

```bash
cd ~/Documents/
./koerper_b.sh
```

Remote desktop: `vncviewer`

## building

Builds with sbt against Scala 2.12.

## assembly

    sbt clean test:assembly

## run

    java -jar koerper-beta.jar

## jack settings

attempts:

- 1024, 4 blocks, real time, soft mode (ignore XRUNs in backend)
- 2048, 3 blocks, real time, soft mode  ; scheint halbwegs zu funktionieren;
  rennt immer noch ab und an in XRUNs phase, aber "ueberlebt" diese denn
  und geht zurueck in clean mode

ideas:

- put each synth in a group to avoid 'FAILURE n_set' (make sure to free the group in onEndTx)

jack:

    /usr/bin/jackd -dalsa -r44100 -p2048 -n3 -s -D -Chw:Device,0 -Phw:Device,0
