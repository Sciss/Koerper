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
