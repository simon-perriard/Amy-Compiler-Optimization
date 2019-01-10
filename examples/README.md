## How to run examples

It suffices to compile the examples using:

```bash

sbt
run library/dependencies examples/nameOfTheFile

```

This will generate mardown files in the corresponding folder.

In order to generate html you can just run the script (only linux):

```bash
./mdTohtml.sh

```


In particular for these two examples it suffices to run the following commands:

```bash

sbt
run  examples/commentedList.scala examples/Example3.scala library/Std.scala library/Option.scala
./mdTohtml.sh

```
