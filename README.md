# Alert System

### Environment Setup

install sbt
```bash
bower install sbt
```

### Run

Through command line, cd to the main folder, place the input file under folder `files`, then type in main folder
```bash
sbt run
```
It would ask to type file name for input and output seperately, 
the alert result would be written back to the output file specified previously under the `files` folder.

### Test

```bash
sbt test
```

### Assumptions
1. currency rate lines in input file are in ascending order of timestamp.
2. the output format of the timestamp is like `1.554933794023E9` instead of `1554933794.023`, 
different from input time format, since it is `Double` type in code, 
and automatically generated in former format.

