# UCHILE

To run the dataVisualizer you must have the following requirements met:
* Install in your LinuxOS or MacOS:
 * graphviz
 * python3-tk
 * python3
 * racket
* Create a virtual environement with
```
$ python3 -m venv venv
```
and then activate it with
```
$ source venv/bin/activate
```
* Install python modules with
```
$ pip install -r requirements
```
* Finally run the code
```
$ python dataVisualizer.py
```
## Use instructions
To run it you must first connect to Racket (actually just call the script) by pressing Start racket.
Then Connect to Racket. Write your code and PreProcess it!
Set the options for your analysis and write initial variables if you need to.
By pressing Do Analysis racket will receive the instructions and await for you to press on Next Step.
You can move forward and backwards until its finished.
When it is done an alert will pop up and you must restart the racket service to do another analysis

## TroubleShooting
If an error occurs or the app never changes to state: connected maybe the socket has not been closed. To force it do
```
$ fuser -k 9876/tcp
```
with the port which is 9876 for this app.

## Requirements

graphviz
python3-tk


## Dataflow Analysis

A Racket implementation of traditional dataflow analyses for an imperative language TIP.

### Target: the TIP Language

These analyses are targeted for S-Expression based TIP language. The syntax is largely extracted from the great lecture notes _Static Program Analysis_[1].

**An Example**

```
{while {> 5 x}
  {{if {== x 3}
       {:= x 4}
       {:= x 5}}
   {:= x {- x 1}}}}
```

### File Descriptions

* `parser.rkt` functions that parse s-exp based TIP to abstract syntax tree (AST).
* `ast.rkt` the abstract syntax tree structure definitions.
* `cfg.rkt` contrlo flow graph (CFG) structure definitions; CFG is transformed from AST.
* `dfa.rkt` chaotic iteration framework and algorithm, which operates on CFG.
* `reaching-def.rkt` reaching definition analysis.
* `very-busy.rkt` very busy expressions analysis.
* `available-expr.rkt` available expressions analysis.
* `live-var.rkt` live variables analysis.

See test cases in each files.

### TODO

* SSA-based analysis
* Pointer analysis

### References

[1] [Static Program Analysis](https://cs.au.dk/~amoeller/spa/)
