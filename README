# Quantum Circuits Compiler with Staging and Continuations

This repository contains an experimental quantum compiler that is implemented using staging and continuations.
The compiler takes a Toffoli-Hadamard quantum circuit as input and generates a C program, which can be further compiled and executed.
Executing the compiled C program simulates the quantum circuit
by classical computation and produces all possible final states
and their probability amplitudes.

- The repository contains an evaluator written in Scala using `shift`/`reset`,
following the [Quantum Continuation](https://andykeep.com/SchemeWorkshop2022/scheme2022-final37.pdf) paper. See [QuantumCont.scala](https://github.com/Kraks/quantum-compiler/blob/main/src/main/scala/QuantumCont.scala)
- The compiler (or, the staged evalutor) is written in CPS (instead of using control operators) and is
implemented with [Lightweight Modular
Staging](https://github.com/TiarkRompf/lms-clean).
See [QCompCPS.scala](https://github.com/Kraks/quantum-compiler/blob/main/src/main/scala/QCompCPS.scala).


## Example

To see an example of the compiler in action, run the following command
in `sbt`:

```
sbt:quantum-lms-compiler> runMain quantum.TestQC
```

This will take the circuit for [the Simon problem](https://en.wikipedia.org/wiki/Simon%27s_problem) as input and execute the generated C program `snippet.cpp`.
The C program is compiled with `g++ -std=c++20 -O3`.
Running the generated program prints all states and their probability amplitudes:

```
0.5|0000⟩
0.5|0011⟩
0.5|1100⟩
-0.5|1111⟩
```
