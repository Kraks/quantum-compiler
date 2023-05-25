# Quantum Circuits Compiler with Staging

This repository contains various experimental quantum circuit evaluators and compilers.
They take a Toffoli-Hadamard quantum circuit as input, and simulates the probability amplitudes of all possible outcomes.

- The directory `src/main/scala/feynman` contains various Feynman-style simulators implemented using continuations.
  - `QContSim1.scala` contains a pure implementation and uses delimited continuations `shift`/`reset` (following the [Quantum Continuation](https://andykeep.com/SchemeWorkshop2022/scheme2022-final37.pdf) paper).
  - `QContSim2.scala` implements an evaluator written in CPS and uses side-effect to perform path summarization.
  - `QCompilerCPS.scala` is a staged CPS evaluator (written with [Lightweight Modular
  Staging](https://github.com/TiarkRompf/lms-clean)) that can generate C code
  for simulation.

- The directory `src/main/scala/schrodinger` contains various Schrodinger-style simulators implemented with linear algebra computation.
  - `Schrodinger.scala` is an unstaged implementation.
  - `StagedSchrodinger.scala` is a staged implementation that specializes over static gate matrices.

## Example

To see an example of the compiler in action, run the following command
in `sbt`:

```
sbt:quantum-lms-compiler> runMain quantum.feynman.staged.TestQC
```

This will take the circuit for [the Simon problem](https://en.wikipedia.org/wiki/Simon%27s_problem)
as input and execute the generated C program `snippet.cpp`.
The C program is compiled with `g++ -std=c++20 -O3`.
Running the generated program prints all states and their probability amplitudes:

```
0.5|0000⟩
0.5|0011⟩
0.5|1100⟩
-0.5|1111⟩
```
