This is the coursework for the graded part of the TDT4255 course at NTNU.

Since it is the authors opinion that most tools out there are vastly underdesigned, this project comes
with a lot of added homegrown utilities, including a RISC-V parser, assembler and interpreter. 

When you test a design with a given program, that program is first parsed, then run in a software interpreter
to get correct output, then assembled into a binary.
This binary will then be loaded to your synthesized design (your processor) by the test harness provided in 
the skeleton code, along with any initial state.

Your processor will run the supplied binary, and the changes to state (memory and registers) will be recorded
and compared with the interpreter log.

If it matches, your processor works, if not, you get an execution trace, hopefully showing what went wrong and
where.

To get started, read the exercise.org file, it goes over the first pieces of the puzzle.


If you want to learn chisel on your own and use this project please send me some feedback on what you liked, 
disliked and what could have been improved :) 
If you end up using it for a course you're teaching I would be thrilled too. 
In this case, you can spend the time you're saving by sending a pull requests with some improvements!

Pull requests are more than welcome!

Nice to have list:
* More sophisticated test feedback. A detailed error report on why the processor design failed.
* Scaffolding to run synthesized designs. Preferrably targeting the PYNQ platform.
* A fix for whatever problems *you* run into when using this project.
* Either a battery of tests to find corner cases stress testing forwarders, hazard detectors etc, or even better, the tools to generate code with hazards automatically.
