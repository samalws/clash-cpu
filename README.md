Instruction set specs / features:
- 32-bit registers
- 16-bit instructions
- Kernel vs. user mode
- Hardware and software interrupts
- Memory mapped IO
- Very uniform instruction set
- Almost all operations support being run conditionally
- Jumps are done via writing to PC register

Need to do:
- Virtual addressing
- Memory permissions
- Tweak MOV instructions so they can be run conditionally and don't have separate machine codes
- Maybe add floats
- Try it out on a real FPGA
