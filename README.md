# RISC-V processor on FPGA
This is a simple implementation of a RISC-V 32I processor using Chisel.

The project is part of 02114 Desing of a RISC-V Microprocessor course at DTU.


# How to Use
You can run the code with:
```
sbt "run "pathToFile""
```
The generated file can now be found in `generated/DataPath.v`. Use this together with `TopModule.V` in Quartus.


The project also has a Makefile to compile assembly- and c-files to RISC-V 32I. Simply run:
```
make cLinux filename="filename"
```
The file has to be in the bin directory. There also exist a `asmWin`, `cWin` and a `asmLinux` command.

# Peripherals
Currently there is only support for LEDs and seven-segment displays.

The processor outputs 32bit LED array for the LEDs and a 8x7bit array for the seven-segment display.

To access the peripherals you have to store your data at `0x4000 0000` for the LEDs and `0x4000 0020` for the seven-segment display.

Beaware that the project has been design on an Altera DE2-115 FPGA-board.
