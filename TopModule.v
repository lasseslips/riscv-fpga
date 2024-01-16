module TopModule(
    input clock,
    input reset,
    //output io_test,
    //output io_reg12_0,
	output [31:0] io_ledOut,
    output [31:0] io_ins,
    output [6:0]  io_sevenSegPins_0,
    output [6:0]  io_sevenSegPins_1,
    output [6:0]  io_sevenSegPins_2,
    output [6:0]  io_sevenSegPins_3,
    output [6:0]  io_sevenSegPins_4,
    output [6:0]  io_sevenSegPins_5,
    output [6:0]  io_sevenSegPins_6,
    output [6:0]  io_sevenSegPins_7,
    output [12:0] io_lcdPins
);


wire pll_clock;

// Instantiate PLL
pll pll_inst (
    .areset(reset),  // Connect external reset
    .inclk0(clock),  // Connect external clock
    .c0(pll_clock),              // PLL output clock (not connected externally)
    .locked()           // PLL locked signal (not used in this example)
);


DataPath dataPath_mod(
    .clock(pll_clock),
    .reset(reset),
    //.io_reg12_0(io_reg12_0),
    //.io_test(io_test),
	.io_ledOut(io_ledOut),
    .io_ins(io_ins),
    .io_sevenSegPins_0(io_sevenSegPins_0),
    .io_sevenSegPins_1(io_sevenSegPins_1),
    .io_sevenSegPins_2(io_sevenSegPins_2),
    .io_sevenSegPins_3(io_sevenSegPins_3),
    .io_sevenSegPins_4(io_sevenSegPins_4),
    .io_sevenSegPins_5(io_sevenSegPins_5),
    .io_sevenSegPins_6(io_sevenSegPins_6),
    .io_sevenSegPins_7(io_sevenSegPins_7),
    .io_lcdPins(io_lcdPins)
);
endmodule
