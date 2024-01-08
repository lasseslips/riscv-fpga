module TopModule(
    input clock,
    input reset,
    output io_test,
    output io_reg12_0,
    output [31:0] io_ins
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
    .io_reg12_0(io_reg12_0),
    .io_test(io_test),
    .io_ins(io_ins)
);
endmodule
