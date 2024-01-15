
asmWin:
	riscv64-unknown-elf-gcc-8.3.0.exe -nostartfiles -nostdlib -march=rv32i -mabi=ilp32 -T linker.ld bin/${filename}.s -o bin/${filename}.out
	riscv64-unknown-elf-objcopy.exe -O binary bin/${filename}.out bin/${filename}.bin
	rm bin/${filename}.out

cWin:
	riscv64-unknown-elf-gcc-8.3.0.exe -nostartfiles -nostdlib -march=rv32i -mabi=ilp32 -O1 -T linker.ld bin/${filename}.c -o bin/${filename}.out
	riscv64-unknown-elf-objcopy.exe -O binary bin/${filename}.out bin/${filename}.bin

asmLinux:
	riscv64-linux-gnu-gcc -nostartfiles -nostdlib -march=rv32i -mabi=ilp32 -T linker.ld bin/${filename}.s -o bin/${filename}.out
	riscv64-linux-gnu-objcopy -O binary bin/${filename}.out bin/${filename}.bin
	rm bin/${filename}.out

cLinux:
	riscv64-linux-gnu-gcc -nostartfiles -nostdlib -march=rv32i -mabi=ilp32 -O1 -T linker.ld bin/${filename}.c -o bin/${filename}.out
	riscv64-linux-gnu-objcopy -O binary bin/${filename}.out bin/${filename}.bin
	rm bin/${filename}.out
