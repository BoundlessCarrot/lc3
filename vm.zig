// This VM will simulate the LC-3, an educational computer architecture commonly used to teach university students computer architecture and assembly.
// It has a simplified instruction set compared to x86, but demonstrates the main ideas used by modern CPUs.
// [LC-3 ISA](https://en.wikipedia.org/wiki/Little_Computer_3)
// The LC-3 has the following features:
//  16 bit word size, 4 bit opcode, 12 bit param space
//  [ 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 ]
//   |OPCODE| |      PARAMETERS     |
//  16 opcodes
//  8 general purpose registers, 1 program counter, 1 condition flag register
//  3 condition flags: P, Z, N

const std = @import("std");
const eql = std.mem.eql;

// We use a simple array to represent memory
const MEMORY_MAX = std.math.maxInt(u16);
const memory: [MEMORY_MAX]u16 = undefined;

// Registers
//  A register is a slot for storing a single value on the CPU
//  If we want to ork with a piece of data, we need to load it into a register first
//  A small number of registers --> only a minimal number of values can be stored at once
//  We can get around this by:
//      1. Loading a value from memory into a register
//      2. Calculating a value into another register
//      3. Storing the final result back into memory
//  The GP registers can be used to perform any prograam calulations
//  The Program Counter is the address of the next instruction in memory to execute
//  The Condition Flags tell us information about the previous calculation
const Registers = enum(u16) { R0 = 0, R1 = 0, R2 = 0, R3 = 0, R4 = 0, R5 = 0, R6 = 0, R7 = 0, R_PC = 0, R_COND = 0, R_COUNT = 10 };

const registerStorage: [Registers.R_COUNT]u16 = undefined;

// Instruction Set
//  An *instruction* is a command that tells the CPU to do some fundamental task
//  Instructions have both an _opcode_ (task to perform) and _parameters_ (inputs on the task being performed)
//  e.g. for the task `2 + 3`:
//      - the task is addition/+ (and the opcode is whatever the opcode is for addition)
//      - the parameters are 2 and 3
// Each opcode is one task the CPU knows how to do
const Opcodes = enum(u16) {
    OP_BR, // BRANCH
    OP_ADD, // ADD
    OP_LD, // LOAD
    OP_ST, // STORE
    OP_JSR, // JUMP REGISTER
    OP_AND, // BITWISE AND
    OP_LDR, // LOAD REGISTER
    OP_STR, // STORE REGSITER
    OP_RTI, // UNUSED
    OP_NOT, // BITWISE NOT
    OP_LDI, // LOAD INDIRECT
    OP_STI, // STORE INDIRECT
    OP_JMP, // JUMP
    OP_RES, // RESERVED (UNUSED)
    OP_LEA, // LOAD EFFECTIVE ADDRESS
    OP_TRAP, // EXECUTE TRAP
};

// Condition flags
//  Condition flags provide information on the most recently executed calculation
//  Also allows us to check logical conditions
//  Each flag indicates the the sign of the previous calculation
const conditionFlags = enum(u16) {
    FL_POS = (1 << 0), // (P)ositive
    FL_ZRO = (1 << 1), // (Z)ero
    FL_NEG = (1 << 2), // (N)egative
};

// A note on Assembly:
//  At the base of our VM we want to transform assembly (very low level human readable code) into 16-bit binary instructions the vm can understand
//  This transformer is called an *assembler*, and the resultant instructions are called *machine code*
//  NOTE: An assembler and a compiler are not the same thing!
//  An assembler simply encodes what the programmer has written in text into binary, replacing symbols with their binary representation and packing them into instructions.

// Main loop
//  The main loop of our VM is quite simple:
//      1. Load an instruction from memory at the address in the PC register
//      2. Increment the PC register
//      3. Look at the opcode to detemine which type of instruction it should perform
//      4. Perform the instruction using the params in the instruction
//      Repeat!

fn handleCLArgs(args: *std.process.ArgIterator, allocator: std.mem.Allocator) !void {
    while (args.next()) |arg| {
        switch (arg) {
            eql(u8, arg, "-h"), eql(u8, arg, "--help") => {
                const help =
                    \\Usage: vm [options]
                    \\ Options:
                    \\     -h, --help  Display this help message
                    \\     -i, --image  Load a program image
                ;

                std.debug.print(help);
                return std.os.exit(0);
            },
            eql(u8, arg, "-i"), eql(u8, arg, "--image") => {
                if (!args.next()) {
                    const help =
                        \\Usage: vm [options]
                        \\ Options:
                        \\     -h, --help  Display this help message
                        \\     -i, --image  Load a program image
                    ;
                    std.debug.print(help);
                    return std.os.exit(1);
                }
                const image = args.next().?;
                const file = try std.fs.cwd().openFile(image, .{});
                defer file.close();

                const fileSize = (try file.stat()).size;

                if (fileSize > MEMORY_MAX) {
                    std.log.err("Error: Program is too large to fit in memory", .{});
                    return std.os.exit(1);
                }

                const buffer = try file.readToEndAlloc(allocator, fileSize);
                std.mem.copyForwards(u8, &memory, buffer, fileSize);
            },
            else => {
                const help =
                    \\Usage: vm [options]
                    \\ Options:
                    \\     -h, --help  Display this help message
                    \\     -i, --image  Load a program image
                ;

                std.debug.print(help);
                return std.os.exit(1);
            },
        }
    }
}

// NOTE: Realistically this could just be a stdlib function
fn signExtend(x: u16, bitCount: u32) u16 {
    if ((x >> (bitCount - 1)) & 1) {
        x |= 0xFFFF << bitCount;
    }
    return x;
}

fn updateFlags(r: u16) void {
    if (registerStorage[r] == 0) {
        registerStorage[Registers.R_COND] = conditionFlags.FL_ZRO;
    } else if (registerStorage[r] >> 15) {
        registerStorage[Registers.R_COND] = conditionFlags.FL_NEG;
    } else {
        registerStorage[Registers.R_COND] = conditionFlags.FL_POS;
    }
}

fn memRead(addr: u16) u16 {
    return memory[addr];
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAIL");
    }

    var args = try std.process.argsWithAllocator(gpa.allocator());
    defer args.deinit();

    _ = args.skip();

    try handleCLArgs(&args, gpa.allocator());

    // We need to have 1 condition flag set at any given time, so we set the Z flag
    registerStorage[Registers.R_COND] = conditionFlags.FL_ZRO;

    // Set the PC to the starting position
    const PC_START = 0x3000;
    registerStorage[Registers.R_PC] = PC_START;

    while (true) {
        // fetch instruction
        Registers.R_PC += 1;
        const instruct: u16 = registerStorage[Registers.R_PC];
        const op: Opcodes = @enumFromInt(instruct >> 12);

        // Switch on the opcode
        switch (op) {
            .OP_ADD => {
                // ADD
                //  ADD takes two values, adds them, and stores them in a register
                //  The encoding for ADD instructions generally goes (from left to right):
                //      - opcode value (4 bits, `0001` for ADD)
                //      - destination register (3 bits)
                //      - SR1, or the first number to add (3 bits)
                //      - mode flag (1 bit)
                //      After this it changes...
                //  The ADD instruction has two different modes that can alter the encoding of the instruction:
                //      - _Register mode_: The second number is stored in a register, just like the first
                //          - Uses 3 bits (bits 0-2, bits 3-4 are unused)
                //          - In assembly this would look like this: `ADD R2 R0 R1 ; add the contents of R0 to R1 and store in R2.`
                //      - _Immediate mode_: The second number is stored directly in the instruction
                //          - Uses 5 bits (bits 0-4)
                //          - This removes the need to load the value from memory, but also limits the size of the second value (max 2^5 = 32)
                //          - Due to this limitation, immediate mode is primarily useful for incrementing instructions
                //          - In assembly this would look like this: `ADD R2 R0 R1 ; add the contents of R0 to R1 and store in R2.`
                //          - In addition, to add a 5-bit number to a 16-bit one, we need to extend the 5-bit number to 16-bits. This process is called *sign-extending*
                //              - For positive numbers we can fill in 0s, for negative numbers we can fill in 1s
                //  Finally, regardless of mode, we have to update the condition flags of a value to indicate its sign ANY time we write to a register

                // Get destination register
                const r0: u16 = (instruct >> 9) & 0x7;

                // Get first param
                const r1: u16 = (instruct >> 6) & 0x7;

                // Check for immediate mode
                const immFlag: u16 = (instruct >> 5) & 0x1;

                if (immFlag) {
                    const imm5: u16 = signExtend(instruct & 0x1F, 5);
                    registerStorage[r0] = registerStorage[r1] + imm5;
                } else {
                    const r2: u16 = instruct & 0x7;
                    registerStorage[r0] = registerStorage[r1] + registerStorage[r2];
                }

                updateFlags(r0);
            },
            .OP_AND => {
                // AND
                //  AND takes two values and does a bitwise calculation, where if the bits of the param match a resultant 1 is produced, otherwise a 0
                //  The resulting complete value is then stored in the destination register
                //  AND uses immediate and register modes like ADD, and has the exact same encoding

                // Get destination register
                const r0: u16 = (instruct >> 9) & 0x7;

                // Get first param
                const r1: u16 = (instruct >> 6) & 0x7;

                // Check for immediate mode
                const immFlag: u16 = (instruct >> 5) & 0x1;

                if (immFlag) {
                    const imm5: u16 = signExtend(instruct & 0x1F, 5);
                    registerStorage[r0] = registerStorage[r1] & imm5;
                } else {
                    const r2: u16 = instruct & 0x7;
                    registerStorage[r0] = registerStorage[r1] & registerStorage[r2];
                }

                updateFlags(r0);
            },
            .OP_NOT => {
                // NOT
                //  NOT returns the bitwise complement of the param given
                //  Encoding:
                //      - Opcode (4 bits, should be `1001`)
                //      - Destination register (3 bits)
                //      - SR1, the register of our only param (3 bits)
                //      - The bits [5:0] are filled with 1s and are unused

                // Get the destination register
                const r0: u16 = (instruct >> 9) & 0x7;

                // Get param register to flip
                const r1: u16 = (instruct >> 6) & 0x7;

                registerStorage[r0] = ~registerStorage[r1];

                updateFlags(r0);
            },
            .OP_BR => {
                // CONDITIONAL BRANCH
                //  If the condition flag in the flag register matches the condition flag in the encoding, jump to the address (PC + offset) and execute that instruction

                // Get program counter offset
                const pcOffset: u16 = signExtend(instruct & 0x1FF, 9);

                // Get condition flag
                const condFlag: u16 = (instruct >> 9) & 0x7;

                if (condFlag & registerStorage[Registers.R_COND]) {
                    registerStorage[Registers.R_PC] += pcOffset;
                }
            },
            .OP_JMP => {
                // JUMP

                // Get destination register
                const r1: u16 = (instruct >> 6) & 0x7;

                registerStorage[Registers.R_PC] = registerStorage[r1];
            },
            .OP_JSR => {
                // JUMP REGISTER
                //  Jumps to a location in memory
                //  Encoding:
                //      - opcode (4 bits, should be `0100`)
                //      - long flag (1 bit)
                //      - PCoffset 11 (11 bits)

                // Check whether we're in long or short mode
                const longFlag = (instruct >> 11) & 1;

                // Save the address of the call routine in register 7
                registerStorage[Registers.R7] = registerStorage[Registers.R_PC];

                if (longFlag) {
                    // JSR case
                    const longPcOffset: u16 = signExtend(instruct & 0x1FF, 11);
                    registerStorage[Registers.R_PC] += longPcOffset;
                } else {
                    // JSRR case
                    const r1: u16 = (instruct >> 6) & 0x7;
                    registerStorage[Registers.R_PC] += registerStorage[r1];
                }
            },
            .OP_LD => {
                // LOAD
                //  Loads a value from a location in memory into a register
                //  Encoding:
                //      - opcode (4 bits, should be `0010`)
                //      - destination register (3 bits)
                //      - PCoffset --> An offset that gets added to the PC register, which points to an address

                // Get destination register
                const r0: u16 = (instruct >> 9) & 0x7;

                // Get PC Offset
                const pcOffset: u16 = signExtend(instruct & 0x1FF, 9);

                registerStorage[r0] = memRead(registerStorage[Registers.R_PC] + pcOffset);

                updateFlags(r0);
            },
            .OP_LDI => {
                // LOAD INDIRECT
                //  Loads a value from a location in memory into a register
                //  Encoding:
                //      - opcode (4 bits, should be `0101`)
                //      - destination register (3 bits)
                //      - PCoffset --> An offset that gets added to the PC register, which points to an address, and that address contains the address which should get loaded into memory
                //  We also need to sign-extend the PCoffset to make it a valid memory address
                //  This may seem roundabout, considering the LD instruction exists, but it is incredibly useful
                //  LD is limited to offsets that are 9 bits, but our memory needs 16 bit addresses
                //  LDI is great for loading data that's stored far away from the current PC, but to use it the address of the final location needs to be stored in a neighborhood nearby
                //  It's similar in concept to having a local variable that's a pointer to some address

                // Get destination register
                const r0: u16 = (instruct >> 9) & 0x7;

                // Get PCoffset 9
                const pcOffset: u16 = signExtend(instruct & 0x1FF, 9);

                // Add PCoffset to the current PC, look into that mem location to get the final address
                registerStorage[r0] = memRead(memRead(registerStorage[Registers.R_PC] + pcOffset));

                // Update flags
                updateFlags(r0);
            },
            .OP_LDR => {
                // LOAD REGISTER
                //  Loads a value from a location in memory into a register
                //  Encoding:
                //      - opcode (4 bits, should be `0110`)
                //      - destination register (3 bits)
                //      - base register (3 bits)
                //      - offset (6 bits)
                //  The offset is added to the base register to get the final address to load from
                //  This is useful for loading data that's stored in a neighborhood of the base register

                // Get destination register
                const r0: u16 = (instruct >> 9) & 0x7;

                // Get base register
                const r1: u16 = (instruct >> 6) & 0x7;

                // Get offset
                const offset: u16 = signExtend(instruct & 0x3F, 6);

                registerStorage[r0] = memRead(registerStorage[r1] + offset);

                // Update flags
                updateFlags(r0);
            },
            .OP_LEA => {},
            .OP_ST => {},
            .OP_STI => {},
            .OP_STR => {},
            .OP_TRAP => {},
            .OP_RES => {},
            .OP_RTI => {},
            else => {
                // bad opcode
                break;
            },
        }
    }
}
