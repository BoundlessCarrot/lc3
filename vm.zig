// Intro
//  This VM will simulate the LC-3, an educational computer architecture commonly used to teach university students computer architecture and assembly.
//  It has a simplified instruction set compared to x86, but demonstrates the main ideas used by modern CPUs.
//  [LC-3 ISA](https://en.wikipedia.org/wiki/Little_Computer_3)
//  The LC-3 has the following features:
//      - 16 bit word size, 4 bit opcode, 12 bit param space
//      - [ 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 ]
//         |OPCODE| |      PARAMETERS     |
//      - 16 opcodes
//      - 8 general purpose registers, 1 program counter, 1 condition flag register
//      - 3 condition flags: P, Z, N

const std = @import("std");
const eql = std.mem.eql;

extern fn disable_input_buffering() c_int;
extern fn restore_input_buffering() void;
extern fn check_key() bool;

// Register int shortcuts
const R_COUNT = @intFromEnum(Registers.R_COUNT);
const R_PC = @intFromEnum(Registers.R_PC);
const R_COND = @intFromEnum(Registers.R_COND);
const R0 = @intFromEnum(Registers.R0);
const R1 = @intFromEnum(Registers.R1);
const R2 = @intFromEnum(Registers.R2);
const R3 = @intFromEnum(Registers.R3);
const R4 = @intFromEnum(Registers.R4);
const R5 = @intFromEnum(Registers.R5);
const R6 = @intFromEnum(Registers.R6);
const R7 = @intFromEnum(Registers.R7);

const KBSR = @intFromEnum(MemoryMappedRegisters.KBSR);
const KBDR = @intFromEnum(MemoryMappedRegisters.KBDR);

// We use a simple array to represent memory
const MEMORY_MAX = std.math.maxInt(u16);
var memory: [MEMORY_MAX]u16 = undefined;

// Registers
//  A register is a slot for storing a single value on the CPU
//  If we want to work with a piece of data, we need to load it into a register first
//  A small number of registers --> only a minimal number of values can be stored at once
//  We can get around this by:
//      1. Loading a value from memory into a register
//      2. Calculating a value into another register
//      3. Storing the final result back into memory
//  The GP registers can be used to perform any program calulations
//  The Program Counter is the address of the next instruction in memory to execute
//  The Condition Flags tell us information about the previous calculation
const Registers = enum(u16) { R0 = 0, R1, R2, R3, R4, R5, R6, R7, R_PC, R_COND, R_COUNT = 10 };

var registerStorage: [R_COUNT]u16 = undefined;

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

// ---
// That's all for hardware mocking!
// ---

// Traps
//  *Trap routines* are routines for getting input from the keyboard and displaying strings to the console
//  You can almost imagine these as an API for  the LC-3
//  Each trap routine is assigned a _trap code_ which operates like an opcode
//  When a trap routine is executed, we move the program counter to the routine's address and execute the procedure's instructions, then move the PC back to where it was before the routine
//  Trap routines technically don't add any new functionality to our system, they just provide a convenient way to perform tasks
const TrapCodes = enum(u16) {
    GETC = 0x20, // Get char from keyboard, not echoed onto terminal
    OUT = 0x21, // Output a char
    PUTS = 0x22, // Output a word string
    IN = 0x23, // Get char from keyboard, echoed onto terminal
    PUTSP = 0x24, // Output a byte string
    HALT = 0x25, // Halt the program
};

// Memory Mapped Registers
//  These are special registers that are inaccessible from the normal register table
//  Instead we reserve a special address for them in memory and read and rite only to their memory location
//  We mostly use these to interact with special hardware devices (such as a keyboard)
//  LC-3 has 2 MMRs we need to implement:
//      - `KBSR`: Keyboard status register, indicates _whether_ a key has been pressed
//      - `KBDR`: Keyboard sata register, idicates _which_ key was pressed
//  We use these because they are non-blocking, they allow us to poll the state of the keyboard and continue execution
//      - The other (RE: simpler) way to do this is to use the `GETC` trap code, but this block execution
const MemoryMappedRegisters = enum(u16) {
    KBSR = 0xFE00, // Keyboard status
    KBDR = 0xFE02, // Keyboard data
};

fn memRead(addr: u16) !u16 {
    if (addr == KBSR) {
        if (check_key()) {
            memory[KBSR] = (1 << 15);
            memory[KBDR] = std.io.getStdIn().reader().readInt(u16, .big) catch {
                return error.InputError;
            };
        } else {
            memory[KBSR] = 0;
        }
    }
    return memory[addr];
}

fn memWrite(address: u16, val: u16) void {
    memory[address] = val;
}

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

fn swap16(x: u16) u16 {
    return (x << 8) | (x >> 8);
}

fn handleCLArgs(args: *std.process.ArgIterator, allocator: std.mem.Allocator) !void {
    while (args.next()) |arg| {
        if (eql(u8, arg, "-h") or eql(u8, arg, "--help")) {
            const help =
                \\Usage: vm [options]
                \\ Options:
                \\     -h, --help  Display this help message
                \\     -i, --image  Load a program image
            ;

            std.debug.print(help, .{});
            std.process.exit(0);
        } else if (eql(u8, arg, "-i") or eql(u8, arg, "--image")) {
            // Check if image path is provided
            const image_path = args.next() orelse {
                std.log.err("Error: No image path provided\n", .{});
                return error.NoImagePath;
            };

            // Open the file
            const file = try std.fs.cwd().openFile(image_path, .{});
            defer file.close();

            // Read origin (first 2 bytes)
            var origin_bytes: [2]u8 = undefined;
            _ = try file.read(&origin_bytes);
            const origin: u16 = std.mem.readInt(u16, &origin_bytes, .big);

            // Calculate maximum bytes we can read
            // const max_read = MEMORY_MAX - origin;
            const file_size = (try file.stat()).size;

            // Read the rest of the file
            var buffer = try allocator.alloc(u8, file_size - 2);
            defer allocator.free(buffer);
            _ = try file.read(buffer);

            // Copy to memory and swap endianness
            // TODO: figure out how to do this without a loop
            var i: usize = 0;
            while (i < buffer.len - 1) : (i += 2) {
                const value = std.mem.readInt(u16, buffer[i..][0..2], .big);
                memory[origin + (i / 2)] = value;
            }
        } else {
            const help =
                \\Usage: vm [options]
                \\ Options:
                \\     -h, --help  Display this help message
                \\     -i, --image  Load a program image
            ;

            std.debug.print(help, .{});
            std.process.exit(1);
        }
    }
}

// NOTE: Realistically this could just be a stdlib function
fn signExtend(x: u16, comptime bitCount: u4) u16 {
    if (((x >> (bitCount - 1)) & 1) == 1) {
        x |= 0xFFFF <<| bitCount;
    }
    return x;
}

fn updateFlags(r: u16) void {
    if (registerStorage[r] == 0) {
        registerStorage[R_COND] = @intFromEnum(conditionFlags.FL_ZRO);
    } else if ((registerStorage[r] >> 15) == 1) {
        registerStorage[R_COND] = @intFromEnum(conditionFlags.FL_NEG);
    } else {
        registerStorage[R_COND] = @intFromEnum(conditionFlags.FL_POS);
    }
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

    _ = disable_input_buffering();
    defer restore_input_buffering();

    // We need to have 1 condition flag set at any given time, so we set the Z flag
    registerStorage[R_COND] = @intFromEnum(conditionFlags.FL_ZRO);

    // Set the PC to the starting position
    const PC_START = 0x3000;
    registerStorage[R_PC] = PC_START;

    // TODO for some reason this needs a try? figure out why
    cpu: while (true) {
        // fetch instruction
        registerStorage[R_PC] += 1;
        const instruct: u16 = registerStorage[R_PC];
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

                if (immFlag == 1) {
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

                if (immFlag == 1) {
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
                //  Encoding:
                //      - Opcode (4 bits, should be `0000`)
                //      - Condition flags (3 bits) - n, z, p flags that determine when to branch
                //      - PCoffset9 (9 bits) - Signed offset from the current PC

                // Get program counter offset
                const pcOffset: u16 = signExtend(instruct & 0x1FF, 9);

                // Get condition flag
                const condFlag: u16 = (instruct >> 9) & 0x7;

                if ((condFlag & registerStorage[R_COND]) != 0) {
                    registerStorage[R_PC] += pcOffset;
                }
            },
            .OP_JMP => {
                // JUMP
                //  Unconditionally jumps to the address stored in the base register
                //  Encoding:
                //      - Opcode (4 bits, should be `1100`)
                //      - Unused (3 bits)
                //      - Base register (3 bits) - Contains the address to jump to
                //      - Unused (6 bits)

                // Get destination register
                const r1: u16 = (instruct >> 6) & 0x7;

                registerStorage[R_PC] = registerStorage[r1];
            },
            .OP_JSR => {
                // JUMP REGISTER
                //  Jumps to a location in memory and saves return address
                //  Encoding:
                //      - opcode (4 bits, should be `0100`)
                //      - long flag (1 bit) - Determines if using JSR (1) or JSRR (0) mode
                //      - If JSR: PCoffset11 (11 bits) - Signed offset from current PC
                //      - If JSRR: unused (2 bits), BaseR (3 bits), unused (6 bits)
                //  Always saves the incremented PC in R7 before jumping

                // Check whether we're in long or short mode
                const longFlag = (instruct >> 11) & 1;

                // Save the address of the call routine in register 7
                registerStorage[R7] = registerStorage[R_PC];

                if (longFlag == 1) {
                    // JSR case
                    const longPcOffset: u16 = signExtend(instruct & 0x1FF, 11);
                    registerStorage[R_PC] += longPcOffset;
                } else {
                    // JSRR case
                    const r1: u16 = (instruct >> 6) & 0x7;
                    registerStorage[R_PC] += registerStorage[r1];
                }
            },
            .OP_LD => {
                // LOAD
                //  Loads a value from a location in memory into a register
                //  Encoding:
                //      - opcode (4 bits, should be `0010`)
                //      - destination register (3 bits)
                //      - PCoffset9 (9 bits) - Signed offset added to the current PC to get memory address

                // Get destination register
                const r0: u16 = (instruct >> 9) & 0x7;

                // Get PC Offset
                const pcOffset: u16 = signExtend(instruct & 0x1FF, 9);

                registerStorage[r0] = try memRead(registerStorage[R_PC] + pcOffset);

                updateFlags(r0);
            },
            .OP_LDI => {
                // LOAD INDIRECT
                //  Loads a value from a location in memory into a register
                //  Encoding:
                //      - opcode (4 bits, should be `0101`)
                //      - destination register (3 bits)
                //      - PCoffset9 (9 bits) - Points to address containing final target address
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
                registerStorage[r0] = try memRead(try memRead(registerStorage[R_PC] + pcOffset));

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
                //      - offset6 (6 bits) - Signed offset added to base register value
                //  The offset is added to the base register to get the final address to load from
                //  This is useful for loading data that's stored in a neighborhood of the base register

                // Get destination register
                const r0: u16 = (instruct >> 9) & 0x7;

                // Get base register
                const r1: u16 = (instruct >> 6) & 0x7;

                // Get offset
                const offset: u16 = signExtend(instruct & 0x3F, 6);

                registerStorage[r0] = try memRead(registerStorage[r1] + offset);

                // Update flags
                updateFlags(r0);
            },
            .OP_LEA => {
                // LOAD EFFECTIVE ADDRESS
                //  Loads the address of a memory location into a register
                //  Encoding:
                //      - opcode (4 bits, should be `1110`)
                //      - destination register (3 bits)
                //      - PCoffset9 (9 bits) - Signed offset from current PC
                //  Unlike other loads, this loads the address itself rather than the value at that address

                // Get destination register
                const r0: u16 = (instruct >> 9) & 0x7;

                // Get sign extended offset
                const pcOffset: u16 = signExtend(instruct & 0x1FF, 9);

                registerStorage[r0] = registerStorage[R_PC] + pcOffset;

                updateFlags(r0);
            },
            .OP_ST => {
                // STORE
                //  Stores a register value into memory
                //  Encoding:
                //      - opcode (4 bits, should be `0011`)
                //      - source register (3 bits) - Register containing value to store
                //      - PCoffset9 (9 bits) - Signed offset from current PC for target address

                // Get storage register
                const sr: u16 = (instruct >> 9) & 0x7;

                // Get PC offset
                const pcOffset: u16 = signExtend(instruct & 0x1FF, 9);

                memWrite(registerStorage[R_PC] + pcOffset, registerStorage[sr]);
            },
            .OP_STI => {
                // STORE INDIRECT
                //  Stores a register value into memory using indirect addressing
                //  Encoding:
                //      - opcode (4 bits, should be `1011`)
                //      - source register (3 bits) - Register containing value to store
                //      - PCoffset9 (9 bits) - Points to address containing final target address
                //  Similar to LDI, allows storing to addresses further than 9-bit offset would allow

                // Get storage register
                const sr: u16 = (instruct >> 9) & 0x7;

                // Get PC offset
                const pcOffset: u16 = signExtend(instruct & 0x1FF, 9);

                memWrite(try memRead(registerStorage[R_PC] + pcOffset), registerStorage[sr]);
            },
            .OP_STR => {
                // STORE REGISTER
                //  Stores a register value into memory using base+offset addressing
                //  Encoding:
                //      - opcode (4 bits, should be `0111`)
                //      - source register (3 bits) - Register containing value to store
                //      - base register (3 bits) - Contains base address
                //      - offset6 (6 bits) - Signed offset added to base register value

                // Get storage register
                const sr: u16 = (instruct >> 9) & 0x7;

                // Get base register
                const br: u16 = (instruct >> 6) & 0x7;

                // Get memory offset
                const offset: u16 = signExtend(instruct & 0x3F, 6);

                memWrite(br + offset, registerStorage[sr]);
            },
            .OP_TRAP => {
                // TRAP
                //  System call that performs various I/O and control operations
                //  Encoding:
                //      - opcode (4 bits, should be `1111`)
                //      - unused (4 bits)
                //      - trapvect8 (8 bits) - Index into trap vector table
                //  Saves current PC in R7 before executing trap routine
                //  Different trap codes perform different operations like:
                //      - GETC: Read character from keyboard
                //      - OUT: Output character
                //      - PUTS: Output string
                //      - IN: Input character with prompt
                //      - PUTSP: Output byte string
                //      - HALT: Halt program execution

                // Store current PC in a register
                registerStorage[R7] = registerStorage[R_PC];

                // Get the trap code
                const trapCode: TrapCodes = @enumFromInt(instruct & 0xFF);

                // Switch on the trap code
                switch (trapCode) {
                    .GETC => {
                        registerStorage[R0] = @as(u16, std.io.getStdIn().reader().readInt(u16, .big) catch {
                            break error.InputError;
                        });

                        updateFlags(R0);
                    },
                    .OUT => {
                        std.io.getStdOut().writer().writeInt(u16, registerStorage[R0], .big) catch {
                            break error.OutputError;
                        };
                    },
                    .PUTS => {
                        var address = registerStorage[R0];
                        var char: u16 = undefined;
                        while (char != 0) {
                            char = try memRead(address);

                            try std.io.getStdOut().writer().writeInt(u16, char, .big);
                            address += 1;
                        }
                    },
                    .IN => {
                        const stdout = std.io.getStdOut().writer();
                        const stdin = std.io.getStdIn().reader();

                        try stdout.writeAll("Enter a character: ");

                        // Read single character
                        const c = try stdin.readByte();

                        // Echo the character
                        try stdout.writeByte(c);

                        // Store in R0 and update flags
                        registerStorage[R0] = @as(u16, c);
                        updateFlags(R0);
                    },
                    .PUTSP => {
                        const stdout = std.io.getStdOut().writer();

                        const c: [*]u16 = @ptrCast(&memory[registerStorage[R0]]);

                        var i: usize = 0;
                        while (c[i] != 0) : (i += 1) {
                            const char1: u8 = @truncate(c[i] & 0xFF);
                            stdout.writeByte(char1) catch return;

                            const char2: u8 = @truncate(c[i] >> 8);
                            if (char2 != 0) {
                                stdout.writeByte(char2) catch return;
                            }
                        }
                    },
                    .HALT => {
                        std.log.err("HALT", .{});
                        break :cpu;
                    },
                }
            },
            else => {
                // bad opcode
                break :cpu;
            },
        }
    }
}
