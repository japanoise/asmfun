# Assembly Tutor

In these hallowed halls are everything I know about programming 16-bit x86
assembly in MS-DOS. Abandon hope all ye who enter here, because this is an
infodump rather than a real tutorial. The information is dense and assumes a
passing familiarity with assembly - you can check out [this tutorial][fasmtut]
if you have no idea whatsoever. Or just read my code... hopefully I can explain
myself at least passably. Comments are free... but comments are cruft.

You'll want to keep the [fasm manual][fasmman] on hand since that's the
assembler I'm familiar with. Here's some other useful links:

* https://en.wikibooks.org/wiki/X86_Assembly
* https://en.wikibooks.org/wiki/X86_Disassembly
* https://www.stanislavs.org/helppc/idx_assembler.html

Also fuck AT&T syntax, all my homies hate AT&T syntax.

[fasmtut]: http://bos.asmhackers.net/docs/FASM%20tutorial/
[fasmman]: https://flatassembler.net/docs.php?article=manual

## Registers

### General purpose registers

There are 8 general purpose registers:

1. Accumulator register (AX). Used in arithmetic operations
2. Counter register (CX). Used in shift/rotate instructions and loops.
3. Data register (DX). Used in arithmetic operations and I/O operations.
4. Base register (BX). Used as a pointer to data (located in segment register
   DS, when in segmented mode).
5. Stack Pointer register (SP). Pointer to the top of the stack.
6. Stack Base Pointer register (BP). Used to point to the base of the stack.
7. Source Index register (SI). Used as a pointer to a source in stream
   operations.
8. Destination Index register (DI). Used as a pointer to a destination in stream
   operations.

It is also possible to address the first four registers (AX, CX, DX and BX) in
their size of 16-bit as two 8-bit halves. The least significant byte (LSB), or
low half, is identified by replacing the 'X' with an 'L'. The most significant
byte (MSB), or high half, uses an 'H' instead. For example, CL is the LSB of the
counter register, whereas CH is its MSB.

### Segment registers

The 6 Segment Registers are:

* Stack Segment (SS). Pointer to the stack ('S' stands for 'Stack').
* Code Segment (CS). Pointer to the code ('C' stands for 'Code').
* Data Segment (DS). Pointer to the data ('D' stands for 'Data').
* Extra Segment (ES). Pointer to extra data ('E' stands for 'Extra').
* F Segment (FS). Pointer to more extra data ('F' comes after 'E').
* G Segment (GS). Pointer to still more extra data ('G' comes after 'F').

### Flags register

There is a 32-bit (not sure if it's 32-bit in 16-bit operation?)  register
called EFLAGS which stores the flags.

The flags are (with their location in EFLAGS, 0-32 least->most significant):

* 0.  CF : Carry Flag. Set if the last arithmetic operation carried (addition)
  or borrowed (subtraction) a bit beyond the size of the register. This is then
  checked when the operation is followed with an add-with-carry or
  subtract-with-borrow to deal with values too large for just one register to
  contain.
* 2.  PF : Parity Flag. Set if the number of set bits in the least significant
  byte is a multiple of 2.
* 4.  AF : Adjust Flag. Carry of Binary Code Decimal (BCD) numbers arithmetic
  operations.
* 6.  ZF : Zero Flag. Set if the result of an operation is Zero (0).
* 7.  SF : Sign Flag. Set if the result of an operation is negative.
* 8.  TF : Trap Flag. Set if step by step debugging.
* 9.  IF : Interruption Flag. Set if interrupts are enabled.
* 10.  DF : Direction Flag. Stream direction. If set, string operations will
  decrement their pointer rather than incrementing it, reading memory backwards.
* 11.  OF : Overflow Flag. Set if signed arithmetic operations result in a value
  too large for the register to contain.
* 12-13.  IOPL : I/O Privilege Level field (2 bits). I/O Privilege Level of the
  current process.
* 14.  NT : Nested Task flag. Controls chaining of interrupts. Set if the
  current process is linked to the next process.
* 16.  RF : Resume Flag. Response to debug exceptions.
* 17.  VM : Virtual-8086 Mode. Set if in 8086 compatibility mode.
* 18.  AC : Alignment Check. Set if alignment checking of memory references is
  done.
* 19.  VIF : Virtual Interrupt Flag. Virtual image of IF.
* 20.  VIP : Virtual Interrupt Pending flag. Set if an interrupt is pending.
* 21.  ID : Identification Flag. Support for CPUID instruction if can be set.


## Instructions

This is a whistlestop tour, and we brake for no man! So this is not going to be
the biggest explainer in the world, nor is it going to be exhaustive. I'm just
going to list everything I care about with a quick explainer of what it
does. Think of it like a cheat-sheet.

The source of truth is the fasm manual and ultimately the datasheets, but I
wouldn't wish reading the datasheets on my worst enemy.

```asm
;; Moving/setting data

mov dest, src

mov al, 255     ; Set register.
; Some registers can't be set immediately like this and need you to go via AX

mov [255], al   ; Set memory with register

;; Exchanging data

xchg dest, src

xchg ax,bx      ; swap two general registers
xchg al,[char]  ; swap register with memory

;; Pushing/popping

push ax         ; store general register
push es         ; store segment register
pushw [bx]      ; store memory
push 1000h      ; store immediate value

pop bx          ; restore general register
pop ds          ; restore segment register
popw [si]       ; restore memory

;; Binary arithmetic

add dest, src   ; Binary addition

add ax,bx       ; add register to register
add ax,[si]     ; add memory to register
add [di],al     ; add register to memory
add al,48       ; add immediate value to register
add [char],48   ; add immediate value to memory

sub dest, src   ; Binary subtraction

inc dest        ; increment (add one) to dest

inc ax          ; increment register by one
inc byte [bx]   ; increment memory by one

dec dest        ; decrement (sub one) from dest

neg dest        ; Flip sign of dest

mul dest        ; Multiplies dest and accumulator (AX)
                ; Stores byte*byte result in ah & al; word*word in dx & ax

div dest        ; Divides dest by accumulator. AX width is twice dest.
                ; byte/AX; quotient -> AL; remainder -> AH
                ; word/(DX AX); quotient -> AX; remainder -> DX

idiv dest       ; Signed division

imul dest       ; Signed multiplication (dest same as inc & mul)
imul dest, src
imul dest, arg3, src

imul bl         ; accumulator by register
imul word [si]  ; accumulator by memory
imul bx,cx      ; register by register
imul bx,[si]    ; register by memory
imul bx,10      ; register by immediate value
imul ax,bx,10   ; register by immediate value to register
imul ax,[si],10 ; memory by immediate value to register

;; Decimal arithmetic
;; I.e. 0x07 = 7 decimal
;; Packed decimal uses both digits i.e. 0 - 99 (0x00 - 0x99)
;; Unpacked uses only the low nybble i.e. 0-9 (0x00 - 0x09)
;; Please RTFM because I'm not copying all that here.

daa             ; adjusts the result of adding two valid packed decimal operands in AL.
das             ; adjusts the result of subtracting two valid packed decimal operands in AL.
aaa             ; daa but with unpacked decimals
aas             ; das but with unpacked decimals
aam             ; corrects the result of a multiplication of two valid unpacked decimal numbers
aad             ; modifies the numerator in AH and AL to prepare for the division of two valid unpacked decimal operands

;; Logical

not dest        ; Inverts (ones complement) the argument. Same rules as for inc

and dest, src   ; Logical AND
or dst, src     ; Logical OR
xor dst, src    ; Logical XOR

bt dest, src    ; Copy bit to CF
bts dest, src   ; Copy bit to CF & set to 1
btr dest, src   ; Copy bit to CF & reset to 0
btc dest, src   ; Copy bit to CF & flip bit to its complement

bt  ax,15        ; test bit in register
bts word [bx],15 ; test and set bit in memory
btr ax,cx        ; test and reset bit in register
btc word [bx],cx ; test and complement bit in memory

bsf dest, src    ; store indef of first set bit forward in dest, zf = 1 if not found, = 0 otherwise
bsf dest, src    ; store indef of first set bit backward in dest, zf = 1 if not found, = 0 otherwise

bsf ax,bx        ; scan register forward
bsr ax,[si]      ; scan memory reverse

shl dest, bits   ; shift left. Zeroes in from right. CF = last bit that exited
sal dest, bits   ; synonym for shl
shr dest, bits   ; shift right. Zeroes in from left. CF = last bit that exited
sar dest, bits   ; shift right, preserving sign.

shl al,1         ; shift register left by one bit
shl byte [bx],1  ; shift memory left by one bit
shl ax,cl        ; shift register left by count from cl
shl word [bx],cl ; shift memory left by count from cl

shld dest, src, count ; shifts dest to the left count times and the bit
                      ; positions opened are filled with the most significant bits of src.

shrd dest, src, count ; shifts dest to the right count times and the bit
                      ; positions opened are filled with the least significant bits of src.

shld ax,bx,1     ; shift register left by one bit
shld [di],bx,1   ; shift memory left by one bit
shld ax,bx,cl    ; shift register left by count from cl
shld [di],bx,cl  ; shift memory left by count from cl

rol dest, bits   ; rotates dest left by bits. Operands same as shl
rcl dest, bits   ; rotates dest left by bits and sets CF

ror dest, bits   ; rotates dest right by bits. Operands same as shl
rcr dest, bits   ; rotates dest right by bits and sets CF

test dest, src   ; logical `and`, but does not update dest, only flags

cmp dest, src    ; `sub`, but does not update dest, only flags

;; Control transfers

jmp addr         ; unconditional jump

jmp 100h         ; direct near jump
jmp 0FFFFh:0     ; direct far jump
jmp ax           ; indirect near jump
jmp pword [ebx]  ; indirect far jump

call addr        ; jump to addr, saving next address on the stack for a ret/retf
                 ; same (*) operands as jmp

retn             ; return near
ret              ; synonym for retn
retf             ; return far

iret             ; return control to an interrupted procedure

jz, jne, etc.    ; conditional jump - see table

loop addr        ; roughly dec cx/jnz addr - also loopz, loope, etc. as table

int arg          ; interrupt; RTFM for your hardware configuration
int3             ; invoke interrupt 3
into             ; invoke interrupt 4 if OF is set

bound idx, array ; determines if idx is in the bounds (two signed words) at array
bound ax, [bx]   ; check word for bounds
                 ; if idx is out of bounds, interrupt 5 occurs

;; Instructions for interfacing with I/O ports
;; Immediate ports (e.g. in al, 20h) must be in range 0-255

in dst, src      ; input from src to dst

in al,20h        ; input byte from port 20h
in ax,dx         ; input word from port addressed by dx

out dst, src     ; output from src to dst

out 20h,ax       ; output word to port 20h
out dx,al        ; output byte to port addressed by dx

;; String operations
;; These operate on one element of a string. They move from si to di,
;; incrementing si and/or di by 1, 2, or 4 bytes for byte, word, dword
;; respectively. If DF is 0, si/di are incremented; if DF is 1, decremented.

movs byte [di],[si]        ; transfer byte
movs word [es:di],[ss:si]  ; transfer word
movsd                      ; transfer double word

cpms dst, src              ; compares dst and src

cmpsb                      ; compare bytes
cmps word [ds:si],[es:di]  ; compare words
cmps dword [fs:esi],[edi]  ; compare double words

scas dst                   ; subtracts destination string from al/ax and updates flags

scas byte [es:di]          ; scan byte
scasw                      ; scan word
scas dword [es:edi]        ; scan double word

stos dst                   ; store al/ax at destination

lods src                   ; places source string into al or ax

lods byte [ds:si]          ; load byte
lods word [cs:si]          ; load word
lodsd                      ; load double word

ins dest, dx               ; transfer a byte from input port to string dest

insb                       ; input byte
ins word [es:di],dx        ; input word
ins dword [edi],dx         ; input double word

outs dx, src               ; transfer a byte from string src to output port

outs dx,byte [si]          ; output byte
outsw                      ; output word
outs dx,dword [gs:esi]     ; output double word

rep (string instruction)   ; Basically loop + string instruction, decrementing
                           ; cx, etc.

rep  movsd                 ; transfer multiple double words
repe cmpsb                 ; compare bytes until not equal
repnz insb                 ; input bytes until zero

;; Flag instructions

stc   ; set the carry flag (CF) to 1
clc   ; clear the CF
cmc   ; sets the CF to its complement

std   ; set the direction flag (DF) to 1
cld   ; clear the DF

sti   ; set the interrupt flag (IF) to 1
cli   ; clear the IF

lahf  ; copies SF, ZF, AF, PF, and CF to bits 7, 6, 4, 2, and 0 of the AH register.

sahf  ; transfers bits 7, 6, 4, 2, and 0 from the AH register into SF, ZF, AF, PF, and CF.

pushf ; pushes the flags to the stack

popf  ; pops the flags from the stack

;; Conditional operations
;; Use the same mnemonics as the conditional jumps table

setne al         ; set al to 1 if zero flag cleared, 0 otherwise
seto byte [bx]   ; set byte to 1 if overflow, 0 otherwise

salc             ; set all bytes in al if carry flag is set; 0 otherwise

cmove ax,bx      ; move when zero flag set
cmovnc eax,[ebx] ; move when carry flag cleared

; cmpxchg compares the value in the AL, AX, or EAX register with the destination
; operand. If the two values are equal, the source operand is loaded into the
; destination operand. Otherwise, the destination operand is loaded into the AL,
; AX, or EAX register. The destination operand may be a general register or
; memory, the source operand must be a general register.

cmpxchg dl,bl    ; compare and exchange with register
cmpxchg [bx],dx  ; compare and exchange with memory

;; Misc instructions

nop             ; Uses one byte and does nothing

ud2             ; Generates an invalid opcode exception

lea dest, src   ; calculates the address of src and loads it into dest

lds arg1, arg2  ; load DS register and other specified register from memory
lds bx,[si]     ; load pointer to ds:bx

; les, lfs, lgs and lss operate identically to lds except that rather than DS
; register the ES, FS, GS and SS is used respectively.
```

### Conditional jumps table

| Mnemonic | Condition tested          | Description           |
|----------|---------------------------|-----------------------|
| `-o`     | OF = 1                    | overflow              |
| `-no`    | OF = 0                    | not overflow          |
| `-c`     | CF = 1                    | carry                 |
| `-b`     | CF = 1                    | below                 |
| `-nae`   | CF = 1                    | not above nor equal   |
| `-nc`    | CF = 0                    | not carry             |
| `-ae`    | CF = 0                    | above or equal        |
| `-nb`    | CF = 0                    | not below             |
| `-e`     | ZF = 1                    | equal                 |
| `-z`     | ZF = 1                    | zero                  |
| `-ne`    | ZF = 0                    | not equal             |
| `-nz`    | ZF = 0                    | not zero              |
| `-be`    | CF `or` ZF = 1            | below or equal        |
| `-na`    | CF `or` ZF = 1            | not above             |
| `-a`     | CF `or` ZF = 0            | not below             |
| `-nbe`   | CF `or` ZF = 0            | not below or equal    |
| `-s`     | SF = 1                    | sign                  |
| `-ns`    | SF = 0                    | not sign              |
| `-p`     | PF = 1                    | parity                |
| `-pe`    | PF = 1                    | parity even           |
| `-np`    | PF = 0                    | not parity            |
| `-po`    | PF = 0                    | parity odd            |
| `-l`     | SF `xor` OF = 1           | less                  |
| `-nge`   | SF `xor` OF = 1           | not greater nor equal |
| `-ge`    | SF `xor` OF = 0           | greater or equal      |
| `-nl`    | SF `xor` OF = 0           | not less              |
| `-le`    | (SF `xor` OF) `or` ZF = 1 | less or equal         |
| `-ng`    | (SF `xor` OF) `or` ZF = 1 | not greater           |
| `-g`     | (SF `xor` OF) `or` ZF = 0 | greater               |
| `-nle`   | (SF `xor` OF) `or` ZF = 0 | not less nor equal    |

## It's the segments, stupid!

You cannot understand 16-bit assembly without understanding segmented memory.

Segmented memory is expressed in the form:

    segment:offset

The segment is a 16-bit number which refers to a particular 16 bytes of memory.
The offset is also 16-bits. This means each segment is 64kb of memory,
overlapping within the actual memory, leaving you with the ability to address
about a megabyte total (specifically a mebibyte - 1,048,575)

For example, if we have segment 0xDEAD, we can address the memory between
0xDEAD0 and 0xEEACF by using offset 0x0000 to 0xFFFF (0xDEAD0 + 0xFFFF =
0xEEACF). This is written 0xDEAD:0x0000 - 0xDEAD:0xFFFF

fasm will allow you to use segmented memory directly or indirectly. For example,
this is a program that sets the extra segment (ES) to 0xB800, then sets the
source index (SI) as 0, then moves some memory to that location. Since 0xB800 is
the start of VGA memory, the effect of this program is to draw `!` all over the
screen.

```asm
;; MS-DOS COM file
org 100h

start:
    ;; Set AX to the start of VGA memory
    mov ax, 0xb800
    ;; Set ES to AX
    mov es, ax
    mov si, 0
    mov bl, 0

@@:
    ;; Set ES:SI to 0x21, '!'
    mov bh, 0x21
    mov [es:si], bh
    ;; Increment by two (so we skip the attribute byte) and loop
    add si,2
    inc bl
    cmp bl, 0xFF
    je start
    jne @b
```

As well as moving to and from segmented memory you can far jump, far call, and
far return:

```asm
;; Near versions
    jmp 100h         ; direct near jump
    jmp ax           ; indirect near jump
    retn             ; return near
    ret              ; same as retn

;; Far versions
    jmp 0FFFFh:0     ; direct far jump
    jmp pword [ebx]  ; indirect far jumpw
    retf             ; return far
```

Per the fasm manual:

>  `call` transfers control to the procedure, saving on the stack the address of
>  the instruction following the `call` for later use by a `ret` (return)
>  instruction. Rules for the operands are the same as for the `jmp`
>  instruction, but the call has no short variant of direct instruction and thus
>  it not optimized.

The meaning of this poorly worded paragraph is left as an exercise to the
reader.

### MZ .exe - segmented executable for use much more memory make longer program

If you start your assembly source with `org 100h`, you're creating a `.COM`
file, i.e. a flat-model executable. Your program is loaded into 0x100 as-is.
It's simple to program this way as you don't have to care about segments.
However, you only have ~64kb of memory & total program size to work with!

To fix this, there's `.EXE` files - MZ files, so called because the two magic
bytes `MZ` start the file - which are 16-bit real mode segmented executables.
You have to define your code in terms of segments, for a little extra
complexity, but that means your programs can be much larger and access much more
memory.

Here's Hello World as an MZ executable:

```asm
format MZ
entry .code:start
segment .code
start:
    mov ax, .data ; put data segment into ax
    mov ds, ax    ; there, I setup the DS for you
    mov dx, msg   ; now I give you the offset in DX. DS:DX now completed.
    mov ah, 9h
    int 21h
    mov ah, 4ch
    int 21h

segment .data
    msg db 'Hello World', '$'
```

An example with far calls, from the fasm sources:

```asm
; fasm example of writing multi-segment EXE program

format MZ

entry main:start            ; program entry point
stack 100h                ; stack size

segment main                ; main program segment

  start:
    mov    ax,text
    mov    ds,ax

    mov    dx,hello
    call    extra:write_text

    mov    ax,4C00h
    int    21h

segment text

  hello db 'Hello world!',24h

segment extra

  write_text:
    mov    ah,9
    int    21h
    retf
```

mz-format.asm is a more verbose example, with some useful output, from this
repository.

## VGA Shit

You could probably use CGA and EGA in a similar way, but I don't care.

Resources:

* http://www.osdever.net/FreeVGA/home.htm
* [Cyrix VGA manual][cyrix]

### Text Mode

#### Video memory

It's poorly explained everywhere but basically at memory address 0xB8000
(i.e. B800:0000) there's an alternating pattern of character bytes then
attribute bytes. I.e. if there's a 0x41 byte at 0xB8000 and a 0x0F byte at
0xB8001 then there will be a bright white 'A' in the top left corner of the
screen. The attribute byte looks like:

    7     | 6   5   4  |  3   2   1   0
    blink | background | foreground color

The colors are the first 16 (or 8, in the background) colors in the VGA palette:

| Value  | Color       | Value  | Color       |
|--------|-------------|--------|-------------|
| 0      | Black       | 8      | Dark grey   |
| 1      | Dark blue   | 9      | Light blue  |
| 2      | Dark green  | 10 (A) | Light green |
| 3      | Dark cyan   | 11 (B) | Light cyan  |
| 4      | Dark red    | 12 (C) | Light red   |
| 5      | Dark pink   | 13 (D) | Light pink  |
| 6      | Dark orange | 14 (E) | Yellow      |
| 7      | Light grey  | 15 (F) | White       |

Note that the default foreground color is 7, i.e. light grey, not white.

#### Registers

There's far more than can be documented here, so RTFM - [the Cyrix
manual][cyrix] is good - but here's a quick summary:

| Register/ Group                 | I/O Read Address | I/O Write Address | Comments                        |
|---------------------------------|------------------|-------------------|---------------------------------|
| Miscellaneous Output Register   | 0x3CC            | 0x3C2 (W)         |                                 |
| Input Status Register 0         | 0x3C2            | -                 |                                 |
| Input Status Register 1         | 0x3?A            | -                 | ? = B (monochrome) or D (color) |
| Feature Control Register        | 0x3CA            | 0x3?A             | ? = B (monochrome) or D (color) |
| Sequencer Index Register        | 0x3C4            | 0x3C4             |                                 |
| Sequencer Data Register         | 0x3C5            | 0x3C5             |                                 |
| CRT Controller Index Register   | 0x3?4            | 0x3?4             | ? = B (monochrome) or D (color) |
| CRT Controller Data Register    | 0x3?5            | 0x3?5             | ? = B (monochrome) or D (color) |
| Graphics Controller Index Reg.  | 0x3CE            | 0x3CE             |                                 |
| Graphics Controller Data Reg.   | 0x3CF            | 0x3CF             |                                 |
| Attribute Controller Index Reg. | 0x3C0            | 0x3C0             |                                 |
| Attribute Controller Data Reg.  | 0x3C1            | 0x3C0             |                                 |
| Video DAC Index (CLUT Writes)   | 0x3C8            | 0x3C8             |                                 |
| Video DAC Index (CLUT Reads)    | -                | 0x3C7             |                                 |
| Video DAC Data Register         | 0x3C9            | 0x3C9             |                                 |
| Video DAC Pel Mask Register     | 0x3C6            | 0x3C6             |                                 |

Basically you write an index to an index register with `in`, then read with `in`
or write with `out` the data register.

For example, to move the cursor to position 0x00FF (i.e. position 255) you
would:

1. Write 0xF to port 0x3D4 (CRT Controller Index) to select the cursor low byte
2. Write 0xFF to port 0x3D5 (CRT Controller Data) to set it
3. Write 0xE to port 0x3D4 (CRT Controller Index) to select the cursor high byte
4. Write 0x00 to port 0x3D5 (CRT Controller Data) to set it

In asm:

```asm
    ;; CRT controller index port
    mov dx, 0x3D4
    ;; Cursor location low byte
    mov al, 0xF
    out dx, al
    ;; CRT controller data port
    inc dx
    mov al, 0xFF
    out dx, al
    ;; Back to the index port
    dec dx
    ;; Cursor location high byte
    mov al, 0xE
    out dx, al
    ;; Back to the data port
    inc dx
    xor al, al      ; i.e. mov al, 0
    out dx, al

    mov ax, 0xB800
    mov es, ax
```

[cyrix]: https://web.archive.org/web/20150816220334/http://www.eyetap.org/cyborgs/manuals/soft_vga.pdf

## fasm macros etc.

Another cheatsheet, this time for the pre-processor etc. in fasm.

```asm
;; macro - define macro
macro name arg1 arg2 ... { body }
; rtfm on this one - lots of cool shit!

;; include - include files
include 'macros.inc'

;; Symbolic constants
foo equ value

;; Numerical constants
x = 1
x = x + 2
byteConst = byte 69

;; if - only compile under certain conditions
if foo
    xor ax,ax
else if bar
    xor bx,bx
else
    xor cx,cx
end if

; Operators for if:
~ ; logical negation
& ; logical and
| ; logical or

= ; equal
< ; less
> ; greater
<= ; less/equal
>= ; greater/equal
<> ; not equal

;; times - repeat single instruction n times
times 5 xor ax, ax
; % is the index of the repetition
times 5 db % ; bytes 1, 2, 3, 4, 5

;; repeat - repeats blocks n times
repeat 8
    mov byte [bx],%
    inc bx
end repeat

; break exits a loop:
s = x/2
repeat 100
    if x/s = s
        break
    end if
    s = (s+x/s)/2
end repeat

;; while - repeats while the condition is true
s = x/2
while x/s <> s
    s = (s+x/s)/2
    if % = 100
        break
    end if
end while

;; align - aligns memory to n bytes by filling with `nop`
align 16
```
