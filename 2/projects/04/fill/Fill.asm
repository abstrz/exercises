// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

//Notes:
//  RAM[16384 + w], 
//  There are 256 rows and 32 words per row. 
//  Thus, we get the final word address of 16384 + 256*32 = 24576
//  Now each word contains 16 pixels. We just have to set the value of the address at a word to be
//  the decimal number whose binary representation is 1111111111111111=65535

//load 65535 to color
@32767 //0111111111111111, the maximum number that we can load in at one time.
D=A
@color1
M=D
@16384 //0100000000000000
D=A
D=D+A
@color2
M=D
@color1
D=M
@color2
D=D+M
@color
M=D



(MAIN)
    @24576
    D=M
    @MAIN
    D; JEQ

    @16384
    D=A
    @counter
    M=D

(BLACK)
    @counter
    D=M
    @24576
    D=A-D
    @WHITEINIT
    D; JLT
    @color
    D=M
    @counter
    A=M
    M=D
    @counter
    M=M+1
    @BLACK
    0;JMP

(WHITEINIT)
    @24576
    D=M
    @WHITEINIT
    D; JNE

    @16384
    D=A
    @counter
    M=D

(WHITE)
    @counter
    D=M
    @24576
    D=A-D
    @MAIN
    D; JLT
    @counter
    A=M
    M=0
    @counter
    M=M+1
    @WHITE
    0;JMP
