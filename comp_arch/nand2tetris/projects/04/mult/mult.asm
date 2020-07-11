// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.

@i //load i into A register
M=1 //Set Memory[i]=1
@R2 //Load R2 into A register
M=0 //Set Memory[R2]=0

(LOOP)
       @i
       D=M //D=Memory[i]=1
       @R1 //Load in value at R1.
       D=D-M //D=Memory[i]-Memory[R1]=1-Memory[R1]
       @END
       D; JGT // If (Memory[i]-Memory[R1])>0 goto END
       @R0 //Load sum to A
       D=M //Set D=Memory[R0]
       @R2 //Load R2 into A.
       M=M+D // Memory[R2] = D + Memory[R2]= Memory[R0] + Memory[R2]
       @i //Load i to A
       M = M+1 //Increment Memory[i] by 1.
       @LOOP
       0;JMP // Goto LOOP
(END)
       @END
       0;JMP //Infinite loop


