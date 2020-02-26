/****************************************CHAPTER 2****************************************/
/* Q: Assume that h is associated with register $s2 and the base address of A is
 *    $s3. What is the MIPS assembly code for the C assignment statement below??
 *
 *    A[12] = h + A[8]
 *
 *    $load $t0, 32($s3)    //int t; t = A[8];
 *    add $t0, $s2, $t0     //t = h+t;
 *    $sw $t0, 48($s3)      //A[12]=t;
 */

/*  int leaf_example (int g, int h, int i, int j)
 *  {
 *      int f;
 *  
 *      f = (g + h) - (i + j);
 *      return f;
 *  }
 *  What is the compiled MIPS assembly code?
 *  
 *   leaf_example:
 *     //push three words to stack
 *     addi $sp, $sp, -3*4         //$sp = $sp - 3*4   
 *  
 *     //spill registers
 *     sw   $t1, 2*4($sp)          //Memory[$sp+2*4] = $t1
 *     sw   $t0, 1*4($sp)          //Memory[$sp+1*4] = $t0
 *     sw   $s0, 0*4($sp)          //Memory[$sp+0*4] = $s0
 *  
 *     //proc body; g -> $a_0, h -> $a_1, i -> $a_2, j -> $a_3, $v0, $v1 return registers
 *     add $to, $a0, $a1           //$t0 = $a0 + $a1
 *     add $t1, $a3, $a4           //$t1 = $a3 + $a4
 *     sub $s0, $t0, $t1           //$s0 = $t0 - $t1
 *     add $v0, $s0, $zero         //$v0 = $s0 + $zero = $s0
 *
 *    //restore old register values
 *    lw $s0, 0*4($sp)             // $s0=Memory[$sp+0*4]
 *    lw $t0, 1*4($sp)             // $t0=Memory[$sp+1*4]
 *    lw $t1, 2*4($sp)             // $t1=Memory[$sp+2*4]
 *  
 *    //pop three words from stack
 *    addi $sp, $sp, 3*4          //$sp = $sp + 3*4
 *    
 *    //jump to return address value stored in $ra.
 *    jr $ra  
 */

/*  int fib (int n) 
 *  {
 *      if (n<1) return 1;
 *      else return n * fib(n-1);
 *  }
 *
 *  What is the compiled MIPS assembly code?
 *  
 *   fib:
 *     addi $sp, $sp, -2*4 //pop 2 to stack
 *     sw $ra, 1*4($sp)    //save address
 *     sw $a0 0($sp)       //save argument
 *     slti $t0, $a0, 1    //if $a0<1, $t0=1, else $t0=0;
 *     beq $t0, $zero, L1  //if $t0=0, jump to L1
 *     addi $v0, zero, 1   //$v0=1
 *     addi $sp, $sp, 2*4  //$sp+=2*4
 *     jr $ra              //jump to $ra
 *  L1:
 *     addi $a0, $a0, -1  //$a0 -=1 
 *     jal fact           //$ra=PC+1*4; go to fact
 *     lw $a0, 0($sp)     //load argument
 *     lw $ra, 1*4($sp)   //load return address
 *     addi $sp, $sp, 2*4 //pop 2 from stack
 *     mul $v0, $a0, $v0  //$v0 = $a0 * $v0
 *     jr $ra             //jump to $ra
 */


