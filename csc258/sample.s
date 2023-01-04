.data 
# TODO: What are the following 5 lines doing? stores the specified strings in the NULL-terminated format, storing strings as variables
promptA: .asciiz "Enter an int A: "
promptB: .asciiz "Enter an int B: "
resultAdd: .asciiz "A + B = "
resultSub: .asciiz "A - B = "
newline: .asciiz "\n"

.globl main
.text

main: 
    # TODO: Set a breakpoint here and step through. 
    # What does this block of 3 lines do?
    # line 18: system call code for print string
    # line 19: address of string to print
    # line 20: print the strings
	li $v0, 4		      
	la $a0, promptA
	syscall    

    # TODO: Set a breakpoint here and step through. 
    # What does this block of 3 lines do?
    # line 27: System call code for read integer
    # line 28: Read the int into v0
    # line 29: Move the value into t0
	li $v0, 5
	syscall 
	move $t0, $v0

    # TODO: What is the value of "promptB"? Hint: Check the
    # value of $a0 and see what it corresponds to.
    # line 16: system call code for print string
    # line 37: address of string to print
    # line 38: print the strings
	li $v0, 4
	la $a0, promptB
	syscall

    # TODO: Explain what happens if a non-integer is entered
    # by the user.
    # Error, 'invalid integer input', program is terminated. 
	li $v0, 5
	syscall 
    # TODO: t stands for "temp" -- why is the value from $v0 
    # being moved to $t1?
	move $t1, $v0

	# TODO: What if I want to get A + 1 and B + 42 instead
	# add $t2, $t0, 1
	# add $t3, $t1, 42
	add $t2, $t1, $t0 
	sub $t3, $t0, $t1

	li $v0, 4
	la $a0, resultAdd
	syscall

    # TODO: What is the difference between "li" and "move"?
    # move copies a value from one register to another. li loads a specific value into a register. 
	li $v0, 1
	move $a0, $t2	
	syscall 

    # TODO: Why is the next block of three lines needed? 
    # Remove them and explain what happens.
    # they are needed for formatting and having the results be prited one below the other 
    # moves the cursor to the line below 
    	li $v0, 4
	la $a0, newline
	syscall 

	li $v0, 4
	la $a0, resultSub
	syscall

	move $a0, $t3	
	li $v0, 1
	syscall 

	li $v0, 4
	la $a0, newline
	syscall 

	li $v0, 10
	syscall