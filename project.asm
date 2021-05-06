;PROJECT - DINO GAME
;Group members: Fateh Ali Aamir (19I-0648) and Hassan Ayyaz (19L-0970)

[org 0x0100]               

jmp  start 
 
;VARIABLES
oldisr:       dd   0                  ; space for saving old isr 
message:      db   'Score: '  		  ; Score string to be printed where the score is shown
m1:			  dw 0x1AB1				  ; The dino body
c1: 		  dw 0x0720               ; blank space used to clear
m4: 		  dw 0x1AAF               ; extraction operator character used to show dino eyes
tickcount:    dw   0 				  ; this variable stores the score as it increases  
seconds:      dw   0                  ; this variable stores the time as it increases  
timerflag:    dw   0 			      ; this flag is used to stop and start the score timer
oldkb:        dd   0				  ; space for saving old isr 
message1: db '----------------------------------------------------------------------------------------------------'

;NUMBER PRINTING SUBROUTINE
;This subroutine prints the time/score
;taken as it is from the book
printnum:     

	push bp               
	mov  bp, sp               
	push es               
	push ax               
	push bx               
	push cx               
	push dx               
	push di 
	 
	mov  ax, 0xb800               
	mov  es, ax             ; point es to video base               
	mov  ax, [bp+4]         ; load number in ax               
	mov  bx, 10             ; use base 10 for division               
	mov  cx, 0              ; initialize count of digits 
	 
	nextdigit:    

		mov  dx, 0              ; zero upper half of dividend              
		div  bx                 ; divide by 10               
		add  dl, 0x30           ; convert digit into ascii value               
		push dx                 ; save ascii value on stack               
		inc  cx                 ; increment count of values                
		cmp  ax, 0              ; is the quotient zero               
	jnz  nextdigit          ; if no divide it again 
	 
	mov  di, 0x0110         ; point di to 70th column 
	   
	nextpos:     
		
		pop  dx                 ; remove a digit from the stack               
		mov  dh, 0x07           ; use normal attribute               
		mov  [es:di], dx        ; print char on screen               
		add  di, 2              ; move to next screen location               
	loop nextpos            ; repeat for all digits on stack 
	 
	pop  di               
	pop  dx               
	pop  cx               
	pop  bx               
	pop  ax 
	pop  es               
	pop  bp               

ret  2 

; DELAY FUNCTION USED TO SLOW DOWN THE PROCESS
; This function has multiple loops that do not do anything but help 
; in delaying the printing sequence so we can easily observe it
delay:

	push cx

	mov cx, 0
	extraA:
		inc cx
		cmp cx, 40000
	jne extraA

	mov cx, 0
	extraB:
		inc cx
		cmp cx, 40000
	jne extraB

	mov cx, 0
	extraC:
		inc cx
		cmp cx, 40000
	jne extraC

	mov cx, 0
	extraD:
		inc cx
		cmp cx, 40000
	jne extraD
	 
	pop cx

ret 

; CLEAR SCREEN FUNCTION
; This is also a basic clear screen function taken from the book
clrscr:
	push es
	push ax
	push di
	mov ax, 0xb800
	mov es, ax
	mov di, 0

	nextloc:
		mov word[es:di], 0x0720
		add di, 2
		cmp di, 4000
	jne nextloc

	pop di
	pop ax
	pop es
ret

; THIS IS THE MAJOR PRINT FUNCTION
; This function takes in variables through the stack
; It takes in the location of printing, length of the string 
; and the ascii code for the character
print:

	push bp               
	mov  bp,sp               
	push ax               
	push cx               
	push si                 
	push di               
	push es               
	push ds
	
	mov  ah, 0x13           ; service 13 - print string               
	mov  bh, 0              ; output on page 0               
	mov  bl, 7              ; normal attrib               
	mov  dx, [bp + 8]       ; row 10 column 3               
	mov  cx, [bp + 6]       ; length of string               
	push cs               
	pop  es                 ; segment of string                
	mov  bp, [bp + 4]        ; offset of string                
	int  0x10               ; call BIOS video service 
	
	pop ds
	pop es
	pop di
	pop si
	pop cx
	pop ax
	pop bp
	
ret 6 

; THIS FUNCTION IS PART OF THE DINO JUMPING PROCESS
; In total there are 4 functions that allow the dino to jump: printabove, clearabove, printbase and clearbase
; The print above function is used when space is pressed and the dino is printed at a greater height
printabove:

	push bp               
	mov  bp,sp               
	push ax               
	push cx               
	push dx                 

	mov  dx, 0x0803         ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, m1
	push ax
	
	call print
	
	mov  dx, 0x0804         ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, m1
	push ax

	call print

	mov  dx, 0x0705       ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, m1
	push ax

	call print

	mov  dx, 0x0706      ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, m4
	push ax

	call print

	pop dx
	pop cx
	pop ax
	pop bp
	
ret

; CLEAR BASE FUNCTION
; This function clears the previous printing done at the base when space is pressed because the dino is at a greater height now
clearbase:

	push bp               
	mov  bp,sp               
	push ax               
	push cx               
	push dx                 

	mov  dx, 0x0A03         ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, c1
	push ax
	
	call print
	
	mov  dx, 0x0A04         ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, c1
	push ax

	call print
		
	mov  dx, 0x0905       ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, c1
	push ax

	call print
	
	mov  dx, 0x0906       ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, c1
	push ax
	
	call print
	
	pop dx
	pop cx
	pop ax
	pop bp
	
ret	

; PRINTING BASE 
; When the space bar is released and the dino has to come back to the ground, this function prints it at the original position
printbase:

	push bp               
	mov  bp,sp               
	push ax               
	push cx               
	push dx  
	
	mov  dx, 0x0A03         ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, m1
	push ax
	
	call print
	
	mov  dx, 0x0A04         ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, m1
	push ax

	call print
		
	mov  dx, 0x0905       ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, m1
	push ax

	call print
	
	mov  dx, 0x0906       ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, m4
	push ax
	
	call print 
	
	pop dx
	pop cx
	pop ax
	pop bp
	
ret 	

; CLEAR ABOVE
; Once space is released and the dino comes back to the ground, this function clears the previous printing done above
clearabove:

	push bp               
	mov  bp,sp               
	push ax               
	push cx               
	push dx  
	
	mov  dx, 0x0803         ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, c1
	push ax
	
	call print
	
	mov  dx, 0x0804         ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, c1
	push ax

	call print
	
	
	mov  dx, 0x0705       ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, c1
	push ax

	call print

	mov  dx, 0x0706       ; row 10 column 3               
	push dx
	mov  cx, 1
	push cx
	mov ax, c1
	push ax

	call print
	
	pop dx
	pop cx
	pop ax
	pop bp
	
ret

;;;;;;;;;;;;Obsticles in the way
animateloop:

push ax
push si
push di

mov ax, 0xb800
mov es, ax
mov di, 1700; startpoint
mov si, 1750; end point

animate:

mov word[es:di],0x08DB
mov word[es:si],0x08DB

mov bx,0

call delay
mov word[es:di],0x0720
mov word[es:si],0x0720

sub si, 2
sub di, 2
cmp di, 1610

jne animate

pop di
pop si
pop ax

ret




; THIS IS THE MAJOR PART OF THE CODE, ALL IMPLEMENTATIONS ARE DONE HERE
; This ISR hooks the spacebar and whenever space is pressed it will start the score counter and also allow the dino to jump
kbisr:        

	push ax               
	push es 
	
	mov  ax, 0xb800               
	mov  es, ax             ; point es to video memory
	mov di, 1700; startpoint
	mov si, 1750; end point
	 
keep:
	 
	in   al, 0x60           ; read a char from keyboard port               
	cmp  al, 0x39          ; has space been pressed 
	jne nextcmp 			; no, try next comparison 	


	mov word[es:di],0x0720		;Removing the Block on the Track
	mov word[es:si],0x0720		;Removing the Block on the Track
call delay

	sub si, 2
	sub di, 2
	
	mov word[es:di],0x08DB		;PRINTING the Block on the Track
	mov word[es:si],0x08DB		;PRINTING the Block on the Track

	;mov bx,0

	
	cmp di, 1610				;comparison of the first block
	jne here1					;checking if block 1 is at the end of the Window if not check block 2
	je here2					;checking if block 1 is at the end of the Window if yes jmp here2
	
	here1:						
	cmp si, 1610				;comparison of the Second block
	jne jumphere1				;If no block reached the end move to jumphere1
	je here3					;If the second box reached the end then go to here3
	
	here2:						
	mov word[es:di],0x0720		;Remove the previous block
	mov di, 1700; startpoint	;Again Intialize it from the start
	cmp si, 1610				;check the Second Block
	jne jumphere1				;If 2nd Block not have reached the window end move to the jumphere1
here3:
	mov word[es:si],0x0720		;Remove the previous block
	mov si, 1750; end point		;Again Intialize it from the start
	

	
jumphere1:



	mov  word [cs:timerflag], 1; set flag to start printing 

	inc  word [cs:seconds]  ; increment tick count(score)             
	push word [cs:seconds]         
    call delay
	call printnum           ; print tick count(score)

;clearing locations;;;;;;;;;;;;;;;;;;;;;;;;
	
call clearbase

;printing above;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
call printabove

nextcmp:
		
	in   al, 0x60        ; read a char from keyboard port 
	cmp  al, 0xb9        ; has the space released               
	jne  keep            ; run the loop from above again
		
	mov word[es:di],0x0720		;Removing the Block on the Track
	mov word[es:si],0x0720		;Removing the Block on the Track
call delay

	sub si, 2
	sub di, 2
	
	mov word[es:di],0x08DB		;PRINTING the Block on the Track
	mov word[es:si],0x08DB		;PRINTING the Block on the Track

	;mov bx,0

	cmp di, 1610				;comparison of the first block
	jne here4					;checking if block 1 is at the end of the Window if not check block 2
	je endLoop					;checking if block 1 hit the Dino then End

	
	here4:
	cmp si, 1610				;checking if block 2 is at the end of the Window if not jmp jumphere2
	jne jumphere2
	je endLoop					;checking if block 2 hit the Dino then End
	
	here5:
	mov word[es:di],0x0720
	mov di, 1700; startpoint
	cmp si, 1610
	jne jumphere1
here6:
	mov word[es:si],0x0720
	mov si, 1750; end point
	
jumphere2:
		
	mov  word [cs:timerflag], 0; if space not pressed, turn the flag off	
		
	mov  word [cs:timerflag], 1; set flag to start printing 

	inc  word [cs:seconds]  ; increment tick count(score)             
	push word [cs:seconds]         
    call delay
	call printnum           ; print tick count(score)


;printing;;;;;;;;;;;;;;;;;;;;;;;;
	
call printbase

;clearing locations;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

call clearabove
	
	jmp  keep               ; go back and run the loop from above again
	
	
	
	nomatch:         
	
		pop  es               
		pop  ax               

		jmp  far [cs:oldisr]    ; call the original ISR 
		 
	exit:         

		mov  al, 0x20               
		out  0x20, al           ; send EOI to PIC 

endLoop:
	mov word[es:di],0x0720
	mov word[es:si],0x0720
	pop  es               
	pop  ax               

iret                    ; return from interrupt 
 
start:        

	call clrscr				;clear screen called first

	;SETTING UP THE PRINTING USING BIOS INTERRUPTS
	mov  ah, 0x13           ; service 13 - print string               
	mov  al, 1              ; subservice 01 – update cursor               
	mov  bh, 0              ; output on page 0               
	mov  bl, 6              ; normal attrib               
	
	;HERE WE ARE PRINTING THE PLATFORM/GROUND OF THE GAME
	
	mov  dx, 0x0B01         ; row 11 column 1               
	mov  cx, 70             ; length of string               
	push cs               
	pop  es                 ; segment of string                
	mov  bp, message1       ; offset of string                
	int  0x10               ; call BIOS video service 

	;HERE WE ARE PRINTING THE 'SCORE:' STRING TO SHOW WHERE THE SCORE IS SHOWN
 	
	mov  ah, 0x13           ; service 13 - print string               
	mov  al, 1              ; subservice 01 – update cursor               
	mov  bh, 0              ; output on page 0               
	mov  bl, 7              ; normal attrib               
	mov  dx, 0x007F         ;              
	mov  cx, 7              ; length of string               
	push cs               
	pop  es                 ; segment of string                
	mov  bp, message        ; offset of string                
	int  0x10               ; call BIOS video service
	
	call printbase			;calling printbase here to create the first dino image

	xor  ax, ax               
	mov  es, ax             ; point es to IVT base   

	mov  ax, [es:9*4]               
	mov  [oldisr], ax       ; save offset of old routine               
	mov  ax, [es:9*4+2]               
	mov  [oldisr+2], ax     ; save segment of old routine               
	cli                     ; disable interrupts      
	mov  word [es:9*4], kbisr ; store offset at n*4               
	mov  [es:9*4+2], cs     ; store segment at n*4+2  
	sti                     ; enable interrupts 

	mov  dx, start          ; end of resident portion               
	add  dx, 15             ; round up to next para               
	mov  cl, 4               
	shr  dx, cl             ; number of paras                

mov  ax, 0x3100         ; terminate and stay resident               
int  0x21 
 