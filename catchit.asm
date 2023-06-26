; hello world in assembly
[org 0x0100]
jmp start
message: db '|'; string to be printed
length: dw   1 ; length of the string
message2: db '#' ; string 2 
message3: db '/' ; string 2 
message4: db ':' ; string 2 
message5: db '<' ; string 2 
message6: db '-' ; string 2 
message7: db '0' 
msg1:		db 'Please enter your name:    $'				;'$' terminated string
msg2:	    db 'Hello '
msg3:       db 'Welcome To Catch It!'
msg4:       db 'Developed By: Mahdiah Aqib (21L-5421)'
inst:       db 'Instructions'
inst2:      db 'How to move the fish?'
inst3:      db 'Press Arrow Up Key to move the fish up'
inst4:      db 'Press Arrow Down Key to move the fish down'
inst5:      db 'Press Arrow Left Key to move the fish left'
inst6:      db 'Press Arrow Right Key to move the fish right'
inst7:      db 'Rules:'
inst8:      db 'Collect green coin = 10 points and red = 50 points'
inst9:      db 'The coins will disappear after a certain time and reappear at a random point'
inst10:     db 'The fish cannot cross the boundary from above or below'
inst11:     db 'Collect maximum points to win ---- BEST OF LUCK!'
inst12:     db 'Press Enter to Continue and Esc to exit'
inst13:     db 'Are you sure you want to quit? Yes (y) No (n)'
inst14:     db 'SCORE: '
oldisr: dd 0
oldTimeisr: dd 0
fishoffset: dw 0
coin1offset: dw 0
coin2offset: dw 0
fishColumn: dw 0
;following is input buffer in format required by service
buffer2:		db 80 							; Byte # 0: Max length of buffer
db 0 											; Byte # 1: number of characters on return
times 80 db 0 									; 80 Bytes for actual buffer space
;buffer ends
buffer3: times 4000 db 0 ; space for 4000 bytes
escflag: dw 0
yesflag: dw 0
enterflag: dw 0
incTime1: dw 0 
incTime2: dw 0
hrs: dw 0
min: dw 0
sec: dw 16
randomnumber: db 0
score: dw 0
tickCount1: db 0
tickCount2: db 9
overlapflag1: dw 0
overlapflag2: dw 0

displacecoin1flag: dw 0
displacecoin2flag: dw 0

fivescounter: dw 0
tenscounter: dw 0

colorflag: db 1 ;0 color means red, 1 green

coin1NewRow: dw 0
coin1NewColumn: dw 0

coin2NewRow: dw 0
coin2NewColumn: dw 0

coin1OldRow: dw 0
coin1OldColumn: dw 0
coin1secondcounter: dw 0
coin1counterflag: dw 0

coin2OldRow: dw 0
coin2OldColumn: dw 0
coin2secondcounter: dw 0
coin2counterflag: dw 0

coin1coinoffset: dw 0
coin2coinoffset: dw 0
coin1colorflag: dw 1
coin2colorflag: dw 0

;--------------------------------------------------------------------
; subroutine to clear the screen
;--------------------------------------------------------------------
clrscr:		push es
			push ax
			push di

			mov ax, 0xb800
			mov es, ax					; point es to video base
			mov di, 0					; point di to top left column

nextloc:	mov word [es:di], 0x0720	; clear next char on screen
			add di, 2					; move to next screen location
			cmp di, 4000				; has the whole screen cleared
			jne nextloc					; if no clear next position

			pop di
			pop ax
			pop es
			ret

delay:      push cx
			mov cx, 0x00FF
loop1:		loop loop1
			mov cx, 0x00FF
loop2:		loop loop2
			pop cx
			ret


;--------------------------------------------------------------------
; subroutine to print mountain
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------

printsky:
            push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds

			mov ax, 0xb800
			mov es, ax				     ; point es to video base

			mov al, 80				     ; load al with columns per row
			mul byte [bp+12]		     ; 80 x r
			add ax, [bp+10]			     ; word number (80xr) + c
			shl ax, 1				     ; byte no (((80xr) + c)x2)

			mov di, ax				     ; point di to required location
			mov si, [bp+6]			     ; point si to string
			mov cx, 720			        
			mov ah, [bp+8]			     ; load attribute in ah

            mov al, [si]			      ; load next char of string
			rep stosw

			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 10



printgrass:

            push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds

			mov ax, 0xb800
			mov es, ax				     ; point es to video base

			mov al, 80				     ; load al with columns per row
			mul byte [bp+12]		     ; 80 x r
			add ax, [bp+10]			     ; word number (80xr) + c
			shl ax, 1				     ; byte no (((80xr) + c)x2)

			mov di, ax				     ; point di to required location
			mov si, [bp+6]			     ; point si to string
			mov cx, 80			        
			mov ah, [bp+8]			     ; load attribute in ah

            mov al, [si]			      ; load next char of string
			rep stosw

			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 10

printmountain: 
            
			push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds

			mov ax, 0xb800
			mov es, ax				; point es to video base

			mov al, 80				; load al with columns per row
			mul byte [bp+12]		     ; 80 x r
			add ax, [bp+10]			; word number (80xr) + c
			shl ax, 1				     ; byte no (((80xr) + c)x2)

			mov di, ax				     ; point di to required location
			mov si, [bp+6]			     ; point si to string
			mov cx, 2			         
			mov ah, [bp+8]			     ; load attribute in ah

firstRow:	mov al, [si]			     ; load next char of string
            rep stosw
			
			add di, 154
			mov cx, 4

secondRow:  rep stosw	                 ; repeat the operation cx times 

			add di, 150
			mov cx, 6

thirdRow:   rep stosw	                  ; repeat the operation cx times 


		    add di, 146
			mov cx, 8

fourthRow:  rep stosw	          ; repeat the operation cx times 

	        add di, 142
			mov cx, 10

fifthRow:   rep stosw	          ; repeat the operation cx times 

			add di, 138
			mov cx, 12

sixthRow:   rep stosw		      ; repeat the operation cx times 

			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 10


printsea1:  push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds

			mov ax, 0xb800
			mov es, ax				     ; point es to video base

			mov al, 80				     ; load al with columns per row
			mul byte [bp+12]		     ; 80 x r
			add ax, [bp+10]			     ; word number (80xr) + c
			shl ax, 1				     ; byte no (((80xr) + c)x2)

			mov di, ax				     ; point di to required location
			mov si, [bp+6]			     ; point si to string
			mov cx, 800			        
			mov ah, [bp+8]			     ; load attribute in ah

            mov al, [si]			      ; load next char of string
			rep stosw

			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 10


printships: push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds

			mov ax, 0xb800
			mov es, ax				; point es to video base

			mov al, 80				; load al with columns per row
			mul byte [bp+12]		; 80 x r
			add ax, [bp+10]			; word number (80xr) + c
			shl ax, 1				; byte no (((80xr) + c)x2)

			mov di, ax				     ; point di to required location
			mov si, [bp+6]			     ; point si to string
			mov cx, 1		             ; 1 for topmost of ship
			mov ah, [bp+16]			     ; load attribute in ah

FR:     	mov al, [si]			      ; load next char of string
			rep stosw                     ; repeat the operation cx times 

			add di, 158
			mov cx, 1
			mov ah, [bp+16]			     ; load attribute in ah

SR:         mov al, [si]			      ; load next char of string
			rep stosw                     ; repeat the operation cx times 

			add di, 158
			mov cx, [bp+14]              ; 30
			
			push cx

			shr cx, 1                    ;15
			
subtract:  sub di, 2
           loop subtract

            pop cx

		    mov ah, [bp+8]		

TR:         mov al, [si]			      ; load next char of string
			rep stosw   	              ; repeat the operation cx times 

            add di, 158
			mov cx, [bp+14]    
			
			push cx
			
			sub cx, 2
			
subtract2:  sub di, 2
            loop subtract2

		    pop cx
			sub cx, 2

fR:         mov al, [si]			      ; load next char of string
			rep stosw   	              ; repeat the operation cx times 

			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp

			ret 14

printsea2:  push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds

			mov ax, 0xb800
			mov es, ax				     ; point es to video base

			mov al, 80				     ; load al with columns per row
			mul byte [bp+12]		     ; 80 x r
			add ax, [bp+10]			     ; word number (80xr) + c
			shl ax, 1				     ; byte no (((80xr) + c)x2)

			mov di, ax				     ; point di to required location
			mov si, [bp+6]			     ; point si to string
			mov cx, 600			        
			mov ah, [bp+8]			     ; load attribute in ah

            mov al, [si]			      ; load next char of string
			rep stosw

			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 10


shiftrowsright: 
             push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds

calculate:  mov ax, 80 ; load chars per row in ax
			mul byte [bp+4] ; calculate source position
			mov si, ax ; load source position in si
			shl si, 1 ; convert to byte offset

			mov dx, 0 ; number of screen locations

		    mov di, 0
			add di, si

			mov dx, di
			add dx, 158 ; check for di to have reached the last column

			add di, 2

			mov ax, 0xb800
			mov es, ax ; point es to video base
			mov ds, ax ; point ds to video base
		
		    cld ; set auto increment mode


firstshift: 
               mov ax, [es:di]
		       movsw             ; move right [es:di] = [ds:si]
			                     ; di and si both updated
			   mov cx, [es:di]
			   stosw             ;ax say utha ker [es:di] mein put, di is incremented
			   add si, 2              


 nextshifts: 
               mov ax, [es:di]
               mov [es:di], cx
			   add si, 2
			   add di, 2
			   cmp dx, di
			   je lastBlockShift

			   mov cx, [es:di]
			   mov [es:di], ax
			   add si, 2
			   add di, 2
			   cmp dx, di
			   jne nextshifts

lastBlockShift: 
			    mov ax, [es:di]
				push ax
                
				mov [es:di], cx
				sub di, 158

				pop ax
				mov [es:di], ax

				mov ax, [bp+4]
				add ax, 1
				mov [bp+4], ax

				mov ax, 17
				cmp ax, [bp+4]
				jne calculate
			
			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 2	
	

;----------------------------------------------------------------------
kbisr:		push ax
            push es

			mov ax, 0xb800
			mov es, ax ; point es to video memory

			in al, 0x60 ; read a char from keyboard port

 nextcmp:   
            cmp al, 0x1c ; has the enter key been pressed
			jne checkenterflag ; no, try next comparison
			mov word[cs:enterflag], 1 ;enter was pressed
			call printeverything
			jmp nomatch ; leave interrupt routine

checkenterflag: cmp word [cs:enterflag], 1 
                jne nextcmp4                 ; means enter was not pressed at all
	
nextcmp0:
            cmp al, 0x4b ; has the left key been pressed
			jne nextcmp1 ; no, try next comparison
			call shiftfishleft
			call checkoverlap
			jmp nomatch ; leave interrupt routine
		
nextcmp1:	cmp al, 0x4d ; has the right key been pressed
			jne nextcmp2 ; no, try next comparison
			call shiftfishright
			call checkoverlap
			jmp nomatch     ; leave interrupt routine
		
nextcmp2:	cmp al, 0x50 ; has the down key been pressed
			jne nextcmp3 ; no, try next comparison
			call shiftfishdown
			call checkoverlap
			jmp nomatch ; leave interrupt routine 

nextcmp3:	cmp al, 0x48 ; has the up key been pressed
			jne nextcmp4 ; no, try next comparison
			call shiftfishup
			call checkoverlap
			jmp nomatch ; leave interrupt routine

nextcmp4:	cmp al, 0x01 ; has the esc key been pressed
			jne nextcmp5 ; no, compare next
			mov word[cs:escflag], 1 ;means esc was pressed
			call confirmationMessage
			jmp nomatch
			
nextcmp5:   cmp al, 0x15 ; y means confirm exit
			jne nextcmp6
			cmp word[cs:escflag], 1 ;esc was pressed hence y active
			jne nomatch
			mov word[cs:yesflag], 1 ;will exit the program
			mov word[enterflag], 0
			jmp nomatch

nextcmp6:   cmp al, 0x31 ; n means no
            jne nomatch
			cmp word[cs:escflag], 1 ;esc was pressed hence n active
			jne nomatch
			mov word[cs:escflag], 0
			call screenRestoreNow
			jmp nomatch   ; leave interrupt routine
		
nomatch:	mov al, 0x20
			out 0x20, al
			pop es
			pop ax
			jmp far [cs:oldisr] ; call the original ISR
			; iret
;----------------------------------------------------------------

;----------------------------------------------------------------
timer:		 push ax
             push es

			 cmp word[cs:enterflag], 1 ;1 means enter was pressed
			 jne skipall1                ; esc was pressed then dont do anything

			 cmp word[cs:escflag], 1   ;1 means esc was pressed
			 je skipall1                ; esc was pressed then dont do anything
			
goahead:	 mov ax, 1
			 push ax
			 call shiftrowsleft
			 
		     mov ax, 8
			 push ax
			 call shiftrowsright   

			 inc byte [cs:tickCount1]           ;tickcount1 add jab generating coin 1 randomly
			 cmp byte [cs:tickCount1], 80       ;tickcount2 add jab generating coin 2 randomly
			 jne xt
			 mov byte[cs:tickCount1], 0

	xt:		 inc byte [cs:tickCount2]
			 cmp byte [cs:tickCount2], 80 
			 jne gh
			 mov byte[cs:tickCount2], 0

	gh:		 inc word [cs:incTime1]        ;coin 1 counter
			 inc word [cs:incTime2]        ;coin 2 counter
             jmp P
		     
skipall1: jmp skipall

		P:	cmp word[cs:coin1colorflag], 0  ; 0=red , 1=green
			je coin1isred
			jmp coin1isgreen


coin1isred: cmp word[cs:incTime1], 89 ;since coin 1 was red check if 5 seconds passed ussko
            jna cc                 ;if 5 seconds haven't passed then do nothing with coin 1, check for coin 2 
			mov word[cs:displacecoin1flag], 1
			call PutCoin           ; if passed, replace the coin randomly
			jmp cc

coin1isgreen: 
            cmp word[cs:incTime1], 179 ;since coin 1 was green check if 10 seconds passed ussko
            jna cc                  ;if 10 seconds haven't passed then do nothing with coin 1, check for coin 2 
			mov word[cs:displacecoin1flag], 1
			call PutCoin            ;if passed, replace the coin randomly
			
cc:			cmp word[cs:coin2colorflag], 0 ;check if coin 2 is red or green
			je coin2isred           
			jmp coin2isgreen

coin2isred: cmp word[cs:incTime2], 89 ;since coin 2 was red check if 5 seconds passed ussko
            jna skipall            ;if 5 seconds haven't passed then do nothing 
			mov word[cs:displacecoin2flag], 1
			call PutCoin           ;if passed, replace the coin randomly
			jmp skipall

coin2isgreen: 
            cmp word[cs:incTime2], 179 ;since coin 2 was green check if 10 seconds passed ussko
            jna skipall             ;if 10 seconds haven't passed then do nothing with coin 1
			mov word[cs:displacecoin2flag], 1
			call PutCoin            ;if passed, replace the coin randomly

skipall:    mov al, 0x20
			out 0x20, al ; send EOI to PIC
			pop es
			pop ax
			iret
;--------------------------------------------------------------------
;--------------------------------------------------------------------
start:	
                call introductionScreen
				   
			xor ax, ax
			mov es, ax ; point es to IVT base

			;oldkbisr save
			mov ax, [es:9*4]
			mov [oldisr], ax ; save offset of old routine
			mov ax, [es:9*4+2]
			mov [oldisr+2], ax ; save segment of old routine

			;oldtimer save
			mov ax, [es:8*4]
			mov [oldTimeisr], ax ; save offset of old routine
			mov ax, [es:8*4+2]
			mov [oldTimeisr+2], ax ; save segment of old routine


			;HOOKING
			cli ; disable interrupts
			mov word [es:9*4], kbisr ; store offset at n*4
			mov [es:9*4+2], cs ; store segment at n*4+2
			mov word [es:8*4], timer ; store offset at n*4
			mov [es:8*4+2], cs ; store segment at n*4+
			sti ; enable interrupts


			a:			
            cmp word[cs:yesflag], 0   ;0 means yes not pressed, 1 pressed
			je a

			;UNHOOKING
		
			mov ax, [oldTimeisr]
			mov bx, [oldTimeisr+2]

			cli
			mov [es:8*4], ax
			mov [es:8*4+2], bx
			sti
			
			mov ax, [oldisr]
			mov bx, [oldisr+2]

			cli
			mov [es:9*4], ax
			mov [es:9*4+2], bx
			sti

			call clrscr

			mov ax, 0x4c00 
			int 0x21 