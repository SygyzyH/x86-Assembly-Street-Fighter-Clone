
IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------
; variables

; files:
        bgPath db 'ASST/bg.bmp', 0
        introPath db 'ASST/intro.bmp', 0
        filehandle dw ?
        Header db 54 dup(?)
        Palette db 256*4 dup(?)
        ScrLine db 320 dup(?)
        ErrorMsg db 'File Open Error', 13, 10,'$'
        Text_Buffer db 20000 dup(?) ; allocate 20000 bytes for reading text files ; lolmi->

; animations:
        sprtData db 50, 100 ; 52, 99 ; 52 width X 99 length
        ; !!charecter limit is 9!!
        idlPath db 'ASST/idlSnes#.txt', 0 ; saves path to idle sprite
        walkPath db 'ASST/wlkSnes#.txt', 0 ; saves path to walk sprite
        lowPath db 'ASST/lowSnes.txt', 0 ; saves path to croutch (crouching makes you lower by 40 px)
		jmpPath db 'ASST/jmpSnes#.txt', 0 ; saves path to jump sprite
		pnchPath db 'ASST/pncSnes#.txt', 0 ; saves path to punch sprite
		hitPath db 'ASST/hitSnes#.txt', 0 ; saves path to hit sprite
		winPath db 'ASST/winSnes#.txt', 0 ; saves path to win sprite

; vars:
        p1loc dw 34*320+80 ; starting location for player 1
        p2loc dw 34*320+268 ; starting location for player 2
        p1state db 0, 0, false, false, false ; idle, first frame, not crouching, not falling, not taking demage
        p2state db 0, 0, false, false, false ; idle, first frame, not crouching, not falling, not taking demage
		p1hp db 30 ; player 1 health
		p2hp db 30 ; player 2 health
        clockRefrance dw ? ; holds the game starting time
        activeKeys db 16 dup(?) ; scan code for currently active keys.
        oldInt9 dd ? ; saves the old int9h to restore after code is done
        quit db false ; if the player wishes to quit
        timeout db false ; if time runs out        
        
; equ:
        true equ 1
        false equ 0
        
        p1 equ 1
        p2 equ 2
        idle equ 0 ; if animation is 0, display idle
        walking equ 1 ; if animation is 1, display walking
        punching equ 2 ; if animation is 3, display punch
        crouching equ 3 ; if animation is 5, display crouching
		jumping equ 4 ; if animation is 6, display jumping
		hit equ 5 ; if animation is 7, display being hit
; --------------------------
CODESEG

; --------------------------
;                         general
; --------------------------

proc delay
; gets amout of hundredths of seconds to delay (up to 59 secs, 99 hundredths or )
; return when time is up
        push bp
        mov bp, sp
        
        push ax
        push bx
        push di
        push dx
        
        ; first, get starting time
        mov ax, 2c00h
        int 21h ; ch = hr, cl = min, dh = sec, dl = hundredths
        mov di, dx ; were only intrested in the seconds and hundredths
        
        
        mov ax, [bp+4]
        mov bx, 100
        xor dx, dx
        div bx ; ax = number of seconds requested, dx = number of hundredths of seconds requested
        mov bx, ax ; copy for later
        mov [bp+4], dx ; use bp+4 as a local variable
        
        delayLoop:
                mov ax, 2c00h
                int 21h ; ch = hr, cl = min, dh = sec, dl = hundredths
                sub dx, di ; get the timer back to the relative strating spot
                cmp bl, dh
                jbe delaySec ; <=
                jmp delayLoop
                delaySec:
                cmp [bp+4], dl
                jbe delayDone ; <=
        jmp delayLoop
        
        delayDone:
        pop dx
        pop di
        pop bx
        pop ax
        pop bp
        ret+2
endp delay

; --------------------------
;                    files
; --------------------------

proc OpenFile
        ; Open file
        mov ah, 3Dh
        xor al, al
        mov dx, offset bgPath
        int 21h
        jc openerror
        mov [filehandle], ax 
        ret
        openerror:
                mov dx, offset ErrorMsg
                mov ah, 9h
                int 21h
                push 200
                call delay
                jmp exit
endp OpenFile
proc ReadHeader
; Read BMP file header, 54 bytes
        mov ah,3fh
        mov bx, [filehandle]
        mov cx,54
        mov dx,offset Header
        int 21h
        ret
endp ReadHeader
proc ReadPalette
; Read BMP file color palette, 256 colors * 4 bytes (400h)
        mov ah,3fh
        mov cx,400h
        mov dx,offset Palette
        int 21h
        ret
endp ReadPalette 
proc CopyPal
; Copy the colors palette to the video memory
; The number of the first color should be sent to port 3C8h
; The palette is sent to port 3C9h
        mov si,offset Palette
        mov cx,256
        mov dx,3C8h
        mov al,0
        ; Copy starting color to port 3C8h
        out dx,al
        ; Copy palette itself to port 3C9h
        inc dx
        PalLoop:
        ; Note: Colors in a BMP file are saved as BGR values rather than RGB.
                mov al,[si+2] ; Get red value.
                shr al,2 ; Max. is 255, but video palette maximal ; HERE1
                ; value is 63. Therefore dividing by 4.
                out dx,al ; Send it.
                mov al,[si+1] ; Get green value.
                shr al,2
                out dx,al ; Send it.
                mov al,[si] ; Get blue value.
                shr al,2
                out dx,al ; Send it.
                add si,4 ; Point to next color.
                ; (There is a null chr. after every color.)
        loop PalLoop
        ret
endp CopyPal
proc CopyBitmap
; BMP graphics are saved upside-down.
; Read the graphic line by line (200 lines in VGA format),
; displaying the lines from bottom to top.
        mov cx,200
        PrintBMPLoop:
                push cx
                ; di = cx*320, point to the correct screen line
                mov di,cx
                shl cx,6
                shl di,8
                add di,cx
                ; Read one line
                mov ah,3fh
                mov cx,320
                mov dx,offset ScrLine
                int 21h
                ; Copy one line into video memory
                cld ; Clear direction flag, for movsb
                mov cx,320
                mov si,offset ScrLine 

                rep movsb ; Copy line to the screen
                ;rep movsb is same as the following code:
                ;mov es:di, ds:si
                ;inc si
                ;inc di
                ;dec cx
                ;... loop until cx=0
                ;call delay
                pop cx
        loop PrintBMPLoop
        ; close the file
        mov bx, [filehandle]
        mov ah, 3eh 
        xor al, al
        int 21h
        ret
endp CopyBitmap

proc ReadFile
; opens file in the name of first para
; saves to offset Text_Buffer and cleans it up a bit
        push bp
        mov bp, sp
        
        push ax
        push bx
        push cx
        push dx
        push di
        
        mov bx, [bp+4]
        
        mov ah, 3dh ; open the file
        mov al, 0 ; open for reading
        lea dx, [offset bx] 
        int 21h 
        mov [filehandle], ax 
        jnc ReadFileCont
        jmp openerror
        
        ReadFileCont:

        mov ah, 3fh  
        xor al, al
        lea dx, [offset Text_Buffer]
        mov cx, 20000 ; Read 20000 Bytes
        mov bx, [filehandle] 
        int 21h ; save ans in DS:DX

        mov bx, [filehandle]
        mov ah, 3eh ; close file
        xor al, al
        int 21h
        
        ; convert numbers to real numbers
        mov cx, 20000 ; loop for the whole buffer
        mov bx, offset Text_Buffer ; buffer pointer
        mov si, bx ; prep si as well
        xor ax, ax ; used for multing
        xor dx, dx ; dl = ASCII number dh = "real" number
        xor di, di ; pointer for current number to save
        ReadFileLoop:
                ; first, check if a number is a ','
                mov dl, [bx]
                cmp dl, ','
                je ReadFileSave ; if it is, save ans.
                cmp dl, 0 ; if this is the final char in the doc
                je ReadFileDone ; just cuts on SOME run time
                ; if its not a ',', add the number to the sum
                sub dl, 30h ; convert to "real" number
                mov ax, 10
                mul dh
                mov dh, al ; move ans of mul
                add dh, dl ; save "real" number to current set
                inc bx
        loop ReadFileLoop
        jmp ReadFileDone
        
        ReadFileSave:
                mov [si], dh
                inc si
                xor dx, dx
                inc bx
        loop ReadFileLoop
        
        ReadFileDone:
        ; once done, it wont detect another ',', so force a save of current value
        mov [si], dh
        inc si
        ; si now is pointing to one after the last position
        ; where a "real" number exists, bx is pointing to the last
        ; point where a value exists. just clean everything in between
        ReadFileClean:
                mov cx, bx
                sub cx, si
                xor al, al
                ReadFileCleanLoop:
                        mov [si], al
                        inc si
                loop ReadFileCleanLoop
        ; after everything is 0, finally leave
        pop di
        pop dx
        pop cx
        pop bx
        pop ax
        pop bp
        ret+2
endp ReadFile

; --------------------------
;                   graphic
; --------------------------

proc drawPic
; recive reverse (1 = true, 0 = false), offset of pic, offset pic data, draw loc on screen
; display pic
        push bp
        mov bp, sp
; to protect ax later:
        push ax
; local vars:
        ; width counter = [bp-4]
        push ax 
        ; length counter = [bp-6]
        push ax
; protect regs:
        push di
        push bx
        push cx
        
; take the length, width and end of pic:
        ; before getting img data length, save width and length localy.
        ; take img data offset
        mov di, [bp+6]
        xor bx, bx
        mov bl, [di]
        mov [bp-4], bx
        mov bl, [di+1]
        mov [bp-6], bx
        
        ; take draw loc
        mov bx, [bp+4]
        ; run for the whole pic
        mov cx, [bp-6]
        ; take start of pic
        mov di, [bp+8]
        ; check if reverse
        mov ax, [bp+10]
        cmp ax, true
        je drawPicReverse
        
        ; if not, run normally.
        drawPicLengthLoop:
                add bx, 320 ; move to next line (320px)
                sub bx, [bp-4] ; move back to the first pixel of the line
                push cx
                ; run for the whole line
                mov cx, [bp-4]
                drawPicWidthLoop:
                        mov al, [di]
                        cmp al, 0
                        je drawPicSkip
                        rol al, 4 ; color fix?
                        mov [es:bx], al
                        drawPicSkip:
                        inc bx
                        inc di
                loop drawPicWidthLoop
                pop cx
        loop drawPicLengthLoop
        ; after loop is done, skip to end so to not redraw mirrored
        jmp drawPicDone
        
        drawPicReverse:
; reverse
        ; to reverse, we read the memory the same way but print from right to left
        ; simply gets caculated the other way, and is given and offset to
        ; start in the correct place
        sub bx, [bp-4]
        drawPicReverseLengthLoop:
                add bx, 320 ; move to next line (320px)
                add bx, [bp-4] ; move back to the first pixel of the line
                push cx
                ; run for the whole line
                mov cx, [bp-4]
                drawPicReverseWidthLoop:
                        mov al, [di]
                        cmp al, 0
                        je drawPicReverseSkip
                        rol al, 4 ; color fix?
                        mov [es:bx], al
                        drawPicReverseSkip:
                        dec bx
                        inc di 
                loop drawPicReverseWidthLoop
                pop cx
        loop drawPicReverseLengthLoop
        
        drawPicDone:
        pop cx
        pop bx
        pop di
        ; pop local vars
        pop ax
        pop ax
        ; pop original val of ax
        pop ax
        pop bp
        ret+8
endp drawPic

proc drawBg
; draw the background
        call OpenFile
        call ReadHeader
        call ReadPalette
        call CopyPal
        call CopyBitmap
        ret
endp drawBg

proc printBgPartial
; pastes the part of the bg based on width and length, and offset
; takes width, length, offset on screen.
        push bp
        mov bp, sp
        
        ; protect:
        push ax
        ; local variables:
        push ax ; [bp-4]
        
        ; continue protecting:
        push bx
        push cx
        push dx
        push di
        push si
        call OpenFile
        call ReadHeader
        call ReadPalette
        call CopyPal
        
        ; first, move the reading to the correct point in file 
        ; Read Bytes up to: (320*(200-(((({screen offset}-{width})-(({screen offset}-{width})%320))/320)+{length}-1))+({screen offset}-{width})%320)
        ;        = (320*(200-(((a-(a%320))/320)+{length}-1)))+(a%320)
        mov ax, [bp+8]
        sub ax, [bp+4]
        
        mov [bp-4], ax ; [bp-4] = a
        mov bx, 320
        xor dx, dx
        div bx ; dx = a%320
        mov cx, dx ; save to ans (cx = a%320)
        
        mov ax, [bp-4]
        sub ax, dx ; ax = a-(a%320)
        mov bx, 320
        xor dx, dx
        div bx ; ax = (a-(a%320))/320
        add ax, [bp+6] ; ax += length
        dec ax ; ax--
        mov bx, 200
        xchg ax, bx
        sub ax, bx ; ax = (200-(((a-(a%320))/320)+{length}-1))
        mov bx, ax
        shl bx,6
        shl ax,8
        add ax, bx ; ax = 320*(200-(((a-(a%320))/320)+{length}-1))
        add cx, ax ; cx = a%320 + ax
        
        mov ah, 42h
        mov dx, cx ; dx low order pointer for interrupt
        xor cx, cx ; cx high order pointer
        mov al, 1 ; current location plus offset (SEEK_CUR)
        mov bx, [filehandle]
        int 21h
        
        ; di starts at offset screen loc + (320*length)
        mov cx, [bp+6]
        mov di, cx
        shl cx,6
        shl di,8
        add di, cx
        add di, [bp+8]
        mov cx, [bp+6] ; length
        ; start printing
        printBgPartialLength:
                push cx
                ; di would go up a line and back to start of pic
                sub di, 320
                sub di, [bp+4]
                
                ; read a single line
                mov ah, 3fh  
                xor al, al
                mov dx, offset ScrLine
                mov cx, [bp+4] ; Read width Bytes
                mov bx, [filehandle] 
                int 21h ; save ans in DS:DX
                cld
                
                mov cx, [bp+4]
                mov si, offset ScrLine
                rep movsb ;rep movsb is same as the following code:
                ;mov es:di, ds:si
                ;inc si
                ;inc di
                ;dec cx
                ;... loop until cx=0

                ; move to the next line
                ; 320-width
                push dx
                mov ah, 42h
                mov dx, 320
                sub dx, [bp+4]
                xor cx, cx
                mov al, 1 ; current location plus offset (SEEK_CUR)
                mov bx, [filehandle]
                int 21h
                pop dx
                pop cx
        loop printBgPartialLength
        
        ; close the file
        mov bx, [filehandle]
        mov ah, 3eh 
        xor al, al
        int 21h
        
        pop si
        pop di
        pop dx
        pop cx
        pop bx
        pop ax
        pop ax
        pop bp
        ret+6
endp printBgPartial

proc animationHandler
; take either p1 or p2 as input, displays their current state and frame.
        push bp
        mov bp, sp
        
        push ax
        push bx
        push cx
        
        mov ax, [bp+4] 
        cmp ax, p1
        je animationP1
        jmp animationP2
        
        animationP1:
				mov al, true
				cmp al, [offset p1state + 4]
				je hitP1Exe
                mov bx, offset p1state
                mov ax, idle
                cmp [bx], al ; if state = idle
                je idleP1Exe
                mov ax, walking
                cmp [bx], al ; if state = walking
                je walkingP1Exe
                mov al, crouching ; if state = crouching
                cmp [bx], al
                je crouchingP1Exe
				mov al, jumping
				cmp [bx], al
				je jumpingP1Exe
				mov al, punching
				cmp [bx], al
				je pnchP1Exe
				; if none of these are applicable, p1 must be taking damege.
        hitP1Exe:
				call hitP1
		jmp animationHandlerRet
		idleP1Exe:
                call idleP1
        jmp animationHandlerRet
        walkingP1Exe:
                call walkP1
        jmp animationHandlerRet
        crouchingP1Exe:
                call crouchP1
        jmp animationHandlerRet
		jumpingP1Exe:
				call jumpP1
		jmp animationHandlerRet
		pnchP1Exe:
				call pnchP1
		jmp animationHandlerRet
        
        animationP2:
				mov al, true
				cmp al, [offset p2state + 4]
				je hitP2Exe
                mov bx, offset p2state
                mov ax, idle
                cmp [bx], al ; if state = idle
                je idleP2Exe
                mov ax, walking
                cmp [bx], al ; if state = walking
                je walkingP2Exe
				mov ax, crouching
				cmp [bx], al
				je crouchingP2Exe
				mov ax, jumping
				cmp [bx], al
				je jumpingP2Exe
				mov ax, punching
				cmp [bx], al
				je pnchP2Exe
				; if none of these are applicable, p1 must be taking damege.
		hitP2Exe:
				call hitP2
		jmp animationHandlerRet
        idleP2Exe:
                call idleP2
        jmp animationHandlerRet
		walkingP2Exe:
				call walkP2
		jmp animationHandlerRet
		crouchingP2Exe:
				call crouchP2
		jmp animationHandlerRet
		jumpingP2Exe:
				call jumpP2
		jmp animationHandlerRet
		pnchP2Exe:
				call pnchP2
		jmp animationHandlerRet
        
        animationHandlerRet:
                pop cx
                pop bx
                pop ax
                pop bp
                ret+2

        ; animations:
		
		proc hitP1
			push ax
			push bx
			
			; update hitPath number to point to the current frame
            mov bx, offset hitPath
            add bx, 12
            mov al, [offset p1state + 1] ; current frame
            add al, 30h ; convert to ASCII
            mov [bx], al 
			
			; take img data
            push offset hitPath
            call ReadFile
			
			; delete the screen on the right place
            push [p1loc]
            xor ah, ah
            mov al, [offset sprtData + 1]
            push ax
            mov al, [offset sprtData]
            push ax
            call printBgPartial
			
			; display img
            push false ; reverse the pic
            push offset Text_Buffer
            push offset sprtData
            push [offset p1loc]
            call drawPic
			
			; update p1state
            mov al, [offset p1state + 1]
            inc al
            cmp al, 3
            je hitp1Rstp1State
            mov [offset p1state + 1], al
            jmp hitp1Ret
            hitp1Rstp1State:
            xor al, al
            mov [offset p1state + 1], al   
			mov al, false
			mov [offset p1state + 4], al
			mov al, idle
			mov [offset p1state], al
			
			hitp1Ret:
			pop bx
			pop ax
			ret
		endp hitP1
		
		proc pnchP1
			push ax
			push bx
			
				; update walkPath number to point to the current frame
                mov bx, offset pnchPath
                add bx, 12
                mov al, [offset p1state + 1] ; current frame
                add al, 30h ; convert to ASCII
                mov [bx], al 
				
				; take img data
                push offset pnchPath
                call ReadFile
				
				; delete the screen on the right place
				mov ax, [p1loc]
				add ax, 5
                push ax
                xor ah, ah
                mov al, [offset sprtData + 1]
                push ax
                mov al, [offset sprtData]
                add al, 20
				push ax
                call printBgPartial
				
				; display img
                push false ; dont reverse the pic
                push offset Text_Buffer
                push offset sprtData
                push [offset p1loc]
                call drawPic
				
				; update p1state
                mov al, [offset p1state + 1]
                inc al
                cmp al, 3
                je pnchP1RstP1State
                mov [offset p1state + 1], al
                jmp pnchP1Ret
                pnchP1RstP1State:
                xor al, al
                mov [offset p1state + 1], al
			
			pnchP1Ret:
			pop bx
			pop ax
			ret
		endp pnchP1
		
		proc jumpP1
				push ax
				push bx
				
				; update walkPath number to point to the current frame
                mov bx, offset jmpPath
                add bx, 12
                mov al, [offset p1state + 1] ; current frame
                add al, 30h ; convert to ASCII
                mov [bx], al 
				
				; take img data
                push offset jmpPath
                call ReadFile
				
				; delete the screen on the right place
				mov ax, [p1loc]
				sub ax, 2
                push ax
                xor ah, ah
                mov al, [offset sprtData + 1]
                push ax
                mov al, [offset sprtData]
                push ax
                call printBgPartial
				
				; display img
                push false ; dont reverse the pic
                push offset Text_Buffer
                push offset sprtData
                push [offset p1loc]
                call drawPic
				
				; update p1state animation
                mov al, [offset p1state + 1]
                inc al
                cmp al, 7
                je jmpP1RstP1State
                mov [offset p1state + 1], al
                jmp jmpP1Ret
                jmpP1RstP1State:
                xor al, al
                mov [offset p1state + 1], al
				
				jmpP1Ret:
						pop bx
						pop ax
						ret
		endp jumpP1
		
        proc crouchP1
                ; display p1 as crouching
                push ax
                push bx
				
                ; take img data
                push offset lowPath
                call ReadFile
                
                ; display img
                push false ; dont reverse the pic
                push offset Text_Buffer
                push offset sprtData
                push [offset p1loc]
                call drawPic
                
                pop bx
                pop ax
				ret
        endp crouchP1
        
        proc walkP1
                ; display p1 as walking
                
                push ax
                push bx
				push dx
				
                ; update walkPath number to point to the current frame
                mov bx, offset walkPath
                add bx, 12
                mov al, [offset p1state + 1] ; current frame
                add al, 30h ; convert to ASCII
                mov [bx], al 
                
                ; take img data
                push offset walkPath
                call ReadFile
                
                ; delete the screen on the right place
				mov ax, [p1loc]
				sub ax, 2
                push ax
                xor ah, ah
                mov al, [offset sprtData + 1]
                push ax
                mov al, [offset sprtData]
                push ax
                call printBgPartial
                
                ; display img
                push false ; dont reverse the pic
                push offset Text_Buffer
                push offset sprtData
                push [offset p1loc]
                call drawPic
                
                ; update p1state
                mov al, [offset p1state + 1]
                inc al
                cmp al, 5
                je walkP1RstP1State
                mov [offset p1state + 1], al
                jmp walkP1Ret
                walkP1RstP1State:
                xor al, al
                mov [offset p1state + 1], al
                
                walkP1Ret:
						pop dx
                        pop bx
                        pop ax
                        ret
        endp walkP1
        
        proc idleP1
                ; display p1 as idling
                
                push ax
                push bx
                
                ; update idlPath number to point to the current frame
                mov bx, offset idlPath
                add bx, 12
                mov al, [offset p1state + 1] ; current frame
                add al, 30h ; convert to ASCII
                mov [bx], al 
                
                ; take img data
                push offset idlPath
                call ReadFile
                
                ; delete the screen on the right place
                push [p1loc]
                xor ah, ah
                mov al, [offset sprtData + 1]
                push ax
                mov al, [offset sprtData]
                push ax
                call printBgPartial
                
                ; display img
                push false ; dont reverse the pic
                push offset Text_Buffer
                push offset sprtData
                push [offset p1loc]
                call drawPic
                
                ; update p1state
                mov al, [offset p1state + 1]
                inc al
                cmp al, 4
                je idleP1RstP1State
                mov [offset p1state + 1], al
                jmp idleP1Ret
                idleP1RstP1State:
                xor al, al
                mov [offset p1state + 1], al
                
                idleP1Ret:
                        pop bx
                        pop ax
                        ret
        endp idleP1

        proc idleP2
                ; display p2 as idling
                
                push ax
                push bx
                
                ; update idlPath number to point to the current frame
                mov bx, offset idlPath
                add bx, 12
                mov al, [offset p2state + 1] ; current frame
                add al, 30h ; convert to ASCII
                mov [bx], al 
                
                ; take img data
                push offset idlPath
                call ReadFile
                
                ; delete the screen on the right place
                push [p2loc]
                xor ah, ah
                mov al, [offset sprtData + 1]
                push ax
                mov al, [offset sprtData]
                push ax
                call printBgPartial
                
                ; display img
                push true ; reverse the pic
                push offset Text_Buffer
                push offset sprtData
                push [offset p2loc]
                call drawPic
                
                ; update p2state
                mov al, [offset p2state + 1]
                inc al
                cmp al, 4
                je idlep2Rstp2State
                mov [offset p2state + 1], al
                jmp idlep2Ret
                idlep2Rstp2State:
                xor al, al
                mov [offset p2state + 1], al
                
                idlep2Ret:
                        pop bx
                        pop ax
                        ret
        endp idleP2
		
		proc walkP2
                ; display p2 as walking
                
                push ax
                push bx
				push dx
                
				; check if walk is legal and in border
				mov ax, [offset p2loc]
				sub ax, 50
				mov bx, 320
				xor dx, dx
				div bx ; p2loc/320
				cmp dx, 0
				jne walkp2legalL
				mov ax, [offset p2loc]
				add ax, 10
				mov [offset p2loc], ax
				walkp2legalL:
				mov ax, [offset p2loc]
				xor dx, dx
				div bx ; p2loc/320
				cmp dx, 310
				jbe walkp2legalR ; >=
				mov ax, [offset p2loc]
				sub ax, 10
				mov [offset p2loc], ax
				walkp2legalR:
				
                ; update walkPath number to point to the current frame
                mov bx, offset walkPath
                add bx, 12
                mov al, [offset p2state + 1] ; current frame
                add al, 30h ; convert to ASCII
                mov [bx], al 
                
                ; take img data
                push offset walkPath
                call ReadFile
                
                ; delete the screen on the right place
				mov ax, [p2loc]
				sub ax, 2
                push ax
                xor ah, ah
                mov al, [offset sprtData + 1]
                push ax
                mov al, [offset sprtData]
                push ax
                call printBgPartial
                
                ; display img
                push true ; reverse the pic
                push offset Text_Buffer
                push offset sprtData
                push [offset p2loc]
                call drawPic
                
                ; update p2state
                mov al, [offset p2state + 1]
                inc al
                cmp al, 5
                je walkp2Rstp2State
                mov [offset p2state + 1], al
                jmp walkp2Ret
                walkp2Rstp2State:
                xor al, al
                mov [offset p2state + 1], al
                
                walkp2Ret:
						pop dx
                        pop bx
                        pop ax
                        ret
        endp walkP2
		
		proc crouchP2
                ; display p2 as crouching
                push ax
                push bx
				
                ; take img data
                push offset lowPath
                call ReadFile
                
                ; display img
                push true ; dont reverse the pic
                push offset Text_Buffer
                push offset sprtData
                push [offset p2loc]
                call drawPic
                
                pop bx
                pop ax
				ret
        endp crouchP2
		
		proc jumpP2
				push ax
				push bx
				
				; update walkPath number to point to the current frame
                mov bx, offset jmpPath
                add bx, 12
                mov al, [offset p2state + 1] ; current frame
                add al, 30h ; convert to ASCII
                mov [bx], al 
				
				; take img data
                push offset jmpPath
                call ReadFile
				
				; delete the screen on the right place
				mov ax, [p2loc]
				sub ax, 2
                push ax
                xor ah, ah
                mov al, [offset sprtData + 1]
                push ax
                mov al, [offset sprtData]
                push ax
                call printBgPartial
				
				; display img
                push true ; reverse the pic
                push offset Text_Buffer
                push offset sprtData
                push [offset p2loc]
                call drawPic
				
				; update p2state animation
                mov al, [offset p2state + 1]
                inc al
                cmp al, 7
                je jmpp2Rstp2State
                mov [offset p2state + 1], al
                jmp jmpp2Ret
                jmpp2Rstp2State:
                xor al, al
                mov [offset p2state + 1], al
				
				jmpp2Ret:
						pop bx
						pop ax
						ret
		endp jumpP2
		
		proc pnchP2
			push ax
			push bx
			
				; update walkPath number to point to the current frame
                mov bx, offset pnchPath
                add bx, 12
                mov al, [offset p2state + 1] ; current frame
                add al, 30h ; convert to ASCII
                mov [bx], al 
				
				; take img data
                push offset pnchPath
                call ReadFile
				
				; delete the screen at the right place
				mov ax, [p2loc]
				add ax, 10
                push ax
                xor ah, ah
                mov al, [offset sprtData + 1]
                push ax
                mov al, [offset sprtData]
                add al, 20
				push ax
                call printBgPartial
				
				; display img
                push true ; reverse the pic
                push offset Text_Buffer
                push offset sprtData
                push [offset p2loc]
                call drawPic
				
				; update p2state
                mov al, [offset p2state + 1]
                inc al
                cmp al, 3
                je pnchp2Rstp2State
                mov [offset p2state + 1], al
                jmp pnchp2Ret
                pnchp2Rstp2State:
                xor al, al
                mov [offset p2state + 1], al
			
			pnchp2Ret:
			pop bx
			pop ax
			ret
		endp pnchP2
		
		proc hitP2
			push ax
			push bx
			
			; update hitPath number to point to the current frame
            mov bx, offset hitPath
            add bx, 12
            mov al, [offset p2state + 1] ; current frame
            add al, 30h ; convert to ASCII
            mov [bx], al 
			
			; take img data
            push offset hitPath
            call ReadFile
			
			; delete the screen on the right place
            push [p2loc]
            xor ah, ah
            mov al, [offset sprtData + 1]
            push ax
            mov al, [offset sprtData]
            push ax
            call printBgPartial
			
			; display img
            push true ; reverse the pic
            push offset Text_Buffer
            push offset sprtData
            push [offset p2loc]
            call drawPic
			
			; update p2state
            mov al, [offset p2state + 1]
            inc al
            cmp al, 3
            je hitp2Rstp2State
            mov [offset p2state + 1], al
            jmp hitp2Ret
            hitp2Rstp2State:
            xor al, al
            mov [offset p2state + 1], al   
			mov al, false
			mov [offset p2state + 4], al
			mov al, idle
			mov [offset p2state], al
			
			hitp2Ret:
			pop bx
			pop ax
			ret
		endp hitP2
endp animationHandler

; --------------------------
;                  mechanics
; --------------------------

proc specialActionHandler
; if a special action is called, it will be handled here
        push ax
        push bx
        
        ; check if player wants to quit
        mov al, [offset quit]
        cmp al, true
        jne specialActionHandlerHpP1
        jmp exit
        
		;check if a player has lost
		specialActionHandlerHpP1:
		mov al, [offset p1hp]
		cmp al, 0
		jne specialActionHandlerHpP2
		push p2
		call gameWon
		specialActionHandlerHpP2:
		mov al, [offset p2hp]
		cmp al, 0
		jne specialActionHandlerTimeout
		push p1
		call gameWon
		
        ; check if time out
        specialActionHandlerTimeout:
        mov al, [offset timeout]
        cmp al, true
        jne specialActionHandlerRet
        jmp exit
        ; !!!SHOULD DISPLAY TIMEOUT SCREEN!!!
        
        specialActionHandlerRet:
        pop bx
        pop ax
        ret
endp specialActionHandler

proc clock
; checks if time is out, if so, updates variables
        push ax
        push cx
        push dx
        
        ; get current system time
        mov ax, 2c00h
        int 21h ; ch = hr, cl = min, dh = sec, dl = hundredths
        ; get strting time
        mov ah, [offset clockRefrance] ; mins
        mov al, [offset clockRefrance + 1] ; secs
        ; get diffrance
        sub cl, ah
        sub dh, al
        ; get time in secs
        xor ah, ah
        mov al, 60
        mul cl ; ax = min*60
        mov dh, dl
        xor dh, dh
        add ax, dx ; ax = min*60+sec
        cmp ax, 99 ; if time >= 99 secs since game start
        jbe clockRet ; >=
        mov al, true
        mov [offset timeout], al
        
        clockRet:
        pop dx
        pop cx
        pop ax
        ret
endp clock

proc inputHandler
; handles input by keyboard, by looking at active keys 
; and updating player states and movement.
; can replace int9h with costume ISR and restore later.
        push ax
        push bx
        push cx ; keeps count of how many times the loop did nothing
		push dx
        
		mov al, [offset p1state + 2]
        cmp al, false ; if player is still croutched, uncroutch
        je inputHandlerDntHigh
        mov al, false
        mov [offset p1state + 2], al
        mov ax, [offset p1loc]
        sub ax, 33*320
        mov [offset p1loc], ax
        inputHandlerDntHigh:
		; gravity here
		mov ax, [offset p1loc]
		cmp ax, 67*320 ; 10560
		jnb inpuHandlerGroundedP1			
		add ax, 10*320
		mov [offset p1loc], ax
		jmp inputHandlerJmpUp
		inpuHandlerGroundedP1:
		; make sure player is leveled with the ground
		mov ax, [offset p1loc]
		xor dx, dx
		mov cx, 320
		div cx
		mov ax, 74*320
		add ax, dx
		mov [offset p1loc], ax
		; if p1 has reached ground, he is allowed to jump again
		mov al, true
		cmp [offset p1state + 3], al
		jne inpuHandlerP1JustGrounded
		mov al, idle
		mov [offset p1state], al
		inpuHandlerP1JustGrounded:
		mov al, false
		mov [offset p1state + 3], al
		inputHandlerJmpUp:
		mov al, [offset p1state]
		cmp al, jumping
		jne checkP2
		mov ax, [offset p1loc]
		; if player has reached max height:
		xor dx, dx
		mov bx, 320
		div bx
		cmp ax, 34
		jb inpuHandlerFalling ; <=
		; only go up if p1 is not falling
		mov al, true
		cmp [offset p1state + 3], al
		je inpuHandlerFalling
		; clear prev position
		push [p1loc]
        xor ah, ah
        mov al, [offset sprtData + 1]
        push ax
        mov al, [offset sprtData]
        push ax
        call printBgPartial
		mov ax, [offset p1loc]
		sub ax, 4*(8*320) ; jump force * gravity+320
		mov [offset p1loc], ax
		jmp checkP2
		inpuHandlerFalling:
		; change falling to true
		mov al, true
		mov [offset p1state + 3], al
		
		checkP2:
		xor cx, cx
		mov al, [offset p2state + 2]
        cmp al, false ; if player is still croutched, uncroutch
        je inputHandlerDntHighP2
        mov al, false
        mov [offset p2state + 2], al
        mov ax, [offset p2loc]
        sub ax, 33*320
        mov [offset p2loc], ax
        inputHandlerDntHighP2:
		; gravity here
		mov ax, [offset p2loc]
		cmp ax, 67*320 ; 10560
		jnb inpuHandlerGroundedP2			
		add ax, 10*320
		mov [offset p2loc], ax
		jmp inputHandlerJmpUpP2
		inpuHandlerGroundedP2:
		; make sure player is leveled with the ground
		mov ax, [offset p2loc]
		xor dx, dx
		mov cx, 320
		div cx
		mov ax, 74*320
		add ax, dx
		mov [offset p2loc], ax
		; if p2 has reached ground, he is allowed to jump again
		mov al, true
		cmp [offset p2state + 3], al
		jne inpuHandlerP2JustGrounded
		mov al, idle
		mov [offset p2state], al
		inpuHandlerP2JustGrounded:
		mov al, false
		mov [offset p2state + 3], al
		inputHandlerJmpUpP2:
		mov al, [offset p2state]
		cmp al, jumping
		jne gameTickCheckRet
		mov ax, [offset p2loc]
		; if player has reached max height:
		xor dx, dx
		mov bx, 320
		div bx
		cmp ax, 34
		jb inpuHandlerFallingP2 ; <=
		; only go up if p2 is not falling
		mov al, true
		cmp [offset p2state + 3], al
		je inpuHandlerFallingP2
		; clear prev position
		push [p2loc]
        xor ah, ah
        mov al, [offset sprtData + 1]
        push ax
        mov al, [offset sprtData]
        push ax
        call printBgPartial
		mov ax, [offset p2loc]
		sub ax, 4*(8*320) ; jump force * gravity + 320
		mov [offset p2loc], ax
		jmp gameTickCheckRet
		inpuHandlerFallingP2:
		; change falling to true
		mov al, true
		mov [offset p2state + 3], al
		
		gameTickCheckRet:
		
        ; go through all active keys, act accordingly.
        ; bx is bouth pointer and counter for loop
        xor bx, bx
        xor cx, cx
		inputHandlerLoop:
                mov al, [offset activeKeys + bx] ; take button press
                cmp al, false ; if this button is not pressed, look to next one
                jne inputHandlerS
                cmp bx, 4
				jbe inputHandlerNoActionP1; <=
				inc cl
                jmp inputHandlerLoopNext
				inputHandlerNoActionP1:
				inc ch
				jmp inputHandlerLoopNext
                
                inputHandlerS:
						cmp bx, 0 ; index 0 = 's'
                        jne inputHandlerA ; if not, check next
                        
						; clear prev position
						push [p1loc]
                        xor ah, ah
                        mov al, [offset sprtData + 1]
                        push ax
                        mov al, [offset sprtData]
                        push ax
                        call printBgPartial
						
                        ; set crouching to true
                        mov al, true
                        mov [offset p1state + 2], al 
                        
                        ; update player location if not already updated
                        mov ax, [offset p1loc]
                        add ax, 33*320
                        mov [offset p1loc], ax
                        
                        ; update player state
                        mov al, crouching
                        mov [offset p1state], al
                        xor al, al
                        mov [offset p1state + 1], al
                        
                        jmp inputHandlerK ; once done, no need to check others
                
                inputHandlerA:
						mov al, [offset p1state + 2]
						cmp al, true
						jne inputHandlerANotCrouched
						jmp inputHandlerW
                        inputHandlerANotCrouched:
						mov al, [offset p1state]
						cmp al, punching
						jne inputHandlerANext
						jmp inputHandlerW
						inputHandlerANext:
                        cmp bx, 1 ; index 1 = 'a'
                        jne inputHandlerW ; if not, check next
                        
                        ; remove bg
                        push [p1loc]
                        xor ah, ah
                        mov al, [offset sprtData + 1]
                        push ax
                        mov al, 10
                        push ax
                        call printBgPartial
                        
						push ax
						push bx
						push dx
						; if p1loc%320 - 50 < 11
						mov ax, [offset p1loc]
						xor dx, dx
						mov bx, 320
						div bx
						mov ax, dx
						sub ax, 50
						cmp ax, 11
						jnb walkP1RightLegal; >=
						mov ax, [offset p1loc]
						add ax, 10
						mov [offset p1loc], ax
						walkP1RightLegal:
						pop dx
						pop bx
						pop ax
						 
                        ; update player location
                        mov ax, [offset p1loc]
                        sub ax, 5
                        mov [offset p1loc], ax
                        
                        ; update animation state
                        mov al, [offset p1state]
                        cmp al, walking ; if animation already walking, no need to reset it
                        je inputHandlerWalkASkip
                        
						; change animation as long as p1 is not jumping
						cmp al, jumping
						je inputHandlerWalkAMomentumBoost
                        mov al, walking
                        mov [offset p1state], al
                        xor al, al
                        mov [offset p1state + 1], al
						
                        inputHandlerWalkAMomentumBoost:
						; update player location again to give more momentum while jumping
						; remove bg
                        mov ax, [p1loc]
                        sub ax, 65 ; width of actual charecter
                        push ax
                        xor ah, ah
                        mov al, [offset sprtData + 1]
						add al, 25
                        push ax
                        mov al, 10
                        push ax
                        call printBgPartial

                        mov ax, [offset p1loc]
                        sub ax, 5
                        mov [offset p1loc], ax
                        inputHandlerWalkASkip:
                        jmp inputHandlerK ; once done, no need to check others
                
                inputHandlerW:
						mov al, [offset p1state + 2]
						cmp al, true
						jne inputHandlerWNotCrouching
						jmp inputHandlerD
						inputHandlerWNotCrouching:
						mov al, [offset p1state]
						cmp al, punching
						jne inputHandlerWNext
						jmp inputHandlerD
                        inputHandlerWNext:
                        cmp bx, 2 ; index 2 = 'w'
                        jne inputHandlerD ; if not, check next
                        
						; remove bg
                        push [p1loc]
                        xor ah, ah
                        mov al, [offset sprtData + 1]
                        push ax
                        mov al, 5
                        push ax
                        call printBgPartial
						
						; update animation state
                        mov al, [offset p1state]
                        cmp al, jumping ; if animation already jumping, no need to reset it
                        je inputHandlerJumpWSkip
                        
                        mov al, jumping
                        mov [offset p1state], al
                        xor al, al
                        mov [offset p1state + 1], al ; reset frames
                        
                        inputHandlerJumpWSkip:
                        jmp inputHandlerK ; once done, no need to check others
                
                inputHandlerD:
						mov al, [offset p1state + 2]
						cmp al, true
						jne inputHandlerDNotCrouching
						jmp inputHandlerE
						inputHandlerDNotCrouching:
						mov al, [offset p1state]
						cmp al, punching
						jne inputHandlerDNext
						jmp inputHandlerE
                        inputHandlerDNext:
                        cmp bx, 3 ; index 3 = 'd'
                        je inputHandlerDCont ; if not, check next
                        jmp inputHandlerE
						inputHandlerDCont:
						
                        ; remove bg
                        mov ax, [p1loc]
                        sub ax, 50 ; width of actual charecter
                        push ax
                        xor ah, ah
                        mov al, [offset sprtData + 1]
                        push ax
                        mov al, 10
                        push ax
                        call printBgPartial
                        
						
						push ax
						push bx
						push dx
						; check if walk is legal
						; if (p1 loc%320 + 50) < p2loc%320
						mov ax, [offset p1loc]
						xor dx, dx
						mov bx, 320
						div bx
						mov ax, dx
						add ax, 50
						push ax ; (p1loc%320 + 50)
						mov ax, [offset p2loc]
						xor dx, dx
						mov bx, 320
						div bx
						mov bx, dx ; p2loc%320
						pop ax ; (p1loc%320 + 50)
						cmp bx, ax
						jnb walkP1legalP2 ; >=
						mov ax, [offset p1loc]
						sub ax, 10
						mov [offset p1loc], ax
						walkP1legalP2:
						pop dx
						pop bx
						pop ax
						
                        ; update player location
                        mov ax, [offset p1loc]
                        add ax, 5
                        mov [offset p1loc], ax
                        
                        ; update animation state
                        mov al, [offset p1state]
                        cmp al, walking ; if animation already walking, no need to reset it
                        je inputHandlerWalkDSkip
                        
						; change animation as long as p1 is not jumping
						cmp al, jumping
						je inputHandlerWalkDMomentumBoost
                        mov al, walking
                        mov [offset p1state], al
                        xor al, al
                        mov [offset p1state + 1], al
                        
						inputHandlerWalkDMomentumBoost:
						; update player location again to give more momentum while jumping
						; remove bg
                        mov ax, [offset p1loc]
						sub ax, 25
                        push ax
                        xor ah, ah
                        mov al, [offset sprtData + 1]
						add al, 25
                        push ax
						mov al, [offset sprtData] 
                        push ax
                        call printBgPartial 

                        mov ax, [offset p1loc]
                        add ax, 5
                        mov [offset p1loc], ax
                        inputHandlerWalkDSkip:
                        jmp inputHandlerK ; once done, no need to check others
				inputHandlerE:
						mov al, [offset p1state + 2]
						cmp al, true
						jne inputHandlerENotCrouching
						jmp inputHandlerK
						inputHandlerENotCrouching:
						cmp bx, 4 ; index 4 = 'e'
                        jne inputHandlerK ; if not, check next
						
						; update player state only if nessacery
						mov al, [offset p1state]
						cmp al, punching
						je inputHandlerEUpdatePlayerHp
						
						mov al, punching
						mov [offset p1state], al
						xor al, al
                        mov [offset p1state + 1], al ; reset frames
						
						inputHandlerEUpdatePlayerHp:
						mov al, [offset p1state + 1]
						cmp al, 1 ; only change hp if this sprite is frame #2 (index 1)
						jne inputHandlerESkip
						mov ax, [offset p1loc]
						mov dx, [offset p2loc]
						sub ax, dx ; get distance
						add ax, 50 ; account for model size
						cmp ax, 50 ; if distance > 50
						jae inputHandlerESkip ; <=
						mov al, true
						mov [offset p2state + 4], al ; change p2 to take demage
						mov al, [offset p2hp] ; remove from player 2's health points
						sub al, 5
						mov [offset p2hp], al
						mov al, hit
						mov [offset p2state], al ; change player 2's sprite to hit
						xor al, al
						mov [offset p2state + 1], al ; reset frames
						
						inputHandlerESkip:
						jmp inputHandlerK ; once done, no need to check others
						
				inputHandlerK:
						cmp bx, 5 ; index 5 = 'k'
						jne inputHandlerJ ; if not, check next
                       
						; clear prev position
						push [p2loc]
						xor ah, ah
						mov al, [offset sprtData + 1]
						push ax
						mov al, [offset sprtData]
						push ax
						call printBgPartial
						
						; set crouching to true
						mov al, true
						mov [offset p2state + 2], al 
                        
						; update player location if not already updated
						mov ax, [offset p2loc]
						add ax, 33*320
						mov [offset p2loc], ax
                       
						; update player state
						mov al, crouching
						mov [offset p2state], al
						xor al, al
						mov [offset p2state + 1], al
                        
						jmp inputHandlerLoopNext ; once done, no need to check others
						
				inputHandlerJ:
						mov al, [offset p2state + 2]
						cmp al, true
						jne inputHandlerJNotCrouched
						jmp inputHandlerI
                        inputHandlerJNotCrouched:
						mov al, [offset p2state]
						cmp al, punching
						jne inputHandlerJNext
						jmp inputHandlerI
						inputHandlerJNext:
                        cmp bx, 6 ; index 6 = 'j'
                        jne inputHandlerI ; if not, check next
                        
                        ; remove bg
                        push [p2loc]
                        xor ah, ah
                        mov al, [offset sprtData + 1]
                        push ax
                        mov al, 10
                        push ax
                        call printBgPartial
                        
						push ax
						push bx
						push dx
						; check if walk is legal
						; if (p1 loc%320 + 50) < p2loc%320
						mov ax, [offset p2loc]
						xor dx, dx
						mov bx, 320
						div bx
						mov ax, dx
						sub ax, 50
						push ax ; (p2loc%320 - 50)
						mov ax, [offset p1loc]
						xor dx, dx
						mov bx, 320
						div bx
						mov bx, dx ; p2loc%320
						pop ax ; (p1loc%320 - 50)
						cmp bx, ax
						jna walkP2legalP1 ; >=
						mov ax, [offset p2loc]
						add ax, 10
						mov [offset p2loc], ax
						walkP2legalP1:
						pop dx
						pop bx
						pop ax
						
                        ; update player location
                        mov ax, [offset p2loc]
                        sub ax, 5
                        mov [offset p2loc], ax
                        
                        ; update animation state
                        mov al, [offset p2state]
                        cmp al, walking ; if animation already walking, no need to reset it
                        je inputHandlerWalkJSkip
                        
						; change animation as long as p2 is not jumping
						cmp al, jumping
						je inputHandlerWalkJMomentumBoost
                        mov al, walking
                        mov [offset p2state], al
                        xor al, al
                        mov [offset p2state + 1], al
						
                        inputHandlerWalkJMomentumBoost:
						; update player location again to give more momentum while jumping
						; remove bg
                        mov ax, [p2loc]
                        push ax
                        xor ah, ah
                        mov al, [offset sprtData + 1]
						add al, 25
                        push ax
                        mov al, [offset sprtData]
                        push ax
                        call printBgPartial

                        mov ax, [offset p2loc]
                        sub ax, 5
                        mov [offset p2loc], ax
                        inputHandlerWalkJSkip:
                        jmp inputHandlerLoopNext ; once done, no need to check others
						
				inputHandlerI:
						mov al, [offset p2state + 2]
						cmp al, true
						jne inputHandlerINotCrouching
						jmp inputHandlerL
						inputHandlerINotCrouching:
						mov al, [offset p2state]
						cmp al, punching
						jne inputHandlerINext
						jmp inputHandlerL
                        inputHandlerINext:
                        cmp bx, 7 ; index 7 = 'i'
                        jne inputHandlerL ; if not, check next
                        
						; remove bg
                        push [p2loc]
                        xor ah, ah
                        mov al, [offset sprtData + 1]
                        push ax
                        mov al, 5
                        push ax
                        call printBgPartial
						
						; update animation state
                        mov al, [offset p2state]
                        cmp al, jumping ; if animation already jumping, no need to reset it
                        je inputHandlerJumpISkip
                        
                        mov al, jumping
                        mov [offset p2state], al
                        xor al, al
                        mov [offset p2state + 1], al ; reset frames
                        
                        inputHandlerJumpISkip:
                        jmp inputHandlerLoopNext ; once done, no need to check others
						
				inputHandlerL:
						mov al, [offset p2state + 2]
						cmp al, true
						jne inputHandlerLNotCrouching
						jmp inputHandlerU
						inputHandlerLNotCrouching:
						mov al, [offset p2state]
						cmp al, punching
						jne inputHandlerLNext
						jmp inputHandlerU
                        inputHandlerLNext:
                        cmp bx, 8 ; index 8 = 'l'
                        jne inputHandlerU ; if not, check next
                        
                        ; remove bg
                        mov ax, [p2loc]
                        sub ax, 50 ; width of actual charecter
                        push ax
                        xor ah, ah
                        mov al, [offset sprtData + 1]
                        push ax
                        mov al, 10
                        push ax
                        call printBgPartial
                        
						push ax
						push bx
						push dx
						; if p2loc%320 - 50 < 309
						mov ax, [offset p2loc]
						xor dx, dx
						mov bx, 320
						div bx
						mov ax, dx
						sub ax, 45
						cmp ax, 265
						jb walkP2LeftLegal ; >=
						mov ax, [offset p2loc]
						sub ax, 10
						mov [offset p2loc], ax
						walkP2LeftLegal:
						pop dx
						pop bx
						pop ax
						
						
                        ; update player location
                        mov ax, [offset p2loc]
                        add ax, 5
                        mov [offset p2loc], ax
                        
                        ; update animation state
                        mov al, [offset p2state]
                        cmp al, walking ; if animation already walking, no need to reset it
                        je inputHandlerWalkLSkip
                        
						; change animation as long as p2 is not jumping
						cmp al, jumping
						je inputHandlerWalkLMomentumBoost
                        mov al, walking
                        mov [offset p2state], al
                        xor al, al
                        mov [offset p2state + 1], al
                        
						inputHandlerWalkLMomentumBoost:
						; update player location again to give more momentum while jumping
						; remove bg
                        mov ax, [p2loc]
                        sub ax, 25 ; width of actual charecter
                        push ax
                        xor ah, ah
                        mov al, [offset sprtData + 1]
						add al, 25
                        push ax
                        mov al, 10
                        push ax
                        call printBgPartial

                        mov ax, [offset p2loc]
                        add ax, 5
                        mov [offset p2loc], ax
                        inputHandlerWalkLSkip:
                        jmp inputHandlerLoopNext ; once done, no need to check others
						
				inputHandlerU:
						mov al, [offset p2state + 2]
						cmp al, true
						jne inputHandlerUNotCrouching
						jmp inputHandlerUSkip
						inputHandlerUNotCrouching:
						cmp bx, 9 ; index 9 = 'u'
                        jne inputHandlerUSkip ; if not, check next
						
						; update player state only if nessacery
						mov al, [offset p2state]
						cmp al, punching
						je inputHandlerUUpdatePlayerHp
						
						mov al, punching
						mov [offset p2state], al
						xor al, al
                        mov [offset p2state + 1], al ; reset frames
						
						inputHandlerUUpdatePlayerHp:
						mov al, [offset p2state + 1]
						cmp al, 1 ; only change hp if this sprite is frame #2 (index 1)
						jne inputHandlerUSkip
						mov ax, [offset p2loc]
						mov dx, [offset p1loc]
						sub ax, dx ; get distance
						cmp ax, 55 ; if distance > 55
						jae inputHandlerUSkip ; <=
						mov al, true
						mov [offset p1state + 4], al ; change p2 to take demage
						mov al, [offset p1hp] ; remove from player 2's health points
						sub al, 5
						mov [offset p1hp], al
						mov al, hit
						mov [offset p1state], al ; change player 2's sprite to hit
						xor al, al
						mov [offset p1state + 1], al ; reset frames
						
						inputHandlerUSkip:
						
        inputHandlerLoopNext:
        inc bx
        cmp bx, 16
        je inputHandlerDone
        jmp inputHandlerLoop
        
        inputHandlerDone:
        cmp ch, 5 ; if p1 did less than 5 nothings, than an action was done. so dont change state
        jne inputHandlerP2Done
        mov al, idle
        mov [offset p1state], al
        mov al, [offset p1state + 1]
        cmp al, 3
        jbe inputHandlerP2Done ; <=
        xor al, al
        mov [offset p1state + 1], al
		inputHandlerP2Done:
		cmp cl, 11
		jne inputHandlerRet
		mov al, idle
		mov [offset p2state], al
		mov al, [offset p2state + 1]
		cmp al, 3
		jbe inputHandlerRet
		xor al, al
		mov [offset p2state + 1], al
        inputHandlerRet:
		pop dx
        pop cx
        pop bx
        pop ax
        ret
        
        proc newInt9
        ; custom keyboard ISR
        ;        . made by NH 
                ; first, save all registers
                push ax
                push bx
                push cx
                push dx
                push di
                push si
                
                ; == update active keys according to press == 
                in al, 60h ; read keyboard scan code
                ; update keyboard state
                xor bh, bh
                mov bl, al ; al = make / break codes
                and bl, 7Fh ; bx = make codes
                
                cmp bl, 1fh ; if click on s (index 0 in array activeKeys)
                jne newInt9CheckA
                mov bl,0
                jmp newInt9EndCheck
                
                newInt9CheckA:
                        cmp bl, 1eh        ; if click on a (index 1 in array activeKeys)
                        jne newInt9CheckW
                        mov bl,1
                        jmp newInt9EndCheck
                
                newInt9CheckW:
                        cmp bl, 11h        ; if click on w (index 2 in array activeKeys)
                        jne newInt9CheckD
                        mov bl,2
                        jmp newInt9EndCheck
                
                newInt9CheckD:
                        cmp bl, 20h ; if click on d (index 3 in array activeKeys)
                        jne newInt9CheckE
                        mov bl,3
                        jmp newInt9EndCheck
                        
                newInt9CheckE:
                        cmp bl, 12h ; if click on e (index 4 in array activeKeys)
                        jne newInt9CheckK
                        mov bl, 4
                        jmp newInt9EndCheck
                        
                newInt9CheckK:
                        cmp bl, 25h ; if click on q (index 5 in array activeKeys)
                        jne newInt9CheckJ
                        mov bl, 5
                        jmp newInt9EndCheck
                        
                newInt9CheckJ:
                        cmp bl, 24h ; if click on f (index 6 in array activeKeys)
                        jne newInt9CheckI
                        mov bl, 6
                        jmp newInt9EndCheck
						
				newInt9CheckI:
						cmp bl, 17h ; if click on f (index 7 in array activeKeys)
                        jne newInt9CheckL
                        mov bl, 7
                        jmp newInt9EndCheck
						
				newInt9CheckL:
						cmp bl, 26h ; if click on f (index 8 in array activeKeys)
                        jne newInt9CheckU
                        mov bl, 8
                        jmp newInt9EndCheck
						
				newInt9CheckU:
						cmp bl, 16h ; if click on f (index 9 in array activeKeys)
                        jne newInt9CheckEsc
                        mov bl, 9
                        jmp newInt9EndCheck
						
                newInt9CheckEsc:
                        cmp bl, 1h ; if click on esc
                        jne newInt9EndCheck ; if failed checking, dont update array
                        mov [byte ptr cs:quit], true
                
                newInt9EndCheck:
                        shr al, 7 ; al = 0 if pressed, 1 if released
                        xor al, 1 ; al = 1 if pressed, 0 if released
                        mov [cs:activeKeys + bx], al ; save pressed buttons in array activeKeys
                ; ============== end of updating ===============
                
                ; send EOI to keyboard
                in al, 61h
                mov ah, al
                or al, 80h
                out 61h, al
                mov al, ah
                out 61h, al
                
                ; send EOI to PIC
                mov al, 20h
                out 20h, al
                
                pop si
                pop di
                pop dx
                pop cx
                pop bx
                pop ax
           
                iret
        endp newInt9
        
        proc replaceInt9
                cli ; disable interrupts, in case any come through half way
                push ax
                push es
                
                xor ax, ax
                mov es, ax
                
                mov ax, [word ptr es:9*4 + 2] ; copy first part of pointer to int9h 00 00 00 00
                mov [word ptr oldInt9 + 2], ax
                mov ax, [word ptr es:9*4] ; copy second part of pointer to int9h
                mov [word ptr oldInt9], ax
                
                mov [word ptr es:9*4], offset newInt9 ; save new code to int9h instead
                mov [es:9*4 + 2], cs
                
                pop es
                pop ax
                sti
                ret
        endp replaceInt9

        proc restoreInt9
                cli ; disable interrupts, in case any come through half way
                push ax
                push es
                
                xor ax, ax
                mov es, ax
                
                mov ax, [word ptr oldInt9 + 2]
                mov [word ptr es:9*4 + 2], ax
                mov ax, [word ptr oldInt9]
                mov [word ptr es:9*4], ax
                
                pop es
                pop ax
                sti
                ret
        endp restoreInt9
endp inputHandler

proc gameWon
; takes as parameter either p1 or p2
; shows an animation of losing player dying, and winning player.
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	
	xor ax, ax
	mov [offset p1state + 1], al
	mov [offset p2state + 1], al
	
	mov ax, [bp+4]
	cmp ax, p2
	jne gameWonP1
	jmp gameWonP2
	
	gameWonP1:
	; if p1 won:
	; update winPath number to point to the current frame
    mov bx, offset winPath
    add bx, 12
    mov al, [offset p1state + 1] ; current frame
    add al, 30h ; convert to ASCII
    mov [bx], al 
    
    ; take img data
    push offset winPath
    call ReadFile
    
	; delete the screen on the right place
    push [p1loc]
    xor ah, ah
    mov al, [offset sprtData + 1]
    push ax
    mov al, [offset sprtData]
    push ax
    call printBgPartial
    
    ; display img
    push false ; dont reverse the pic
    push offset Text_Buffer
    push offset sprtData
    push [offset p1loc]
    call drawPic
	
	mov al, [offset p1state + 1]
	inc al
	mov [offset p1state + 1], al
	
	push 10
	call delay
	
	mov bx, offset winPath
    add bx, 12
    mov al, [offset p1state + 1] ; current frame
    add al, 30h ; convert to ASCII
    mov [bx], al 
    
    ; take img data
    push offset winPath
    call ReadFile
    
	; delete the screen on the right place
    push [p1loc]
    xor ah, ah
    mov al, [offset sprtData + 1]
    push ax
    mov al, [offset sprtData]
    push ax
    call printBgPartial
    
    ; display img
    push false ; dont reverse the pic
    push offset Text_Buffer
    push offset sprtData
    push [offset p1loc]
    call drawPic
	
	mov al, [offset p1state + 1]
	inc al
	mov [offset p1state + 1], al
	
	push 10
	call delay
	
	mov bx, offset winPath
    add bx, 12
    mov al, [offset p1state + 1] ; current frame
    add al, 30h ; convert to ASCII
    mov [bx], al 
    
    ; take img data
    push offset winPath
    call ReadFile
    
	; delete the screen on the right place
    push [p1loc]
    xor ah, ah
    mov al, [offset sprtData + 1]
    push ax
    mov al, [offset sprtData]
    push ax
    call printBgPartial
    
    ; display img
    push false ; dont reverse the pic
    push offset Text_Buffer
    push offset sprtData
    push [offset p1loc]
    call drawPic
	
	mov al, [offset p1state + 1]
	inc al
	mov [offset p1state + 1], al
	
	push 200
	call delay
	jmp gameWonRet
	
	gameWonP2:
	
	; if p2 won:
	; update winPath number to point to the current frame
    mov bx, offset winPath
    add bx, 12
    mov al, [offset p2state + 1] ; current frame
    add al, 30h ; convert to ASCII
    mov [bx], al 
    
    ; take img data
    push offset winPath
    call ReadFile
    
	; delete the screen on the right place
    push [p2loc]
    xor ah, ah
    mov al, [offset sprtData + 1]
    push ax
    mov al, [offset sprtData]
    push ax
    call printBgPartial
    
    ; display img
    push false ; dont reverse the pic
    push offset Text_Buffer
    push offset sprtData
    push [offset p2loc]
    call drawPic
	
	mov al, [offset p2state + 1]
	inc al
	mov [offset p2state + 1], al
	
	push 10
	call delay
	
	mov bx, offset winPath
    add bx, 12
    mov al, [offset p2state + 1] ; current frame
    add al, 30h ; convert to ASCII
    mov [bx], al 
    
    ; take img data
    push offset winPath
    call ReadFile
    
	; delete the screen on the right place
    push [p2loc]
    xor ah, ah
    mov al, [offset sprtData + 1]
    push ax
    mov al, [offset sprtData]
    push ax
    call printBgPartial
    
    ; display img
    push false ; dont reverse the pic
    push offset Text_Buffer
    push offset sprtData
    push [offset p2loc]
    call drawPic
	
	mov al, [offset p2state + 1]
	inc al
	mov [offset p2state + 1], al
	
	push 10
	call delay
	
	mov bx, offset winPath
    add bx, 12
    mov al, [offset p2state + 1] ; current frame
    add al, 30h ; convert to ASCII
    mov [bx], al 
    
    ; take img data
    push offset winPath
    call ReadFile
    
	; delete the screen on the right place
    push [p2loc]
    xor ah, ah
    mov al, [offset sprtData + 1]
    push ax
    mov al, [offset sprtData]
    push ax
    call printBgPartial
    
    ; display img
    push false ; dont reverse the pic
    push offset Text_Buffer
    push offset sprtData
    push [offset p2loc]
    call drawPic
	
	mov al, [offset p2state + 1]
	inc al
	mov [offset p2state + 1], al
	
	push 200
	call delay
	
	
	gameWonRet:
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	jmp exit
endp gameWon

; --------------------------
;                    main
; --------------------------

start:
        mov ax, @data
        mov ds, ax
        
        ; set to graphic mode
        mov ax, 13h
        int 10h
        
        ; move to corret offset in extra seg
        mov ax, 0a000h
        mov es, ax
        
        ; draw intro
        mov ah, 3Dh
        xor al, al
        mov dx, offset introPath
        int 21h
        jnc introCont
        jmp openerror
        introCont:
        mov [filehandle], ax
        call ReadHeader
        call ReadPalette
        call CopyPal
        call CopyBitmap
        
        ; wait for keypress
        mov ah,1 
        int 21h
        call drawBg
        
        ; replace int9h, disable all interrupt 16h functions and some int 21h keyboard functions.
        call replaceInt9
        
        ; get game starting time
        mov ax, 2c00h
        int 21h ; ch = hr, cl = min, dh = sec, dl = hundredths
        mov [offset clockRefrance], cl ; mins
        mov [offset clockRefrance + 1], dh ; secs
        
        main:
        ; wait
        push 10 ; 100ms
        call delay
		; update action according to player input
        call inputHandler
        ; update animations
        push p2
        call animationHandler
        push p1
        call animationHandler
        ; update timer
        ; call clock
        ; check if any special action is called (quit, timeout...)
        call specialActionHandler
        jmp main
        
exit:
        ; RESTORE PALLETE
        ; restore int9h
        call restoreInt9
        ; leave graphic mode
        mov ax, 2
        int 10h
        ; return to DOS
        mov ax, 4c00h
        int 21h
END start