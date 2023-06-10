SECTION header vstart=0
    program_length  dd program_end
    code_entry      dw start
                    dd section.code.start

    realloc_tbl_len dw (header_end - code_segment) / 4   
    code_segment    dd section.code.start
    data_segment    dd section.data.start
    stack_segment   dd section.stack.start
    
    header_end:                

SECTION code align=16 vstart=0         ; Interrupt program.
new_int_0x70:
      push ax
      push bx
      push cx
      push dx
      push es
      
  .w0:                                    
      mov al, 0x0a                     
      or al, 0x80                      ; Disable NMI for now.
      out 0x70, al
      in al, 0x71                      ; Read register A from RTC.
      test al, 0x80                    ; Test if bit 7 equals to 0 (is it able to access CMOS RAM?).
      jnz .w0                          ; Busy waiting.       
                                    
      xor al, al
      or al, 0x80
      out 0x70, al
      in al, 0x71                      ; Read cell 0 from CMOS RAM (second).
      push ax

      mov al, 2
      or al, 0x80
      out 0x70, al
      in al, 0x71                      ; Read cell 2 from CMOS RAM (minute).
      push ax

      mov al, 4
      or al, 0x80
      out 0x70, al
      in al, 0x71                      ; Read cell 4 from CMOS RAM (hour).
      push ax

      mov al, 0x0c                     
      out 0x70, al
      in al, 0x71                      ; Reset flags in register C.

      mov ax, 0xb800
      mov es, ax

      pop ax
      call bcd_to_ascii                ; BCD -> ASCII.
      mov bx, 12 * 160 + 36 * 2

      mov [es:bx], ah
      mov [es:bx + 2], al

      mov al, ':'
      mov [es:bx + 4], al 
      not byte [es:bx + 5]

      pop ax
      call bcd_to_ascii
      mov [es:bx + 6], ah
      mov [es:bx + 8], al

      mov al, ':'
      mov [es:bx + 10], al
      not byte [es:bx + 11] 

      pop ax
      call bcd_to_ascii
      mov [es:bx + 12], ah
      mov [es:bx + 14], al
      
      mov al, 0x20                      ; Send EOI (End Of Interrupt) signal (0x20) to ISR.
      out 0xa0, al                      ; Send EOI to slave chip.
      out 0x20, al                      ; Send EOI to master chip.

      pop es
      pop dx
      pop cx
      pop bx
      pop ax
      iret

bcd_to_ascii: 
      mov ah, al
      and al, 0x0f          ; Clear high 4 bits.
      add al, 0x30          ; Translate to ASCII char.

      shr ah, 4 
      and ah, 0x0f                        
      add ah, 0x30
      ret 

start:
      mov ax, [stack_segment] 
      mov ss, ax                       ; Keep the sequences on modifying ss and sp.
      mov sp, ss_pointer
      mov ax, [data_segment]
      mov ds, ax
      
      mov bx, init_msg 
      call put_string

      mov bx, inst_msg 
      call put_string
      
      mov al, 0x70                     ; Find interrupt no.0x70.
      mov bl, 4
      mul bl                           ; Calculate the offset of the interrupt in the IVT by * 4 (offset + segment address).
      mov bx, ax                          

      cli                              ; Clear interrupt bit.

      push es
      mov ax, 0x0000
      mov es, ax
      mov word [es:bx], new_int_0x70   ; Locate the IVP entry, save the offset address of the handler.              
      mov word [es:bx + 2], cs         ; Save the segment address of the handler.
      pop es

      mov al, 0x0b                     ; Config to access register B in CMOS RAM.
      or al, 0x80                      ; Disable NMI for now.
      out 0x70, al
      mov al, 0x12                     ; Set config for interrupe (0001 0010).
      out 0x71, al                     ; Write config to CMOS RAM.

      mov al, 0x0c                     ; Config to access register C in CMOS RAM.
      out 0x70, al                     ; Write to 0x70 (NMI is enabled).
      in al, 0x71                      ; Read from 0x71, reset corresponding flag in register C.

      in al, 0xa1                      ; Read from IMR.
      and al, 0xfe                     ; Clear bit 0.
      out 0xa1, al                     ; Write back to IMR, enable the interrupt from pos IR0.

      sti                              ; Enable device interrupt.

      mov bx, done_msg 
      call put_string

      mov bx, tips_msg
      call put_string
      
      mov cx, 0xb800                   ; Show characters on the screen.
      mov ds, cx
      mov byte [12 * 160 + 33 * 2], '@'
       
 .idle:
      hlt                              ; Get into "halt" state (could be woken up by external interrupt).
      not byte [12 * 160 + 33 * 2 + 1] 
      jmp .idle

put_string: 
         mov cl, [bx]
         or cl, cl
         jz .exit
         call put_char
         inc bx
         jmp put_string
   .exit:
         ret

put_char: 
         push ax
         push bx
         push cx
         push dx
         push ds
         push es

         mov dx, 0x3d4
         mov al, 0x0e
         out dx, al
         mov dx, 0x3d5
         in al, dx 
         mov ah, al

         mov dx, 0x3d4
         mov al, 0x0f
         out dx, al
         mov dx, 0x3d5
         in al, dx
         mov bx, ax

         cmp cl, 0x0d 
         jnz .put_0a 
         mov ax, bx
         mov bl, 80                       
         div bl
         mul bl
         mov bx, ax
         jmp .set_cursor

 .put_0a:
         cmp cl, 0x0a
         jnz .put_other 
         add bx, 80
         jmp .roll_screen

 .put_other:
         mov ax, 0xb800
         mov es, ax
         shl bx, 1
         mov [es:bx], cl
         shr bx, 1
         add bx, 1

 .roll_screen:
         cmp bx, 2000
         jl .set_cursor

         push bx

         mov ax, 0xb800
         mov ds, ax
         mov es, ax
         cld
         mov si, 0xa0
         mov di, 0x00
         mov cx, 1920
         rep movsw
         mov bx, 3840
         mov cx, 80
 .cls:
         mov word[es:bx], 0x0720
         add bx, 2
         loop .cls

         pop bx
         sub bx, 80

 .set_cursor:
         mov dx, 0x3d4
         mov al, 0x0e
         out dx, al
         mov dx, 0x3d5
         mov al, bh
         out dx, al
         mov dx, 0x3d4
         mov al, 0x0f
         out dx, al
         mov dx, 0x3d5
         mov al, bl
         out dx, al

         pop es
         pop ds
         pop dx
         pop cx
         pop bx
         pop ax
         ret

SECTION data align=16 vstart=0
    init_msg       db 'Starting...',0x0d,0x0a,0       
    inst_msg       db 'Installing a new interrupt 70H...',0
    done_msg       db 'Done.',0x0d,0x0a,0
    tips_msg       db 'Clock is now working.',0
                   
SECTION stack align=16 vstart=0
                 resb 256
ss_pointer:
 
SECTION program_trail
program_end:
