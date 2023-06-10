SECTION header vstart=0
    program_length  dd program_end                      ; [0x00] length of the prgram.
    code_entry      dw start                            ; [0x04] offset of the code entry point.
                    dd section.code_1.start             ; [0x06] code entry address.
    
    realloc_tbl_len dw (header_end - code_1_segment) / 4  ; [0x0a]
    code_1_segment  dd section.code_1.start             ; [0x0c]
    code_2_segment  dd section.code_2.start             ; [0x10]
    data_1_segment  dd section.data_1.start             ; [0x14]
    data_2_segment  dd section.data_2.start             ; [0x18]
    stack_segment   dd section.stack.start              ; [0x1c]
    
    header_end:                

SECTION code_1 align=16 vstart=0
put_string: 
         mov cl, [bx]
         or cl, cl
         jz .exit                ; Exit if it meets '\0'.
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
         out dx, al           ; Ready to operate on register 0xe (higher 8 bits).
         mov dx, 0x3d5
         in al, dx
         mov ah, al           ; Read from 0xe -> ah.

         mov dx, 0x3d4
         mov al, 0x0f
         out dx, al
         mov dx, 0x3d5
         in al, dx            ; Read from 0xe -> al (lower 8 bits).
         mov bx, ax           ; Save ax -> bx.

         cmp cl, 0x0d         ; Check if it's '0xd'?
         jnz .put_0a          
         mov ax, bx
         mov bl, 80                       
         div bl               ; al => ax / bl (drop the remainder).
         mul bl               ; ax => al * bl (get the location of the line head).
         mov bx, ax
         jmp .set_curso       ; Set up the position of cursor.

 .put_0a:
         cmp cl, 0x0a         ; Check if it's '0xa'?
         jnz .put_other       
         add bx, 80           ; Move to the next line by adding 80.
         jmp .roll_screen

 .put_other:                  ; Normal character.
         mov ax, 0xb800       
         mov es, ax           ; Point to the area of graphic memory.
         shl bx, 1            ; Get the cursor offset by multiplying 2.
         mov [es:bx], cl      ; Write character.
         shr bx, 1            ; Reset bx.
         add bx, 1            ; Move cursor to the next position.

 .roll_screen:
         cmp bx, 2000         ; Check if we need to roll out the screen (overflow)?
         jl .set_cursor

         push bx
         mov ax, 0xb800
         mov ds, ax
         mov es, ax
         cld                  ; 0 -> df (from low to high).
         mov si, 0xa0
         mov di, 0x00
         mov cx, 1920         ; Move 1920 characters (iterations).
         rep movsw            ; ds:si -> es:di.
         mov bx, 3840         ; Clear the last line.
         mov cx, 80
 .cls:
         mov word[es:bx], 0x0720
         add bx, 2
         loop .cls
         pop bx
         sub bx, 80           ; Reset cursor to the head of the last line.

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

  start:
         mov ax, [stack_segment]           ; Initialize ss.
         mov ss, ax
         mov sp, stack_end
         mov ax, [data_1_segment]          ; Initialize ds.
         mov ds, ax

         mov bx, msg0
         call put_string                   ; Call sub procedure to print the chars at msg0.

         push word [es:code_2_segment]     ; Push segement address of "code_2_segment" on the stack.
         mov ax, begin
         push ax                           ; Push offset of "begin" on the stack.
         retf                              ; Update ip and cs, then jump.
         
  continue:
         mov ax, [es:data_2_segment]       
         mov ds, ax                        ; Change data segment.
         mov bx, msg1
         call put_string
         jmp $

SECTION code_2 align=16 vstart=0 
  begin:
         push word [es:code_1_segment]
         mov ax, continue
         push ax
         retf 

SECTION data_1 align=16 vstart=0
    msg0 db '  This is NASM - the famous Netwide Assembler. '
         db 'Back at SourceForge and in intensive development! '
         db 'Get the current versions from http://www.nasm.us/.'
         db 0x0d,0x0a,0x0d,0x0a
         db '  Example code for calculate 1+2+...+1000:',0x0d,0x0a,0x0d,0x0a
         db '     xor dx,dx',0x0d,0x0a
         db '     xor ax,ax',0x0d,0x0a
         db '     xor cx,cx',0x0d,0x0a
         db '  @@:',0x0d,0x0a
         db '     inc cx',0x0d,0x0a
         db '     add ax,cx',0x0d,0x0a
         db '     adc dx,0',0x0d,0x0a
         db '     inc cx',0x0d,0x0a
         db '     cmp cx,1000',0x0d,0x0a
         db '     jle @@',0x0d,0x0a
         db '     ... ...(Some other codes)',0x0d,0x0a,0x0d,0x0a
         db 0

SECTION data_2 align=16 vstart=0

    msg1 db '  The above contents is written by LeeChung. '
         db '2011-05-06'
         db 0

SECTION stack align=16 vstart=0
         resb 256

stack_end:  

SECTION trail align=16
program_end:
