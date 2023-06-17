         ; Segment selectors in GDT for kernel's use.
         core_code_seg_sel     equ  0x38 
         core_data_seg_sel     equ  0x30 
         sys_routine_seg_sel   equ  0x28
         video_ram_seg_sel     equ  0x20 
         core_stack_seg_sel    equ  0x18 
         mem_0_4_gb_seg_sel    equ  0x08

         ; Header section (starting at offset 0x0).
         core_length      dd core_end                      ; The size of the kernel.
         sys_routine_seg  dd section.sys_routine.start     ; Offset of the routine section. 
         core_data_seg    dd section.core_data.start       ; Offset of the core data section.
         core_code_seg    dd section.core_code.start       ; Offset of the core code section.

         ; Offset of the core entry (will be called after MBR has loaded the kernel).
         core_entry       dd start                         ; Entry offset -> eip.                        
                          dw core_code_seg_sel             ; Segment selector (16 bits).

;===============================================================================
         [bits 32]
;===============================================================================
SECTION sys_routine vstart=0 
;-------------------------------------------------------------------------------
put_string:                                 ; The start address of string is passed by ebx.
         push ecx
  .getc:
         mov cl, [ebx]
         or cl, cl
         jz .exit
         call put_char
         inc ebx
         jmp .getc

  .exit:
         pop ecx
         retf                               ; Far return (cross-segment).

;-------------------------------------------------------------------------------
put_char: 
         pushad 
         mov dx, 0x3d4                      ; Specify port number (index register).
         mov al, 0x0e                       ; Access cursor register (high 8 bits).
         out dx, al
         inc dx                             ; Specify port number (retrieving data).
         in al, dx                          
         mov ah, al                         ; High 8 bits of cursor position -> ah.

         dec dx                             
         mov al, 0x0f
         out dx, al
         inc dx                             
         in al, dx                          ; Low 8 bits of cursor position -> al.
         mov bx, ax 

         cmp cl, 0x0d                       ; Is it "Enter"?
         jnz .put_0a                        ; Negative -> .put_0a.
         mov ax, bx          
         mov bl, 80
         div bl
         mul bl                             ; The position of head of current line -> bx.
         mov bx, ax
         jmp .set_cursor

  .put_0a:
         cmp cl, 0x0a                       ; Is it "Carriage Return"?
         jnz .put_other                     ; Negative -> .put_other.
         add bx, 80                         ; Move to next line.
         jmp .roll_screen

  .put_other:                               
         push es
         mov eax, video_ram_seg_sel         ; Set segment descriptor for visual memory.
         mov es, eax
         shl bx, 1                          ; Multiply the cursor position by 2.
         mov [es:bx], cl                    ; Character position -> memory location.
         pop es


         shr bx, 1                          ; Memory location -> character position.
         inc bx                             ; Move to next available location.

  .roll_screen:
         cmp bx, 2000                       ; Is it able to show all the characters in one screen?
         jl .set_cursor

         push bx                           
         push ds
         push es
         mov eax, video_ram_seg_sel
         mov ds, eax
         mov es, eax
         cld
         mov esi, 0xa0      
         mov edi, 0x00       
         mov ecx, 1920                      ; Count of texts need to be moved.
         rep movsw                          ; Copy text (rolling screen upward), ds:esi -> es:edi.
         mov bx, 3840 
         mov ecx, 80 
  .cls:
         mov word[es:bx], 0x0720            ; Clear the last line.
         add bx, 2
         loop .cls

         pop es
         pop ds


         pop bx                          
         sub bx, 80                         ; Place the cursor at the head of line.

  .set_cursor:
         mov dx, 0x3d4                      ; Write the cursor position back to cursor register.
         mov al, 0x0e
         out dx, al
         inc dx 
         mov al, bh                         ; Write high 8 bits.
         out dx, al
         dec dx       
         mov al, 0x0f
         out dx, al
         inc dx
         mov al, bl                         ; Write low 8 bits.
         out dx, al

         popad                              ; Reset all GPRs.
         ret                                

read_hard_disk_0:
         push eax 
         push ecx
         push edx
      
         push eax
         
         mov dx, 0x1f2
         mov al, 1
         out dx, al                     

         inc dx    
         pop eax
         out dx, al    

         inc dx      
         mov cl, 8
         shr eax, cl
         out dx, al     

         inc dx                   
         shr eax, cl
         out dx, al                

         inc dx                
         shr eax, cl
         or al, 0xe0                 
         out dx, al

         inc dx                   
         mov al, 0x20  
         out dx, al

  .waits:
         in al, dx
         and al, 0x88
         cmp al, 0x08
         jnz .waits           

         mov ecx, 256        
         mov dx, 0x1f0
  .readw:
         in ax, dx
         mov [ebx], ax
         add ebx, 2
         loop .readw

         pop edx
         pop ecx
         pop eax
      
         retf    

put_hex_dword: 
         pushad
         push ds

         mov ax, core_data_seg_sel  
         mov ds, ax

         mov ebx, bin_hex  
         mov ecx, 8
  .xlt:
         rol edx, 4
         mov eax, edx
         and eax, 0x0000000f
         xlat                             ; Table Look-up Translation.

         push ecx
         mov cl, al
         call put_char
         pop ecx

         loop .xlt

         pop ds
         popad

         retf

allocate_memory: 
         push ds
         push eax
         push ebx
      
         mov eax, core_data_seg_sel
         mov ds, eax
      
         mov eax, [ram_alloc]
         add eax, ecx                     ; Calculate next available memory allocation address.
      

          
         mov ecx, [ram_alloc]             ; The memory address will be returned to caller.

         mov ebx, eax
         and ebx, 0xfffffffc              
         add ebx, 4                       ; Align to 4 bytes boundary (bigger than applied).
         test eax, 0x00000003             ; If original eax is aligned?
         cmovnz eax, ebx       
         mov [ram_alloc], eax             ; Update next available memory address.
                                         
         pop ebx
         pop eax
         pop ds

         retf

set_up_gdt_descriptor:                             ; edx - high 4 bytes.
                                                   ; eax - low 4 bytes.
         push eax
         push ebx
         push edx
      
         push ds
         push es
      
         mov ebx, core_data_seg_sel                
         mov ds, ebx

         sgdt [pgdt]                               ; Save GDTR value into kernel data.

         mov ebx, mem_0_4_gb_seg_sel               ; Let es point to 4GB segment.
         mov es, ebx

         movzx ebx, word [pgdt]                    ; Read limit (size) into ebx with zero extend.
         inc bx                                    ; Get full size of GDT items (use bx instead of ebx to accommodate the start-up status).
         add ebx, [pgdt + 2]                       ; Plus base address.
      
         mov [es:ebx], eax                         ; Install descriptor.
         mov [es:ebx + 4], edx
      
         add word [pgdt], 8                        ; Update the limit (low 8 bits).
      
         lgdt [pgdt]                               ; Reload GDTR.
       
         mov ax, [pgdt]                            ; Generate segment selector (in cx).
         xor dx, dx
         mov bx, 8
         div bx         
         mov cx, ax                          
         shl cx, 3                           

         pop es
         pop ds

         pop edx
         pop ebx
         pop eax
      
         retf 

make_seg_descriptor:
         mov edx, eax
         shl eax, 16
         or ax, bx         

         and edx, 0xffff0000      
         rol edx, 8
         bswap edx             

         xor bx, bx
         or edx, ebx   

         or edx, ecx    

         retf

;===============================================================================
SECTION core_data vstart=0   
;-------------------------------------------------------------------------------
         pgdt             dw  0      
                          dd  0

         ram_alloc        dd  0x00100000

         salt:
         salt_1           db  '@PrintString'
                     times 256 - ($ - salt_1) db 0
                          dd  put_string
                          dw  sys_routine_seg_sel

         salt_2           db  '@ReadDiskData'
                     times 256 - ($ - salt_2) db 0
                          dd  read_hard_disk_0
                          dw  sys_routine_seg_sel

         salt_3           db  '@PrintDwordAsHexString'
                     times 256 - ($ - salt_3) db 0
                          dd  put_hex_dword
                          dw  sys_routine_seg_sel

         salt_4           db  '@TerminateProgram'
                     times 256 - ($ - salt_4) db 0
                          dd  return_point
                          dw  core_code_seg_sel

         salt_item_len   equ $ - salt_4
         salt_items      equ ($ - salt) / salt_item_len

         message_1        db  '  If you seen this message,that means we '
                          db  'are now in protect mode,and the system '
                          db  'core is loaded,and the video display '
                          db  'routine works perfectly.',0x0d,0x0a,0

         message_5        db  '  Loading user program...',0
         
         do_status        db  'Done.',0x0d,0x0a,0
         
         message_6        db  0x0d,0x0a,0x0d,0x0a,0x0d,0x0a
                          db  '  User program terminated,control returned.',0

         bin_hex          db '0123456789ABCDEF'
                                   
         core_buf   times 2048 db 0 

         esp_pointer      dd 0 

         cpu_brnd0        db 0x0d,0x0a,'  ',0
         cpu_brand  times 49 db 0
         cpu_brnd1        db 0x0d,0x0a,0x0d,0x0a,0

;===============================================================================
SECTION core_code vstart=0
;-------------------------------------------------------------------------------
load_relocate_program: 
         push ebx
         push ecx
         push edx
         push esi
         push edi
      
         push ds
         push es
      
         mov eax, core_data_seg_sel
         mov ds, eax                                     ; Set descriptor (core data) for ds.

         mov eax, esi                                    ; Starting sector passed in (esi).
         mov ebx, core_buf                               ; Update ebx (kernel buffer memory).
         call sys_routine_seg_sel:read_hard_disk_0       ; Read first 512 bytes (1 sector).

         mov eax, [core_buf]                             ; Retrieve the length of user program.  
         mov ebx, eax                                 
         and ebx, 0xfffffe00                             ; Clear the last 9 bits.
         add ebx, 512                                    ; Add one more sector.
         test eax, 0x000001ff                            ; If it's a multiple of 512, we use eax.
         cmovnz eax, ebx                                 ; Otherwise, use ebx.
      
         mov ecx, eax                         
         call sys_routine_seg_sel:allocate_memory        ; Allocate memory, save address into ecx.
         mov ebx, ecx                                    ; ecx -> ebx.
         push ebx                                        ; Save ebx (program memory).
         xor edx, edx
         mov ecx, 512
         div ecx                                         ; edx:eax / 512 => eax, edx.
         mov ecx, eax                                    ; Get the count of sectors.
      
         mov eax, mem_0_4_gb_seg_sel 
         mov ds, eax

         mov eax, esi                                    ; Set up starting sector number.
  .b1:
         call sys_routine_seg_sel:read_hard_disk_0
         inc eax
         loop .b1                                        ; Read ecx times (each a sector).

         pop edi                                         ; Reset program head address (edi).
         mov eax, edi             
         mov ebx, [edi + 0x04]                           ; Read program header length.
         dec ebx                                         ; Create limit.          
         mov ecx, 0x00409200                          
         call sys_routine_seg_sel:make_seg_descriptor    ; Create segment descriptor (edx, eax).
         call sys_routine_seg_sel:set_up_gdt_descriptor  ; Install descriptor and return selector (cx).
         mov [edi + 0x04], cx                            ; Write the selector back to program header.

         ; Insert segment descriptor for user code.
         mov eax, edi
         add eax, [edi + 0x0c] 
         mov ebx, [edi + 0x10] 
         dec ebx              
         mov ecx, 0x00409800              
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi + 0x0c], cx

         ; Insert segment descriptor for user data.
         mov eax, edi
         add eax, [edi + 0x14]         
         mov ebx, [edi + 0x18]       
         dec ebx                      
         mov ecx, 0x00409200             
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi + 0x14], cx

         ; Insert segment descriptor for user stack.
         mov eax, edi
         add eax, [edi + 0x1c]             
         mov ebx, [edi + 0x20]            
         dec ebx                        
         mov ecx, 0x00409200              
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi + 0x1c], cx


         mov eax, [edi + 0x04]
         mov es, eax                                  ; Access program header with es.
         mov eax, core_data_seg_sel
         mov ds, eax                                  ; Access kernel data with ds.

         cld                                          ; DF = 0 (forward).

         mov ecx, [es:0x24]                           ; Get SALT item count in app side.
         mov edi, 0x28                                ; Save SALT offset in app side.
  .b2:
         push ecx
         push edi

         mov ecx, salt_items                          ; Get kernel defined SALT items.
         mov esi, salt                                ; Get kernel SALT address.
  .b3:
         push edi
         push esi
         push ecx

         mov ecx, 64                                  ; 256 / 4 = 64.
         repe cmpsd                                   ; Compare via ds:esi (kernel) and es:edi (app).
         jnz .b4                                      ; If same, ZF = 1.
         mov eax, [esi]                               ; Otherwise, update routine selector and offset into app header.
         mov [es:edi - 256], eax                      ; Update offset (4 bytes).
         mov ax, [esi + 4]
         mov [es:edi - 252], ax                       ; Update GDT selector (2 bytes).
  .b4:

         pop ecx
         pop esi
         add esi, salt_item_len                       ; Move to next internal round.
         pop edi    
         loop .b3

         pop edi
         add edi, 256                                 ; Move to next SALT item.
         pop ecx
         loop .b2

         mov ax, [es:0x04]                            ; Return app header selector to caller.

         pop es           
         pop ds            
      
         pop edi
         pop esi
         pop edx
         pop ecx
         pop ebx
      
         ret
      
;-------------------------------------------------------------------------------
start:   ; Entry of the kernel code.
         mov ecx, core_data_seg_sel 
         mov ds, ecx                             ; Initialize ds segment register.

         mov ebx, message_1
         call sys_routine_seg_sel:put_string     ; Call public routine.

         ; Get CPU information.
         mov eax, 0x80000002
         cpuid
         mov [cpu_brand + 0x00], eax
         mov [cpu_brand + 0x04], ebx
         mov [cpu_brand + 0x08], ecx
         mov [cpu_brand + 0x0c], edx
      
         mov eax, 0x80000003
         cpuid
         mov [cpu_brand + 0x10], eax
         mov [cpu_brand + 0x14], ebx
         mov [cpu_brand + 0x18], ecx
         mov [cpu_brand + 0x1c], edx

         mov eax, 0x80000004
         cpuid
         mov [cpu_brand + 0x20], eax
         mov [cpu_brand + 0x24], ebx
         mov [cpu_brand + 0x28], ecx
         mov [cpu_brand + 0x2c], edx

         mov ebx, cpu_brnd0 
         call sys_routine_seg_sel:put_string
         mov ebx, cpu_brand
         call sys_routine_seg_sel:put_string     ; Print CPU information.
         mov ebx, cpu_brnd1
         call sys_routine_seg_sel:put_string

         mov ebx, message_5
         call sys_routine_seg_sel:put_string
         mov esi, 50                             ; Set the Starting sector of the user program on the disk.
         call load_relocate_program
      
         mov ebx, do_status  
         call sys_routine_seg_sel:put_string
      
         mov [esp_pointer], esp                  ; Save esp value for kernel.
       
         mov ds, ax                              ; Ready to access app header.

         jmp far [0x08]                          ; Jump to user program.
 

return_point:                              
         mov eax, core_data_seg_sel              ; Use kernel data.
         mov ds, eax

         mov eax, core_stack_seg_sel             ; Use kernel stack.
         mov ss, eax 
         mov esp, [esp_pointer]                  ; Reset kernel stack pointer.

         mov ebx, message_6
         call sys_routine_seg_sel:put_string

         hlt
            
;===============================================================================
SECTION core_trail
;-------------------------------------------------------------------------------
core_end:
