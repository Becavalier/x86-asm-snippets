         core_code_seg_sel     equ  0x38 
         core_data_seg_sel     equ  0x30
         sys_routine_seg_sel   equ  0x28 
         video_ram_seg_sel     equ  0x20 
         core_stack_seg_sel    equ  0x18
         mem_0_4_gb_seg_sel    equ  0x08 

;-------------------------------------------------------------------------------
         core_length      dd core_end 
         sys_routine_seg  dd section.sys_routine.start
         core_data_seg    dd section.core_data.start
         core_code_seg    dd section.core_code.start
         core_entry       dd start    
                          dw core_code_seg_sel

;===============================================================================
         [bits 32]
;===============================================================================
SECTION sys_routine vstart=0   
;-------------------------------------------------------------------------------
put_string:  
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
         retf   

;-------------------------------------------------------------------------------
put_char:           
         pushad

         mov dx, 0x3d4
         mov al, 0x0e
         out dx, al
         inc dx     
         in al, dx   
         mov ah, al

         dec dx  
         mov al, 0x0f
         out dx, al
         inc dx   
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
         push es
         mov eax, video_ram_seg_sel 
         mov es, eax
         shl bx, 1
         mov [es:bx], cl
         pop es

         shr bx, 1
         inc bx

  .roll_screen:
         cmp bx, 2000    
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
         mov ecx, 1920
         rep movsw
         mov bx, 3840 
         mov ecx, 80 
  .cls:
         mov word[es:bx], 0x0720
         add bx, 2
         loop .cls

         pop es
         pop ds

         pop bx   
         sub bx, 80 

  .set_cursor:
         mov dx, 0x3d4
         mov al, 0x0e
         out dx, al
         inc dx              
         mov al, bh
         out dx, al
         dec dx   
         mov al, 0x0f
         out dx, al
         inc dx  
         mov al, bl
         out dx, al

         popad
         ret

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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
         xlat
      
         push ecx
         mov cl, al                           
         call put_char
         pop ecx
       
         loop .xlt
      
         pop ds
         popad
         retf
      
;-------------------------------------------------------------------------------
allocate_memory: 
         push ds
         push eax
         push ebx
      
         mov eax, core_data_seg_sel
         mov ds, eax
      
         mov eax, [ram_alloc]
         add eax, ecx   
          
         mov ecx, [ram_alloc] 

         mov ebx, eax
         and ebx, 0xfffffffc
         add ebx, 4           
         test eax, 0x00000003  
         cmovnz eax, ebx    
         mov [ram_alloc], eax   

         pop ebx
         pop eax
         pop ds

         retf

;-------------------------------------------------------------------------------
set_up_gdt_descriptor: 
         push eax
         push ebx
         push edx

         push ds
         push es

         mov ebx, core_data_seg_sel    
         mov ds, ebx

         sgdt [pgdt]                  

         mov ebx, mem_0_4_gb_seg_sel
         mov es,ebx

         movzx ebx, word [pgdt]          
         inc bx                       
         add ebx, [pgdt + 2] 

         mov [es:ebx], eax
         mov [es:ebx + 4], edx

         add word [pgdt], 8   

         lgdt [pgdt]    

         mov ax, [pgdt]        
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
make_gate_descriptor:                       ; eax - offset.
                                            ; bx - segment descriptor.
                                            ; cx - gate attribute.
         push ebx
         push ecx

         mov edx, eax                   
         mov dx, cx                         ; Make high 32 bits.

         and eax, 0x0000ffff                ; Get low 16 bits of the offset.
         shl ebx, 16
         or eax, ebx                        ; Make low 32 bits.

         pop ecx
         pop ebx

         retf

sys_routine_end:

;===============================================================================
SECTION core_data vstart=0 
;------------------------------------------------------------------------------- 
         pgdt             dw  0   
                          dd  0

         ram_alloc        dd  0x00100000 

         salt:
         salt_1           db  '@PrintString'
                     times 256-($-salt_1) db 0
                          dd  put_string
                          dw  sys_routine_seg_sel

         salt_2           db  '@ReadDiskData'
                     times 256-($-salt_2) db 0
                          dd  read_hard_disk_0
                          dw  sys_routine_seg_sel

         salt_3           db  '@PrintDwordAsHexString'
                     times 256-($-salt_3) db 0
                          dd  put_hex_dword
                          dw  sys_routine_seg_sel

         salt_4           db  '@TerminateProgram'
                     times 256-($-salt_4) db 0
                          dd  return_point
                          dw  core_code_seg_sel

         salt_item_len   equ $ - salt_4
         salt_items      equ ($ - salt) / salt_item_len

         message_1        db  '  If you seen this message,that means we '
                          db  'are now in protect mode,and the system '
                          db  'core is loaded,and the video display '
                          db  'routine works perfectly.',0x0d,0x0a,0

         message_2        db  '  System wide CALL-GATE mounted.',0x0d,0x0a,0

         message_3        db  0x0d,0x0a,'  Loading user program...',0
         
         do_status        db  'Done.',0x0d,0x0a,0
         
         message_6        db  0x0d,0x0a,0x0d,0x0a,0x0d,0x0a
                          db  '  User program terminated,control returned.',0

         bin_hex          db '0123456789ABCDEF'
                                       

         core_buf   times 2048 db 0

         esp_pointer      dd 0 

         cpu_brnd0        db 0x0d,0x0a,'  ',0
         cpu_brand  times 52 db 0
         cpu_brnd1        db 0x0d,0x0a,0x0d,0x0a,0

         tcb_chain        dd  0

core_data_end:
               
;===============================================================================
SECTION core_code vstart=0
;-------------------------------------------------------------------------------
fill_descriptor_in_ldt:                     ; edx:eax - 64 bits descriptor.
                                            ; ebx - base address of the current TCB (for getting the base address and limit of the LDT).
         push eax
         push edx
         push edi
         push ds

         mov ecx, mem_0_4_gb_seg_sel 
         mov ds, ecx                        ; ds -> 4GB segment.

         mov edi, [ebx + 0x0c]              ; Get the base address of LDT.
         
         xor ecx, ecx
         mov cx, [ebx + 0x0a]               ; Get the current limit of LDT.
         inc cx                             ; size = limit + 1.
         ; Install descriptor, location = base address + size.     
         mov [edi + ecx + 0x00], eax 
         mov [edi + ecx + 0x04], edx 

         add cx, 8                          ; Update LDT limit.                     
         dec cx   

         mov [ebx + 0x0a], cx   

         mov ax, cx                         ; Get the index number of the newly added descriptor.
         xor dx, dx
         mov cx, 8
         div cx
         
         mov cx, ax                         ; Create and return the selector.
         shl cx, 3                         
         or cx, 0000_0000_0000_0100B 

         pop ds
         pop edi
         pop edx
         pop eax
     
         ret
      
;------------------------------------------------------------------------------- 
load_relocate_program: 
         pushad
      
         push ds
         push es
      
         mov ebp, esp                         ; Update ebp for accessing stack values.
      
         mov ecx, mem_0_4_gb_seg_sel
         mov es, ecx                          ; es -> (4GB segment descriptor)
      
         mov esi, [ebp + 11 * 4]              ; TCB address -> esi.

         ; Allocate memory for LDT (160 bytes for 20 descriptors).
         mov ecx, 160          
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi + 0x0c], ecx             ; Save basd address of LDT into TCB.
         mov word [es:esi + 0x0a], 0xffff     ; Save limit of LDT (size - 1) into TCB.


         mov eax, core_data_seg_sel
         mov ds, eax                          ; ds -> kernel data area.
       
         mov eax, [ebp + 12 * 4]              ; Get starting sector of the user program.
         mov ebx, core_buf                    ; Read its header into kernel buffer.
         call sys_routine_seg_sel:read_hard_disk_0

         mov eax, [core_buf]                  ; Retrieve the length of user program.
         mov ebx, eax
         and ebx, 0xfffffe00                  ; Clear the last 9 bits.
         add ebx, 512                         ; Add one more sector.
         test eax, 0x000001ff                 ; If it's a multiple of 512, we use eax.
         cmovnz eax, ebx                      ; Otherwise, use ebx.
      
         mov ecx, eax 
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi + 0x06], ecx             ; Save the base address of user program into TCB.
      
         mov ebx, ecx                       
         xor edx, edx
         mov ecx, 512
         div ecx                              ; edx:eax / 512 => eax, edx.
         mov ecx, eax                         ; Get the count of sectors.
      
         mov eax, mem_0_4_gb_seg_sel       
         mov ds, eax

         mov eax, [ebp + 12 * 4]              ; Get starting sector of the user program.
  .b1:
         call sys_routine_seg_sel:read_hard_disk_0
         inc eax
         loop .b1                             ; Read ecx times (each a sector).
         mov edi, [es:esi + 0x06]             ; Read base address of user program from TCB.

         mov eax, edi                         ; Set start address of the segment.
         mov ebx, [edi + 0x04]                ; Set the limit (size) of the segment.
         dec ebx                         
         mov ecx, 0x0040f200                  ; Set the segment attribute (DPL = 3)

         ; 64 bits segment descriptor will be returned in edx:dax.
         call sys_routine_seg_sel:make_seg_descriptor
      
         mov ebx, esi                     
         call fill_descriptor_in_ldt

         or cx, 0000_0000_0000_0011B          ; Update the returned selector (RPL = 3)
         mov [es:esi + 0x44], cx              ; Update in TCB.
         mov [edi + 0x04], cx                 ; Update in program header.
      
         ; Create code segment for user program, and install in LDT.
         mov eax, edi
         add eax, [edi + 0x0c]
         mov ebx, [edi + 0x10]
         dec ebx 
         mov ecx, 0x0040f800  
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx, esi   
         call fill_descriptor_in_ldt
         or cx, 0000_0000_0000_0011B 
         mov [edi + 0x0c], cx 

         ; Create data segment for user program, and install in LDT.
         mov eax, edi
         add eax, [edi + 0x14] 
         mov ebx, [edi + 0x18]
         dec ebx     
         mov ecx, 0x0040f200  
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx, esi   
         call fill_descriptor_in_ldt
         or cx, 0000_0000_0000_0011B 
         mov [edi + 0x14], cx 

         ; Create stack segment for user program, and install in LDT.
         mov eax, edi
         add eax, [edi + 0x1c]  
         mov ebx, [edi + 0x20]  
         dec ebx   
         mov ecx, 0x0040f200  
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx, esi   
         call fill_descriptor_in_ldt
         or cx, 0000_0000_0000_0011B 
         mov [edi + 0x1c], cx  

         ; Relocate user side SALT.
         mov eax, mem_0_4_gb_seg_sel
         mov es, eax  
   
         mov eax, core_data_seg_sel
         mov ds, eax
      
         cld

         mov ecx, [es:edi + 0x24]              ; Get U-SALT item count in app side.
         add edi, 0x28                         ; edi -> U-SALT base address.
  .b2: 
         push ecx
         push edi
      
         mov ecx, salt_items
         mov esi, salt
  .b3:
         push edi
         push esi
         push ecx

         mov ecx, 64                           ; 256 / 4 = 64.      
         repe cmpsd                            ; Compare via de:esi (kernel) and es:edi (app).
         jnz .b4                               ; If same, ZF = 1.
         mov eax, [esi]                        ; Otherwise, update routine selector and offset into app header.
         mov [es:edi - 256], eax               ; Update offset (4 bytes).
         mov ax, [esi + 4]
         or ax, 0000000000000011B              ; Change RPL to 3.

         mov [es:edi - 252], ax                ; Update gate selector (2 bytes).
  .b4:
      
         pop ecx
         pop esi
         add esi, salt_item_len
         pop edi                
         loop .b3
      
         pop edi
         add edi, 256
         pop ecx
         loop .b2

         mov esi, [ebp + 11 * 4]               ; Get TCB base address.

         ; Create segement descriptor for privilege 0 stack.
         mov ecx, 0                            ; limit = 0 (size = 1, for 4KB unit).
         mov [es:esi + 0x1a], ecx              ; Update "Privilege 0 Stack Size (/4KB)" in TCB.
         inc ecx
         shl ecx, 12                           ; Get stack size in bytes.
         push ecx                              ; Save stack size on stack.
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi + 0x1e], ecx              ; Update the allocated memory for stack in TCB.
         mov eax, ecx
         mov ebx, [es:esi + 0x1a]              ; Get limit of the stack from TCB.
         mov ecx, 0x00c09200                   ; Prepare the attribute for segment decriptor (DPL = 0, 4KB unit).
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx, esi                          ; Pass TCB base address.
         call fill_descriptor_in_ldt           ; Install the segment decriptor in LDT.

         mov [es:esi + 0x22], cx               ; Update segment descriptor in TCB.
         pop dword [es:esi + 0x24]             ; Pop stack size to TCB.

         ; Create segement descriptor for privilege 1 stack.
         mov ecx, 0
         mov [es:esi + 0x28], ecx    
         inc ecx
         shl ecx, 12       
         push ecx
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi + 0x2c], ecx 
         mov eax, ecx
         mov ebx, [es:esi + 0x28]   
         mov ecx, 0x00c0b200 
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx, esi     
         call fill_descriptor_in_ldt
         or cx, 0000_0000_0000_0001 
         mov [es:esi + 0x30], cx     
         pop dword [es:esi + 0x32]    

         ; Create segement descriptor for privilege 2 stack.
         mov ecx, 0
         mov [es:esi + 0x36], ecx  
         inc ecx
         shl ecx, 12        
         push ecx
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi+0x3a], ecx 
         mov eax, ecx
         mov ebx, [es:esi + 0x36]    
         mov ecx, 0x00c0d200
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx, esi         
         call fill_descriptor_in_ldt
         or cx, 0000_0000_0000_0010 
         mov [es:esi + 0x3e], cx    
         pop dword [es:esi + 0x40] 

         mov eax, [es:esi + 0x0c]              ; Get the base address of LDT.
         movzx ebx, word [es:esi + 0x0a]       ; Get the length of LDT.
         mov ecx, 0x00008200                   ; Descriptor attributes (DPL = 0).
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi + 0x10], cx               ; Update the descriptor selector in TCB.
       
         mov ecx, 104                          ; Allocate memory for TSS.
         mov [es:esi + 0x12], cx                   
         dec word [es:esi + 0x12]              ; Update TSS limit in TCB.          
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi + 0x14], ecx              ; Update TSS base address in TCB.
      
         ; Update 0, 1, 2 privileged stack info (ss, esp).
         mov edx, [es:esi + 0x24] 
         mov [es:ecx + 4], edx                 ; ESP0.

         mov dx, [es:esi + 0x22]  
         mov [es:ecx + 8], dx                  ; SS0.

         mov edx, [es:esi + 0x32]                 
         mov [es:ecx + 12], edx                ; ESP1.

         mov dx, [es:esi + 0x30]               
         mov [es:ecx + 16], dx                 ; SS1.

         mov edx, [es:esi + 0x40]   
         mov [es:ecx + 20], edx                ; ESP2.

         mov dx, [es:esi + 0x3e]               
         mov [es:ecx + 24], dx                 ; SS2.

         mov dx, [es:esi + 0x10] 
         mov [es:ecx + 96], dx                 ; LDT selector.

         mov dword [es:ecx + 100], 0x00670000  ; T = 0, and IO mapping area address = 103 (invalid).

         mov eax, [es:esi + 0x14]              ; Get TSS base address.
         movzx ebx, word [es:esi + 0x12]       ; Get TSS limit.
         mov ecx, 0x00008900                   ; TSS descriptor attributes (DPL = 0).
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi + 0x18], cx               ; Save TSS selector (RPL = 0) in TCB.

         pop es    
         pop ds   
         popad
         ret 8  
      
;-------------------------------------------------------------------------------
append_to_tcb_link:                         ; ecx - address of the TCB.    
         push eax
         push edx
         push ds
         push es
         
         mov eax, core_data_seg_sel         ; ds -> kernel data segment.
         mov ds, eax
         mov eax, mem_0_4_gb_seg_sel        ; es -> dynamically allocated memory (with 4GB segment selector).
         mov es, eax
         
         mov dword [es:ecx + 0x00], 0       ; Mark the tail of chain.
                                             
         mov eax, [tcb_chain]               
         or eax, eax                        ; Test if chain is empty?
         jz .notcb                          ; Jump to .notch if positive.
         
  .searc:
         mov edx, eax
         mov eax, [es:edx + 0x00]
         or eax, eax               
         jnz .searc                         ; Skip non-empty TCBs.
         
         mov [es:edx + 0x00], ecx           ; Append TCB to the last when possible.
         jmp .retpc
         
  .notcb:       
         mov [tcb_chain], ecx               ; Append TCB to the chain directly.
         
  .retpc:
         pop es
         pop ds
         pop edx
         pop eax
         ret
         
;-------------------------------------------------------------------------------
start:
         mov ecx, core_data_seg_sel 
         mov ds, ecx                             ; Initialize ds segment register.
 
         mov ebx, message_1                    
         call sys_routine_seg_sel:put_string     ; Call public routine.
                                         
         ; Get CPU information.
         mov eax,0x80000002
         cpuid
         mov [cpu_brand + 0x00],eax
         mov [cpu_brand + 0x04],ebx
         mov [cpu_brand + 0x08],ecx
         mov [cpu_brand + 0x0c],edx
      
         mov eax,0x80000003
         cpuid
         mov [cpu_brand + 0x10],eax
         mov [cpu_brand + 0x14],ebx
         mov [cpu_brand + 0x18],ecx
         mov [cpu_brand + 0x1c],edx

         mov eax,0x80000004
         cpuid
         mov [cpu_brand + 0x20],eax
         mov [cpu_brand + 0x24],ebx
         mov [cpu_brand + 0x28],ecx
         mov [cpu_brand + 0x2c],edx

         mov ebx,cpu_brnd0 
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brand
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brnd1
         call sys_routine_seg_sel:put_string

         ; Install call gates.
         mov edi, salt                                       ; Load the address of kernel SALT.
         mov ecx, salt_items                                 ; Iteration time (gates)
  .b3:
         push ecx
         mov eax, [edi + 256]                                ; Move offset to eax.
         mov bx, [edi + 260]                                 ; Move segment selector to bx.
         mov cx, 1_11_0_1100_000_00000B                      ; Move gate attribute to cx (P = 1, DPL = 3).
                 
                                           
         call sys_routine_seg_sel:make_gate_descriptor       ; Create a gate descriptor.
         call sys_routine_seg_sel:set_up_gdt_descriptor      ; Install it in kernel GDT.
         mov [edi + 260], cx                                 ; Replace the original segment selector with the created gate selector in C-SALT.    
         add edi, salt_item_len                              ; Move to next SALT item.
         pop ecx                                             ; Restore ecx.
         loop .b3

         ; Test just created call gates.
         mov ebx, message_2
         call far [salt_1 + 256]                             ; Call kernel routine via gate selector (the 32 bits offset would be ignored).       

         mov ebx, message_3
         call sys_routine_seg_sel:put_string                 ; Call kernel routine directly.

         ; Allocate memory for creating a TCB.
         mov ecx, 0x46
         call sys_routine_seg_sel:allocate_memory
         call append_to_tcb_link                             ; Append created TCB to the chain.
      
         push dword 50                                       ; Save start sector of user program on disk (via stack).
         push ecx                                            ; Save the address of TCB (saved in ecx by allocate_memory).              
       
         call load_relocate_program
      
         mov ebx, do_status
         call sys_routine_seg_sel:put_string
      
         mov eax, mem_0_4_gb_seg_sel
         mov ds, eax
      
         ltr [ecx + 0x18]                                    ; Load TR.
         lldt [ecx + 0x10]                                   ; Load LDTR.
      
         mov ds, [ecx + 0x44]                                ; Get program header selector from TCB.

         push dword [0x1c]                                   ; Push program stack selector.
         push dword 0                                        ; Push program stack pointer.

         push dword [0x0c]                                   ; Push program cs segment selector.
         push dword [0x08]                                   ; Push program entry offset.

         retf                                                ; Return from a "gate call" (CPL -> 3).

return_point:                
         mov eax, core_data_seg_sel  
         mov ds, eax               
                                 
         mov ebx, message_6
         call sys_routine_seg_sel:put_string

         hlt

core_code_end:

;-------------------------------------------------------------------------------
SECTION core_trail
;-------------------------------------------------------------------------------
core_end:
