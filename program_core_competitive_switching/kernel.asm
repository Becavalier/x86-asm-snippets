         core_code_seg_sel     equ  0x38
         core_data_seg_sel     equ  0x30  
         sys_routine_seg_sel   equ  0x28 
         video_ram_seg_sel     equ  0x20 
         core_stack_seg_sel    equ  0x18
         mem_0_4_gb_seg_sel    equ  0x08
         idt_linear_address    equ  0x1f000               ; The memory location of IDT.

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
         cli
  .getc:
         mov cl,[ebx]
         or cl,cl
         jz .exit
         call put_char
         inc ebx
         jmp .getc

  .exit:
         sti
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
         mov word [es:bx], 0x0720
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
          
         mov ecx,[ram_alloc]   

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
         mov es, ebx

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
make_gate_descriptor: 
         push ebx
         push ecx
      
         mov edx, eax
         and edx, 0xffff0000 
         or dx, cx     
       
         and eax, 0x0000ffff   
         shl ebx, 16                          
         or eax, ebx      
      
         pop ecx
         pop ebx
      
         retf                                   
                             
;-------------------------------------------------------------------------------
initiate_task_switch: 
         pushad
         push ds
         push es

         mov eax, core_data_seg_sel
         mov es, eax

         mov eax, mem_0_4_gb_seg_sel
         mov ds, eax

         mov eax, [es:tcb_chain]               ; Get the linear address of the first TCB node.
         cmp eax, 0                            ; See if we have available tasks.
         jz .return                            ; If negative, then return.

         ; Search the task with busy state in the TCB chain.
  .b0:
         cmp word [eax + 0x04], 0xffff         ; If it's a busy task?
         cmove esi, eax                        ; Save TCB address to esi if positive.
         jz .b1
         mov eax, [eax]                        ; Otherwise, keeping searching.
         jmp .b0

         ; Keep searching the task to find a task with "ready" state.
  .b1:
         mov ebx, [eax]                        ; Get the next task after the busy one.
         or ebx, ebx                           ; If it's the end?
         jz .b2                                ; Go to .b2 if positive.
         cmp word [ebx + 0x04], 0x0000         ; If it's ready?
         cmove edi, ebx                        ; Save TCB address to esi if positive.
         jz .b3
         mov eax, ebx                          ; Search the next node.      
         jmp .b1

  .b2:
         mov ebx, [es:tcb_chain]               ; Move to the head of the TCB chain.
  .b20:
         cmp word [ebx + 0x04], 0x0000         ; If it's ready?
         cmove edi, ebx                        ; Save TCB address to esi if positive.
         jz .b3
         mov ebx, [ebx]                        
         or ebx, ebx                           ; If it's the end again?
         jz .return                            ; No available task exists right now.
         jmp .b20

         ; Perform task switch.
  .b3:
         not word [esi + 0x04]                 ; Reverse the state of the busy task.
         not word [edi + 0x04]                 ; Reverse the state of the ready task.
         jmp far [edi + 0x14]                  ; Jump via TSS selector (2 bytes) and base address (4 bytes).

  .return:
         pop es
         pop ds
         popad

         retf

;-------------------------------------------------------------------------------
terminate_current_task:        
         mov eax, core_data_seg_sel
         mov es, eax

         mov eax, mem_0_4_gb_seg_sel
         mov ds, eax

         mov eax, [es:tcb_chain]
  .s0:
         cmp word [eax + 0x04], 0xffff      ; Find a busy task. 
         jz .s1      
         mov eax, [eax]
         jmp .s0

  .s1:
         mov word [eax + 0x04], 0x3333      ; Change state to "HALT".
         mov ebx, [es:tcb_chain]     
  .s2:
         cmp word [ebx + 0x04], 0x0000      ; Find a ready task.
         jz .s3                            
         mov ebx, [ebx]
         jmp .s2
  .s3:
         not word [ebx + 0x04]              ; Change the ready task to busy.
         jmp far [ebx + 0x14]               ; Jump to run this task.

;-------------------------------------------------------------------------------
general_interrupt_handler:                  ; A generic interrupt handler.
         push eax

         mov al, 0x20                       ; Send halt signal to 8259A.
         out 0xa0, al   
         out 0x20, al  

         pop eax

         iretd

;-------------------------------------------------------------------------------
general_exception_handler:                  ; A generic exception handler.
         mov ebx, excep_msg
         call sys_routine_seg_sel:put_string

         hlt

;-------------------------------------------------------------------------------
rtm_0x70_interrupt_handle: 
         pushad

         mov al, 0x20                       ; Send "EOI" instruction to 8259A.
         out 0xa0, al    
         out 0x20, al   

         mov al, 0x0c                       ; Reset register C in CMOS.
         out 0x70, al
         in al, 0x71      
         call sys_routine_seg_sel:initiate_task_switch

         popad
         iretd
;-------------------------------------------------------------------------------
do_task_clean:  
         retf

sys_routine_end:

;===============================================================================
SECTION core_data vstart=0      
;------------------------------------------------------------------------------- 
         pgdt             dw  0  
                          dd  0
                        
         pidt             dw  0
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
                          dd  terminate_current_task
                          dw  sys_routine_seg_sel

         salt_5           db  '@InitTaskSwitch'
                     times 256 - ($ - salt_5) db 0
                          dd  initiate_task_switch
                          dw  sys_routine_seg_sel

         salt_item_len   equ $ - salt_5
         salt_items      equ ($ - salt) / salt_item_len

         message_0        db  '  If you seen this message,it means we are now '
                          db  'in protected mode, and the IDT is mounted.'
                          db  0x0d,0x0a,0
                          
         cpu_brnd0        db  0x0d,0x0a,'  ',0
         cpu_brand  times 52  db 0
         cpu_brnd1        db  0x0d,0x0a,0x0d,0x0a,0
         
         message_1        db  '  System wide CALL-GATE mounted and test OK.'
                          db  0x0d,0x0a,0
                          
         excep_msg        db  '********Exception encounted********',0

         bin_hex          db '0123456789ABCDEF'

         core_buf   times 2048 db 0  

         tcb_chain        dd  0

         core_msg1        db  'Core task created.',0x0d,0x0a,0
                 
         core_msg2        db  '[CORE TASK]: I am working!',0x0d,0x0a,0

core_data_end:
               
;===============================================================================
SECTION core_code vstart=0
;-------------------------------------------------------------------------------
fill_descriptor_in_ldt: 
         push eax
         push edx
         push edi
         push ds

         mov ecx, mem_0_4_gb_seg_sel
         mov ds, ecx

         mov edi, [ebx + 0x0c]  
         
         xor ecx, ecx
         mov cx, [ebx + 0x0a] 
         inc cx      
         
         mov [edi + ecx + 0x00], eax
         mov [edi + ecx + 0x04], edx   

         add cx, 8                           
         dec cx               

         mov [ebx + 0x0a], cx          

         mov ax, cx
         xor dx, dx
         mov cx, 8
         div cx
         
         mov cx, ax
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
      
         mov ebp, esp                        ; Update ebp for accessing stack values.
      
         mov ecx, mem_0_4_gb_seg_sel
         mov es, ecx                         ; es -> (4GB segment descriptor)
      
         mov esi, [ebp + 11 * 4]             ; TCB address -> esi.

         ; Allocate memory for LDT (160 bytes for 20 descriptors).
         mov ecx, 160          
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi + 0x0c], ecx            ; Save basd address of LDT into TCB.
         mov word [es:esi + 0x0a], 0xffff    ; Save limit of LDT (size - 1) into TCB.


         mov eax, core_data_seg_sel
         mov ds, eax                         ; ds -> kernel data area.
       
         mov eax,[ebp + 12 * 4]              ; Get starting sector of the user program.
         mov ebx, core_buf                   ; Read program header into kernel buffer.   
         call sys_routine_seg_sel:read_hard_disk_0

         // Calculate how many sectors need to read.
         mov eax, [core_buf]                 ; Retrieve the length of user program.
         mov ebx, eax
         and ebx, 0xfffffe00                 ; Clear the last 9 bits.
         add ebx, 512                        ; Add one more sector.
         test eax, 0x000001ff                ; If it's a multiple of 512, we use eax.
         cmovnz eax, ebx                     ; Otherwise, use ebx.
      
         mov ecx, eax                        ; Size of user program.
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi + 0x06], ecx            ; Save the base address of user program into TCB.
      
         mov ebx, ecx      
         xor edx, edx
         mov ecx, 512
         div ecx                             ; edx:eax / 512 => eax, edx.
         mov ecx, eax                        ; Get the count of sectors need to read.
      
         mov eax, mem_0_4_gb_seg_sel
         mov ds, eax

         mov eax, [ebp + 12 * 4]             ; Get starting sector of the user program.
  .b1:
         call sys_routine_seg_sel:read_hard_disk_0
         inc eax
         loop .b1                            ; Read ecx times (each a sector).

         mov edi, [es:esi + 0x06]            ; Read base address of user program from TCB.

         ; Create header segment for user program, and install in LDT.
         mov eax, edi                        ; Set start address of the segment.
         mov ebx, [edi + 0x04]               ; Set the limit (size) of the segment.
         dec ebx                        
         mov ecx, 0x0040f200                 ; Set the segment attribute (DPL = 3)
         call sys_routine_seg_sel:make_seg_descriptor
      
         ; 64 bits segment descriptor will be returned in edx:dax.
         mov ebx, esi 
         call fill_descriptor_in_ldt

         or cx, 0000_0000_0000_0011B         ; Update the returned selector (RPL = 3)
         mov [es:esi + 0x44], cx             ; Update in TCB.
         mov [edi + 0x04], cx                ; Update in program header.
      
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
         or cx,0 000_0000_0000_0011B 
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
         push ecx
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi + 0x1e], ecx      
         mov eax, ecx
         mov ebx, [es:esi + 0x1a]  
         mov ecx, 0x00c09200 
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx, esi     
         call fill_descriptor_in_ldt
         or cx, 0000_0000_0000_0001   
         mov [es:esi + 0x22], cx  
         pop dword [es:esi + 0x24]

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
         mov [es:esi + 0x3a], ecx    
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
      
         ; Update previous TSS and 0, 1, 2 privileged stack info (ss, esp).
         mov word [es:ecx + 0], 0             

         mov edx, [es:esi + 0x24]              
         mov [es:ecx + 4], edx                 ; ESP0.

         mov dx, [es:esi + 0x22]            
         mov [es:ecx + 8], dx                  ; SS0.

         mov edx,[es:esi + 0x32]            
         mov [es:ecx + 12], edx                ; ESP1.

         mov dx, [es:esi + 0x30]            
         mov [es:ecx + 16], dx                 ; SS1.

         mov edx, [es:esi + 0x40]             
         mov [es:ecx + 20], edx                ; ESP2.

         mov dx, [es:esi + 0x3e]              
         mov [es:ecx + 24], dx                 ; SS2.

         mov dx, [es:esi + 0x10]         
         mov [es:ecx + 96], dx                 ; LDT selector.

         mov dx, [es:esi + 0x12]
         mov [es:ecx + 102], dx                ; IO mapping area address = 103.

         mov word [es:ecx + 100], 0            ; T = 0.

         mov dword [es:ecx + 28], 0            ; CR3 = 0.

         ; Update fields in TSS for running the task later.
         mov ebx, [ebp + 11 * 4]               ; Get TCB base address.
         mov edi, [es:ebx + 0x06]              ; Get program load base address.

         mov edx, [es:edi + 0x08]              ; Get program entry offset.
         mov [es:ecx + 32], edx                ; TSS.EIP.

         mov dx, [es:edi + 0x0c]               ; Get selector for program cs.
         mov [es:ecx + 76], dx                 ; TSS.CS.

         mov dx, [es:edi + 0x1c]               ; Get selector for program stack.
         mov [es:ecx + 80], dx                 ; TSS.SS.

         mov dx, [es:edi + 0x04]               ; Get selector for program ds.
         mov word [es:ecx + 84], dx            ; TSS.DS.
      
         mov word [es:ecx + 72], 0             ; TSS.ES = 0.

         mov word [es:ecx + 88], 0             ; TSS.FS = 0.

         mov word [es:ecx + 92], 0             ; TSS.GS = 0.

         pushfd
         pop dword [es:ecx + 36]               ; TSS.EFLAGS.


         mov eax,[es:esi + 0x14]               ; Get TSS base address.
         movzx ebx, word [es:esi + 0x12]       ; Get TSS limit.
         mov ecx, 0x00008900                   ; TSS descriptor attributes (DPL = 0).
         call sys_routine_seg_sel:make_seg_descriptor
         ; Install TSS descriptor in GDT.
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

                                             
         mov eax, [tcb_chain]               ; Test if chain is empty?
         or eax, eax                        ; Jump to .notch if positive.
         jz .notcb 
         
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
         mov ds, ecx

         mov ecx, mem_0_4_gb_seg_sel 
         mov es, ecx                                    ; es -> 4GB area.

         mov eax, general_exception_handler             ; Set the offset of generic exception handler.
         mov bx, sys_routine_seg_sel                    ; Set the selector for kernel routine segment.
         mov cx, 0x8e00                                 ; Set the attributes for interrupt gate (32 bits, DPL = 0).
         call sys_routine_seg_sel:make_gate_descriptor  ; Create a gate (returned by edx, eax).

         mov ebx, idt_linear_address                    ; Set the base address of IDT.
         xor esi, esi
  .idt0:
         mov [es:ebx + esi * 8], eax
         mov [es:ebx + esi * 8 + 4], edx
         inc esi
         cmp esi, 19                                    ; Create 20 IDT entires.
         jle .idt0


         mov eax, general_interrupt_handler             ; Set the offset of generic interrupt handler.
         mov bx, sys_routine_seg_sel                    ; Set the selector for kernel routine segment.
         mov cx, 0x8e00                                 ; Set the attributes for interrupt gate (32 bits, DPL = 0).
         call sys_routine_seg_sel:make_gate_descriptor

         mov ebx, idt_linear_address                    ; Set the base address of IDT.
  .idt1:
         mov [es:ebx + esi * 8], eax
         mov [es:ebx + esi * 8 + 4], edx
         inc esi
         cmp esi, 255                                   ; Create the remaining 236 IDT entires.
         jle .idt1

         ; Install the interrupt gate for switching tasks.
         mov eax, rtm_0x70_interrupt_handle            
         mov bx, sys_routine_seg_sel 
         mov cx, 0x8e00    
         call sys_routine_seg_sel:make_gate_descriptor

         mov ebx, idt_linear_address 
         mov [es:ebx + 0x70 * 8], eax
         mov [es:ebx + 0x70 * 8 + 4],edx

         
         mov word [pidt], 256 * 8 - 1                   ; Calculate and save the limit of IDT.
         mov dword [pidt + 2], idt_linear_address       ; Save the base address of IDT.
         lidt [pidt]                                    ; Load above information into IDTR.

         ; Reset 8259A to map its interrupt vectors to 0x20~0x27.
         mov al, 0x11
         out 0x20, al     
         mov al, 0x20
         out 0x21, al      
         mov al, 0x04
         out 0x21, al      
         mov al, 0x01
         out 0x21, al        

         mov al, 0x11
         out 0xa0, al      
         mov al, 0x70
         out 0xa1, al      
         mov al, 0x02     
         out 0xa1, al    
         mov al, 0x01
         out 0xa1, al      


         mov al, 0x0b   
         or al, 0x80   
         out 0x70, al
         mov al, 0x12   
         out 0x71, al    

         in al, 0xa1    
         and al, 0xfe     
         out 0xa1, al     

         mov al, 0x0c
         out 0x70, al
         in al, 0x71 

         sti                                            ; Enable interrupt.

         mov ebx, message_0             
         call sys_routine_seg_sel:put_string
                                         
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
         call sys_routine_seg_sel:put_string
         mov ebx, cpu_brnd1
         call sys_routine_seg_sel:put_string

         ; Install call gates.
         mov edi, salt                           ; Load the address of kernel SALT.
         mov ecx, salt_items                     ; Set iteration time (for gates).
  .b3:
         push ecx   
         mov eax, [edi + 256]                    ; Move routine offset to eax.
         mov bx, [edi + 260]                     ; Move routine segment selector to bx.
         mov cx, 1_11_0_1100_000_00000B          ; Move gate attribute to cx (P = 1, DPL = 3).

         ; Create a gate descriptor, and install it in kernel GDT.
         call sys_routine_seg_sel:make_gate_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi + 260], cx                     ; Replace the original segment selector with the created gate selector in C-SALT.    
         add edi, salt_item_len                  ; Move to next SALT item.
         pop ecx                                 ; Restore ecx.
         loop .b3

         ; Test just created call gates.
         mov ebx,message_1
         call far [salt_1 + 256]                 ; Call kernel routine via gate selector (the 32 bits offset would be ignored).       

         ; Allocate memory for creating a TCB.
         cli
         mov ecx, 0x46                           ; Specify the size of a TCB.
         call sys_routine_seg_sel:allocate_memory
         call append_to_tcb_link                 ; Append created TCB to the chain.
         mov esi, ecx                            ; Save the base address of this TCB.

         ; Create a TSS for kernel task.
         mov ecx, 104                            ; Specify the size of a TSS.
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi + 0x14], ecx                ; Save TSS base address into TCB.
      
         ; Update other fields in TSS.
         mov word [es:ecx + 96], 0               ; LDT Selector -> none.
         mov word [es:ecx + 102], 103            ; I/O Permission Bitmap -> none.
         mov word [es:ecx + 0], 0                ; TSS Pointer for Next Task -> none.
         mov dword [es:ecx + 28], 0              ; CR3 -> none.
         mov word [es:ecx + 100], 0              ; T -> 0.


         ; Create TSS descriptor.
         mov eax, ecx                            ; Specify TSS base address.
         mov ebx, 103                            ; Specify TSS limit.
         mov ecx, 0x00008900                     ; Specify TSS attributes.
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov word [es:esi + 0x18], cx            ; Update TSS selector in TCB.
         mov word [es:esi + 0x04], 0xffff        ; Update kernel state (busy) in TCB.

         
         ; Load kernel TSS into TR (TSS.B = 1).
         ltr cx
         sti

         mov ebx, core_msg1
         call sys_routine_seg_sel:put_string

         ; Create the first user task.
         cli
         mov ecx, 0x46
         call sys_routine_seg_sel:allocate_memory
         mov word [es:ecx + 0x04], 0  
         call append_to_tcb_link 
      
         push dword 50  
         push ecx     
       
         call load_relocate_program
         sti

         nop
         nop
         nop

         ; Create the second user task.
         cli
         mov ecx, 0x46
         call sys_routine_seg_sel:allocate_memory
         mov word [es:ecx + 0x04], 0   
         call append_to_tcb_link     

         push dword 100        
         push ecx            

         call load_relocate_program
         sti

  .do_switch:
         mov ebx, core_msg2
         call sys_routine_seg_sel:put_string
         call sys_routine_seg_sel:do_task_clean
         hlt

         jmp .do_switch

core_code_end:

;-------------------------------------------------------------------------------
SECTION core_trail
;-------------------------------------------------------------------------------
core_end:
