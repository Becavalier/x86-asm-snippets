         flat_core_code_seg_sel  equ  0x0008          ; Selector for data segment (RPL = 0).
         flat_core_data_seg_sel  equ  0x0010          ; Selector for code segment (RPL = 0).
         flat_user_code_seg_sel  equ  0x001b          ; Selector for data segment (RPL = 3).
         flat_user_data_seg_sel  equ  0x0023          ; Selector for code segment (RPL = 3).

         idt_linear_address    equ  0x8001f000        ; Virtual address of kernel IDT.
         core_lin_alloc_at     equ  0x80100000        ; Virtual address of allocatable memory for kernel.
         core_lin_tcb_addr     equ  0x8001f800        ; Virtual address of kernel TCB.

;-------------------------------------------------------------------------------
; Virtual address of the kernel program (all the offset within this segment should add this value before addressing).
SECTION header vstart=0x80040000                      

         core_length      dd core_end                 ; Global offset (no "vstart" and "vfollows" in that segment). 

         core_entry       dd start                    ; Offset based on "vfollows".

;===============================================================================
         [bits 32]
;===============================================================================
SECTION sys_routine vfollows=header                   ; The virtual address continues based on above segment. 
;-------------------------------------------------------------------------------
  
put_string:                
         push ebx
         push ecx

         pushfd                                       ; Save eflags (IF is included).
         cli                                          ; Disable interrupt.

  .getc:
         mov cl, [ebx]
         or cl, cl            
         jz .exit                
         call put_char
         inc ebx
         jmp .getc

  .exit:
         popfd                                        ; Reset eflags.

         pop ecx
         pop ebx

         ret

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
         and ebx, 0x0000ffff                

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
         shl bx, 1
         mov [0x800b8000 + ebx], cl               ; Character position -> memory location (virtual address).

         shr bx,1
         inc bx

  .roll_screen:
         cmp bx, 2000                        
         jl .set_cursor

         push bx                           

         cld
         mov esi, 0x800b80a0                 
         mov edi, 0x800b8000                 
         mov ecx, 1920
         rep movsd
         mov bx, 3840                       
         mov ecx, 80                        
  .cls:
         mov word [0x800b8000 + ebx], 0x0720
         add bx, 2
         loop .cls
                    
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
         cli

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
         add ebx,2
         loop .readw

         pop edx
         pop ecx
         pop eax

         sti

         ret

;-------------------------------------------------------------------------------
put_hex_dword: 
         pushad

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

         popad
         ret

;-------------------------------------------------------------------------------
set_up_gdt_descriptor: 
         push eax
         push ebx
         push edx

         sgdt [pgdt]       

         movzx ebx,word [pgdt]              
         inc bx                             
         add ebx, [pgdt + 2]                  

         mov [ebx], eax
         mov [ebx + 4], edx

         add word [pgdt], 8                  

         lgdt [pgdt]                       

         mov ax, [pgdt]                     
         xor dx, dx
         mov bx, 8
         div bx                             
         mov cx, ax
         shl cx, 3                           

         pop edx
         pop ebx
         pop eax

         ret
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

         ret

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

         ret

;-------------------------------------------------------------------------------
allocate_a_4k_page:           


         push ebx
         push ecx
         push edx

         xor eax,eax
  .b1:
         bts [page_bit_map], eax              ; Find the first bit which is 0.
         jnc .b2                              ; Jump to ".b2" if CF = 0.
         inc eax 
         cmp eax, page_map_len * 8
         jl .b1

         mov ebx, message_3
         call put_string
         hlt                          

  .b2:
         shl eax, 12                          ; Return the physical address of the selected page.

         pop edx
         pop ecx
         pop ebx

         ret

;-------------------------------------------------------------------------------
alloc_inst_a_page:                          ; ebx - the virtual address for memory allocation.


         push eax
         push ebx
         push ecx
         push esi

 
         mov esi, ebx                       ; ebx -> esi.
         and esi, 0xffc00000                ; Only keep higher 10 bits for indexing PDT.
         shr esi, 20                        ; Right shift by 20 bits.
         or esi, 0xfffff000                 ; Mask the higher 20 bits to 1 (0xfffff is able to access PDT and the lower 12 bits are offset in PDT).

         test dword [esi], 0x00000001       ; Test if the LSB is 1 (P bit).
         jnz .b1                            ; The PDT item exists if positive.

         ; Create a new PDT item.
         call allocate_a_4k_page            ; Allocate a 4KB page in physical memory for PT.
         or eax, 0x00000007                 ; Set attributes for just created PDT item (US = 1, RW = 1, P = 1).
         mov [esi], eax                     ; Save the PDT item into PDT.


         mov eax, ebx
         and eax, 0xffc00000                ; Only keep higher 10 bits.
         shr eax, 10
         or eax, 0xffc00000                 ; Mask the higher 10 bits to 1.
         mov ecx, 1024
  .cls0:
         mov dword [eax], 0x00000000        ; Zero all the items in PT.
         add eax, 4
         loop .cls0

  .b1:
         ; Check PT and see if the corresponding PT item is valid.
         mov esi, ebx
         and esi, 0xfffff000               
         shr esi, 10                       
         or esi, 0xffc00000   

         test dword [esi], 0x00000001       ; Test bit "P".
         jnz .b2     

         ; Create a new 4KB page if P = 0.
         call allocate_a_4k_page            
         or eax, 0x00000007                 ; Set up attributes.
         mov [esi], eax                     ; Update the PT item.

  .b2:
         pop esi
         pop ecx
         pop ebx
         pop eax

         ret

;-------------------------------------------------------------------------------
create_copy_cur_pdir:   


         push esi
         push edi
         push ebx
         push ecx

         call allocate_a_4k_page            ; Allocate a 4KB physical page.
         mov ebx, eax
         or ebx, 0x00000007                 ; Add attributes (US = 1, RW = 1, P = 1).
         mov [0xfffffff8], ebx              ; Save this address into the 2nd PDT entry from the last (the address in this entry points to the newly created PDT for user task).

         invlpg [0xfffffff8]                ; Refresh TLB.

         mov esi, 0xfffff000                ; esi -> kernel PDT·
         mov edi, 0xffffe000                ; edi -> task PDT.
         mov ecx, 1024                      ; Copy over 1024 items.
         cld
         repe movsd                         ; Copy data in batch.

         pop ecx
         pop ebx
         pop edi
         pop esi

         ret

;-------------------------------------------------------------------------------
task_alloc_memory:                          ; ecx - memory size that needs to relocate.
                                            ; ebx - base address of TCB.


         push eax

         push ebx           


         mov ebx, [ebx + 0x06]              ; Retrieve the allocatable address.
         mov eax, ebx                       ; ebx -> eax.
         add ecx, ebx                       ; Update the next available address, and save it into ecx.

         push ecx            


         and ebx, 0xfffff000                ; Zero the lower 12 bits for easy comparison.
         and ecx, 0xfffff000
  .next:
         call alloc_inst_a_page             ; Update PDT and PT, allocate physical memory accordingly (1KB every time).

         add ebx, 0x1000                    ; See if we need to allocate more pages (4KB each)?
         cmp ebx, ecx
         jle .next


         pop ecx  

         test ecx, 0x00000003               ; If it aligns to 4 bits (test by "and" operation).
         jz .algn        
         add ecx, 4                         ; Align manually.
         and ecx, 0xfffffffc                ; Zero lower 3 bits.

  .algn:
         pop ebx     

         mov [ebx + 0x06], ecx              ; Update next allocatable address.  
         mov ecx, eax                       ; Return allocated memory address.

         pop eax

         ret

;-------------------------------------------------------------------------------
allocate_memory:   


         push eax
         push ebx


         mov eax, [tcb_chain]         


  .s0:
         cmp word [eax + 0x04], 0xffff          ; Find the task with busy state.
         jz .s1                      
         mov eax, [eax]
         jmp .s0


  .s1:
         mov ebx, eax                           ; Save the base address of the found TCB.
         call task_alloc_memory

         pop ebx
         pop eax

         ret

;-------------------------------------------------------------------------------
resume_task_execute:                     ; edi - new task's TCB address.

         mov eax, [edi + 10]             ; Retrieve new ESP0.
         mov [tss + 4], eax              ; Reset it in TSS.

         mov eax, [edi + 22]             ; Retrieve new cr3.
         mov cr3, eax       
         ; Reset all the other fields.
         ; !!! cs, ip can only be updated by instruction call. !!!
         mov ds, [edi + 34]
         mov es, [edi + 36]
         mov fs, [edi + 38]
         mov gs, [edi + 40]
         mov eax, [edi + 42]
         mov ebx, [edi + 46]
         mov ecx, [edi + 50]
         mov edx, [edi + 54]
         mov esi, [edi + 58]
         mov ebp, [edi + 66]

         test word [edi + 32], 3         ; If it's privilege 0 (SS.RPL)?
         jnz .to_r3                      
         mov esp, [edi + 70]             ; Reset ESP.
         mov ss, [edi + 32]              ; Reset SS.
         jmp .do_sw

  .to_r3:
         push dword [edi + 32]             ; Push SS.
         push dword [edi + 70]             ; Push ESP.
  .do_sw:
         push dword [edi + 74]             ; Push EFLAGS.
         push dword [edi + 30]             ; Push CS.
         push dword [edi + 26]             ; Push EIP.

         not word [edi + 0x04]             ; Reverse task state.
         mov edi, [edi + 62]               ; Reset edi.

         iretd                             ; Emualte an interrupt return.

;-------------------------------------------------------------------------------
initiate_task_switch:    


         push eax
         push ebx
         push esi
         push edi

         mov eax, [tcb_chain]                ; Get the linear address of the first TCB node.
         cmp eax, 0                          ; See if we have available tasks.
         jz .return                          ; If negative, then return.

         ; Search the task with busy state in the TCB chain.
  .b0:
         cmp word [eax + 4], 0xffff          ; If it's a busy task?
         cmove esi, eax                      ; Save TCB address to esi if positive.
         jz .b1
         mov eax, [eax]                      ; Otherwise, keeping searching.
         jmp .b0                            

         ; Keep searching the task to find a task with "ready" state.
  .b1:
         mov ebx, [eax]                      ; Get the next task after the busy one.
         or ebx, ebx                         ; If it's the end?
         jz .b2                              ; Go to .b2 if positive.
         cmp word [ebx + 4], 0               ; If it's ready?
         cmove edi, ebx                      ; Save TCB address to esi if positive.
         jz .b3
         mov eax, ebx                        ; Search the next node.      
         jmp .b1

  .b2:
         mov ebx, [tcb_chain]                ; Move to the head of the TCB chain.
  .b20:
         cmp word [ebx + 4], 0               ; If it's ready?
         cmove edi, ebx                      ; Save TCB address to esi if positive.
         jz .b3
         mov ebx, [ebx]
         or ebx, ebx                         ; If it's the end again?
         jz .return                          ; No available task exists right now.
         jmp .b20

  .b3:
         ; Perform task switch (esi - current task TCB, edi - next task TCB).
         mov eax, cr3
         mov [esi + 22], eax                 ; Save cr3.
         ; Save other information into TCB.
         mov [esi + 50], ecx                 ; Other GPRs will be reset from the stack.
         mov [esi + 54], edx
         mov [esi + 66], ebp
         mov [esi + 70], esp
         mov dword [esi + 26], .return       ; Save return address to TCB.EIP (where old task will resume).
         mov [esi + 30], cs
         mov [esi + 32], ss
         mov [esi + 34], ds
         mov [esi + 36], es
         mov [esi + 38], fs
         mov [esi + 40], gs
         pushfd
         pop dword [esi + 74]
         not word [esi + 4]                  ; Reverse task state to "Ready".

         jmp resume_task_execute        

  .return:
         pop edi
         pop esi
         pop ebx
         pop eax

         ret

;-------------------------------------------------------------------------------
terminate_current_task: 
         mov edi, [tcb_chain]
  .s0:
         cmp word [edi + 4], 0xffff
         jz .s1                            
         mov edi, [edi]
         jmp .s0

  .s1:
         mov word [edi + 4], 0x3333
         mov edi, [tcb_chain] 
  .s2:
         cmp word [edi + 4], 0x0000
         jz .s3   
         mov edi, [edi]
         jmp .s2

  .s3:
         jmp resume_task_execute     

;-------------------------------------------------------------------------------
general_interrupt_handler:     
         push eax

         mov al, 0x20                  
         out 0xa0, al                        
         out 0x20, al                       

         pop eax
         iretd

;-------------------------------------------------------------------------------
general_exception_handler:  
         mov ebx, excep_msg
         call put_string

         cli

         hlt

;-------------------------------------------------------------------------------
rtm_0x70_interrupt_handle:           
         push eax

         mov al, 0x20                       ; Send "EOI" instruction to 8259A.
         out 0xa0, al                     
         out 0x20, al                     

         mov al, 0x0c                       ; Reset register C in CMOS.
         out 0x70, al
         in al, 0x71                      
                                          
         call initiate_task_switch  

         pop eax

         iretd

;-------------------------------------------------------------------------------
do_task_clean:  
         ret

;-------------------------------------------------------------------------------
int_0x88_handler:   

         call [eax * 4 + sys_call]            ; Jump to corresponding system call hander.
         iretd

;===============================================================================
SECTION core_data vfollows=sys_routine  
;------------------------------------------------------------------------------- 
         pgdt             dw  0          
                          dd  0

         pidt             dw  0
                          dd  0

         page_bit_map     db  0xff,0xff,0xff,0xff,0xff,0xff,0x55,0x55
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
         page_map_len     equ $-page_bit_map

         ; System call table.
         sys_call         dd put_string
                          dd read_hard_disk_0
                          dd put_hex_dword
                          dd terminate_current_task
                          dd initiate_task_switch
                          dd allocate_memory

         message_0        db 'Setup interrupt system and system-call......', 0

         message_1        db  'Done.', 0x0d, 0x0a, 0

         message_2        db  'TSS is created.', 0x0d, 0x0a, 0

         cpu_brnd0        db  0x0d,0x0a,'  ',0
         cpu_brand  times 52  db 0
         cpu_brnd1        db  0x0d,0x0a,0x0d,0x0a,0

         message_3        db  '********No more pages********',0

         excep_msg        db  '********Exception encounted********',0

         bin_hex          db '0123456789ABCDEF'
                                            

         core_buf   times 2048 db 0  

         tcb_chain        dd  0         

         tss              times 128 db  0   

         core_msg1        db  'Core task created.',0x0d,0x0a,0
                 
         core_msg2        db  '[CORE TASK]: I am working!',0x0d,0x0a,0

;===============================================================================
SECTION core_code vfollows=core_data
;-------------------------------------------------------------------------------
load_relocate_program:                  


         pushad
      
         mov ebp, esp                   
      
         ; Point to the current kernel PDT.
         mov ebx, 0xfffff000
         xor esi, esi
  .clsp:
         mov dword [ebx + esi * 4], 0x00000000           ; Clear the first 512 PDT entries (user land).
         inc esi
         cmp esi, 512
         jl .clsp

         mov ebx, cr3                                    ; Refresh TLB.
         mov cr3, ebx


         mov eax, [ebp + 10 * 4]                         ; Get start sector of the user program.
         mov ebx, core_buf            
         call read_hard_disk_0                           ; Read program header.

         ; Calculate how many sectors need to read for the whole program.
         mov eax, [core_buf]               
         mov ebx, eax
         and ebx, 0xfffffe00                
         add ebx, 512                      
         test eax, 0x000001ff               
         cmovnz eax, ebx                  

         mov esi, [ebp + 9 * 4]                          ; Get TCB address -> esi.

         mov ecx, eax                                    ; Memory allocation size.
         mov ebx, esi
         call task_alloc_memory                          ; Allocate memory with given TCB (ebx).

         mov ebx, ecx                       
         xor edx, edx
         mov ecx, 512
         div ecx
         mov ecx, eax                                    ; Sector count pending to read.               

         mov eax, [ebp + 10 * 4]                         ; Get start sector of the user program.
  .b1:
         call read_hard_disk_0                           ; Read program data from disk.
         inc eax
         loop .b1                      

         ; Allocate memory for level 3 user stack.
         mov ebx, esi                                    ; Address of task TCB.  
         mov ecx, 4096                                   ; 1KB.
         call task_alloc_memory
         mov ecx, [esi + 6]                              ; Save next allocatable memory address to ecx (moving downward as stack pointer).
         mov dword [esi + 70], ecx                       ; Save it to field esp in TCB (ESP).

         ; Allocate memory for level 0 user stack.
         mov ebx, esi
         mov ecx, 4096                
         call task_alloc_memory
         mov ecx, [esi + 6]               
         mov dword [esi + 10], ecx                       ; Save it to field esp in TCB (ESP0).


         ; Copy the current PDT for user task.
         call create_copy_cur_pdir
         mov [esi + 22], eax                             ; Save PDT address to TCB (CR3).

         mov word [esi + 30], flat_user_code_seg_sel     ; TCB - CS.
         mov word [esi + 32], flat_user_data_seg_sel     ; TCB - SS.
         mov word [esi + 34], flat_user_data_seg_sel     ; TCB - DS.
         mov word [esi + 36], flat_user_data_seg_sel     ; TCB - CS.
         mov word [esi + 38], flat_user_data_seg_sel     ; TCB - FS.
         mov word [esi + 40], flat_user_data_seg_sel     ; TCB - GS.
         mov eax, [0x04]                                 ; Get user task entrance address.
         mov [esi + 26], eax                             ; TCB - EIP.
         pushfd
         pop dword [esi + 74]                            ; TCB - EFLAGS.
         mov word [esi + 4], 0                           ; TCB - State to "Ready".

         popad
      
         ret 8                             
      
;-------------------------------------------------------------------------------
append_to_tcb_link: 
         push eax
         push edx

         pushfd
         cli
         mov dword [ecx + 0x00], 0  
                                      
                                             
         mov eax, [tcb_chain]                
         or eax, eax                     
         jz .notcb 
         
  .searc:
         mov edx, eax
         mov eax, [edx + 0x00]
         or eax, eax               
         jnz .searc

         mov [es:edx + 0x00], ecx
         jmp .retpc
         
  .notcb:       
         mov [tcb_chain], ecx             
         
  .retpc:
         popfd

         pop edx
         pop eax
         
         ret
         
;-------------------------------------------------------------------------------
start:

         mov ebx, message_0
         call put_string

         ; Set up IDT and system call.
         mov eax, general_exception_handler            ; Set the offset of generic exception handler.
         mov bx, flat_core_code_seg_sel                ; Set the selector for kernel routine segment.
         mov cx, 0x8e00                                ; Set the attributes for interrupt gate (32 bits, DPL = 0).
         call make_gate_descriptor                     ; Create a gate (returned by edx, eax).

         mov ebx, idt_linear_address                   ; Set the virtual base address of IDT.
         xor esi, esi
  .idt0:
         mov [ebx + esi * 8], eax
         mov [ebx + esi * 8 + 4], edx
         inc esi
         cmp esi, 19                                   ; Create 20 IDT entires.
         jle .idt0


         mov eax, general_interrupt_handler            ; Set the offset of generic interrupt handler.
         mov bx, flat_core_code_seg_sel                ; Set the selector for kernel routine segment.
         mov cx, 0x8e00                                ; Set the attributes for interrupt gate (32 bits, DPL = 0).
         call make_gate_descriptor

         mov ebx, idt_linear_address                   ; Set the virtual base address of IDT.
  .idt1:
         mov [ebx + esi * 8], eax
         mov [ebx + esi * 8 + 4], edx
         inc esi
         cmp esi, 255                                  ; Create the remaining 236 IDT entires.
         jle .idt1

         ; Install the interrupt gate for switching tasks.
         mov eax, rtm_0x70_interrupt_handle
         mov bx, flat_core_code_seg_sel 
         mov cx, 0x8e00                 
         call make_gate_descriptor

         mov ebx, idt_linear_address        
         mov [ebx + 0x70 * 8], eax           
         mov [ebx + 0x70 * 8 + 4], edx

         ; Create a new gate (interrupt 0x88) for system call.
         mov eax, int_0x88_handler          
         mov bx, flat_core_code_seg_sel 
         mov cx, 0xee00                                ; Set the attributes for interrupt gate (32 bits, DPL = 3).
         call make_gate_descriptor

         mov ebx, idt_linear_address 
         mov [ebx + 0x88 * 8], eax                     ; Install as 88th interrupt.
         mov [ebx + 0x88 * 8 + 4], edx

         ; Load IDTR.
         mov word [pidt], 256 * 8 - 1                  ; Calculate and save the limit of IDT.
         mov dword [pidt + 2], idt_linear_address      ; Save the base address of IDT.
         lidt [pidt]                                   ; Load above information into IDTR.


         mov ebx, message_1
         mov eax, 0                        
         int 0x88                          

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

         sti                                           ; Enable interrupt.

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
         call put_string
         mov ebx, cpu_brand
         call put_string
         mov ebx, cpu_brnd1
         call put_string

         ; Create only one global TSS for task switch.
         ; Task related states will be saved in their own TCB.
         mov ecx, 32
         xor ebx, ebx
  .clear:
         mov dword [tss + ebx], 0                      ; Clear TSS area (128 bytes).
         add ebx, 4
         loop .clear



         mov word [tss + 8], flat_core_data_seg_sel    ; Set SS0 -> kernel data area.
         mov word [tss + 102], 103                     ; Set I/O bitmap.

         ; Create TSS descriptor and install it in GDT.
         mov eax, tss                                  ; Specify TSS base address.
         mov ebx, 103                                  ; Specify TSS limit.
         mov ecx, 0x00008900                           ; Specify TSS attributes.
         call make_seg_descriptor
         call set_up_gdt_descriptor

         ; Load kernel TSS into TR (TSS.B = 1).
         ltr cx

         mov ebx, message_2
         call put_string


         mov ecx, core_lin_tcb_addr                    ; Set address of TCB.
         mov word [ecx + 4], 0xffff                    ; Set kernel task state to busy.
         mov dword [ecx + 6], core_lin_alloc_at        ; Set next allocatable memory address for kernel task.
         call append_to_tcb_link           


         mov ebx, core_msg1
         call put_string

         ; Create TCB for user task.
         mov ecx, 128                                  ; TCB size.
         call allocate_memory                          ; TCB will be used by kernel, so allocate them in kernel space.
         mov word [ecx + 0x04], 0                      ; Update task state to "Ready".
         mov dword [ecx + 0x06], 0                     ; Update task allocatable virtual memory address (initial state -> 0x0).

         push dword 50                                 ; Start sector for user task on disk.
         push ecx                                      ; Start address of TCB.
         call load_relocate_program
         call append_to_tcb_link                       ; Append task TCB (in ecx) to the TCB chain.    

         ; Create TCB for user task.
         mov ecx, 128                                   
         call allocate_memory
         mov word [ecx + 0x04], 0           
         mov dword [ecx + 0x06], 0         

         push dword 100      
         push ecx                     

         call load_relocate_program
         call append_to_tcb_link       

  .do_switch:
         mov ebx, core_msg2
         call put_string


         call do_task_clean

         hlt

         jmp .do_switch

;-------------------------------------------------------------------------------
SECTION core_tail 
;-------------------------------------------------------------------------------
core_end:
