

        ; ----------------------------------------------------                  -|
        ; |              Text Mode Visual Buffer             |                   |
        ; |                  (Selector: 0x20)                |                   |
        ; ----------------------------------------------------  0x000b8000       |
        ; ~                                                  ~                   |
        ; ----------------------------------------------------                   |
        ; |        Kernel Code Segment (Selector: 0x38)      |                   |
        ; |        Kernel Data Segment (Selector: 0x30)      |                   |
        ; |  Kernel Public Routines Segment (Selector: 0x28) |                   |
        ; ----------------------------------------------------  0x00040000       |- 0~4GB Data Segment
        ; ~                                                  ~                   |   (Selector: 0x08)
        ; ----------------------------------------------------                   |
        ; |                  Kernel's first PT               |                   |
        ; ----------------------------------------------------  0x00021000       |
        ; |                     Kernel PDT                   |                   |  
        ; ----------------------------------------------------  0x00020000       |
        ; ~                                                  ~                   | 
        ; ----------------------------------------------------                   |
        ; |                 TCB for Kernel Tasks             |                   |  
        ; ----------------------------------------------------  0x0001f800       |
        ; |                     Global IDT                   |                   |  
        ; ----------------------------------------------------  0x0001f000       |
        ; ~                                                  ~                   | 
        ; ----------------------------------------------------                   |
        ; |                     Global GDT                   |                   |  
        ; ----------------------------------------------------  0x00007e00       |
        ; |                 Initial Code (MBR)               |                   |  
        ; ----------------------------------------------------  0x00007c00       |
        ; |                    Kernel Stack                  |                   |
        ; |                  (Selector: 0x18)                |                   | 
        ; ----------------------------------------------------  0x00006c00      -|
        ; ~                                                  ~
     
         core_code_seg_sel     equ  0x38 
         core_data_seg_sel     equ  0x30 
         sys_routine_seg_sel   equ  0x28  
         video_ram_seg_sel     equ  0x20  
         core_stack_seg_sel    equ  0x18  
         mem_0_4_gb_seg_sel    equ  0x08  
         idt_linear_address    equ  0x1f000 
         core_lin_alloc_at     equ  0x80100000        
         core_lin_tcb_addr     equ  0x8001f800
                                          
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
allocate_a_4k_page:      


         push ebx
         push ecx
         push edx
         push ds

         mov eax, core_data_seg_sel
         mov ds, eax

         xor eax, eax
  .b1:
         bts [page_bit_map], eax                ; Find the first bit which is 0.
         jnc .b2                                ; Jump to ".b2" if CF = 0.
         inc eax
         cmp eax, page_map_len * 8
         jl .b1

         mov ebx, message_3
         call sys_routine_seg_sel:put_string
         hlt                               

  .b2:
         shl eax, 12                            ; Return the physical address of the selected page.

         pop ds
         pop edx
         pop ecx
         pop ebx

         ret

;-------------------------------------------------------------------------------
alloc_inst_a_page:                                 ; ebx - the virtual address for memory allocation.

         ; Test if the corresponding PDT item (map to higher 10 bits) exists, create it if negative.
         push eax
         push ebx
         push ecx
         push esi
         push ds

         mov eax, mem_0_4_gb_seg_sel               ; For accessing PDT and PT.
         mov ds, eax


         mov esi, ebx                              ; ebx -> esi.
         and esi, 0xffc00000                       ; Only keep higher 10 bits for indexing PDT.
         shr esi, 20                               ; Right shift by 20 bits.
         or esi, 0xfffff000                        ; Mask the higher 20 bits to 1 (0xfffff is able to access PDT and the lower 12 bits are offset in PDT).

         test dword [esi], 0x00000001              ; Test if the LSB is 1 (P bit).
         jnz .b1                                   ; The PDT item exists if positive.

         ; Create a new PDT item.
         call allocate_a_4k_page                   ; Allocate a 4KB page in physical memory for PT.
         or eax, 0x00000007                        ; Set attributes for just created PDT item (US = 1, RW = 1, P = 1).
         mov [esi], eax                            ; Save the PDT item into PDT.


         mov eax, ebx                              ; ebx -> eax.
         and eax, 0xffc00000                       ; Only keep higher 10 bits.
         shr eax, 10                              
         or eax, 0xffc00000                        ; Mask the higher 10 bits to 1.
         mov ecx, 1024                             
  .cls0:
         mov dword [es:eax], 0x00000000            ; Zero all the items in PT.
         add eax, 4
         loop .cls0

  .b1:
         ; Check PT and see if the corresponding PT item is valid.
         mov esi, ebx
         and esi, 0xfffff000                       ; Only keep the higher 20 bits.
         shr esi, 10                             
         or esi, 0xffc00000           

         test dword [esi], 0x00000001              ; Test bit "P".
         jnz .b2                                   

         ; Create a new 4KB page if P = 0.
         call allocate_a_4k_page            
         or eax, 0x00000007                        ; Set up attributes.
         mov [esi], eax                            ; Update the PT item.

  .b2:
         pop ds
         pop esi
         pop ecx
         pop ebx
         pop eax

         retf

;-------------------------------------------------------------------------------
create_copy_cur_pdir:  


         push ds
         push es
         push esi
         push edi
         push ebx
         push ecx

         mov ebx, mem_0_4_gb_seg_sel
         mov ds, ebx
         mov es, ebx

         call allocate_a_4k_page             ; Allocate a 4KB physical page.
         mov ebx, eax
         or ebx, 0x00000007                  ; Add attributes (US = 1, RW = 1, P = 1).
         mov [0xfffffff8], ebx               ; Save this address into the 2nd PDT entry from the last (the address in this entry points to the newly created PDT for user task).

         invlpg [0xfffffff8]                 ; Refresh TLB.

         mov esi, 0xfffff000                 ; esi -> kernel PDT·
         mov edi, 0xffffe000                 ; edi -> task PDT.
         mov ecx, 1024                       ; Copy over 1024 items.
         cld
         repe movsd                          ; Copy data in batch.

         pop ecx
         pop ebx
         pop edi
         pop esi
         pop es
         pop ds

         retf

;-------------------------------------------------------------------------------
task_alloc_memory:                                   ; ecx - memory size.
                                                     ; ebx - base address of TCB.
   

         push eax

         push ds

         push ebx    


         mov ax, mem_0_4_gb_seg_sel
         mov ds, ax

         mov ebx, [ebx + 0x46]                       ; Retrieve the start allocation address.
         mov eax, ebx                                ; ebx -> eax.
         add ecx, ebx                                ; Update the next available address.

         push ecx                                    ; Save ecx.         


         and ebx, 0xfffff000                         ; Zero the lower 12 bits for easy comparison.
         and ecx, 0xfffff000
  .next: 
         call sys_routine_seg_sel:alloc_inst_a_page  ; Update PDT and PT, allocate physical memory accordingly.
                                            
         add ebx, 0x1000                             ; See if we need to allocate more pages (4KB each)?
         cmp ebx, ecx
         jle .next

         
         pop ecx                                     

         test ecx, 0x00000003                        ; Align the address to 4 bits ("and" operation).
         jz .algn      
         add ecx, 4                                  ; Align manually.
         and ecx, 0xfffffffc                         ; Zero lower 3 bits.

  .algn:
         pop ebx                   

         mov [ebx + 0x46], ecx                       ; Save the next available address.
         mov ecx, eax                                ; Return allocated memory address.

         pop ds

         pop eax

         retf

;-------------------------------------------------------------------------------
allocate_memory:   


         push eax
         push ebx

         push ds


         mov eax, core_data_seg_sel                   ; ds -> kernel data segment.
         mov ds, eax

         mov eax, [tcb_chain]                

         mov ebx, mem_0_4_gb_seg_sel
         mov ds, ebx


  .s0:
         cmp word [eax + 0x04], 0xffff                ; Find the task with busy state.
         jz .s1            
         mov eax,[eax]
         jmp .s0


  .s1:
         mov ebx, eax                                 ; Save the base address of the found TCB.
         call sys_routine_seg_sel:task_alloc_memory   ; Allocate memory.

         pop ds

         pop ebx
         pop eax

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
SECTION core_data vstart=0                  ;ÏµÍ³ºËÐÄµÄÊý¾Ý¶Î 
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
                          dd  terminate_current_task
                          dw  sys_routine_seg_sel

         salt_5           db  '@InitTaskSwitch'
                     times 256-($-salt_5) db 0
                          dd  initiate_task_switch
                          dw  sys_routine_seg_sel

         salt_6           db  '@malloc'
                     times 256-($-salt_6) db 0
                          dd  allocate_memory
                          dw  sys_routine_seg_sel

         salt_item_len   equ $-salt_6
         salt_items      equ ($-salt)/salt_item_len

         message_0        db  '  System core is runing in protect mode,'
                          db  'IDT is mounted.',0x0d,0x0a,0

         cpu_brnd0        db  0x0d,0x0a,'  ',0
         cpu_brand  times 52  db 0
         cpu_brnd1        db  0x0d,0x0a,0x0d,0x0a,0

         message_1        db  '  Paging is enabled.System core is mapped to'
                          db  ' linear address 0x80000000.',0x0d,0x0a,0

         message_2        db  '  System wide CALL-GATE mounted and test OK.'
                          db  0x0d,0x0a,0

         message_3        db  '********No more pages********',0

         excep_msg        db  '********Exception encounted********',0

         bin_hex          db '0123456789ABCDEF'
                                           

         core_buf   times 2048 db 0        

         tcb_chain        dd  0

         core_msg1        db  '  Core task created.',0x0d,0x0a,0
                 
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
      
         mov ebp, esp                                   ; Save esp for quickly accessing stack.
      
         mov ecx, mem_0_4_gb_seg_sel
         mov es, ecx

         ; Point to the current kernel PDT.
         mov ebx, 0xfffff000
         xor esi, esi
  .clsp:
         mov dword [es:ebx + esi * 4], 0x00000000       ; Clear the first 512 PDT entries (user land).
         inc esi
         cmp esi, 512
         jl .clsp

         mov ebx, cr3                                   ; Refresh TLB.
         mov cr3, ebx

         mov esi, [ebp + 11 * 4]                        ; Get TCB address -> esi.

         ; Create LDT for user task.
         mov ebx, esi
         mov ecx, 160                                   ; Specify LDT size.    
         call sys_routine_seg_sel:task_alloc_memory     ; Allocate memory in the task space (with task TCB).
         mov [es:esi + 0x0c], ecx                       ; Update LDT address in TCB.
         mov word [es:esi+0x0a],0xffff                  ; Update LDT limit (0 - 1 = 0xfffff) in TCB.


         mov eax, core_data_seg_sel
         mov ds, eax    
       
         mov eax, [ebp + 12 * 4]                        ; Get start sector of the user program.
         mov ebx, core_buf                              ; Read program header into kernel buffer.
         call sys_routine_seg_sel:read_hard_disk_0

         ; Calculate how many sectors need to read for the whole program.
         mov eax, [core_buf]                      
         mov ebx, eax
         and ebx, 0xfffffe00             
         add ebx, 512                
         test eax, 0x000001ff               
         cmovnz eax, ebx                
      
         mov ecx, eax                       
         mov ebx, esi
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi + 0x06], ecx                       ; Save program start address in TCB.
      
         mov ebx, ecx           
         xor edx, edx
         mov ecx, 512
         div ecx
         mov ecx, eax                       
      
         mov eax, mem_0_4_gb_seg_sel     
         mov ds, eax

         mov eax, [ebp + 12 * 4]                
  .b1:
         call sys_routine_seg_sel:read_hard_disk_0      ; Read program data from disk.
         inc eax
         loop .b1                          

         mov edi, [es:esi + 0x06]     

         ; Create segment descriptors and store them in LDT.
         mov eax, edi        
         mov ebx, [edi + 0x04]              
         dec ebx                           
         mov ecx, 0x0040f200               
         call sys_routine_seg_sel:make_seg_descriptor
      

         mov ebx, esi                        
         call fill_descriptor_in_ldt

         or cx, 0000_0000_0000_0011B    
         mov [es:esi + 0x44], cx          
         mov [edi + 0x04], cx                
      
     
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

         ; Relocate user SALT in the user program header (header segment).
         mov eax, mem_0_4_gb_seg_sel                    ; For accessing program SALT.
         mov es, eax                                    
                                                   
         mov eax, core_data_seg_sel                     ; For accessing kernel SALT.
         mov ds, eax
      
         cld

         mov ecx, [es:edi + 0x24]                       ; Get U-SALT item count in app side.
         add edi, 0x28                                  ; edi -> U-SALT base address.
  .b2: 
         push ecx
         push edi
      
         mov ecx, salt_items
         mov esi, salt
  .b3:
         push edi
         push esi
         push ecx

         mov ecx, 64                                    ; 256 / 4 = 64.    
         repe cmpsd                                     ; Compare via de:esi (kernel) and es:edi (app). 
         jnz .b4                                        ; If same, ZF = 1.
         mov eax, [esi]                                 ; Otherwise, update routine selector and offset into app header.
         mov [es:edi - 256], eax                        ; Update offset (4 bytes).
         mov ax, [esi + 4]
         or ax, 0000000000000011B                       ; Change RPL to 3.

         mov [es:edi - 252], ax                         ; Update gate selector (2 bytes).
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

         mov esi, [ebp + 11 * 4]                        ; Get TCB base address.

         ; Create a segement descriptor for privilege 0 stack.
         mov ecx, 0                  
         mov [es:esi + 0x1a], ecx            
         inc ecx
         shl ecx, 12                     
         push ecx
         mov ebx, esi
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi + 0x1e], ecx              
         mov eax, ecx
         mov ebx, [es:esi + 0x1a]            
         mov ecx, 0x00c09200               
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx, esi                      
         call fill_descriptor_in_ldt         
         mov [es:esi + 0x22], cx              
         pop dword [es:esi + 0x24]           

         ; Create a segement descriptor for privilege 1 stack.
         mov ecx, 0
         mov [es:esi + 0x28], ecx            
         inc ecx
         shl ecx, 12                        
         push ecx
         mov ebx, esi
         call sys_routine_seg_sel:task_alloc_memory
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

         ; Create a segement descriptor for privilege 2 stack.
         mov ecx, 0
         mov [es:esi + 0x36], ecx       
         inc ecx
         shl ecx, 12                     
         push ecx
         mov ebx, esi
         call sys_routine_seg_sel:task_alloc_memory
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

         ; Create a descriptor for LDT and install it in GDT.
         mov eax, [es:esi + 0x0c]             
         movzx ebx, word [es:esi + 0x0a]      
         mov ecx, 0x00008200                
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi + 0x10], cx          
       
         ; Create TSS for user task.
         mov ecx, 104                     
         mov [es:esi + 0x12], cx              
         dec word [es:esi + 0x12]             
         call sys_routine_seg_sel:allocate_memory       ; Allocate memory in kernel space.
         mov [es:esi + 0x14], ecx                       ; Save TSS address into TCB.           
      
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

         ; Update fields in TSS for running the task later.
         mov ebx, [ebp + 11 * 4]               ; Get TCB base address.
         mov edi, [es:ebx + 0x06]              ; Get program load base address.

         mov edx, [es:edi + 0x08]              ; Get program entry offset.
         mov [es:ecx + 32], edx                ; TSS.EIP.

         mov dx, [es:edi + 0x0c]               ; Get selector for program cs.
         mov [es:ecx + 76], dx                 ; TSS.CS.

         mov dx, [es:edi + 0x1c]               ; Get selector for program stack.
         mov [es:ecx + 80], dx                 ; TSS.SS.

         mov edx,[es:edi + 0x20]              
         mov [es:ecx + 56], edx                ; TSS.ESP.

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

       
         ; Copy the current kernel PDT for user program.
         call sys_routine_seg_sel:create_copy_cur_pdir
         mov ebx, [es:esi + 0x14]       
         mov dword [es:ebx + 28], eax          ; Save the returned physical address of PDT into TSS (for cr3's use).

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

         cli

         mov eax, core_data_seg_sel   
         mov ds, eax
         mov eax, mem_0_4_gb_seg_sel  
         mov es, eax
         
         mov dword [es:ecx + 0x00], 0       ; Mark the tail of chain.
 
                                             
         mov eax, [tcb_chain]               ; Test if chain is empty?
         or eax, eax                        ; Jump to ".notch" if positive.
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
         sti

         pop es
         pop ds
         pop edx
         pop eax
         
         ret
         
;-------------------------------------------------------------------------------
start:
         mov ecx, core_data_seg_sel                      ; ds -> kernel data area.
         mov ds, ecx

         mov ecx, mem_0_4_gb_seg_sel  
         mov es, ecx                                     ; es -> 4GB area.





         mov eax, general_exception_handler              ; Set the offset of generic exception handler.
         mov bx, sys_routine_seg_sel                     ; Set the selector for kernel routine segment.
         mov cx, 0x8e00                                  ; Set the attributes for interrupt gate (32 bits, DPL = 0).
         call sys_routine_seg_sel:make_gate_descriptor   ; Create a gate (returned by edx, eax).

         mov ebx, idt_linear_address                     ; Set the base address of IDT.
         xor esi, esi
  .idt0:
         mov [es:ebx + esi * 8], eax
         mov [es:ebx + esi * 8 + 4],edx
         inc esi
         cmp esi, 19                                     ; Create 20 IDT entires.
         jle .idt0


         mov eax, general_interrupt_handler              ; Set the offset of generic interrupt handler.
         mov bx, sys_routine_seg_sel                     ; Set the selector for kernel routine segment.
         mov cx, 0x8e00                                  ; Set the attributes for interrupt gate (32 bits, DPL = 0).
         call sys_routine_seg_sel:make_gate_descriptor

         mov ebx, idt_linear_address                     ; Set the base address of IDT.
  .idt1:
         mov [es:ebx + esi * 8], eax
         mov [es:ebx + esi * 8 + 4], edx
         inc esi
         cmp esi, 255                                    ; Create the remaining 236 IDT entires.
         jle .idt1
   
         mov eax, rtm_0x70_interrupt_handle 
         mov bx, sys_routine_seg_sel
         mov cx, 0x8e00 
         call sys_routine_seg_sel:make_gate_descriptor

         mov ebx, idt_linear_address                     
         mov [es:ebx + 0x70 * 8], eax
         mov [es:ebx + 0x70 * 8 + 4], edx


         mov word [pidt], 256 * 8 - 1                    ; Calculate and save the limit of IDT.
         mov dword [pidt + 2], idt_linear_address        ; Save the base address of IDT.
         lidt [pidt]                                     ; Load above information into IDTR.

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

         mov ecx, 1024                                  ; Zero PDT area (set P = 1).
         mov ebx, 0x00020000                            ; Physical address of the PDT table.
         xor esi, esi
  .b1:
         mov dword [es:ebx + esi], 0x00000000
         add esi, 4
         loop .b1

         ; Set up the last item in PDT (for quickly accessing PDT).
         mov dword [es:ebx + 4092], 0x00020003          ; (P = 1, RW = 1, US = 0).

         ; Set up the first item in PDT (for kernel).
         mov dword [es:ebx + 0], 0x00021003 

         ; Set up the first 256 page table items (for kernel).
         mov ebx, 0x00021000  
         xor eax, eax  
         xor esi, esi
  .b2:
         mov edx, eax
         or edx, 0x00000003                             ; Leave only the first 20 bits and add other attributes (P = 1, RW = 1, US = 0).
         mov [es:ebx + esi * 4], edx                    ; Set the corresponding PTE.
         add eax, 0x1000                                ; Move to next page table item.
         inc esi
         cmp esi, 256                                   ; Iterate 256 times (x 4KB -> lower 1MB).
         jl .b2

  .b3:                                       
         mov dword [es:ebx + esi * 4], 0x00000000       ; Zero other PTEs.
         inc esi
         cmp esi, 1024
         jl .b3


         mov eax, 0x00020000 
         mov cr3, eax                                   ; Reset cr3 with the address of PDT (PCD = 0, PWT = 0).

         cli

         mov eax, cr0                                   ; Enable paging (bit 31 on cr0).
         or eax, 0x80000000
         mov cr0, eax                   







         ; Kernel area (higher 2GB).
         mov dword [es:0xfffff800], 0x00021003          ; Map the original kernel space to a higher address.


         sgdt [pgdt]
 
         mov ebx, [pgdt + 2]                            ; Get base address of GDT.

         or dword [es:ebx + 0x10 + 4], 0x80000000       ; Change the base address of descriptors in GDT.
         or dword [es:ebx + 0x18 + 4], 0x80000000
         or dword [es:ebx + 0x20 + 4], 0x80000000
         or dword [es:ebx + 0x28 + 4], 0x80000000
         or dword [es:ebx + 0x30 + 4], 0x80000000
         or dword [es:ebx + 0x38 + 4], 0x80000000

         add dword [pgdt + 2], 0x80000000               ; Map the original GDT to a higher address.

         lgdt [pgdt]


         sidt [pidt] 
         add dword [pidt + 2], 0x80000000               ; Map the original IDT to a higher address.
         lidt [pidt]

         jmp core_code_seg_sel:flush                    ; Flush cs to use the descriptor in the new location .

   flush:  
         mov eax, core_stack_seg_sel                    ; Flush ss, ds as well (es keeps the same).
         mov ss, eax

         mov eax, core_data_seg_sel
         mov ds, eax

         sti

         mov ebx, message_1
         call sys_routine_seg_sel:put_string



         mov edi, salt           
         mov ecx, salt_items      
  .g0:
         push ecx   
         mov eax, [edi + 256]          
         mov bx, [edi + 260]                
         mov cx, 1_11_0_1100_000_00000B    
                      
                     
         call sys_routine_seg_sel:make_gate_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi + 260], cx                   
         add edi, salt_item_len   
         pop ecx
         loop .g0


         mov ebx, message_2
         call far [salt_1 + 256]  

         ; Create kernel task.
         mov ecx, core_lin_tcb_addr                     ; Set the address of kernel TCB.
         mov word [es:ecx + 0x04], 0xffff               ; Set task state to busy (0xffff).
         mov dword [es:ecx + 0x46], core_lin_alloc_at   ; Set the next memory allocatable address for the current task.
                                            
         call append_to_tcb_link                        ; Add task to the task chain.

         mov esi, ecx

         ; Create TSS for kernel task.
         mov ecx, 104                                   ; Memory size for kernel TSS.
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi + 0x14], ecx                       ; Save the returned virtual address (TSS location) into TCB.

         ; Fill in TSS fields.
         mov word [es:ecx + 0], 0              
         mov eax, cr3
         mov dword [es:ecx + 28], eax                   ; Save cr3 into TSS.
         mov word [es:ecx + 96], 0                      ; No LDT.
         mov word [es:ecx + 100], 0                     ; T = 0.
         mov word [es:ecx + 102], 103                   ; Set I/O Bitmap.


         
         ; Create a TSS descriptor, and install it into GDT.
         mov eax, ecx              
         mov ebx, 103                  
         mov ecx, 0x00008900          
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov word [es:esi + 0x18], cx                   ; Update the TSS selector in TCB.

         
         ; Update TR with the current kernel task's TSS selector. 
         ltr cx


         mov ebx, core_msg1
         call sys_routine_seg_sel:put_string

         ; Create a TCB (74 bytes) for user task.
         mov ecx, 0x4a
         call sys_routine_seg_sel:allocate_memory       ; Allocate memory in kernel space. 
         mov word [es:ecx + 0x04], 0                    ; Update task state to "Ready".
         mov dword [es:ecx + 0x46], 0                   ; Update task allocatable virtual memory address (initial state -> 0x0).

         push dword 50                                  ; Start sector for user task on disk.
         push ecx                                       ; Start address of TCB.
         call load_relocate_program
         call append_to_tcb_link                        ; Add user task into TCB chain.      

         ; Create another user task.
         mov ecx, 0x4a
         call sys_routine_seg_sel:allocate_memory       ; Allocate memory in kernel space.
         mov word [es:ecx + 0x04], 0          
         mov dword [es:ecx + 0x46], 0        

         push dword 100                
         push ecx                         

         call load_relocate_program
         call append_to_tcb_link      

  .do_switch:
         mov ebx, core_msg2
         call sys_routine_seg_sel:put_string


         call sys_routine_seg_sel:do_task_clean

         hlt                                            ; halt but will reflect to interrupt.

         jmp .do_switch

core_code_end:

;-------------------------------------------------------------------------------
SECTION core_trail
;-------------------------------------------------------------------------------
core_end:
