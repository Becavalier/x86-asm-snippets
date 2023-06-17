       
        ; GDT structure after the kernel has been loaded.
        ; ----------------------------------------------------
        ; |          kernel - core code (DPL = 0)            |  0x38
        ; ----------------------------------------------------
        ; |          kernel - core data (DPL = 0)            |  0x30
        ; ----------------------------------------------------
        ; |          routine (000400000 ~, DPL = 0)          |  0x28
        ; ----------------------------------------------------
        ; |   visual memory (000b8000 ~ 000bffff, DPL = 0)   |  0x20
        ; ----------------------------------------------------
        ; |   initial stack (00006c00 ~ 00007c00, DPL = 0)   |  0x18
        ; ----------------------------------------------------
        ; |   initial code (00007c00 ~ 00007dff, DPL = 0)    |  0x10
        ; ----------------------------------------------------
        ; |    0~4GB data (00000000 ~ ffffffff, DPL = 0)     |  0x08
        ; ----------------------------------------------------
        ; |                  NULL                            |  0x00
        ; ----------------------------------------------------

         core_base_address equ 0x00040000     ; The location of memory it will be loaded into.
         core_start_sector equ 0x00000001     ; The starting sector of kernel program.
         
         mov ax, cs                           ; Initialize cs, ss, sp.
         mov ss, ax 
         mov sp, 0x7c00                       ; Stack starts at 0x7c00, going downward.
      
         mov eax, [cs:pgdt + 0x7c00 + 0x02]   ; Load the full linear memory of GDT.
         xor edx, edx
         mov ebx, 16
         div ebx                              ; Get segment address of GDT.

         mov ds, eax                          ; Set the segment address of GDT.
         mov ebx, edx                         ; Set the offset of GDT.

         ; Set up GDT items.
         mov dword [ebx + 0x08], 0x0000ffff   ; Descriptor for data segment (0 ~ 4GB).
         mov dword [ebx + 0x0c], 0x00cf9200 

         mov dword [ebx + 0x10], 0x7c0001ff   ; Descriptor for initial code segment.
         mov dword [ebx + 0x14], 0x00409800

         ; Max limit: 0xffffffff.
         mov dword [ebx + 0x18], 0x7c00fffe   ; Descriptor for initail stack segment.
         mov dword [ebx + 0x1c], 0x00cf9600
            
         mov dword [ebx + 0x20], 0x80007fff   ; Descriptor for visual memory.
         mov dword [ebx + 0x24], 0x0040920b
         
         mov word [cs:pgdt + 0x7c00], 39      ; Set up the limit of GDT.
 
         lgdt [cs:pgdt + 0x7c00]              ; Load GDT into GDTR.
      
         in al, 0x92                          ; Enable A20.
         or al, 0000_0010B
         out 0x92, al  

         cli                                  ; Disable interrupt.

         mov eax, cr0
         or eax, 1
         mov cr0, eax                         ; Enable protect mode.
      
         jmp 0x0010:flush                     ; Jump to load the core.
                   
         [bits 32]               
  flush:                                  
         mov eax, 0x0008                      ; Set 0~4GB segment (ds).
         mov ds, eax

         mov eax, 0x0018                      ; Set stack segment (ss).
         mov ss, eax
         xor esp, esp                         
         
         mov edi, core_base_address 
      
         mov eax, core_start_sector
         mov ebx, edi 
         call read_hard_disk_0                ; Read the core from disk.

         mov eax, [edi]                       ; Read "program_length" of the kernel.
         xor edx, edx 
         mov ecx, 512                         
         div ecx                              ; See how many blocks we need to read from disk.

         or edx, edx                          ; Do we need an extra read? 
         jnz @1                               ; No extra read needed.
         dec eax                              
   @1:
         or eax, eax                          ; Do we have all the kernel data in hand?
         jz setup                             ; Yes!

         mov ecx, eax                         ; Times to read.
         mov eax, core_start_sector           ; Set up target disk sector number.
         inc eax                 
   @2:
         call read_hard_disk_0
         inc eax
         loop @2             

 setup:                                       ; Install segment descriptors for kernel.
         mov esi, [pgdt + 0x7c00 + 0x02]      ; Get the base address of GDT.

         ; Routine section.
         mov eax, [edi + 0x04]                ; Retrieve the compiling address of routine section.
         mov ebx, [edi + 0x08]                ; Retrieve the compiling address of core data section.
         sub ebx, eax                         ; Calculate the size of routine section.
         dec ebx                              ; Calculate the limit (size - 1).
         add eax, edi                         ; Calculate the base address (plus loading address).
         mov ecx, 0x00409800                  ; Set up the segment attributes.
         call make_gdt_descriptor             ; Install the descriptor.
         mov [esi + 0x28], eax                ; Insert the newly created GDT item.
         mov [esi + 0x2c], edx
       
         ; Kernel data section.
         mov eax, [edi + 0x08]    
         mov ebx, [edi + 0x0c]      
         sub ebx, eax
         dec ebx          
         add eax, edi        
         mov ecx, 0x00409200        
         call make_gdt_descriptor
         mov [esi + 0x30], eax
         mov [esi + 0x34], edx 
      
         ; Kernel code section.
         mov eax, [edi + 0x0c]  
         mov ebx, [edi + 0x00] 
         sub ebx, eax
         dec ebx                
         add eax, edi      
         mov ecx, 0x00409800      
         call make_gdt_descriptor
         mov [esi + 0x38], eax
         mov [esi + 0x3c], edx

         mov word [0x7c00 + pgdt], 63         ; Revise the size of GDT.
                                        
         lgdt [0x7c00 + pgdt]                 ; Reload GDT into GDTR.

         jmp far [edi + 0x10]                 ; Jump into kernel (with 4GB ds segment).
       
read_hard_disk_0:  
         push eax 
         push ecx
         push edx
      
         push eax
         
         mov dx, 0x1f2                      ; Set up sector count -> 1.
         mov al, 1
         out dx, al 

         inc dx                             ; Set up LBA low byte (bits 0~7).     
         pop eax
         out dx, al 

         inc dx                             
         mov cl, 8                          ; Get LBA mid byte (by right-shift).           
         shr eax, cl
         out dx, al                         ; Set up LBA mid byte (bits 8~15).           

         inc dx  
         shr eax, cl
         out dx, al                         ; Set up LBA hi byte (bits 16~23).           

         inc dx      
         shr eax, cl
         or al, 0xe0                        ; Set up remaining 4 bits.           
         out dx, al

         inc dx 
         mov al, 0x20                       ; Set up command -> "Read".
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
         mov [ebx], ax                      ; Read data to [ds:ebx], 2 bytes a time.
         add ebx, 2         
         loop .readw

         pop edx
         pop ecx
         pop eax
         ; The value of ebx will be increased by 512 when return (point to next memory block).
         ret                                

make_gdt_descriptor: 
         mov edx, eax                        ; Construct a segment descriptor (low 32 bits)
         shl eax, 16                     
         or ax, bx                      
      
         and edx, 0xffff0000              
         rol edx, 8                          ; Rotational move left by 8 bits.
         bswap edx                           ; Reverse the byte order.
      
         and ebx, 0x000f0000                 ; Retrieve segment limit (bit 19~16).
         or edx, ebx                         ; Combine segment limit with its base address.
      
         or edx, ecx                         ; Combine with segment attributes.
      
         ret
      
         pgdt             dw 0
                          dd 0x00007e00 

         times 510-($-$$) db 0
                          db 0x55, 0xaa
