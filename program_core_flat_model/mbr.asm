         core_base_address equ 0x00040000          ; Physical loading address of the kernel.
         core_start_sector equ 0x00000001          ; Start sector on disk of the kernel program.

;===============================================================================
SECTION  mbr  vstart=0x00007c00         

         xor ax, ax                     
         mov ds, ax                                ; ds -> 0.
         mov ss, ax                                ; ss -> 0.
         mov sp, 0x7c00
      
         ; In real mode as of now (the maxminu size of segment is 64KB, 16 btis).
         mov eax, [pgdt + 0x02]                    ; Get the physical address of GDT.
         xor edx, edx
         mov ebx, 16
         div ebx                                   ; Covert it to logical address (segment + offset).

         mov ds, eax                               ; ds -> the segment where GDT resides.
         mov ebx, edx                              ; ebx -> offset in the segment.              

         ; Create segment descriptors.
         ; Privilege 0 - data segment.
         mov dword [ebx + 0x08], 0x0000ffff        ; Limit - 0xfffff, 4KB, base address - 0x0.
         mov dword [ebx + 0x0c], 0x00cf9800        ; DPL - 0x0.

         ; Privilege 0 - code segment.
         mov dword [ebx + 0x10], 0x0000ffff        ; Limit - 0xfffff, 4KB, base address - 0x0.
         mov dword [ebx + 0x14], 0x00cf9200        ; DPL - 0x0.

         ; Privilege 3 - data segment.
         mov dword [ebx + 0x18], 0x0000ffff        ; Limit - 0xfffff, 4KB, base address - 0x0.
         mov dword [ebx + 0x1c], 0x00cff800        ; DPL - 0x3.

         ; Privilege 3 - code segment.
         mov dword [ebx + 0x20], 0x0000ffff        ; Limit - 0xfffff, 4KB, base address - 0x0.
         mov dword [ebx + 0x24], 0x00cff200        ; DPL - 0x3.


         ; Set up the limit of GDT (40 - 1), lower 16 bits.
         mov word [cs:pgdt], 39  
 
         lgdt [cs:pgdt]                            ; Load GDT into GDTR.
      
         in al, 0x92                               ; Enable A20.
         or al, 0000_0010B
         out 0x92, al               

         cli                                       ; Disable interrupt.

         mov eax, cr0                  
         or eax, 1
         mov cr0, eax                              ; Enable protect mode.
      
         ; Jump to load the kernel.
         ; Register cs will be refreshed with the selector 1, pointing to a new code segment.
         jmp dword 0x0008:flush                    
                                          
         [bits 32]               
  flush:                                  
         mov eax, 0x00010                          ; Set up other segment registers.
         mov ds, eax
         mov es, eax
         mov fs, eax
         mov gs, eax
         mov ss, eax                         
         mov esp, 0x7c00                           ; Set up the stack top (moving downward).
         
         ; Start loading kernel program.
         mov edi, core_base_address

         mov eax, core_start_sector
         mov ebx, edi     
         call read_hard_disk_0                     ; Read the core from disk.


         mov eax, [edi]                            ; Read "program_length" of the kernel.
         xor edx, edx
         mov ecx, 512                        
         div ecx                                   ; See how many blocks we need to read from disk.

         or edx, edx                               ; Do we need an extra read? 
         jnz @1                                    ; No extra read needed if no remainder.      
         dec eax                           
   @1:
         or eax, eax                               ; Do we have all the kernel data in hand already (only 1 sector)?
         jz pge                                    ; Yes!

         ; Read remaining sectors.
         mov ecx, eax                              ; Times to read.
         mov eax, core_start_sector                ; Set up target disk sector number (starting from sector 2).
         inc eax                       
   @2:
         call read_hard_disk_0
         inc eax
         loop @2                    

   pge:
         ; Enable paging.
         mov ebx, 0x00020000                       ; Set the physical address of kernel PDT.
         
         ; Address in the last cell of PDT -> PDT.
         mov dword [ebx + 4092], 0x00020003        ; P = 1, PW = 1.

         ; Address in the cell offset 0x0 of PDT -> PD.
         mov edx, 0x00021003               
         mov [ebx + 0x000], edx               
                              
         ; Address in the cell offset 0x800 of PDT -> PD.
         mov [ebx + 0x800], edx       

         ; Map the first 256 pages in PD (lower 1MB physical memory for kernel).
         mov ebx, 0x00021000              
         xor eax, eax                  
         xor esi, esi
  .b1:       
         mov edx, eax
         or edx, 0x00000003                        ; Add attributes. attributes.                                                  
         mov [ebx + esi * 4], edx                  ; Copy the entry content.
         add eax, 0x1000                           ; Move to next PD entry.
         inc esi
         cmp esi, 256                        
         jl .b1
         

         mov eax, 0x00020000                       ; Set current active PDT address.
         mov cr3, eax

         ; Relocate GDT items (the base address part).
         sgdt [pgdt]

         add dword [pgdt + 2], 0x80000000          ; Move kernel area to a higher address.
         lgdt [pgdt]

         mov eax, cr0
         or eax, 0x80000000
         mov cr0, eax                              ; Enable paging.
   


         ; Reposition the kernel stack.
         add esp, 0x80000000                 
                                             
         jmp [0x80040004]                          ; Jump to the kernel.
       
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
      
         ret
;-------------------------------------------------------------------------------
         pgdt             dw 0                    ; Limit of GDT.
                          dd 0x00008000           ; Physical address of GDT.
;-------------------------------------------------------------------------------                             
         times 510-($-$$) db 0
                          db 0x55,0xaa
