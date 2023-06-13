         mov eax, cs      
         mov ss, eax                           ; Initialize segment register with a 32 bits GPR.
         mov sp, 0x7c00
      
         mov eax, [cs:pgdt + 0x7c00 + 0x02]    ; Load address of GDT.
         xor edx, edx
         mov ebx, 16
         div ebx

         mov ds, eax                           ; Set the segment address of GDT.
         mov ebx, edx                          ; Set the offset (effective address of GDT within the segement).


         mov dword [ebx + 0x00], 0x00000000
         mov dword [ebx + 0x04], 0x00000000  

         ; Install segment descriptor for ds 
         ; (base: 0x0, limit: 0xfffff, granularity: 4KB, scope: 0~4GB).
         mov dword [ebx + 0x08], 0x0000ffff      
         mov dword [ebx + 0x0c], 0x00cf9200

         ; Install segment descriptor for cs.
         ; (base: 0x00007C00, limit: 0x001ff, granularity: byte, scale: 32 bits).
         mov dword [ebx + 0x10], 0x7c0001ff      
         mov dword [ebx + 0x14], 0x00409800

         ; Install segment descriptor for ds.
         ; (base: 0x00007C00, limit: 0x001ff, granularity: byte, scale: 32 bits).
         mov dword [ebx + 0x18], 0x7c0001ff      
         mov dword [ebx + 0x1c], 0x00409200 

         ; Install segment descriptor for ss.
         ; (base: 0x00007C00, limit: 0xffffe, granularity: 4KB).
         mov dword [ebx + 0x20], 0x7c00fffe 
         mov dword [ebx + 0x24], 0x00cf9600
         
         mov word [cs:pgdt + 0x7c00], 39       ; Set the size of GDT.
         lgdt [cs:pgdt + 0x7c00]
      
         in al, 0x92 
         or al, 0000_0010B
         out 0x92, al

         cli 

         mov eax, cr0
         or eax, 1
         mov cr0, eax 

         jmp 0x0010:dword flush                ; Far jump with a segment selector and a 32 bits offset (effective address).

         [bits 32]                          
  flush:                                     
         mov eax, 0x0018                       ; 3rd selector.                    
         mov ds, eax

         mov eax, 0x0008                       ; 1st selector
         mov es, eax
         mov fs, eax
         mov gs, eax

         mov eax, 0x0020                       ; 4th selector.
         mov ss, eax
         xor esp, esp 
      
         mov dword [es:0x0b8000], 0x072e0750   ; Write to display memory via ds.
         mov dword [es:0x0b8004], 0x072e074d
         mov dword [es:0x0b8008], 0x07200720
         mov dword [es:0x0b800c], 0x076b076f 

         mov ecx, pgdt - string - 1            ; External loop of Bubble Sort.
  @@1:
         push ecx                              ; Save ecx.
         xor bx, bx                            ; Zero bx.
  @@2:                      
         mov ax, [string + bx]                 ; Read two adjacent characters.
         cmp ah, al                            ; Compare them.
         jge @@3 
         xchg al, ah                           ; Exchange al and ah.
         mov [string + bx], ax                 ; Save ax to the original pos.
  @@3:
         inc bx 
         loop @@2 
         pop ecx                               ; Reset ecx for external loop.
         loop @@1                              ; ecx = ecx - 1.
      
         mov ecx, pgdt - string
         xor ebx, ebx              
  @@4:               
         mov ah, 0x07
         mov al, [string + ebx]
         mov [es:0xb80a0 + ebx * 2], ax        ; Show the result of sorted string.
         inc ebx
         loop @@4
      
         hlt 

     string           db 's0ke4or92xap3fv8giuzjcy5l1m7hd6bnqtw.'

     pgdt             dw 0
                      dd 0x00007e00

     times 510-($-$$) db 0
                      db 0x55, 0xaa
