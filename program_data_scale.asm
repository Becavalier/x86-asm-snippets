         mov ax, cs
         mov ss, ax
         mov sp, 0x7c00

         mov ax, [cs:gdt_base + 0x7c00] 
         mov dx, [cs:gdt_base + 0x7c00 + 0x02]
         mov bx, 16        
         div bx            
         mov ds, ax  
         mov bx, dx 
      
         mov dword [bx + 0x00], 0x00
         mov dword [bx + 0x04], 0x00  

         mov dword [bx + 0x08], 0x8000ffff
         mov dword [bx + 0x0c], 0x0040920b

         mov dword [bx + 0x10], 0x7c0001ff            ; Set up descriptor for the current code segment. 
         mov dword [bx + 0x14], 0x00409800

         mov word [cs:gdt_size + 0x7c00], 23                        
         lgdt [cs:gdt_size + 0x7c00]
      
         in al, 0x92   
         or al, 0000_0010B
         out 0x92, al  

         cli     

         mov eax, cr0
         or eax, 1
         mov cr0, eax   
         ; Far jump by specifying segment selector and offest (the descriptor cache for cs will be updated).
         ; Basc address -> 0x00007c00.
         ; Limit -> 0x1ff.
         ; D -> 1 (use eip to fetch instruction).
         jmp 0000000000010_0_00B:flush                

         bits 32                                      ; Compile below code with 32 mode.
flush:
         mov cx, 0000000000001_0_00B                  ; Update segment descriptor for ds.
         mov ds, cx

         mov byte [0x00], 'P'
         mov byte [0x02], 'r'
         mov byte [0x04], 'o'
         mov byte [0x06], 't'
         mov byte [0x08], 'e'
         mov byte [0x0a], 'c'
         mov byte [0x0c], 't'
         mov byte [0x0e], ' '
         mov byte [0x10], 'm'
         mov byte [0x12], 'o'
         mov byte [0x14], 'd'
         mov byte [0x16], 'e'
         mov byte [0x18], ' '
         mov byte [0x1a], 'O'
         mov byte [0x1c], 'K'
         mov byte [0x1e], '.'
         hlt 
     
         gdt_size         dw 0
         gdt_base         dd 0x00007e00 
                             
         times 510-($-$$) db 0
                          db 0x55,0xaa
