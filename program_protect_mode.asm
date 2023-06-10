         ; Still in real mode as of now.
         mov ax, cs                              ; Initialize stack.
         mov ss, ax
         mov sp, 0x7c00

         mov ax, [cs:gdt_base + 0x7c00]          ; Low 16 bit of GDT address. 
         mov dx, [cs:gdt_base + 0x7c00 + 0x02]   ; High 16 bit of GDT address. 
         mov bx, 16
         div bx                                  ; Get segment address.

         mov ds, ax                              ; Segement Address -> ax.
         mov bx, dx                              ; Offset -> dx.

         mov dword [bx + 0x00], 0x00             ; Set up null descriptor.
         mov dword [bx + 0x04], 0x00

         mov dword [bx + 0x08], 0x8000ffff       ; Set up descriptor for display memory.
         mov dword [bx + 0x0c], 0x0040920b

         mov word [cs:gdt_size + 0x7c00], 15     ; Set the size (limit) of gdt.
         lgdt [cs:gdt_size + 0x7c00]             ; Load the address and limit of gdt into gdtr.

         in al, 0x92                             ; Read data from port 0x92. 
         or al, 0000_0010B                       ; Enable A20 (bit 1).
         out 0x92, al                            ; Write back.

         cli                                     ; Disable interrupt.
 
         mov eax, cr0
         or eax, 1
         mov cr0, eax                            ; Enable protect mode via modifying cr0.

         mov cx, 0000000000001_000B             ; Set descriptor selector.
         mov ds, cx

         mov byte [0x00], 'P'                   ; Write texts into memory.
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
         gdt_base         dd 0x00007e00         ; Inital physical address of GDT.  

         times 510-($-$$) db 0
                          db 0x55, 0xaa
