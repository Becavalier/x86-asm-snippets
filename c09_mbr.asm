app_lba_start equ 100
SECTION mbr align=16 vstart=0x7c00                                     
         mov ax, 0                    ; Initialize stack-related registers for program loader.
         mov ss, ax                   ; Start from 0xffff, going downward.
         mov sp, ax
      
         mov ax, [cs:phy_base]        ; Lower 4 bits.
         mov dx, [cs:phy_base + 0x2]  ; Higher 16 bits.
         mov bx, 16
         div bx                       ; Calculate segment address.         
         mov ds, ax                   ; Initialize ds & es.
         mov es, ax                        
    
         xor di, di            
         mov si, app_lba_start        ; LBA for user program on disk (static storage location).
         xor bx, bx 
         call read_hard_disk_0
      
         mov dx, [2]                  ; Read the program size (32 bits) field specified in its header.
         mov ax, [0]
         mov bx, 512                  ; 512 bytes a sector.
         div bx                       ; Calculate how many bytes need to read for the rest of the program code.
         cmp dx, 0
         jnz @1                       ; If any remainder?
         dec ax                       ; Minus 1 if the program just fits into complete segements.
   @1:
         cmp ax, 0
         jz direct
                                      ; Read the rest of the program code.
         push ds                      ; Save ds for later use.
         mov cx, ax                   ; Save the remaining sectors need to be read into cx.
   @2:
         mov ax, ds
         add ax, 0x20                 ; Save the incoming program code data into next segment.
         mov ds, ax  
                              
         xor bx, bx                   ; Offset inside the segment. 
         inc si                       ; Point to the next sector on the dist.
         call read_hard_disk_0
         loop @2
         pop ds                       ; Restore ds -> data segment for user program.
      
   direct:
         mov dx, [0x08]               ; Fetch code entry address.
         mov ax, [0x06]
         call calc_segment_base
         mov [0x06], ax               ; Write back the segment base.
      
         mov cx, [0x0a]               ; Get the length of the realloc table (for iteration).
         mov bx, 0x0c                 ; Get the head address of the realloc table.
          
 realloc:
         mov dx, [bx + 0x02]          
         mov ax, [bx]
         call calc_segment_base
         mov [bx], ax
         add bx, 4                    ; Overwrite the original low words.
         loop realloc 
         jmp far [0x04]               ; Jump to user program ([0x6] -> cs, [0x4] -> ip).
 
read_hard_disk_0:                    
         push ax
         push bx
         push cx
         push dx
      
         mov dx, 0x1f2             ; Set up sector count -> 1.
         mov al, 1
         out dx, al                       

         inc dx                    ; Set up LBA low byte (bits 0~7).     
         mov ax, si
         out dx, al                  

         inc dx                    ; Set up LBA mid byte (bits 8~15).           
         mov al, ah
         out dx, al               

         inc dx                    ; Set up LBA hi byte (bits 16~23).           
         mov ax, di
         out dx, al                  

         inc dx                    ; 1B1D TOP4LBA: B=LBA, D=drive；
         mov al, 0xe0              ; "1110 0000" -> 0xe0.   
         out dx, al

         inc dx 
         mov al, 0x20              ; Set up command -> "Read".
         out dx, al
  .waits:
         in al, dx                 ; Read disk status.
         and al, 0x88              ; Extract bits 3 and 7 by and "1000 1000".
         cmp al, 0x08              ; Disk is ready to be read if the status code is "0000 1000" (change from "1000 0000").
         jnz .waits                ; Re-check if still not ready.     

         mov cx, 256               ; Set up count of bytes we want to read.             
         mov dx, 0x1f0             ; Read from data port (0x1f0).
  .readw:
         in ax, dx
         mov [bx], ax              ; Read data to [ds:bx], 2 bytes a time.
         add bx, 2
         loop .readw

         pop dx
         pop cx
         pop bx
         pop ax
         ret

calc_segment_base:                     ; Receive a 32-bits address (dx:ax), return a 16-bits address (ax).
         push dx                       ; Save dx.             
         
         add ax, [cs:phy_base]         ; Add lower 16 bits.
         adc dx, [cs:phy_base + 0x02]  ; Add higher 16 bits with carry.
         shr ax, 4                     ; Right shift ax by 4 bits.
         shl dx, 12                    ; Left shift dx by 12 bits.
         or ax, dx                     ; Save result into ax.
         pop dx
         ret

         ; Physical memory address where to load the user program at (20 bits), 
         ; it should be 16-bit aligned, sticking to the boundary of a segment.
         phy_base dd 0x10000
         
 times 510-($-$$) db 0
                  db 0x55, 0xaa
