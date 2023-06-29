SECTION header vstart=0

         program_length   dd program_end     
         entry_point      dd start         
header_end:
  
;===============================================================================
SECTION data vfollows=header

         message_1        db  '[USER TASK]: ,,,,,,,,,,,,,,,,,,,,,,,',0x0d,0x0a,0

         reserved  times 4096*5 db 0           
data_end:

;===============================================================================
      [bits 32]
;===============================================================================
SECTION code vfollows=data
start:
         mov eax, 5                ; Call memory allocation system call.
         mov ecx, 128                                
         int 0x88                                   
         mov ebx, ecx                              

         mov esi, message_1
         mov edi, ecx
         mov ecx, reserved - message_1
         cld
         repe movsb

.show:
         mov eax, 0
         int 0x88
         jmp .show

code_end:

;-------------------------------------------------------------------------------
SECTION trail
;-------------------------------------------------------------------------------
program_end:
