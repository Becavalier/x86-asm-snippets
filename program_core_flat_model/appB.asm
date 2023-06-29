SECTION header vstart=0

         program_length   dd program_end  
         entry_point      dd start    
header_end:
  
;===============================================================================
SECTION data vfollows=header

         message_1        db  '[USER TASK]: CCCCCCCCCCCCCCCCCCCCCCC',0x0d,0x0a,0

         reserved  times 4096*5 db 0  
data_end:

;===============================================================================
      [bits 32]
;===============================================================================
SECTION code vfollows=data
start:
         mov eax, 0                          ; Call 0th system call.
         mov ebx, message_1
         int 0x88                
         jmp start

code_end:

;-------------------------------------------------------------------------------
SECTION trail
;-------------------------------------------------------------------------------
program_end:
