SECTION header vstart=0
         program_length   dd program_end 
         head_len         dd header_end 
         prgentry         dd start    
         code_seg         dd section.code.start 
         code_len         dd code_end  
         data_seg         dd section.data.start
         data_len         dd data_end  
         stack_seg        dd section.stack.start 
         stack_len        dd stack_end 
         salt_items       dd (header_end - salt) / 256
         salt:                              
         PrintString      db  '@PrintString'
                     times 256 - ($ - PrintString) db 0
                     
         TerminateProgram db  '@TerminateProgram'
                     times 256 - ($ - TerminateProgram) db 0
                     
         ReadDiskData     db  '@ReadDiskData'
                     times 256 - ($ - ReadDiskData) db 0

         InitTaskSwitch   db  '@InitTaskSwitch'
                     times 256 - ($ - InitTaskSwitch) db 0
                 
header_end:
;===============================================================================
SECTION data vstart=0                
         message_1        db  '[USER TASK]: CCCCCCCCCCCCCCCCCCCCCCC',0x0d,0x0a,0

data_end:
;===============================================================================
SECTION stack vstart=0
        times 2048        db 0 

stack_end:

;===============================================================================
      [bits 32]
;===============================================================================
SECTION code vstart=0
start:
         mov eax, ds
         mov fs, eax
     
         mov ax, [data_seg]
         mov ds, ax
     
.do_prn:
         mov ebx, message_1
         call far [fs:PrintString]
         jmp .do_prn

         call far [fs:TerminateProgram] 

code_end:

;-------------------------------------------------------------------------------
SECTION trail
;-------------------------------------------------------------------------------
program_end:
