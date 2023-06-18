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

         message_1        db  0x0d,0x0a
                          db  '[USER TASK]: Hi! nice to meet you,'
                          db  'I am running at CPL=',
         cpl              db  0
                          db  '.',0x0d,0x0a,0
                          
         message_2        db  '[USER TASK]: I need to have a rest...'
                          db  0x0d,0x0a,0

         message_3        db  '[USER TASK]: I am back again.'
                          db  'Now,I must exit...',0x0d,0x0a,0

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

         mov eax, ds                       ; Save original ds (pointing to its header).
         mov fs, eax
     
         mov ax, [data_seg]                ; ds ->  program data segment.
         mov ds, ax

         mov ax, cs
         and al, 0000_0011B                ; Get current CPL from cs (last 2 bits).
         or al, 0x30
         mov [cpl], al

         mov ebx, message_1
         call far [fs:PrintString]

         mov ebx, message_2
         call far [fs:PrintString]

         call far [fs:InitTaskSwitch]      ; Call task switch.

         mov ebx, message_3
         call far [fs:PrintString]

         call far [fs:TerminateProgram] 
    
code_end:

;-------------------------------------------------------------------------------
SECTION trail
;-------------------------------------------------------------------------------
program_end:
