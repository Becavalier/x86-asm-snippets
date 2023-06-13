;===============================================================================
SECTION header vstart=0
         program_length   dd program_end            ; Program length (2 words).
         head_len         dd header_end             ; Header length.
         prgentry         dd start                  ; Entry offset within segment.
         code_seg         dd section.code.start     ; Code section assembly address.
         code_len         dd code_end      
         data_seg         dd section.data.start   
         data_len         dd data_end           
         stack_seg        dd section.stack.start
         stack_len        dd stack_end          

         ; Count of items inside SALT tble.
         salt_items       dd (header_end - salt) / 256 
         ; Set up a Symbol-Address Lookup Table (for routines).
         salt:                                      
         PrintString      db  '@PrintString'
                     times 256 - ($ - PrintString) db 0

         TerminateProgram db  '@TerminateProgram'
                     times 256 - ($ - TerminateProgram) db 0

         ReadDiskData     db  '@ReadDiskData'
                     times 256 - ($ - ReadDiskData) db 0

header_end:

;===============================================================================
SECTION data vstart=0

         buffer times 1024 db  0 

         message_1         db  0x0d,0x0a,0x0d,0x0a
                           db  '**********User program is runing**********'
                           db  0x0d,0x0a,0
         message_2         db  '  Disk data:',0x0d,0x0a,0

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
         mov fs, eax                         ; Free ds for kernel use.

         mov ss, [fs:stack_seg]              ; Use program stack.
         mov esp, stack_end

         mov ds, [fs:data_seg]               ; Use program data.

         mov ebx, message_1
         call far [fs:PrintString]           ; Call kernel routine.

         mov eax, 100                     
         mov ebx, buffer       
         call far [fs:ReadDiskData]  

         mov ebx, message_2
         call far [fs:PrintString]

         mov ebx, buffer
         call far [fs:PrintString] 

         jmp far [fs:TerminateProgram]        ; Return back to kernel.

code_end:

;===============================================================================
SECTION trail
;-------------------------------------------------------------------------------
program_end:
