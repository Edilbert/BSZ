*************************************
* BSZ = Bit Shifter's Z interpreter *
*       for MEGA65      02-Sep-2020 *
*************************************

.CPU 45GS02

***********************
* Commodore KEY codes *
***********************

KEY_F1   = 133
KEY_F3   = 134
KEY_F5   = 135
KEY_F7   = 136
KEY_F2   = 137
KEY_F4   = 138
KEY_F6   = 139
KEY_F8   = 140

*************************
* Commodore Color Codes *
*************************

BLACK    =  0
WHITE    =  1
RED      =  2
CYAN     =  3
PURPLE   =  4
GREEN    =  5
BLUE     =  6
YELLOW   =  7
ORANGE   =  8
BROWN    =  9
PINK     = 10
DARKGREY = 11
GREY     = 12
LT_GREEN = 13
LT_BLUE  = 14
LT_GREY  = 15

*************************
* display control codes *
*************************

BACKSPACE     = $08
TAB           = $09
CR            = $0d
CURSOR_DOWN   = $11
REVERSE_ON    = $12
HOME          = $13
DEL           = $14
ESC           = $1b
CURSOR_RIGHT  = $1d
CLEAR         = $93
CURSOR_UP     = $91
REVERSE_OFF   = $92
CURSOR_LEFT   = $9d

********************************************************
* Interpreter Zero page variables (occupy BASIC space) *
********************************************************

& = $02

; Instruction pointer     LDZ QI0  ->  LDA (RAM_LO),Z

Z_Code         .BSS 1 ; current code byte
QI0            .BSS 1 ; Byte 0  pc = (QI0/QI1/QI2)
QI1            .BSS 1 ; Byte 1  Floppy block low
QI2            .BSS 1 ; Byte 2  Floppy block high

; Data pointer            LDZ QD0  ->  LDA (RAM_LO),Z

QDL            .BSS 1 ; current packed data low
QDH            .BSS 1 ; current packed data high
QD0            .BSS 1 ; Byte 0  pc = (QD0/QD1/QD2)
QD1            .BSS 1 ; Byte 1  Floppy block low
QD2            .BSS 1 ; Byte 2  Floppy block high

; keep above variables together (block push/pull)

RAM_LO         .BSS 1 ; RAM address for disk access
RAM_HI         .BSS 1
RAM_BA         .BSS 2 ; RAM BANK

; Multi purpose local variables

LV0            .BSS 1
LV1            .BSS 1
LV2            .BSS 1
LV3            .BSS 1

Alphabet       .BSS 1
Block_Lo       .BSS 1
Block_Hi       .BSS 1
C_Save_Col     .BSS 1 ; save column
C_Save_Row     .BSS 1 ; save row
Charbuf_Ptr    .BSS 1
Charbuf_End    .BSS 1
Chars_Left     .BSS 1
Info_Pages     .BSS 1
MORE_Counter   .BSS 1
NUMBER         .BSS 5
OP_Type        .BSS 2
ParNum         .BSS 1
Parse_Index    .BSS 1
Prop_Mask      .BSS 1
QuotL          .BSS 1
QuotH          .BSS 1
RemL           .BSS 1
RemH           .BSS 1
Resident_Pages .BSS 1
Save_Unit      .BSS 1
Status_Col     .BSS 1
Upper_Size     .BSS 1 ; rows of upper window
Version        .BSS 1 ; $80 = version > 3
Vocab_Length   .BSS 1 ; length of packed vocab
Win_Top        .BSS 1 ; upper row of active window
Word_Length    .BSS 1 ; length of dictionary words
z_stack_ptr    .BSS 1
z_frame_ptr    .BSS 1
DPL            .BSS 1 ; dictionary pointer
DPH            .BSS 1
DPI            .BSS 1
A0L            .BSS 1 ; primary   address register
A0H            .BSS 1
A1L            .BSS 1 ; secondary address register
A1H            .BSS 1
X0L            .BSS 1 ; primary value   register
X0H            .BSS 1
X1L            .BSS 1 ; parameter register
X1H            .BSS 1 ; X1L - X4H must be contiguous
X2L            .BSS 1
X2H            .BSS 1
X3L            .BSS 1
X3H            .BSS 1
X4L            .BSS 1
X4H            .BSS 1
X5L            .BSS 1
X5H            .BSS 1
X6L            .BSS 1
X6H            .BSS 1
X7L            .BSS 1
X7H            .BSS 1
X8L            .BSS 1
X8H            .BSS 1

Cursor_Col     .BSS 1
Cursor_Row     .BSS 1
Cursor_Vis     .BSS 1
Scr_Adr        .BSS 2 ; screen RAM 16 bit address
Z_Mem_Ptr      .BSS 2 ; Z memory pointer
Col_Adr        .BSS 4 ; colour RAM 32 bit address
DICT_WORD      .BSS 6 ; packed ZSCII dictionary word

ZP_END         .BSS 1


********************
* system variables *
********************

COLS        =  80
ROWS        =  25
R6510       = $01              ; C64 bank switching CPU port
IO_STATUS   = $90              ; used by I/O routines
SAP         = $ac              ; source address pointer
TAP         = $ae              ; target address pointer
FNLEN       = $b7
FA          = $ba
FNADR       = $bb
MEMUSS      = $c3              ; string address
RVS         = $c7              ; reverse flag
BLNSW       = $cc              ; cursor blink flag
BLNCT       = $cd
BLNON       = $cf
Charbuf     = $200
COLOR       = $286
SCNMPG      = $288             ; screen memory page for C64 mode
CINV        = $314             ; kernal vector table
DSTATUS     = $33c             ; 40 bytes disk status
SCREEN      = $0800            ; character RAM in 80 column mode
COLRAM      = $d800            ; color     RAM
Raster      = $d012
BorderCol   = $d020
BackgCol0   = $d021
Random      = $d41b
ROM_Vectors = $fd30
Init_IO     = $fda3
Init_Editor = $ff5b

**********
* Kernal *
******** *

SETMSG = $ff90
SECOND = $ff93
TKSA   = $ff96
ACPTR  = $ffa5
CIOUT  = $ffa8
UNTLK  = $ffab
UNLSN  = $ffae
LISTEN = $ffb1
TALK   = $ffb4
SETNAM = $ffbd
GETIN  = $ffe4
PLOT   = $fff0
RESET  = $fffc

************
* Z arrays *
************

Z_STATUS    = $1f00               ; version 3
Z_VAR       = [EOP + $ff] & $ff00
Z_STACK_LO  = Z_VAR      + $100   ; keep this area together
Z_STACK_HI  = Z_STACK_LO + $100
Z_HEADER    = Z_STACK_HI + $100
Lvar_Lo     = Z_VAR
Lvar_Hi     = Z_VAR + $10


****************
* STORY HEADER *
****************

h_version             = Z_HEADER
h_config              = Z_HEADER +   1
h_resident_size_hi    = Z_HEADER +   4
h_resident_size_lo    = Z_HEADER +   5
h_start_pc_hi         = Z_HEADER +   6
h_start_pc_lo         = Z_HEADER +   7
h_dictionary_hi       = Z_HEADER +   8
h_dictionary_lo       = Z_HEADER +   9
h_objects_hi          = Z_HEADER +  10
h_objects_lo          = Z_HEADER +  11
h_globals_hi          = Z_HEADER +  12
h_globals_lo          = Z_HEADER +  13
h_dynamic_size_hi     = Z_HEADER +  14
h_dynamic_size_lo     = Z_HEADER +  15
h_flags_hi            = Z_HEADER +  16
h_flags_lo            = Z_HEADER +  17
h_serial              = Z_HEADER +  18
h_abbreviations_hi    = Z_HEADER +  24
h_abbreviations_lo    = Z_HEADER +  25
h_file_size_hi        = Z_HEADER +  26 ; in words for version 1-3
h_file_size_lo        = Z_HEADER +  27 ; in words for version 1-3
h_checksum_hi         = Z_HEADER +  28
h_checksum_lo         = Z_HEADER +  29
h_interpreter_number  = Z_HEADER +  30
h_interpreter_version = Z_HEADER +  31
h_screen_rows         = Z_HEADER +  32
h_screen_cols         = Z_HEADER +  33
h_screen_width_hi     = Z_HEADER +  34
h_screen_width_lo     = Z_HEADER +  35
h_screen_height_hi    = Z_HEADER +  36
h_screen_height_lo    = Z_HEADER +  37
h_font_width          = Z_HEADER +  38
h_font_height         = Z_HEADER +  39
h_functions_offset_hi = Z_HEADER +  40
h_functions_offset_lo = Z_HEADER +  41
h_strings_offset_hi   = Z_HEADER +  42
h_strings_offset_lo   = Z_HEADER +  43
h_default_bg_color    = Z_HEADER +  44
h_default_fg_color    = Z_HEADER +  45

h_Alphabet_hi         = Z_HEADER +  52
h_Alphabet_lo         = Z_HEADER +  53

***************
* Print Macro *
***************

MACRO Print(lab)
          LDA #<lab
          LDY #>lab
          LDX #?lab
          JSR PrintText
ENDMAC

MACRO ERROR(num)
          LDA #num
          JMP INTERNAL_ERROR
ENDMAC

MACRO MAC_Color(color)
           LDA #color
           STA COLOR
ENDMAC

START = $2001   ; *** BASIC ***  C65

* = START

          .LOAD START
          .STORE START,EOP-START,"bsz-mega65"

****************
* BASIC header *
****************

          .WORD Link
          .WORD 2020      ; line number
          .BYTE $8b       ; IF   token
          .BYTE $c2       ; PEEK token
          .BYTE "(44)"
          .BYTE $b2       ; =    token
          .BYTE "8"
          .BYTE $a7       ; THEN token
          .BYTE $9e       ; SYS  token
          .BYTE "(2112):" ; C64  start
          .BYTE $d5       ; ELSE token
          .BYTE $fe,$02   ; BANK token
          .BYTE "0:"      ; BANK argument
          .BYTE $9e       ; SYS  token
          .BYTE "(8253):" ; C65  start
          .BYTE $8f       ; REM  token
          .BYTE " BIT SHIFTER 02-SEP-20",0
Link      .WORD 0         ; BASIC end marker

; SYS entry for MEGA65 mode

          JMP MEGA_Setup

; SYS entry for C64 mode

          lda #65   ; 40MHz CPU
          sta 0

          LDY #0
          STY A0L
          STY A1L
          LDA #>[EOP - $1800]
          STA A0H
          LDA #>EOP
          STA A1H

ReLoop    LDA (A0L),Y
          STA (A1L),Y
          INY
          BNE ReLoop
          DEC A0H
          DEC A1H
          LDA A1H
          CMP #$20
          BCS ReLoop
          JMP MEGA_Setup


*****************
Module MEGA_Setup
*****************

          SEI
          LDA #0              ; Configure MEGA65 memory
          TAX
          TAY
          TAZ
          MAP
          EOM

          LDA #$36            ; I/O & kernal
          STA R6510
          LDA #65   ; 40 MHz
          STA 0

          JSR Init_IO
          JSR Set_Kernal_Vectors
          LDA #$04            ; C64 default value
          STA SCNMPG          ; set screen memory page
          JSR Init_Editor

          LDA #-1             ; cursor off
          STA BLNSW
          CLI
          LDX #8
          STX FA
          STX Game_Unit
          STX Save_Unit       ; default SAVE unit = GAME unit

          JSR Load_Config
          JSR Set_Mode_80
          JSR Screen_Setup
          LDA FG_Color
          STA COLOR
          LDA BG_Color
          STA BackgCol0
          LDA BO_Color
          STA BorderCol
          JSR Clear_Screen
          JSR SETMSG          ; disable kernal messages
          JMP z_restart
EndMod

******************
Module Set_Mode_80
******************

*         make VIC IV registers visible
*         by using the knock sequence $47 $53

          LDA #$47
          STA $d02f
          LDA #$53
          STA $d02f

          LDA #$c0           ; 80 columns, fast
          STA $d031
          LDA #$26           ; SCR = $0800,  CB = $0C00
          STA $d018
          RTS
EndMod

*************************
Module Set_Kernal_Vectors
*************************

          LDY #$1F          ; 16 vectors
_loop     LDA ROM_Vectors,Y
          STA CINV,Y
          DEY
          BPL _loop
          RTS
EndMod

**************
Module PC_LOOP
**************

* read next instruction byte from Z-program counter
* read optional operand bytes and call operator subroutine

          LDA #0
          STA ParNum           ; reset # of operands
          JSR Next_Instruction
          STA Z_Code           ; remember op code

          BPL z_op_two         ; [$00 - $7f] -> codes with  2 operands
          CMP #$b0
          BCC z_op_one         ; [$80 - $af] -> codes with  1 operand
          CMP #$c0
          BCC z_op_zero        ; [$b0 - $bf] -> codes with no operand

          JSR Next_Instruction ; [$c0 - $ff] -> codes with 0-4 operands

; max 4 operands (11223344) for version 3
;  or 8 operands for version > 3
; ---------------------------------------
; 00 = 16 bit constant
; 01 =  8 bit constant
; 10 = 16 bit variable
; 11 = no operand (end marker)

          LDX Z_Code
          CPX #$fa            ; call with 8 arguments
          BNE opvar_10
          JSR Load_8_Operands
          JMP opvar_20
opvar_10  JSR Load_4_Operands
opvar_20  LDA Z_Code
          CMP #$e0
          BCC z_exe_1f        ; [$c0 - $df] -> use 2 operand table
          AND #$1f            ; [$e0 - $ff] -> use variable operands
          CLC
          ADC #[opcodes_var - z_opcode] >> 1
          BNE z_execute       ; always

*********
z_op_zero
*********

; opcodes with no operand, opcode = $b0 - $bf

          SBC #$af - [[opcodes_0op - z_opcode] >> 1] ; carry is clear
          BNE z_execute       ; always

********
z_op_one
********

; opcode = 10tt cccc  opcodes with 1 operand
;     tt = 00 : 16 bit constant $8x
;     tt = 01 :  8 bit constant $9x
;     tt = 10 :    variable     $ax

          ASL A
          ASL A
          JSR Get_Operand
          LDA Z_Code
          AND #15
          CLC
          ADC #[opcodes_1op - z_opcode] >> 1
          BNE z_execute

********
z_op_two
********

; opcode = 0fsc cccc  opcodes with 2 operands
;      f = 0 : 1st. op = short constant
;      f = 1 : 1st. op = variable
;      s = 0 : 2nd. op = short constant
;      s = 1 : 2nd. op = variable

          CLC
          AND #$40            ; $40 if 1st. op variable
          ADC #$40            ; $80 if 1st. op variable
          JSR Get_Operand     ; $80 variable, $40 8 bit constant
          LDA Z_Code
          ASL A               ; C=0
          AND #$40
          ADC #$40
          JSR Get_Operand
          LDA Z_Code
z_exe_1f  AND #$1f            ; fall through

*********
z_execute
*********

; Input : A = index to opcode table

          ASL A               ; convert to word index
          TAY
          LDA z_opcode,Y
          STA JSRMOD+1
          LDA z_opcode+1,Y
          STA JSRMOD+2
JSRMOD    JSR $ffff
          JMP PC_LOOP
EndMod

z_error_2 ERROR(2)
z_error_4 ERROR(4)

********
z_opcode
********

; $00-$1f : byte const + short const
; $20-$3f : byte const + variable
; $40-$5f : variable   + byte const
; $60-$7f : variable   + variable
; $c0-$df : 0-4 arguments defined in follow up byte

          .WORD z_error_4       ; 00 20 40 60   c0
          .WORD z_je            ; 01 21 41 61   c1
          .WORD z_jl            ; 02 22 42 62   c2
          .WORD z_jg            ; 03 23 43 63   c3
          .WORD z_dec_chk       ; 04 24 44 64   c4
          .WORD z_inc_chk       ; 05 25 45 65   c5
          .WORD z_jin           ; 06 26 46 66   c6
          .WORD z_test          ; 07 27 47 67   c7
          .WORD z_or            ; 08 28 48 68   c8
          .WORD z_and           ; 09 29 49 69   c9
          .WORD z_test_attr     ; 0a 2a 4a 6a   ca
          .WORD z_set_attr      ; 0b 2b 4b 6b   cb
          .WORD z_clear_attr    ; 0c 2c 4c 6c   cc
          .WORD z_store         ; 0d 2d 4d 6d   cd
          .WORD z_insert_obj    ; 0e 2e 4e 6e   ce
          .WORD z_loadw         ; 0f 2f 4f 6f   cf
          .WORD z_loadb         ; 10 30 50 70   d0
          .WORD z_get_prop      ; 11 31 51 71   d1
          .WORD z_get_prop_addr ; 12 32 52 72   d2
          .WORD z_get_next_prop ; 13 33 53 73   d3
          .WORD z_add           ; 14 34 54 74   d4
          .WORD z_sub           ; 15 35 55 75   d5
          .WORD z_mul           ; 16 36 56 76   d6
          .WORD z_div           ; 17 37 57 77   d7
          .WORD z_mod           ; 18 38 58 78   d8
          .WORD z_call_s        ; 19 39 59 79   d9
          .WORD z_call_n        ; 1a 3a 5a 7a   da
          .WORD z_set_color     ; 1b 3b 5b 7b   db
          .WORD z_error_4       ; 1c 3c 5c 7c   dc
          .WORD z_error_4       ; 1d 3d 5d 7d   dd
          .WORD z_error_4       ; 1e 3e 5e 7e   de
          .WORD z_error_4       ; 1f 3f 5f 7f   df

; ***********
  opcodes_1op
; ***********

; $80-$8f : word constant
; $90-$9f : byte constant
; $a0-$af : variable

          .WORD z_jz            ; 80 90 a0
          .WORD z_get_sibling   ; 81 91 a1
          .WORD z_get_child     ; 82 92 a2
          .WORD z_get_parent    ; 83 93 a3
          .WORD z_get_prop_len  ; 84 94 a4
          .WORD z_inc           ; 85 95 a5
          .WORD z_dec           ; 86 96 a6
          .WORD z_print_addr    ; 87 97 a7
          .WORD z_call_s        ; 88 98 a8
          .WORD z_remove_obj    ; 89 99 a9
          .WORD z_print_obj     ; 8a 9a aa
          .WORD z_ret           ; 8b 9b ab
          .WORD z_jump          ; 8c 9c ac
          .WORD z_print_paddr   ; 8d 9d ad
          .WORD z_load          ; 8e 9e ae
ZV8F      .WORD z_call_n        ; 8f 9f af  V3 = z_not

; ***********
  opcodes_0op
; ***********

          .WORD z_rtrue         ; b0
          .WORD z_rfalse        ; b1
          .WORD z_print         ; b2
          .WORD z_print_ret     ; b3
          .WORD z_nop           ; b4
          .WORD z_save          ; b5
          .WORD z_restore       ; b6
          .WORD z_restart       ; b7
          .WORD z_ret_popped    ; b8
ZVB9      .WORD z_catch         ; b9 V3 = z_pop
          .WORD z_quit          ; ba
          .WORD z_new_line      ; bb
          .WORD z_show_status   ; bc
          .WORD Main_True       ; bd z_verify
          .WORD z_extension     ; be
          .WORD z_error_2       ; bf

; ***********
  opcodes_var
; ***********

          .WORD z_call_s         ; e0 V3 call with 0-3 args
          .WORD z_storew         ; e1
          .WORD z_storeb         ; e2
          .WORD z_put_prop       ; e3
          .WORD z_read           ; e4
          .WORD z_print_char     ; e5
          .WORD z_print_num      ; e6
          .WORD z_random         ; e7
          .WORD z_push           ; e8
          .WORD z_pull           ; e9
          .WORD z_split_window   ; ea
          .WORD z_set_window     ; eb
          .WORD z_error_1        ; ec z_call_s 0-8 args
          .WORD z_erase_window   ; ed
          .WORD z_error_1        ; ee z_erase_line
          .WORD z_set_cursor     ; ef
          .WORD z_error_1        ; f0 z_get_cursor
          .WORD z_set_text_style ; f1
          .WORD z_buffer_mode    ; f2
          .WORD z_output_stream  ; f3
          .WORD z_error_1        ; f4 z_input_stream
          .WORD z_sound_effect   ; f5
          .WORD z_read_char      ; f6
          .WORD z_scan_table     ; f7
          .WORD z_not            ; f8
          .WORD z_call_n         ; f9 z_call_n 0-3 args
          .WORD z_call_n         ; fa z_call_n 0-8 args
          .WORD z_tokenize       ; fb
          .WORD z_encode_text    ; fc
          .WORD z_copy_table     ; fd
          .WORD z_print_table    ; fe
          .WORD z_check_argc     ; ff

z_error_1 ERROR(1)

; ***********
  opcodes_ext
; ***********

          .WORD z_ext_save       ; 00
          .WORD z_ext_restore    ; 01
          .WORD z_log_shift      ; 02
          .WORD z_error12        ; 03 z_art_shift,
          .WORD z_set_font       ; 04
          .WORD z_error12        ; 05 z_draw_picture,
          .WORD z_error12        ; 06 z_picture_data,
          .WORD z_error12        ; 07 z_erase_picture,
          .WORD z_error12        ; 08 z_set_margins,
          .WORD z_save_undo      ; 09

;    0a z_restore_undo,
;    0b z_print_unicode,
;    0c z_check_unicode,
;    0d __illegal__,
;    0e __illegal__,
;    0f __illegal__,
;    10 z_move_window,
;    11 z_window_size,
;    12 z_window_style,
;    13 z_get_wind_prop,
;    14 z_scroll_window,
;    15 z_pop_stack,
;    16 z_read_mouse,
;    17 z_mouse_window,
;    18 z_push_stack,
;    19 z_put_wind_prop,
;    1a z_print_form,
;    1b z_make_menu,
;    1c z_picture_table


; ***********
  Get_Operand
; ***********

; Input: (A) = tt00 0000
; ----------------------
; tt = 00 : 16 bit constant
; tt = 01 :  8 bit constant
; tt = 10 :    variable
; tt = 11 :    none

; Output: Store 1st. in X1, 2nd. in X2, etc. until X8
;         Overflow set if none

          STA OP_Type
          LDA #0
          BIT OP_Type
          BMI geop_02
          BVS geop_01
          JSR Next_Instruction ; 00 -> word
geop_01   STA X0H
          JSR Next_Instruction ; 01 -> byte
          STA X0L
          JMP geop_03
geop_02   BVS geop_04          ; 11 -> end
          JSR Get_Var_A        ; 10 -> variable
geop_03   INC ParNum
          LDA ParNum
          ASL A
          TAX
          LDA X0L
          STA X0L,X
          LDA X0H
          STA X0H,X
          CLV
geop_04   RTS

; ***************
  Load_4_Operands
; ***************

; max 4 operands (2 type bits)
; ----------------------------
; 00 = 16 bit constant
; 01 =  8 bit constant
; 10 = 16 bit variable
; 11 = no operand (end marker)

          JSR Get_Operand
          BVS L4OP_20         ; V=1 -> end of args
          LDA OP_Type
          ASL A
          ASL A               ; shift next 2 type bits to 7 and 6
          LDX ParNum
          CPX #4
          BCC Load_4_Operands
L4OP_20   LDA Z_Code
          RTS

; ***************
  Load_8_Operands
; ***************

; max 8 operands (2 type bits)
; ----------------------------
; 00 = 16 bit constant
; 01 =  8 bit constant
; 10 = 16 bit variable
; 11 = no operand (end marker)

          PHA                  ; 1st. type byte
          JSR Next_Instruction ; 2nd. type byte
          STA OP_Type+1
          PLA
L8OP_10   JSR Get_Operand
          BVS L8OP_20         ; V=1 -> end of args
          LDA OP_Type
          ASL OP_Type+1
          ROL A
          ASL OP_Type+1
          ROL A
          LDX ParNum
          CPX #8
          BCC L8OP_10
L8OP_20   LDA Z_Code
          RTS

; ***********
  Get_Var_X1L
; ***********

          LDA X1L
          BNE GeVa_10
          JSR z_pop
          JMP Push_X0

; *********
  Get_Var_A
; *********

          JSR Next_Instruction
          BNE GeVa_10
          JMP z_pop

GeVa_10   CMP #16
          BCS Get_Global_Var
          TAX
          LDA Lvar_Hi,X
          STA X0H
          LDA Lvar_Lo,X
          STA X0L
          RTS

; **************
  Get_Global_Var
; **************

          JSR Get_Global_Var_Addr
          LDA (A0L),Y
          STA X0H
          INY
          LDA (A0L),Y
          STA X0L
          RTS

; *******
  Put_Var
; *******

          LDA X1L             ; X1L == 0 : push  X0
          BNE stx0_01         ; X1L != 0 : store X0
          DEC z_stack_ptr      ; X0 replaces top of stack

; *******
  Push_X0
; *******

          LDA X0L
          LDY X0H

; *******
  Push_AY
; *******

; Input : A = low  byte
;         Y = high byte
; X register is preserved

          PHA
          TYA
          LDY z_stack_ptr
          STA Z_STACK_HI,Y
          PLA
          STA Z_STACK_LO,Y
          INC z_stack_ptr
          BEQ z_error_6
          RTS

z_error_6 ERROR(6)

; **********
  Store_Zero
; **********

          LDA #0

; **********
  Store_Byte
; **********

          LDX #0

; ********
  Store_AX
; ********

          STX X0H

; ***********
  Store_A_X0H
; ***********

          STA X0L

; ********
  Store_X0
; ********

          JSR Next_Instruction
          BEQ Push_X0         ;    0: push   value

stx0_01   CMP #16             ; 1-15: local  variable
          BCS Put_Global_Var  ; > 15: global variable
          TAX
          LDA X0L
          STA Lvar_Lo,X
          LDA X0H
          STA Lvar_Hi,X
          RTS

; **************
  Put_Global_Var
; **************

          JSR Get_Global_Var_Addr
          LDA X0H             ; store in big endian order
          STA (A0L),Y
          INY
          LDA X0L
          STA (A0L),Y
          RTS

; *******************
  Get_Global_Var_Addr
; *******************

;  Input: (A)  = Variable # ( 16 - 255)
; Output: (A0) = Address of Variable
;         (Y)  = 0  IMPORTANT: used by callers

; A0 = Z_HEADER + h_globals + 2 * (A)

          SEC
          SBC #16             ; variable index = number - 16
          LDY #0
          STY A0H
          ASL A
          ROL A0H             ; (A/A0H) = index * 2 (C=0)
          ADC h_globals_lo
          STA A0L
          LDA A0H             ; 0 or 1
          ADC h_globals_hi
          ADC #>Z_HEADER
          STA A0H
GGVA_Ret  RTS

; Take a jump after an instruction based on the flag, either true or
; false. The branch can be short or long; it is encoded in one or two
; bytes respectively. When bit 7 of the first byte is set, the jump
; takes place if the flag is true; otherwise it is taken if the flag
; is false. When bit 6 of the first byte is set, the branch is short;
; otherwise it is long. The offset occupies the bottom 6 bits of the
; first byte plus all the bits in the second byte for long branches.
; Uniquely, an offset of 0 means return false, and an offset of 1 is
; return true. The branch distance is offset - 2.

; **********
  Main_False
; **********

          JSR Next_Instruction
          BPL Bran_10
Bran_01   AND #$40
          BNE GGVA_Ret
          JMP Next_Instruction

; *********
  Main_True
; *********

          JSR Next_Instruction  ; A = specifier
          BPL Bran_01
Bran_10   TAX                   ; X = specifier
          AND #$40              ; bit 6: 1=short 0=long
          BEQ Bran_11           ; ---------------------
          TXA                   ; short forward branch
          AND #$3f              ; A = offset (6 bits)
          LDX #0                ; X = 0 (high offset)
          BEQ Bran_14           ; ---------------------
Bran_11   TXA                   ; long branch
          AND #$20              ; sign bit of offset
          BEQ Bran_12           ; -> positive offset
          TXA
          ORA #$c0              ; negative offset
          BNE Bran_13           ; -> always
Bran_12   TXA
          AND #$3f              ; positive offset
Bran_13   STA X0H               ; high byte offset (6 bits)
          JSR Next_Instruction  ; A = low  byte of long offset
          LDX X0H               ; X = high byte of long offset
          BNE Branch_XA         ; -> long branch
Bran_14   TAY                   ; Y = offset
          BEQ z_rfalse          ; Offset 0: return false
          DEY                   ; Y = offset - 1
          BEQ z_rtrue           ; Offset 1: return true

; *********
  Branch_XA
; *********

; branch to IP + (A/X) - 2
; Input : (A/X) = signed word with 14 significant bits

          STX X0H             ; offset high
          SEC
          SBC #2
          BCS Bran_20
          DEX
Bran_20   CLC                 ; (A/X) = offset - 2
          ADC QI0
          STA QI0
          TXA
          ADC QI1
          STA QI1
          LDA #0              ; A = 0
          BIT X0H             ; check sign of offset
          BPL Bran_21         ; -> positive
          LDA #-1             ; A = -1
Bran_21   ADC QI2             ; add carry and sign
          STA QI2
          RTS

; ***************************
  z_rtrue  ; op0 opcode # $00
; ***************************

          LDX #1
          STX X1L
          DEX
          STX X1H
          JMP z_ret

; ****************************
  z_rfalse ; op0 opcode # $01
; ****************************

          LDX #0
          STX X1L
          STX X1H
          JMP z_ret

; ********
  X1_TO_X0
; ********

          LDA X1L
          STA X0L
          LDA X1H
          STA X0H
          RTS

; ********
  X2_TO_X0
; ********

          LDA X2L
          STA X0L
          LDA X2H
          STA X0H
          RTS

; ********
  X1_TO_A0
; ********

          LDA X1L
          STA A0L
          LDA X1H
          STA A0H
          RTS

; ********
  X2_TO_A0
; ********

          LDA X2L
          STA A0L
          LDA X2H
          STA A0H
          RTS

; ********
  X0_TO_A0
; ********

          LDA X0L
          STA A0L
          LDA X0H
          STA A0H
          RTS

; ***************************
  z_print  ; op0 opcode # $02
; ***************************

; print text from instruction pointer (QI).

          LDX #2
z_prin_1  LDA QI0,X           ; copy QI -> QD
          STA QD0,X
          DEX
          BPL z_prin_1
          JSR Decode_Text     ; print text from (QD)
          LDX #2
zprin_2   LDA QD0,X           ; copy QD -> QI
          STA QI0,X
          DEX
          BPL zprin_2

; *******************************
  z_nop        ; op0 opcode # $04
; *******************************

          RTS

; *******************************
  z_print_ret  ; op0 opcode # $03
; *******************************

; print text from instruction pointer (QI) add a new line and return true.

          JSR z_print
          JSR z_new_line
          JMP z_rtrue

; *******************************
  z_ret_popped ; op0 opcode # $08
; *******************************

          JSR z_pop
          STA X1L
          STY X1H
          JMP z_ret

; *************************
  z_pop      ; opcode # $b9
; *************************

; pop 16 bit word from Z stack
; Output: (X0) = (A/Y)  = value
; X register preserved

          DEC z_stack_ptr
          BEQ z_error_5
          LDY z_stack_ptr
          LDA Z_STACK_LO,Y
          PHA
          LDA Z_STACK_HI,Y
          TAY
          PLA
          STY X0H
          STA X0L
          RTS

z_error_5 ERROR(5)

; ********************************
  z_quit        ; op0 opcode # $0a
; ********************************

          JSR Save_Config
          Print(EOS)

quit_20   JSR GETIN           ; entry for early quit
          BEQ quit_20
          JMP (RESET)

; *******
  z_catch
; *******
          RTS                 ; not implemented

; ********************************
  z_jz          ; op1 opcode # $00
; ********************************

          LDA X1L
          ORA X1H
          BEQ z_jz_t
          JMP Main_False
z_jz_t    JMP Main_True

***************************************
Module z_get_sibling ; op1 opcode # $01
***************************************

          LDY #5              ; version = 3 sibling
          BIT Version
          BPL zg_chi_1
          LDY #8              ; version > 3 sibling
          BRA zg_chi_1

***************************************
Module z_get_child   ; op1 opcode # $02
***************************************

          LDY #6              ; version = 3 child
          BIT Version
          BPL zg_chi_1
          LDY #10             ; version > 3 child
zg_chi_1  JSR Get_Object_X1   ; X1 object's address -> A0
          JSR Store_AX        ; (A/X) -> (X0) -> Store
          LDA X0L
          ORA X0H
          BEQ zg_chi_f
          JMP Main_True       ; there is an object
zg_chi_f  JMP Main_False      ; there is no object
EndMod

****************************************
Module z_get_parent   ; op1 opcode # $03
****************************************

          LDY #4              ; version = 3 parent
          BIT Version
          BPL _get
          LDY #6              ; version > 3 parent
_get      JSR Get_Object_X1   ; (A/X) = parent object
          JMP Store_AX
EndMod

; *********************************
  z_inc          ; op1 opcode # $05
; *********************************

          JSR Get_Var_X1L
          INW X0L
          JMP Put_Var

; *********************************
  z_dec          ; op1 opcode # $06
; *********************************

          JSR Get_Var_X1L
          DEW X0L
          JMP Put_Var

; *********************************
  z_print_addr   ; op1 opcode # $07
; *********************************

          LDY X1L
          LDX X1H
          JMP Decode_YX

****************************************
Module z_remove_obj   ; op1 opcode # $09
****************************************

; Remove (unlink) object (X1)

          BIT Version
          BPL _V3

          LDY #6              ; parent offset
          JSR Get_Object_X1   ; object's address -> A0
          LDA A0L             ; object's address -> A1
          STA A1L
          LDA A0H
          STA A1H
          TXA                 ; parent high
          ORA (A0L),Y         ; parent low
          BEQ _ret40          ; return if no parent

; Get parent's 1st. child

          LDA (A0L),Y         ; parent low (X = high)
          LDY #10             ; child offset
          JSR Get_Object_Reg  ; parent's address -> A0
          CMP X1L             ; object == parent's 1st. Child ?
          BNE _ro_10          ; -> no
          CPX X1H             ; object == parent's 1st. Child ?
          BNE _ro_10          ; -> no

; Parent's 1st. child is this object, so
; make object's sibling the 1st. child of parent

          LDY #8              ; sibling offset
          LDA (A1L),Y         ; object's sibling high
          INY
          INY                 ; Y = 10
          STA (A0L),Y         ; parent's child high
          DEY                 ; Y =  9
          LDA (A1L),Y         ; object's sibling low
          INY
          INY                 ; Y = 11
          STA (A0L),Y         ; parent's child low
          BNE _ro_20          ; always (INY)

; Parent's 1st. child is not this object
; Loop through siblings until found

_ro_10    LDY #8              ; sibling offset
          JSR Get_Object_Reg  ; parent's child -> A0
          CMP X1L             ; object == parent's child's sibling ?
          BNE _ro_10          ; -> no
          CPX X1H             ; object == parent's child's sibling ?
          BNE _ro_10          ; -> no, try next sibling (A/X)

; Link younger sibling to older sibling

          LDA (A1L),Y         ; younger sibling low
          STA (A0L),Y         ; older   sibling low
          DEY                 ; Y = 8
          LDA (A1L),Y         ; younger sibling high
          STA (A0L),Y         ; older   sibling high

; clear object's parent & sibling (Y=6 .. 9)

_ro_20    LDA #0
          LDY #6              ; parent offset
_ro_30    STA (A1L),Y         ; clear parent & sibling
          INY
          CPY #10             ; after sibling
          BCC _ro_30
_ret40    RTS

_V3       LDY #4              ; parent offset
          JSR Get_Object_X1
          LDA A0L
          STA A1L
          LDA A0H
          STA A1H
          LDA (A0L),Y         ; A = parent object
          BEQ _ret70          ; -> has no parent
          LDY #6              ; child offset
          JSR Get_Object_Reg  ; A = child of parent
          CMP X1L             ; is it me ?
          BNE _ro_50          ; -> no
          LDY #5              ; sibling offset
          LDA (A1L),Y         ; my sibling
          INY                 ; Y = child offset
          STA (A0L),Y         ; is parent's cild
          BNE _ro_60          ; always

_ro_50    LDY #5              ; A = sibling of parnent's
          JSR Get_Object_Reg  ; child
          CMP X1L             ; me ?
          BNE _ro_50          ; -> no
          LDY #5              ; sibling offset
          LDA (A1L),Y         ; my sibling is
          STA (A0L),Y         ; parent's child sibling

_ro_60    LDA #0
          LDY #4              ; parent offset
          STA (A1L),Y         ; I have no parnet
          INY                 ; Y = sibling offset
          STA (A1L),Y         ; I have no sibling
_ret70    RTS
EndMod

****************************************
Module z_print_obj    ; op1 opcode # $0a
****************************************

          LDA X1L
          LDX X1H

*************
z_print_obj_A
*************

          LDY #12             ; version > 3 offset
          BIT Version
          BMI _prio_1
          LDY #7              ; version = 3 offset
_prio_1   JSR Get_Object_Reg
          BIT Version
          BMI _V4
          TAX                 ; object prop high
          INY
          LDA (A0L),Y
_V4       TAY                 ; object prop low
          INY
          BNE _decode
          INX                 ; object prop high
_decode   JMP Decode_YX
EndMod

; *****************************
  z_ret          ; opcode # $ab
; *****************************

          LDA z_frame_ptr      ; stack ptr = frame ptr
          STA z_stack_ptr

          JSR z_pop
          STY Z_Arg_Count     ; # of args
          TAX                 ; # of local vars
          BEQ zret_20         ; -> no locals vars

zret_10   JSR z_pop           ; pop next local var
          STA Lvar_Lo,X       ; and restore it
          TYA
          STA Lvar_Hi,X
          DEX
          BNE zret_10         ; -> loop

zret_20   JSR z_pop
          STA Z_Call_Type
          STY QI0             ; restore instruction pointer L

          JSR z_pop
          STA QI1             ; restore instruction pointer M
          STY QI2             ; restore instruction pointer H

          JSR z_pop
          STA z_frame_ptr

          LDA Z_Call_Type
          BNE zret_30         ; -> no return value
          LDA X1L
          LDX X1H
          JMP Store_AX
zret_30   RTS

; *********************************
  z_jump         ; op1 opcode # $0c
; *********************************

          LDA X1L
          LDX X1H
          JMP Branch_XA

****************************************
Module z_print_paddr  ; op1 opcode # $0d
****************************************

          LDA X1L
          ASL A
          STA QD0
          LDA X1H
          ROL A
          STA QD1
          LDA #0
          ROL A
          STA QD2
          BIT Version
          BPL _label
          ASL QD0
          ROL QD1
          ROL QD2
_label    JMP Decode_Text
EndMod

; *********************************
  z_load         ; op1 opcode # $0e
; *********************************

          JSR Get_Var_X1L
          JMP Store_X0

; *********************************
  z_not          ; op1 opcode # $0f
; *********************************
          LDA X1L
          EOR #$ff
          TAX
          LDA X1H
          EOR #$ff

; ********
  Store_XA
; ********

          STX X0L
          STA X0H
          JMP Store_X0

; *********************************
  z_je           ; var opcode # $01
; *********************************

          DEC ParNum
          BEQ z_error_9
          LDA X1L      ; (X1 == X2)
          LDX X1H
          CMP X2L
          BNE z_je_1
          CPX X2H
          BEQ z_je_t
z_je_1    DEC ParNum
          BEQ z_je_f
          CMP X3L
          BNE z_je_2
          CPX X3H
          BEQ z_je_t
z_je_2    DEC ParNum
          BEQ z_je_f
          CMP X4L
          BNE z_je_f
          CPX X4H
          BNE z_je_f
z_je_t    JMP Main_True
z_je_f    JMP Main_False
z_error_9 ERROR(9)

; *********************************
  z_jl           ; var opcode # $02
; *********************************

; jump true if (X2 < X1)

          JSR X1_TO_X0
          JSR X2_TO_A0
          JSR CMP_A0_X0
          BCS z_jl_f
          JMP Main_True
z_jl_f    JMP Main_False

; *********************************
  z_jg           ; var opcode # $03
; *********************************

; jump true if (X2 > X1)

          JSR X1_TO_A0
          JSR X2_TO_X0
          JSR CMP_A0_X0
          BCS z_jg_f
          JMP Main_True
z_jg_f    JMP Main_False

; *********************************
  z_dec_chk      ; var opcode # $04
; *********************************

; jump true if (--X0 < X2)

          JSR z_dec
          JSR X2_TO_A0
          JSR CMP_A0_X0
          BCS z_dec_cf
          JMP Main_True
z_dec_cf  JMP Main_False

; *********************************
  z_inc_chk      ; var opcode # $05
; *********************************

; jump true if (++X0 < X2)

          JSR z_inc
          JSR X0_TO_A0
          JSR X2_TO_X0
          JSR CMP_A0_X0
          BCS z_inc_cf
          JMP Main_True
z_inc_cf  JMP Main_False

; *********
  CMP_A0_X0
; *********

; Compare A0 with X0
; A0 >= X0 : Carry set
; A0 <  X0 : Carry clear

          LDA A0H
          EOR X0H
          BPL cmpax_1 ; equal signs
          LDA A0H
          CMP X0H
          RTS
cmpax_1   LDA X0H
          CMP A0H
          BNE cmpax_2
          LDA X0L
          CMP A0L
cmpax_2   RTS

****************************************
Module z_jin          ; var opcode # $06
****************************************

          LDY #4              ; version = 3 parent
          BIT Version
          BPL _get
          LDY #6              ; version > 3 parent
_get      JSR Get_Object_X1
          CPX X2H
          BNE _false
          CMP X2L
          BNE _false
          JMP Main_True
_false    JMP Main_False
EndMod

; *********************************
  z_test         ; var opcode # $07
; *********************************

          LDA X2L
          AND X1L
          CMP X2L
          BNE z_test_f
          LDA X2H
          AND X1H
          CMP X2H
          BNE z_test_f
          JMP Main_True
z_test_f  JMP Main_False

; *********************************
  z_or           ; var opcode # $08
; *********************************

          LDA X1H
          ORA X2H
          TAX
          LDA X1L
          ORA X2L
          JMP Store_AX

; *********************************
  z_and          ; var opcode # $09
; *********************************

          LDA X1H
          AND X2H
          TAX
          LDA X1L
          AND X2L
          JMP Store_AX

; *********************************
  z_test_attr    ; var opcode # $0a
; *********************************

          JSR Get_Attr_Addr
          AND (A0L),Y
          BNE z_teat_t
          JMP Main_False
z_teat_t  JMP Main_True

; *********************************
  z_set_attr     ; var opcode # $0b
; *********************************

          JSR Get_Attr_Addr
          ORA (A0L),Y
          STA (A0L),Y
          RTS

; *********************************
  z_clear_attr   ; var opcode # $0c
; *********************************

          JSR Get_Attr_Addr
          EOR #$ff
          AND (A0L),Y
          STA (A0L),Y
          RTS

; *******
  z_store
; *******

          JSR X2_TO_X0
          JMP Put_Var

****************************************
Module z_insert_obj   ; var opcode # $0e
****************************************

; insert object (X1) as 1st. child of object (X2)

          JSR z_remove_obj    ; unlink object (X1)

          BIT Version
          BPL _V3

          LDY #6              ; parent offset
          LDA X2H
          STA (A1L),Y         ; X1's parent = X2 high
          TAX
          INY                 ; Y = 7
          LDA X2L
          STA (A1L),Y         ; X1's parent = X2 high

          LDY #10             ; child offset
          JSR Get_Object_Reg  ; X2 object's address -> A0
          STA LV0             ; old child of X2 low
          LDA X1L             ; Y = 11
          STA (A0L),Y         ; new child low  = X1L
          DEY                 ; Y = 10
          LDA X1H
          STA (A0L),Y         ; new child high = X1H

          TXA                 ; X2's old child high
          ORA LV0             ; X2's old child low
          BEQ _return         ; -> old child was zero

          DEY                 ; Y =  9
          LDA LV0             ; X2's old child   low
          STA (A1L),Y         ; X1's new sibling low
          DEY                 ; Y =  8
          TXA                 ; X2's old child   high
          STA (A1L),Y         ; X1's new sibling high
          RTS

_V3       LDA X2L
          LDY #4              ; parent offset
          STA (A1L),Y         ; object's new parent
          LDY #6              ; child offset
          JSR Get_Object_Reg  ; get new parent's child
          TAX                 ; X = parent's first child
          LDA X1L
          STA (A0L),Y         ; object becomes parent's first child
          TXA
          BEQ _return         ; parent had no child before
          LDY #5              ; sibling offset
          STA (A1L),Y         ; old child becommes sibling
_return   RTS
EndMod

; *********************************
  z_loadw        ; var opcode # $0f
; *********************************

          JSR Word_Array      ; set &X1[X2]
          STA X0H             ; put value
          JSR Next_Datum      ; get low  byte
          JMP Store_A_X0H     ; return word

; *********************************
  z_loadb        ; var opcode # $10
; *********************************

          JSR Byte_Array      ; set &X1[X2]
          JMP Store_Byte      ; return byte

; **********
  Word_Array
; **********

; address QD = base (X1) + index (X2 * 2)

          ASL X2L
          ROL X2H

; **********
  Byte_Array
; **********

; address QD = base (X1) + index (X2)

          CLC
          LDA X1L
          ADC X2L
          STA QD0
          LDA X1H
          ADC X2H
          STA QD1
          LDA #0
          ROL A               ; add carry for address > 64K
          STA QD2
          JMP Next_Datum      ; get next byte

; ****************************************
  z_get_prop_addr ; opcodes 12 32 52 72 d2
; ****************************************

; get property address of property X2 of object X1
; store address of property if found
; else store zero

          JSR Find_Property
          BNE gepa_10
          LDA A0L
          SBC #<[Z_HEADER-2]  ; carry from Find_Property
          TAX                 ; set address after ID/size info
          LDA A0H
          SBC #>[Z_HEADER-2]
          JMP Store_XA
gepa_10   JMP Store_Zero
          .SIZE

; ***********************************
  z_get_prop ; opcodes 11 31 51 71 d1
; ***********************************

; get property X2 of object X1
; if X1 has no property X2 use default value
; the routine must access properties of size byte or word only

          JSR Find_Property
          BEQ zgp_10          ; -> found property

          LDA h_objects_lo    ; use default
          STA A0L
          LDA h_objects_hi
          ADC #>Z_HEADER      ; carry is clear
          STA A0H             ; A0 = address of default properties
          LDA X2L             ; property #
          SBC #0              ; (C=0) : minus 1
          ASL A               ; default properties have word size
          TAY
          BPL zgp_20          ; branch always

zgp_10    JSR Property_Size   ; get size
          TAX                 ; 1: word
          BEQ zgp_30          ; 0: byte (X=0)

zgp_20    LDA (A0L),Y         ; property value high
          TAX
          INY
zgp_30    LDA (A0L),Y         ; property value low
          JMP Store_AX
          .SIZE

; ****************************************
  z_get_next_prop ; opcodes 13 33 53 73 d3
; ****************************************

; for X2L == 0 find first property of object X1
; otherwise find next property after property X2L

          LDA X2L
          BNE zgnp_10         ; -> next prop after X2
          JSR First_Property
          JMP Store_Byte
zgnp_10   JSR Find_Property
          BNE zgnp_20         ; -> prop X2 was not found
          JSR Next_Property
          JMP Store_Byte
zgnp_20   JMP Store_Zero
          .SIZE

****************************************
Module z_get_prop_len ; opcodes 84 94 a4
****************************************

; get length of property, which address is in X1
; The length info is stored one byte before (X1)

          CLC
          LDA X1L
          ADC #<[Z_HEADER-1]
          STA A0L
          LDA X1H
          ADC #>[Z_HEADER-1]
          STA A0H             ; (A0) = (X1) + Header - 1
          LDY #0
          BIT Version
          BPL _V3
          LDA (A0L),Y
          BMI _mask           ; -> length = lower 6 bits
          ASL A               ; bit 7 = length info
          ASL A               ; carry = length info
          TYA                 ; A = 0
          ADC #1              ; A = 1 or 2
_mask     AND #$3f            ; version > 3 mask
          JMP Store_Byte

_V3       JSR Property_Size
          INC A
          JMP Store_Byte
EndMod


; *********************************
  z_add          ; var opcode # $14
; *********************************

          CLC
          LDA X1L
          ADC X2L
          TAX
          LDA X1H
          ADC X2H
          JMP Store_XA

; *********************************
  z_sub          ; var opcode # $15
; *********************************

          SEC
          LDA X1L
          SBC X2L
          TAX
          LDA X1H
          SBC X2H
          JMP Store_XA

; *********************************
  z_mul          ; var opcode # $16
; *********************************

; 16 bit multiplication
;
; (X2) = (X1) * (X2)
;

          JSR Prep_Mult_Div
z_mul_1   ROR LV1
          ROR LV0
          ROR X2H
          ROR X2L
          BCC z_mul_2
          CLC
          LDA X1L
          ADC LV0
          STA LV0
          LDA X1H
          ADC LV1
          STA LV1
z_mul_2   DEX
          BPL z_mul_1
          LDX X2L
          LDA X2H
          JMP Store_XA

; *********************************
  z_div          ; var opcode # $17
; *********************************

          JSR Divide_Signed
          LDX QuotL
          LDA QuotH
          JMP Store_XA

; *********************************
  z_mod          ; var opcode # $18
; *********************************

          JSR Divide_Signed
          LDX RemL
          LDA RemH
          JMP Store_XA

; *************
  Divide_Signed
; *************

; (Quot) = (X1) / (X2)
; (Rem ) = (X1) % (X2)

          LDA X1H
          STA LV2          ; Rem_Sign
          EOR X2H
          STA LV3          ; Quot_Sign
          LDA X1L
          STA QuotL
          LDA X1H
          STA QuotH
          BPL Divi_01
          JSR Negate_Quot ; Make Quot positive
Divi_01   LDA X2L
          STA RemL
          LDA X2H
          STA RemH
          BPL Divi_02
          JSR Divi_04     ; Make Rem positive
Divi_02   JSR Divide_Unsigned
          LDA LV3         ; Quot_Sign
          BPL Divi_03
          JSR Negate_Quot ; (+/-) or (-/+)
Divi_03   LDA LV2         ; Rem_Sign
          BPL Divi_05
Divi_04   LDA #0
          SEC
          SBC RemL
          STA RemL
          LDA #0
          SBC RemH
          STA RemH
Divi_05   RTS

; ***********
  Negate_Quot
; ***********

          LDA #0
          SEC
          SBC QuotL
          STA QuotL
          LDA #0
          SBC QuotH
          STA QuotH
          RTS

; ***************
  Divide_Unsigned
; ***************

          LDA RemL
          ORA RemH
          BEQ z_error_8
          JSR Prep_Mult_Div
DiUn_01   ROL QuotL
          ROL QuotH
          ROL LV0
          ROL LV1
          LDA LV0
          SEC
          SBC RemL
          TAY
          LDA LV1
          SBC RemH
          BCC DiUn_02
          STY LV0
          STA LV1
DiUn_02   DEX
          BNE DiUn_01
          ROL QuotL
          ROL QuotH
          LDA LV0
          STA RemL
          LDA LV1
          STA RemH
          RTS
z_error_8 ERROR(8)  ; Divide by zero

; *************
  Prep_Mult_Div
; *************

          LDX #16
          LDA #0
          STA LV0
          STA LV1
          CLC
          RTS

*******************
Module z_scan_table
*******************

; Input : X1 = search value
;         X2 = address of table
;         X3 = number of table entries
;         X4 = type (default = $82)

          LDA X3H
          BMI zst_fa          ; length  < 0 : false
          ORA X3L
          BEQ zst_fa          ; length == 0 : false

          LDA ParNum
          CMP #4              ; type parameter given ?
          BNE zst_10          ; -> use default type $82
          LDA X4L
          BNE zst_20          ; -> use type parameter

zst_10    LDA #$82            ; default: word table, size=2
          STA X4L             ; word/byte flag

zst_20    LDX X3L             ; countdown low
          BIT X4L
          BMI zst_30          ; -> word
          LDA X1L
          STA X1H             ; byte to search

zst_30    LDA X2L             ; setup table address
          STA QD0
          LDA X2H
          STA QD1
          LDA #0
          STA QD2

zst_40    LDA QD0             ; remember address
          STA X0L
          LDA QD1
          STA X0H

          JSR Next_Datum      ; next table item high (or byte)
          CMP X1H             ; compare
          BNE zst_50          ; -> no match
          BIT X4L
          BPL zst_tr          ; -> compare bytes
          JSR Next_Datum      ; next table item low
          CMP X1L             ; compare
          BEQ zst_tr          ; -> match

zst_50    LDA X4L
          AND #$7f            ; length
          CLC
          ADC X0L             ; QD = X0 + item length
          STA QD0
          BCC zst_60          ; -> no page crossed
          LDA #0
          ADC X0H
          STA QD1

zst_60    TXA                 ; countdown low
          BNE zst_70
          DEC X3H
zst_70    DEX
          TXA
          ORA X3H
          BNE zst_40

zst_fa    JSR Store_Zero      ; Store 0 and return false
          JMP Main_False

zst_tr    JSR Store_X0        ; store X and return true
          JMP Main_True
EndMod

******************
Module Clear_Table
******************

          CLC
          LDA X1H
          ADC #>Z_HEADER
          STA X1H
          LDY #0              ; Y = 0
ClTa_10   LDA #0              ; A = 0
          STA (X1L),Y
          INY
          BNE ClTa_20
          INC X1H
ClTa_20   DEW X3L
          BNE ClTa_10
          RTS
EndMod

*******************
Module z_copy_table
*******************

; Input : X1 = source
;         X2 = target
;         X3 = size

          LDA X2L
          ORA X2H
          BEQ Clear_Table
          LDA X3L
          ORA X3H
          BEQ cota_ret        ; size = 0

          LDA X3H
          BPL cota_20         ; -> safe copy

          SEC                 ; make size positive
          LDA #0
          SBC X3L
          STA X3L
          LDA #0
          SBC X3H
          STA X3H
          JMP cota_30         ; -> forced forward

cota_20   LDA X1L
          CMP X2L
          LDA X1H
          SBC X2H
          BCS cota_30         ; -> (X1 > X2) forward

          LDA X1L             ; check overlap (X1 < X2)
          ADC X3L             ; C=0
          TAX                 ; X = end source low
          LDA X1H
          ADC X3H             ; (X/A) = end source + 1
          CMP X2H
          BCC cota_30         ; no overlap -> forward
          BNE cota_45         ;    overlap -> backward
          CPX X2L
          BEQ cota_30         ; source end == dest start
          BCS cota_45

; forward copy (X1 may point to high memory)

cota_30   LDA #0              ; data pointer = source
          STA QD2
          LDA X1H
          STA QD1
          LDA X1L
          STA QD0

          CLC
          LDA X2H
          ADC #>Z_HEADER
          STA X2H

cota_35   JSR Next_Datum      ; LDA (source)
          LDY #0
          STA (X2L),Y         ; STA (target)
          INW X2L             ; ++X2
          DEW X3L             ; --X3 (counter)
          BNE cota_35         ; -> loop
cota_ret  RTS

; backward copy

cota_45   CLC                 ; set source end
          LDA X1L
          ADC X3L
          STA X1L
          LDA X1H
          ADC X3H
          ADC #>Z_HEADER
          STA X1H
          CLC                 ; set target end
          LDA X2L
          ADC X3L
          STA X2L
          LDA X2H
          ADC X3H
          ADC #>Z_HEADER
          STA X2H
          LDY #0
cota_50   DEW X1L             ; --X1
          DEW X2L             ; --X2
          LDA (X1L),Y
          STA (X2L),Y         ; copy byte
          DEW X3L             ; --X3
          BNE cota_50         ; loop
          RTS
EndMod

********************
Module z_print_table
********************

; Input : X1 = zscii text
;         X2 = width
;         X3 = height
;         X4 = skip

          JSR Print_Buffer
          LDA X1L             ; set text address
          STA QD0
          LDA X1H
          STA QD1
          LDA #0
          STA QD2
          LDA ParNum
          CMP #3
          BCS zpt_10
          LDA #1
          STA X3L             ; default height = 1
zpt_10    JSR Save_Cursor
zpt_20    JSR Restore_Cursor
          LDX X2L             ; width
          BEQ zpt_40
          STX X2H
zpt_30    JSR Next_Datum
          JSR CHROUT
          DEC X2H             ; column countdown
          BNE zpt_30
          DEC X3L             ; row    countdown
          BEQ zpt_40
          INC C_Save_Row
          JMP zpt_20
zpt_40    RTS
EndMod

*****************
Module z_tokenize
*****************

;         X1 = text input buffer
;         X2 = parse buffer
;         X3 = dictionary
;         X4 = flag

          CLC
          LDA X1H
          ADC #>Z_HEADER
          STA X1H             ; input buffer
          LDX ParNum
          DEX
          DEX
          BEQ _standard       ; standard dictionary
          DEX
          STX Tokenizer_Flag  ; 0 or 1
          LDA X3L
          LDY X3H
          JMP Parse_AY_Dict
_standard JMP Parse_Input
EndMod

****************************************
Module z_check_argc   ;     opcode # $ff
****************************************

          LDA Z_Arg_Count
          CMP X1L
          BCS zca_10
          JMP Main_False
zca_10    JMP Main_True
          RTS
EndMod

******************
Module z_log_shift
******************

; Input : X1 = number
;         X2 = shifts

          LDA X1L
          LDY X2L
          BMI _right
_left     ASL A
          ROL X1H
          DEY
          BNE _left
          BEQ _store

_right    LSR X1H
          ROR A
          INY
          BNE _right

_store    LDX X1H
          JMP Store_AX
EndMod

; **********
  z_set_font
; **********

; 0 : previous font
; 1 : normal   font
; 2 : picture  font
; 3 : character graphics
; 4 : fixed pitch

;          JSR Print_Buffer
;          LDA E_Attribute
;          LDX X1L
;          CPX #3
;          BNE SeFo_10
;          AND #$7f
;          BIT
;SeFo_10   ORA #$80
;          STA E_Attribute
;          LDA Z_Active_Font
;          STX Z_Active_Font
;          JMP Store_Byte
          JMP Store_Zero

*******************************************
Module z_set_color ; opcodes 1b 3b 5b 7b db
*******************************************

; Input : X1 = foreground color
;         X2 = background color

          JSR Print_Buffer

          LDX X1L
          LDA BG_CODE,X
          STA FG_Color
          LDX X2L
          LDA BG_CODE,X
          STA BG_Color
          RTS
EndMod

BG_CODE   .BYTE $00 ;  0 current
          .BYTE $00 ;  1 default
          .BYTE $00 ;  2 black
          .BYTE $08 ;  3 red
          .BYTE $04 ;  4 green
          .BYTE $0d ;  5 yellow
          .BYTE $02 ;  6 blue
          .BYTE $0a ;  7 magenta
          .BYTE $06 ;  8 cyan
          .BYTE $0f ;  9 white

****************************************
Module z_call_n       ; op2 opcode # $f9
****************************************

          LDA #1
          BRA call_00         ; always
EndMod

****************************************
Module z_call_s       ; op2 opcode # $00
****************************************

; call subroutine (X1) with 0 - 3 arguments
; X1 = subroutine address / 2 (word count)
; X2 - X4 arguments
; If (X1 == 0) push zero and return
; Push saved stack pointer & QI0
; Push QI1 & QI2
; For # of args do:
;     Push local variable
;     Set default from instruction stream
;     Replace from argument if set
; Push argument count & count eor'd with $ff

          LDA #0
call_00   STA Z_Call_Type
          LDA X1L
          ORA X1H
          BNE call_20
          LDA Z_Call_Type
          BEQ call_10
          RTS
call_10   JMP Store_Byte

call_20   LDA z_frame_ptr      ; push frame pointer
          JSR Push_AY

          LDY QI2             ; push instruction pointer H
          LDA QI1             ; push instruction pointer M
          JSR Push_AY

          LDY QI0             ; push instruction pointer L
          LDA Z_Call_Type     ; push call type
          JSR Push_AY

          LDA X1L             ; compute call address
          ASL A
          STA QI0
          LDA X1H
          ROL A
          STA QI1
          LDA #0
          ROL A
          STA QI2             ; IP = (X1) * 2
          BIT Version
          BPL call_25
          ASL QI0
          ROW QI1             ; IP = (X1) * 4
call_25   JSR Next_Instruction
          STA A1H             ; number of local variables
          BEQ call_50         ; no local variables
          LDX #1

call_30   LDY Lvar_Hi,X
          LDA Lvar_Lo,X
          JSR Push_AY         ; push local var of caller
;-----------------------------
          BIT Version
          BPL call_32
          LDA #0              ; initialize with zero
          STA Lvar_Hi,X
          STA Lvar_Lo,X
          BRA call_36
;-----------------------------
call_32   JSR Next_Instruction; initialize with default value
          STA Lvar_Hi,X
          JSR Next_Instruction; from instruction stream
          STA Lvar_Lo,X
;-----------------------------
call_36   CPX ParNum          ; index > # of args ?
          BCS call_40         ; -> stay with default value

          TXA
          ASL A               ; or replace with arg value
          TAY
          LDA X1L,Y
          STA Lvar_Lo,X
          LDA X1H,Y
          STA Lvar_Hi,X
call_40   CPX A1H             ; # of vars to process
          INX
          BCC call_30

call_50   LDY Z_Arg_Count
          LDA A1H             ; push # of local vars
          JSR Push_AY
          LDY ParNum
          DEY
          STY Z_Arg_Count
          LDA z_stack_ptr
          STA z_frame_ptr      ; set new frame pointer
          RTS
EndMod

***********************************
Module z_storew       ; X1[X2] = X3
***********************************

          ASW X2L
          JSR X1_PLUS_X2_TO_A0
          LDA X3H
          STA (A0L),Y
          INY
          LDA X3L
          STA (A0L),Y
          RTS
EndMod

; ****************************
  z_storeb       ; X1[X2] = X3
; ****************************

          JSR X1_PLUS_X2_TO_A0
          LDA X3L
          STA (A0L),Y
          RTS

; ****************
  X1_PLUS_X2_TO_A0
; ****************

          CLC
          LDA X2L
          ADC X1L
          STA A0L
          LDA X2H
          ADC X1H
          ADC #>Z_HEADER
          STA A0H
          LDY #0
          RTS

; *********************************
  z_put_prop     ; op2 opcode # $03
; *********************************

; object (X1) property (X2) = X3

          JSR Find_Property
          BNE z_error10
          JSR Property_Size
          BEQ zpupr_3         ; 0: byte
          CMP #1              ; 1: word
          BNE z_error11
          LDA X3H
          STA (A0L),Y
          INY
zpupr_3   LDA X3L
          STA (A0L),Y
          RTS

z_error10 ERROR(10)
z_error11 ERROR(11)

; ************
  z_print_char
; ************

          LDA X1L
          JMP Print_Formatted

; ***********
  z_print_num
; ***********

          LDX X1L
          LDA X1H
          BPL zprnu_1
          LDA #'-'
          JSR Print_Formatted
          SEC
          LDA #0
          SBC X1L
          TAX
          LDA #0
          SBC X1H
zprnu_1   JSR Format_XA
          LDX #0
zprnu_2   LDA NUMBER,X
          CMP #' '
          BEQ zprnu_3
          STX LV2
          JSR Print_Formatted
          LDX LV2
zprnu_3   INX
          CPX #5
          BCC zprnu_2
          RTS

; ********
  z_random
; ********

          LDA X1L
          STA X2L
          LDA X1H
          STA X2H
          LDA Random
          LDX Raster
          STX X1L
          AND #$7f
          STA X1H
          JSR Divide_Signed
          LDA RemH
          STA X0H
          LDX RemL
          STX X0L
          INW X0L
          JMP Store_X0

; ******
  z_push
; ******

          LDA X1L
          LDY X1H
          JMP Push_AY

; ******
  z_pull
; ******
          JSR z_pop
          JMP Put_Var


; *************
  z_encode_text
; *************
; Input : X1 = text buffer
;         X2 = length of word
;         X3 = start index
;         X4 = encoded text

          CLC
          LDA X1H
          ADC #>Z_HEADER
          STA X1H
          LDA #0
          LDX #9              ; word length
zet_10    STA TEXT_WORD-1,X
          DEX
          BNE zet_10

          LDY X3L
zet_20    LDA (X1L),Y
          PHX
          PHY
          JSR Sep_All
          PLY
          PLX
          BCS zet_30
          STA TEXT_WORD,X
          INY
          INX
          CPX Word_Length
          BCS zet_30
          CPX X2L
          BCC zet_20

zet_30    JSR Encode
          CLC
          LDA X4L
          STA A0L
          LDA X4H
          ADC #>Z_HEADER
          STA A0H
          LDY Vocab_Length
          DEY
zet_40    LDA DICT_WORD,Y
          STA (A0L),Y
          DEY
          BPL zet_40
          RTS

; ***********
  z_read_char
; ***********
          JSR Print_Buffer
          LDA Upper_Size
          STA MORE_Counter
          JSR Get_Character
          JMP Store_Byte

*************
Module z_read
*************

          BIT Version
          BMI _V4
          JSR z_show_status
_V4       JSR Print_Buffer    ; print prompt
          LDX #0
          STX Tokenizer_Flag
          CLC
          LDA X1H
          ADC #>Z_HEADER
          STA X1H
          JSR Get_Line_X1     ; read input from user
          DEC ParNum
          BEQ _no_parse
          LDA X2L
          ORA X2H
          BEQ _no_parse
          JSR Parse_Input
_no_parse BIT Version
          BPL _return
          LDA #CR
          JMP Store_Byte
_return   RTS
EndMod

******************
Module Parse_Input
******************

;         X1    = text input buffer
;         X1[0] = buffer size
;         X1[1] = string length (Version > 3)

;         X2    = parse buffer  (item size = 4)
;         X2[0] = buffer size   (max. 59 items)
;         X2[1] = buffer length (items)

;         X7L   = item[0] dictionary address high
;         X7H   = item[1] dictionary address low
;         X8L   = item[2] # of characters
;         X8H   = item[3] start of word in text buffer

;         X5L   = index to text buffer
;         X5H   = size of word (characters)
;         X6L   = X2[1] = items parsed
;         X6H   = X2[0] = parse buffer limit

          LDA h_dictionary_lo
          LDY h_dictionary_hi

*************
Parse_AY_Dict
*************

          STA DPL
          STY DPH
          CLC
          LDA X2H             ; X2 += Header
          ADC #>Z_HEADER      ; parse buffer
          STA X2H

; limit buffer capacity to 59 items

          LDY #0
          LDA (X2L),Y
          BEQ Parse_10        ; -> use default
          CMP #60
          BCC Parse_15        ; -> OK (1..59)
Parse_10  LDA #59
          STA (X2L),Y         ; use default
Parse_15  STA X6H             ; copy of parse buffer limit
          BIT Version
          BPL Parse_17
          INY                 ; Y = 1
          LDA (X1L),Y
          STA Chars_Left      ; Version > 3
Parse_17  LDY #2              ; Y = 2
          STY Parse_Index     ; start in parse buffer
          BIT Version
          BMI Parse_18
          DEY                 ; start at 1 for Version = 3
Parse_18  STY X5L             ; buffer index
          LDY #0
          STY X6L             ; items parsed
          STY X5H             ; word size = 0

; parse loop for items (words or delimiters)

Parse_20  LDX X6L             ; items parsed
          CPX X6H             ; items max.
          BCS Parse_25        ; -> exceeded # of words
          LDA Chars_Left
          ORA X5H             ; word size
          BNE Parse_30

Parse_25  LDY #1              ; finish
          TXA
          STA (X2L),Y         ; items parsed
          RTS                 ; exit

; continue parsing word

Parse_30  LDA X5H             ; word size
          CMP Word_Length
          BCC Parse_35        ; -> word length < max
          JSR Skip_Surplus    ; skip to next delimiter

Parse_35  LDA X5H             ; word size
          BNE Parse_45        ; -> search word in dictionary

          LDX #8              ; clear Word buffer
Parse_40  STA TEXT_WORD,X
          DEX
          BPL Parse_40

; not a word, is this character a delimiter ?

          LDY X5L             ; buffer index
          STY X7L+3           ; X8H = word position
          LDA (X1L),Y
          JSR Sep_Dict        ; dot, comma, quote
          BCS Parse_50        ; -> in dictionary
          JSR Sep_Std         ; standard separators
          BCC Parse_45
          DEC Chars_Left      ; ignore ! or ?
          INC X5L             ; ++buffer index
          BNE Parse_20        ; always

Parse_45  LDA Chars_Left
          BEQ Parse_55
          LDY X5L             ; buffer index
          LDA (X1L),Y
          JSR Sep_All         ; check all separators
          BCS Parse_55
          LDX X5H             ; word size
          STA TEXT_WORD,X
          DEC Chars_Left
          INC X5H             ; ++word size
          INC X5L             ; ++buffer index
          BRA Parse_20

Parse_50  STA TEXT_WORD       ; word is a separator
          DEC Chars_Left
          INC X5H             ; ++word size
          INC X5L             ; ++buffer index

; start parsing

Parse_55  LDA X5H             ; word size
          BEQ Parse_20        ; -> nothing to parse
          STA X7L+2           ; X8L = # of letters
          JSR Encode          ; convert to packed format
          JSR Dictionary_Search ; result -> X7
          INC X6L             ; Inc # of items
          LDX #0
          STX X5H             ; word size
          LDY Parse_Index
          LDA Tokenizer_Flag  ; 1 : store matches only
          BEQ Parse_60        ; -> store anyhow
          LDA X7L
          ORA X7H
          BNE Parse_60
          INY                 ; skip this entry
          INY                 ; no synonym replacement
          INY
          INY
          BNE Parse_70        ; 0 : Dict address hi
Parse_60  LDA X7L,X           ; 1 : Dict address lo
          STA (X2L),Y         ; 2 : # of letters
          INY                 ; 3 : text buffer position
          INX
          CPX #4
          BCC Parse_60
Parse_70  STY Parse_Index     ; Parse_Index += 4
          JMP Parse_20
EndMod

; ************
  Skip_Surplus
; ************

          LDA Chars_Left
          BEQ SkSu_Ret
SkSu_10   LDY X5L             ; buffer index
          LDA (X1L),Y
          JSR Sep_All
          BCS SkSu_Ret
          INC X5H             ; word size
          INC X5L             ; buffer index
          DEC Chars_Left
          BNE SkSu_10
SkSu_Ret  RTS

; *******
  Sep_All
; *******

; Input : (A) = character
; Output: C=1 is separator, C=0 is not

; check standard list and dictionary list

          JSR Sep_Std
          BCS SeDi_Ret         ; -> is separator

; ********
  Sep_Dict
; ********

; Input : (A) = character
; Output: C=1 is separator, C=0 is not

; typical list: , . "

          PHA
          STA SeDi_10+4       ; search character
          LDA h_dictionary_lo
          LDY h_dictionary_hi
          JSR Set_Data_AY
          JSR Next_Datum
          TAX                 ; X = # of separators
SeDi_10   JSR Next_Datum
          CMP #' '            ; modified !
          BEQ SeDi_20         ; yes: return with C=1
          DEX
          BNE SeDi_10
          CLC                 ; no : return with C=0
SeDi_20   PLA
SeDi_Ret  RTS

; *******
  Sep_Std
; *******

; Input : (A) = character
; Output: C=1 is separator, C=0 is not

; standard list: ! ? , . CR SPACE

          LDX #?Sep_Std_List-1 ; size of separator list
SeSt_10   CMP Sep_Std_List,X
          BEQ SeSt_Ret          ; yes: return with C=1
          DEX
          BPL SeSt_10
          CLC                  ; no : return with C=0
SeSt_Ret  RTS

; *********
  Skip_Data
; *********

; Input : A = # of bytes to skip
; Output: QD pointer updated

          CLC
          ADC QD0
          STA QD0
          BCC SkDa_Ret
          INC QD1
          BNE SkDa_Ret
          INC QD2
SkDa_Ret  RTS

; ***********
  Set_Data_AY
; ***********

          STA QD0
          STY QD1
          LDY #0
          STY QD2
          RTS

; *****************
  Dictionary_Search
; *****************

; Input : DP = dictionary
;         RA = search token

          LDA DPL             ; start of dictionary
          LDY DPH
          JSR Set_Data_AY

          JSR Next_Datum      ; size of separator list
          JSR Skip_Data       ; skip list
          JSR Next_Datum      ; item size
          STA DPI
          JSR Next_Datum      ; item count hi
          STA X4H
          JSR Next_Datum      ; item count lo
          STA X4L
          ORA X4H
          BEQ DiSe_70         ; -> empty dictionary
          LDA X4H
          BMI DiSe_20         ; -> entries are unsorted

          SEC
          LDA #0              ; positive countdown is sorted
          SBC X4L             ; make count down negative
          STA X4L
          LDA #0
          SBC X4H
          STA X4H

DiSe_20   LDA QD0             ; X7 = address of next item
          STA X7H
          LDA QD1
          STA X7L
          LDX #0              ; X = token byte index
DiSe_30   JSR Next_Datum
          INX
          CMP DICT_WORD-1,X
          BNE DiSe_50
          CPX Vocab_Length
          BCC DiSe_30
          RTS                 ; -> match

DiSe_50   JSR Next_Datum      ; skip to next item
          INX
          CPX DPI             ; item_Size
          BCC DiSe_50

          INW X4L             ; count up to zero
          BNE DiSe_20

          LDA #0
DiSe_70   STA X7H             ; not found: return (X7) = 0
          STA X7L
DiSe_Ret  RTS


;                6789abcdef0123456789abcdef
;                --------------------------
;         .BYTE "abcdefghijklmnopqrstuvwxyz"
;         .BYTE "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ZSCII_2   .BYTE "\0\r0123456789.,!?_#'\"/\\-:()"

; *********
  Decode_YX
; *********

; print 5bit encoded text from address (Y/X)

          STY QD0
          STX QD1
          LDA #0
          STA QD2

; ***********
  Decode_Text
; ***********

          LDX #0
          STX QDH
DeTe_10   LDX #0
          STX Alphabet
DeTe_15   JSR Get_Packed_Char
          BCS DiSe_Ret        ; return
          BNE DeTe_20
          LDA #' '            ; 0 : space
          BNE DeTe_40
DeTe_20   CMP #4              ; 1,2,3 : abbreviations
          BCC DeTe_55
          CMP #6              ; shift 4:capital, 5:numeric
          BCS DeTe_25
          STA Alphabet
          BCC DeTe_15
DeTe_25   LDX Alphabet
          CPX #5              ; numeric
          BNE DeTe_30
          TAX
          LDA ZSCII_2-6,X
          BNE DeTe_40
          BEQ DeTe_50

DeTe_30   CPX #4              ; upper case
          BNE DeTe_35
          ADC #$3a            ; carry is set   from CPX
          BNE DeTe_40
DeTe_35   ADC #$5b            ; carry is clear from CPX
DeTe_40   JSR Print_Formatted
          JMP DeTe_10

DeTe_50   JSR Get_Packed_Char ; 10 bit ZSCII
          ASL A
          ASL A
          ASL A
          ASL A
          ASL A
          STA LV0
          JSR Get_Packed_Char
          ORA LV0
          JMP DeTe_40

; process code for abbreviations (1,2,3) plus abbr. index

DeTe_55   SBC #0        ; subtract 1 (C=0)
          ASL A         ; 0, 1, 2
          ASL A
          ASL A
          ASL A
          ASL A
          ASL A
          STA LV2             ; -> $00, $40, $80
          JSR Get_Packed_Char
          ASL A
          CLC
          ADC LV2             ; add offset
          TAY                 ; index to abbr. table

; push QD2, QD1, QD0, QDH, QDL for recursive call

          LDX #4
DeTe_60   LDA QDL,X
          PHA
          DEX
          BPL DeTe_60

; recursive call of Decode_Text for abbreviation

          CLC
          LDA h_abbreviations_lo
          STA A0L
          LDA h_abbreviations_hi
          ADC #>Z_HEADER
          STA A0H             ; (A0) = start of abbr. pointer
          INY
          LDA (A0L),Y         ; address low
          ASL A               ; x 2
          STA QD0
          DEY
          LDA (A0L),Y         ; address high
          ROL A               ; x 2
          STA QD1
          LDA #0
          ROL A               ; high mem
          STA QD2

; recursive call

          JSR Decode_Text

; pull QDL, QDH, QD0, QD1, QD2 after recursive call

          LDX #0
DeTe_65   PLA
          STA QDL,X
          INX
          CPX #5
          BCC DeTe_65
          JMP DeTe_10
          .SIZE

; ***************
  Get_Packed_Char
; ***************

          LDA QDH
          BEQ GPC_20          ; load next word
          CMP #%100 00000     ; EOS flag
          BEQ GPC_Ret         ; return with C=1 : End-Of-String
          BIT QDH             ; valid 2nd. char ?
          BVC GPC_10          ; goto  3rd. cahr
          AND #%100 11111     ; save EOS clear bit 6
          ORA #%001 00000     ; make non zero
          STA QDH
          AND #%000 11111     ; extract data
          CLC
          RTS                 ; return with 2nd. char

GPC_10    AND #%100 00000     ; save EOS only
          STA QDH
          LDA QDL
          AND #%000 11111     ; extract 5 bit
          CLC
          RTS                 ; return with 3rd. char

GPC_20    JSR Next_Datum
          TAX                 ; x = first half
          AND #%0000 0011     ; extract part of 2nd. char
          STA QDH
          JSR Next_Datum
          STA QDL
          ASL A
          ROL QDH
          ASL A
          ROL QDH
          ASL A
          ROL QDH
          TXA
          AND #%1000 0000     ; save EOS flag
          ORA #%0100 0000     ; set valid 2nd. char flag
          ORA QDH             ; add value
          STA QDH             ; save 2nd. char

          TXA
          LSR A
          LSR A
          AND #%000 11111
          CLC
GPC_Ret   RTS                 ; return with 1st. char

; ******
  Encode
; ******

; The encode takes 9 ASCII characters (6 in version 3) and
; converts them to packed ZSCII. This routine is called from the parser
; only, which already converts uppercase to lowercase letters.
; Also the output length is restricted to 6 characters (4 in bersion 3)
; because this is the size of a dictionary entry.

; Input:  6-9 ASCII charcaters in TEXT_WORD
; Output: 4-6 Z     characters in DICT_WORD

          LDY #0              ; TEXT_WORD[Y]
          LDX #0              ; DICT_WORD[X]
          BEQ Enco_30

Enco_10   LDA #5              ; void (numeric shift)
Enco_20   STA DICT_WORD,X     ; store next ZSCII
          INX
          CPX Word_Length
          BCS Enco_70

          INY
Enco_30   LDA TEXT_WORD,Y     ; get next ASCII char
          BEQ Enco_10         ; insert void char

          CMP #'Z'+1          ; (A-Z) upper range
          BCS Enco_35
          CMP #'A'            ; (A-Z) lower range
          BCC Enco_35
          SBC #$3b            ; (A-Z) -> $06-$1f
          BNE Enco_20         ; -> loop always

Enco_35   CMP #'z'+1          ; (a-z) upper range
          BCS Enco_40
          CMP #'a'            ; (a-z) lower range
          BCC Enco_40
          SBC #$5b            ; (a-z) -> $06-$1f
          BNE Enco_20         ; -> loop always

Enco_40   STY L_Encode_Y
          STA L_Encode_A
          LDA #5
          STA DICT_WORD,X
          INX
          LDA L_Encode_A
          LDY #25
Enco_50   CMP ZSCII_2,Y
          BEQ Enco_60         ; found ZSCII in table 3
          DEY
          BNE Enco_50

          LDA #6              ; shift to 10 bit char
          STA DICT_WORD,X
          INX
          LDA L_Encode_A
          LSR A
          LSR A
          LSR A
          LSR A
          LSR A
          AND #3
          STA DICT_WORD,X
          INX
          LDA L_Encode_A
          AND #$1f
          LDY L_Encode_Y
          JMP Enco_20

Enco_60   TYA                 ; use index to table 3 as value
          LDY L_Encode_Y      ; restore read index
          ADC #5              ; add offset 6 (5 + carry)
          BNE Enco_20         ; loop always

Enco_70   LDX #0
          LDY #0
Enco_80   LDA DICT_WORD+1,X
          ASL A
          ASL A
          ASL A
          ASL A
          ROL DICT_WORD,X
          ASL A
          ROL DICT_WORD,X
          ORA DICT_WORD+2,X
          PHA
          LDA DICT_WORD,X
          STA DICT_WORD,Y
          PLA
          STA DICT_WORD+1,Y
          INX
          INX
          INX
          INY
          INY
          CPX Word_Length
          BCC Enco_80
          LDA DICT_WORD-2,Y
          ORA #$80            ; end of string flag
          STA DICT_WORD-2,Y
          RTS

*************
Module A0_x_8
*************

          STA A0L
          STX A0H             ; (A0) = object
          ASL A
          ROL A0H             ; (A/A0H) = object *  2
          ASL A
          ROL A0H             ; (A/A0H) = object *  4
          ASL A
          ROL A0H             ; (A/A0H) = object *  8
          RTS
EndMod

********************
Module Object_Offset
********************

; V3 Address = (h_objects) + 53 + 9 * object

          BIT Version
          BMI _V4
          LDX #0
          JSR A0_x_8
          ADC A0L
          BCC _label
          INC A0H             ; (A/A0H) = object *  9
          CLC
_label    ADC #53             ; V3 offset 53
          RTS

; V4 Address = (h_objects) + 112 + 14 * object

_V4       JSR A0_x_8
          SEC
          SBC A0L
          PHA                 ; low  of  (object *  7)
          LDA A0H
          STX A0H
          SBC A0H
          STA A0H             ; high of  (object *  7)
          PLA
          ASL A
          ROL A0H             ; (A/A0H) = object * 14
          ADC #112            ; V4 offset 112
          RTS
EndMod

********************
Module Get_Object_X1
********************

          LDA X1L
          LDX X1H

**************
Get_Object_Reg
**************

; Compute address of object

; Version > 3:
; -----------------------------------------
; Address = (h_objects) + 112 + 14 * object
; Input : (A/X) = input  object id
;         Y     = object item
; Output: (A/X) = output object id
;         (A0L) = address

          JSR Object_Offset
          BCC _label
          INC A0H
          CLC
_label    ADC h_objects_lo
          STA A0L
          LDA A0H
          ADC h_objects_hi
          ADC #>Z_HEADER
          STA A0H
          LDX #0             ; object hi for V3
          BIT Version
          BPL _objlow
          LDA (A0L),Y
          TAX                ; object hi
          INY
_objlow   LDA (A0L),Y        ; object lo
          RTS
EndMod

*********************
Module First_Property
*********************

; Find address of property table of object (X1)

; Input : (X1) = object ID
; Output: (A0) = pointer to property table after NAME
;          A   = ID of first property
;          Y   = 0

          LDY #7              ; version = 3 offset to property pointer
          BIT Version
          BPL _fips_1
          LDY #12             ; version > 3 offset to property pointer
_fips_1   JSR Get_Object_X1
          BIT Version
          BMI _fips_2
          TAX                 ; property pointer high
          INY
          LDA (A0L),Y         ; property pointer low
_fips_2   STA A0L             ; property table low
          TXA                 ; property table high
          ADC #>Z_HEADER      ; C=0 from Get_Object
          STA A0H             ; (A0) = property table
          LDY #0              ; offset to NAME property
          LDA (A0L),Y         ; size of name (words)
          ASL A               ; size of name (bytes)
          SEC                 ; add size + 1
          ADC A0L
          STA A0L
          BCC FiPr_10
          INC A0H
FiPr_10   LDA (A0L),Y
          AND Prop_Mask       ; A = property ID
          RTS
EndMod

********************
Module Property_Size
********************

          BIT Version
          BPL _V3
          LDA (A0L),Y
          BPL _one
          INY
          LDA (A0L),Y         ; 2nd. size byte
          INY
          AND Prop_Mask
          RTS

_one      INY                 ; Y points after size
          ASL A               ; bit 7 = length info
          ASL A               ; carry = length info
          ROL A               ; bit 0 = length info
          AND #1
          RTS

_V3       LDA (A0L),Y
          INY
          LSR A
          LSR A
          LSR A
          LSR A
          LSR A
          RTS
EndMod

; *************
  Next_Property
; *************

; use size info and point to next property (V3-V5)

; Input : (A0) = pointer to current property
; Output: (A0) = pointer to next property
;            Y = 0

          JSR Property_Size
          CLC
          ADC #2
          ADC A0L
          STA A0L
          BCC NePr_20
          INC A0H
NePr_20   LDY #0
          LDA (A0L),Y
          AND Prop_Mask
          RTS

********************
Module Find_Property
********************

; search for property X2L of object X1

; Input : (X1)  = object   ID
;         (X2L) = property ID

; Output: Z=0 if property was not found
;         Z=1 if property was found
;             A = 0
;             Y = 0
;             C = 1 if separate length byte

          JSR First_Property  ; -> (A0) and Y = 0
_loop     CMP X2L
          BEQ _match
          BCC _return         ; -> not in table (Z=0)
          JSR Next_Property
          BRA _loop
_match    CLC
          BIT Version
          BPL _ok
          LDA (A0L),Y         ; prepare pointer advancement
          ASL A               ; C=1 for 2 byte header
_ok       LDA #0              ; set zero flag
_return   RTS                 ; Z=1 success, Z=0 not found
EndMod

; *************
  Get_Attr_Addr
; *************

; Input
; -----
; X1L = object #
; X2L = flag   #
;
; Output
; ------
; (A0L),Y = Address of attribute
; (A)     = Attribute mask

          JSR Get_Object_X1
          LDA X2L      ; flag #
          LSR A
          LSR A
          LSR A
          TAY          ; offset = flag / 8
          LDA X2L
          AND #7       ; flag mod 8
          TAX
          LDA #$80     ; flag 0
          BNE gaa_02
gaa_01    LSR A
gaa_02    DEX
          BPL gaa_01
          RTS

; *********
  Dump_Code
; *********

          LDA Z_Code
          JSR ASCII_Hex
          PHA
          TXA
          JSR CHROUT
          PLA
          JMP CHROUT


; **************
  INTERNAL_ERROR
; **************

          JSR ASCII_TS
          STX INTERR+7
          STA INTERR+8
          JSR z_new_line
          JSR Dump_Code
          Print(INTERR)
          LDX #2
INER_10   LDA 0,X
          STA $4000,X
          INX
          BNE INER_10
          JMP z_quit

; ***************
  Print_To_Status
; ***************

          STA Z_STATUS,Y
          CPY #COLS-1
          BCS PTS_01
          INC Status_Col
PTS_01    RTS

****************
Module Print_Mem
****************

         LDY #0
         STA (Z_Mem_Ptr),Y
         INW Z_Mem_Ptr
         RTS
EndMod

; ****************
  Print_Unbuffered
; ****************

          JMP CHROUT

**********************
Module Print_Formatted
**********************

          BIT Version
          BPL PrFo_10
          BIT Z_Mem_Flag
          BMI Print_Mem       ; print to stream # 3
          LDY Z_Buffer_Mode
          BEQ Print_Unbuffered
          LDY Z_Active_Window
          BNE Print_Unbuffered
          BEQ PrFo_20
PrFo_10   LDY Status_Col      ; Version 3
          BPL Print_To_Status
PrFo_20   CMP #CR
          BEQ z_new_line
          CMP #' '            ; not printable ?
          BCC PrFo_25
          LDY Charbuf_Ptr
          STA Charbuf,Y
          TAX                 ; X = char
          CLC
          TYA
          ADC Cursor_Col
          CMP #COLS
          BCS PrFo_30         ; -> buffer full: print row


          INC Charbuf_Ptr
PrFo_25   RTS

PrFo_30   LDA #' '        ; scan backwards for blank
          STY Charbuf_End
PrFo_35   CMP Charbuf,Y
          BEQ PrFo_40
          DEY
          BNE PrFo_35
          LDY Charbuf_End ; no blanks found
PrFo_40   STY Charbuf_Ptr ; print line before Charbuf_Ptr
          TYA
          PHA             ; save line break col
          JSR z_new_line  ; print line
          PLA
          TAX             ; line break col
          LDY #0
          BEQ PrFo_50

PrFo_45   LDA Charbuf,X   ; scroll rest of buffer
          STA Charbuf,Y
          INY
PrFo_50   CPX Charbuf_End
          INX
          BCC PrFo_45
          STY Charbuf_Ptr
          RTS
EndMod

; ************
  z_new_line
; ************

          INC MORE_Counter
          LDX MORE_Counter
          CPX #ROWS-2
          BCC Terminate_Buffer
          JSR z_show_status
          LDA Upper_Size
          STA MORE_Counter
          JSR Empty_Keyboard_Queue
          JSR Save_Cursor
          Print(MORE)
znl_10    JSR GETIN
          TAX
          BEQ znl_10
          JSR Restore_Cursor
          Print(BLANKS)
          JSR Restore_Cursor

; ****************
  Terminate_Buffer
; ****************

          LDX Charbuf_Ptr
          LDA #CR
          STA Charbuf,X
          INC Charbuf_Ptr

; ************
  Print_Buffer
; ************

          LDA Charbuf_Ptr
          BEQ PrBu_Ret        ; -> nothing to print
          LDX #0
PrBu_10   LDA Charbuf,X       ; flush buffer
          JSR CHROUT
          INX
          CPX Charbuf_Ptr
          BCC PrBu_10
          LDX #0
          STX Charbuf_Ptr
PrBu_Ret  RTS

; **************
  Format_Integer
; **************

          LDX X0L
          LDA X0H
Format_XA LDY #$2f
          SEC
FORINT_01 INY
          STX LV0
          STA LV1
          TXA
          SBC #<10000
          TAX
          LDA LV1
          SBC #>10000
          BCS FORINT_01
          STY NUMBER
          LDX LV0
          LDA LV1
          LDY #$2f
          SEC
FORINT_02 INY
          STX LV0
          STA LV1
          TXA
          SBC #<1000
          TAX
          LDA LV1
          SBC #>1000
          BCS FORINT_02
          STY NUMBER+1
          LDX LV0
          LDA LV1
          LDY #$2f
          SEC
FORINT_03 INY
          STX LV0
          STA LV1
          TXA
          SBC #100
          TAX
          LDA LV1
          SBC #0
          BCS FORINT_03
          STY NUMBER+2
          LDA LV0
          LDY #$2f
          SEC
FORINT_04 INY
          SBC #10
          BCS FORINT_04
          STY NUMBER+3
          ADC #$3a
          STA NUMBER+4
          LDX #0
          LDA #' '
FORINT_05 LDY NUMBER,X
          CPY #'0'
          BNE FORINT_06
          STA NUMBER,X
          INX
          CPX #4
          BCC FORINT_05
FORINT_06 RTS

********************
Module Status_Number
********************

; This routine is used in version 3 stories
; Insert number (score, moves, time) into status line
; Imput : (A) = global variable
;         (Y) = status line column

          PHY
          JSR Get_Global_Var
          JSR Format_Integer
          PLY
          LDX #0
StNu_10   LDA NUMBER,X
          CMP #' '
          BEQ StNu_20
          STA SCORE,Y
          INY
StNu_20   INX
          CPX #5
          BCC StNu_10
          RTS
EndMod

*****************
Module Print_Time
*****************

          LDA #$11            ; hours var
          JSR Get_Global_Var
          JSR Format_Integer
          LDY #STIME_COL
          LDA NUMBER+3
          STA STIME,Y
          LDA NUMBER+4
          STA STIME+1,Y

          LDA #$12            ; minutes var
          JSR Get_Global_Var
          JSR Format_Integer
          LDY #STIME_COL
          LDA NUMBER+3
          ORA #'0'            ; blank -> '0'
          STA STIME+3,Y
          LDA NUMBER+4
          STA STIME+4,Y

          LDX #0
_loop     LDA STIME,X
          STA Z_STATUS+88,X
          INX
          CPX #12
          BCC _loop
          RTS
EndMod

******************
Module Print_Score
******************

          LDA #' '
          LDY #SCORE_COL+4
_clr_sc   STA SCORE,Y         ; clear score
          DEY
          CPY #SCORE_COL
          BNE _clr_sc
          LDA #$11            ; score var
          JSR Status_Number   ; print score
          LDA #' '
          LDY #MOVES_COL+4
_clr_mv   STA SCORE,Y         ; clear score
          DEY
          CPY #MOVES_COL
          BNE _clr_mv
          LDA #$12            ; moves var
          JSR Status_Number

          LDX #0
_loop     LDA SCORE,X
          STA Z_STATUS+STAT_SCORE,X
          INX
          CPX #COLS-STAT_SCORE
          BCC _loop
          RTS
EndMod

*******************
Module Print_Status
*******************

          JSR Select_Status_Window
          LDA #YELLOW
          STA COLOR
          Print(PRE_STATUS)
          LDA #<Z_STATUS
          LDY #>Z_STATUS
          LDX #COLS-1
          JSR PrintText
          LDA #REVERSE_OFF
          JSR CHROUT
          LDA #WHITE
          STA COLOR
          JMP Select_Text_Window
EndMod

*******************
MODULE z_set_window
*******************
          JSR Print_Buffer
          LDX X1L             ; new active window
          CPX Z_Active_Window
          BEQ _return         ; -> no change
          STX Z_Active_Window
          LDA FG_Color,X
          STA COLOR
          TXA
          BEQ _lower

; switch from window lower window 0 to upper window 1

          LDX #0
          STX Win_Top
          DEX                 ; X = -1
          STX Z_Monospace
          LDX Upper_Size
          DEX
          STX Win_Bot
_return   RTS

; switch from upper window 1 to lower window 0

_lower    LDA Upper_Size    ; window 0: lower
          STA Win_Top
          LDX #ROWS-1
          STX Win_Bot
          LDY #0
          STY Z_Monospace
          JMP Set_Row_Col
ENDMOD

; *************
  Set_Underline
; *************

; Input : Z-Flag

          BEQ Sund_10
          LDA #%1111 1100
Sund_10   STA Z_Underline
          RTS

; ***********
  Set_Reverse
; ***********

; Input : Z-Flag

          BEQ Srev_10
          LDA #$80
          STA RVS             ; reverse on
          RTS

Srev_10   LDA #0              ; reverse off
          STA RVS
          RTS

; ****************
  z_set_text_style
; ****************

; % 76543210 Z                C128
;       ^--- 8: fixed pitch   ignore (font is fixed anyway)
;        ^-- 4: Italic        Att Bit 5 = underline
;         ^- 2: Bold
;          ^ 1: Reverse       RVS = $ff
;            0: Normal        RVS = 0    Underline = 0

          JSR Print_Buffer
          LDA X1L
          AND #1
          JSR Set_Reverse
          LDA X1L
          AND #4
          JMP Set_Underline

; *************
  z_buffer_mode
; *************
          JSR Print_Buffer
          LDA X1L
          STA Z_Buffer_Mode
          RTS

; ************
  z_set_cursor
; ************

; Input : X1 = new row position relative to window top
;         X2 = new column position

          LDA Upper_Size
          STA MORE_Counter
          JSR Print_Buffer    ; the Z  cursor home is [1:1]

          LDX X1L             ; the OS cursor home is [0:0]
          DEX                 ; so subtract 1 from each
          LDY X2L
          DEY

;  JSR Monitor_Break

          TXA
          CLC
          ADC Win_Top
          TAX
          JMP Set_Row_Col

**********************
Module z_output_stream
**********************

; Input:  (X1L) = stream number  3:  select memory
;                               -3:deselect memory
;         (X2)  = table address

          JSR Print_Buffer    ; flush buffer
          LDA X1L             ; stream number
          CMP #-3             ; deselect memory stream ?
          BEQ zos_20          ; -> do
          CMP #3              ; select memory stream ?
          BNE zos_10          ; no -> return
          LDA #-1
          STA Z_Mem_Flag      ; open memory channel
          CLC
          LDA X2H
          ADC #>Z_HEADER
          STA Z_Mem_Base+1    ; Z_Mem_Base = TABLE
          STA Z_Mem_Ptr+1
          LDA X2L
          STA Z_Mem_Base
          ADC #2
          STA Z_Mem_Ptr       ; Z_Mem_Ptr = TABLE+2
          BCC zos_10
          INC Z_Mem_Ptr+1
zos_10    RTS

; close memory channel
; store length of TABLE in first word of (Z_Mem_Base)

zos_20    LDY #1
          LDA Z_Mem_Base      ; (A0) = (Z_Mem_Base)
          STA A0L
          LDA Z_Mem_Base+1
          STA A0H
          SEC
          LDA Z_Mem_Ptr       ; (A/X) = (Z_Mem_Ptr)-2
          LDX Z_Mem_Ptr+1
          SBC #2
          BCS zos_30
          DEX
          SEC
zos_30    SBC A0L
          STA (A0L),Y         ; length low
          DEY                 ; Y = 0
          STY Z_Mem_Flag      ; close channel
          TXA
          SBC A0H
          STA (A0L),Y         ; length high
          RTS
EndMod

; **************
  z_erase_window
; **************
          JSR Print_Buffer
          LDX X1L
          CPX #-1
          BNE zew_10
          LDA #0
          STA Upper_Size
          STA Win_Top
          STA Z_Active_Window
          LDA #CLEAR
          JMP CHROUT
zew_10    JMP Erase_Upper_Window

; ***********
  z_extension
; ***********
          PLA                  ; remove return address
          PLA
          JSR Next_Instruction ; extendedcode
          STA Z_Code
          JSR Next_Instruction ; operand type
          JSR Load_4_Operands
          LDA Z_Code
          AND #$1f
          CMP #10
          BCS z_error12
          ASL A
          TAY
          LDA opcodes_ext,Y
          STA JSRMOD+1
          LDA opcodes_ext+1,Y
          STA JSRMOD+2
          JMP JSRMOD

z_error12 ERROR(12)


; ***********
  z_save_undo
; ***********

          LDA #$FF
          TAX
          JMP Store_AX

; **************
  z_sound_effect
; **************

          LDX X1L
          DEX
          BEQ Click
          DEX
          BEQ Error_Beep
          RTS

; **********
  Error_Beep
; **********

          LDA #7
          JMP CHROUT

; *****
  Click
; *****
          RTS




; ********************
  Empty_Keyboard_Queue
; ********************

          JSR GETIN
          BNE Empty_Keyboard_Queue
          RTS

; ***********
  Decode_Unit
; ***********
          CMP #'0'
          BCS DeUn_10
          TXA                 ; one digit
          BNE DeUn_20
DeUn_10   CPX #'1'
          BNE DeUn_20
          ADC #9              ; two digits 10 - 15
DeUn_20   AND #15
          RTS


; ***********
  Unit_Dialog
; ***********

          LDX #<Unit_Buf
          LDY #>Unit_Buf
          JSR Got_Line
          LDX Unit_Text       ; 10
          LDA Unit_Text+1     ;  1
          JSR Decode_Unit
          STA Save_Unit
          RTS

; ***************
  Wait_for_Return
; ***************

          LDA #<PRESSRET
          LDY #>PRESSRET
          LDX #[SAVEUNIT-PRESSRET]
          JSR PrintText
          JSR Empty_Keyboard_Queue
wfret_10  JSR Get_Character
          BEQ wfret_99        ; -> CR
;         JSR Error_Beep
          JMP wfret_10
wfret_99  RTS

; ***********
  File_Dialog
; ***********

          LDX #<File_Buf
          LDY #>File_Buf
          JMP Got_Line

; ****************
  Read_Disk_Status
; ****************
          TXA
          PHA
          LDA FA
          JSR TALK
          LDA #$6f
          JSR TKSA
          LDY #0
          STY IO_STATUS
RDS_10    JSR ACPTR           ; read status
          LDX IO_STATUS
          BNE RDS_30
          STA DSTATUS,Y
          CMP #' '
          BCC RDS_20
          INY
          CPY #40
          BCC RDS_10
RDS_20    JSR UNTLK           ; close channel
RDS_30    LDA #CR
          STA DSTATUS,Y
          LDA #0
          STA DSTATUS+1,Y
          PLA
          TAX
          LDA DSTATUS
          AND #15             ; return with 1st. value
          RTS

; **********
  z_ext_save
; **********
          JSR Save_Game
          LDA #1
          JMP Store_Byte
; ******
  z_save
; ******

          JSR Save_Game
          JMP Main_True

; *********
  Save_Game
; *********
          Print(SAVEUNIT)
          JSR Unit_Dialog
          Print(SAVEFILE)
          JSR File_Dialog

          LDA Z_HEADER+2        ; save config
          STA Z_VAR+$20
          LDA Z_HEADER+3
          STA Z_VAR+$21
          LDA z_stack_ptr      ; save Z stack pointer
          STA Z_VAR+$22
          LDA z_frame_ptr
          STA Z_VAR+$24
          LDX #2
WG_10     LDA QI0,X           ; save IP
          STA Z_VAR+$26,X
          DEX
          BPL WG_10
          INX
          STX IO_STATUS       ; clear status
          LDA Save_Unit
          STA FA
          JSR LISTEN          ; open Pos,Device,3
          LDA #$f3
          JSR SECOND
          LDY #0
WG_20     LDA File_Text,Y
          CMP #' '
          BCC WG_30
          CMP #'a'
          BCC WG_25
          SBC #$20
WG_25     JSR CIOUT
          INY
          BNE WG_20

WG_30     LDA #','
          JSR CIOUT
          LDA #'W'
          JSR CIOUT
          JSR UNLSN
          LDA IO_STATUS
          BNE WG_Err

          LDA FA
          JSR LISTEN
          LDA #$63
          JSR SECOND
          LDA #>Z_VAR        ; Save variables & stack
          STA RAM_HI
          LDX #3
WG_40     JSR Write_Block
          LDA IO_STATUS
          BNE WG_Err
          DEX
          BNE WG_40
          LDA #>Z_HEADER
          STA RAM_HI
          LDX h_dynamic_size_hi
          INX                 ; # of blocks
WG_50     JSR Write_Block
          LDA IO_STATUS
          BNE WG_Err
          DEX
          BNE WG_50
          JSR UNLSN
          JMP Close_Save_File

WG_Err    JSR UNLSN
          JSR Read_Disk_Status
          Print(DSTATUS)
          JMP Close_Save_File

; ***********
  Write_Block
; ***********

          LDY #0
          STY X0L
          STY IO_STATUS
WrBl_10   LDA (RAM_LO),Y
          JSR CIOUT
          INY
          BNE WrBl_10
          INC RAM_HI
          LDA #'.'
          JMP CHROUT

; *********
  z_restore
; *********

          JSR Restore_Game
          JMP Main_True

; *************
  z_ext_restore
; *************

          JSR Restore_Game
          LDA #2
          JMP Store_Byte

; ************
  Restore_Game
; ************

          Print(LOADUNIT)
          JSR Unit_Dialog
          Print(LOADFILE)
          JSR File_Dialog

          LDX #$1f             ; Save local variables
RG_02     LDA Z_VAR,X          ; to stack bottom -
          STA Charbuf,X        ; if restoring fails
          DEX                  ; we can retrieve them
          BPL RG_02
          LDA Save_Unit
          STA FA
          JSR LISTEN           ; open Pos,Device,3
          LDA #$f3
          JSR SECOND
          LDY #0
RG_10     LDA File_Text,Y
          CMP #' '
          BCC RG_20
          CMP #'a'
          BCC RG_15
          SBC #$20
RG_15     JSR CIOUT
          INY
          BPL RG_10
RG_20     JSR UNLSN

; Check drive error channel

          LDA Save_Unit
          STA FA
          JSR TALK
          LDA #$63
          JSR TKSA
          LDA #>Z_VAR
          STA RAM_HI
          JSR Read_Block      ; Read Z_VAR
          LDA Z_VAR+$20
          CMP Z_HEADER+2      ; Check for
          BNE RG_03           ; correct version
          LDA Z_VAR+$21
          CMP Z_HEADER+3
          BEQ RG_05           ; OK -> continue restoring
RG_03     LDX #$1f            ; Restore local variables
RG_04     LDA Charbuf,X       ; and abort restoring
          STA Z_VAR,X
          DEX
          BPL RG_04
          JSR Reset_Screen
          JMP Main_False

RG_05     LDA h_flags_hi      ; save flags
          PHA
          LDA h_flags_lo
          PHA

          LDX #2
          STX A0L
RG_77     JSR Read_Block
          DEC A0L
          BNE RG_77

          LDA #>Z_HEADER
          STA RAM_HI
          LDX h_dynamic_size_hi
          INX                 ; # of blocks
          STX A0L             ; block counter
RG_06     JSR Read_Block
          DEC A0L
          BNE RG_06

          PLA                 ; restore flags
          STA h_flags_lo
          PLA
          STA h_flags_hi

          LDA Z_VAR+$22
          STA z_stack_ptr
          LDA Z_VAR+$24
          STA z_frame_ptr
          LDX #2
RG_07     LDA Z_VAR+$26,X
          STA QI0,X
          DEX
          BPL RG_07
          JSR UNTLK
          JMP Close_Save_File


; ***************
  Close_Save_File
; ***************

          LDA Save_Unit
          STA FA
          JSR LISTEN
          LDA #$e3
          JSR SECOND
          JMP UNLSN

; **********
  Read_Block
; **********

          LDY #0
          STY X0L
          STY IO_STATUS
ReBl_10   JSR ACPTR
          STA (RAM_LO),Y
          INY
          BNE ReBl_10
          INC RAM_HI
          LDA #'.'
          JMP CHROUT



********
* DATA *
********

Row_Lo    .BYTE <[SCREEN]
          .BYTE <[SCREEN +  1 * COLS]
          .BYTE <[SCREEN +  2 * COLS]
          .BYTE <[SCREEN +  3 * COLS]
          .BYTE <[SCREEN +  4 * COLS]
          .BYTE <[SCREEN +  5 * COLS]
          .BYTE <[SCREEN +  6 * COLS]
          .BYTE <[SCREEN +  7 * COLS]
          .BYTE <[SCREEN +  8 * COLS]
          .BYTE <[SCREEN +  9 * COLS]
          .BYTE <[SCREEN + 10 * COLS]
          .BYTE <[SCREEN + 11 * COLS]
          .BYTE <[SCREEN + 12 * COLS]
          .BYTE <[SCREEN + 13 * COLS]
          .BYTE <[SCREEN + 14 * COLS]
          .BYTE <[SCREEN + 15 * COLS]
          .BYTE <[SCREEN + 16 * COLS]
          .BYTE <[SCREEN + 17 * COLS]
          .BYTE <[SCREEN + 18 * COLS]
          .BYTE <[SCREEN + 19 * COLS]
          .BYTE <[SCREEN + 20 * COLS]
          .BYTE <[SCREEN + 21 * COLS]
          .BYTE <[SCREEN + 22 * COLS]
          .BYTE <[SCREEN + 23 * COLS]
          .BYTE <[SCREEN + 24 * COLS]

Row_Hi    .BYTE >[SCREEN]
          .BYTE >[SCREEN +  1 * COLS]
          .BYTE >[SCREEN +  2 * COLS]
          .BYTE >[SCREEN +  3 * COLS]
          .BYTE >[SCREEN +  4 * COLS]
          .BYTE >[SCREEN +  5 * COLS]
          .BYTE >[SCREEN +  6 * COLS]
          .BYTE >[SCREEN +  7 * COLS]
          .BYTE >[SCREEN +  8 * COLS]
          .BYTE >[SCREEN +  9 * COLS]
          .BYTE >[SCREEN + 10 * COLS]
          .BYTE >[SCREEN + 11 * COLS]
          .BYTE >[SCREEN + 12 * COLS]
          .BYTE >[SCREEN + 13 * COLS]
          .BYTE >[SCREEN + 14 * COLS]
          .BYTE >[SCREEN + 15 * COLS]
          .BYTE >[SCREEN + 16 * COLS]
          .BYTE >[SCREEN + 17 * COLS]
          .BYTE >[SCREEN + 18 * COLS]
          .BYTE >[SCREEN + 19 * COLS]
          .BYTE >[SCREEN + 20 * COLS]
          .BYTE >[SCREEN + 21 * COLS]
          .BYTE >[SCREEN + 22 * COLS]
          .BYTE >[SCREEN + 23 * COLS]
          .BYTE >[SCREEN + 24 * COLS]

L_Encode_A  .BYTE 0 ;
L_Encode_Y  .BYTE 0 ;

; data for separators / delimiters in parsing

Sep_Std_List    .BYTE "!?,.\r " ; built in standard
Z_Arg_Count     .BYTE 0 ; argument count
Z_Buffer_Mode   .BYTE 1 ; output buffering on or off
Z_Call_Type     .BYTE 0 ; type of subroutine call
Z_Monospace     .BYTE 0
Z_Underline     .BYTE 0
Z_Active_Window .BYTE 0

******************************
* Cursor, Windows and Screen *
******************************

******************
Set_Screen_Pointer
******************

          LDX Cursor_Row

********************
Set_Screen_Pointer_X
********************

          LDA Row_Lo,X
          STA Scr_Adr
          STA Col_Adr
          LDA Row_Hi,X
          STA Scr_Adr+1
          AND #7
          STA Col_Adr+1
          RTS


***********
Window_Home
***********

          LDX Upper_Size
          LDY #0

***********
Set_Row_Col
***********

; Input : X = screen row    (0 .. 24)
;         Y = screen column (0 .. 79)

          CPX #ROWS
          BCC SRC_10
          LDX #ROWS-1
SRC_10    CPY #COLS
          BCC SRC_20
          LDY #COLS-1

SRC_20    STX Cursor_Row
          STY Cursor_Col
          JSR Set_Screen_Pointer

***********
Get_Row_Col
***********

; Output: X = screen row    (0 .. 24)
;         Y = screen column (0 .. 79)

          LDX Cursor_Row
          LDY Cursor_Col
          RTS

**********
Info_Print
**********

; Input: (X) = First page, (A) = Last page, (Y) = print pos
;----------------------------------------------------------

          PHA
          TXA
          JSR ASCII_Hex
          STA InfoPro + 10,Y
          TXA
          STA InfoPro +  9,Y
          PLA
          JSR ASCII_Hex
          STA InfoPro + 17,Y
          TXA
          STA InfoPro + 16,Y
          LDA #0
          STA X0H
          LDA Info_Pages
          STA X0L

; *********
  Info_Size
; *********
          TYA
          PHA
          JSR Format_Integer
          PLA
          TAY
          LDX #1
InSi_10   LDA NUMBER,X
          STA InfoPro+21,Y
          INY
          INX
          CPX #5
          BCC InSi_10
          RTS

; ************
  Screen_Setup
; ************

          LDA #$0F       ; colour RAM: $FF80000
          STA Col_Adr+3
          LDA #$F8
          STA Col_Adr+2
          RTS

; ***********
  Story_Pages
; ***********

; Leave story size in X0L/X0H for printing

          LDA #0
          STA X0H
          LDA h_file_size_hi    ; size in words high
          STA X0L
          LDA h_file_size_lo    ; size in words low
          ASL A
          ROL X0L
          ROL X0H               ; size * 2
          BIT Version
          BPL STTS_05
          ASL A
          ROL X0L
          ROL X0H               ; size * 4
STTS_05   CMP #0
          BEQ STTS_10           ; at page boundary
          INW X0L               ; add 1 to round up
STTS_10   RTS


; *********
  z_restart
; *********

          CLD
          LDX #$fb       ; Commodore default stack initialisation
          TXS
          LDA #0         ; clear ZP variables
          LDX #Z_Code    ; start of interpreter variables
Start_10  STA 0,X
          INX
          CPX #ZP_END
          BCC Start_10
          LDA #1
          STA Upper_Size       ; status line for version 3
          JSR Screen_Setup
          INC z_stack_ptr      ;  1
          INC z_frame_ptr      ;  1
          DEC Status_Col       ; -1
          JSR Open_Story       ; open  8,8,8,"z3*"
          LDA #>Z_HEADER
          STA RAM_HI
          JSR Load_Page        ; load first block to Z_HEADER
          LDA h_version
          CMP #6
          BCS Start_20         ; version > 5
          CMP #3
          BCC Start_20         ; version < 3
          BEQ Start_15         ; version = 3
          ORA #$80             ; version > 3
          STA Version
          LDA #9
          STA Word_Length
          LDA #6
          STA Vocab_Length
          LDA #$3f
          STA Prop_Mask
          BRA Start_22

; Version 3 initialisation

Start_15  STA Version
          LDA #6
          STA Word_Length
          LDA #4
          STA Vocab_Length
          LDA #$1f
          STA Prop_Mask
          LDA #<z_not
          STA ZV8F
          LDA #>z_not
          STA ZV8F+1
          LDA #<z_pop
          STA ZVB9
          LDA #>z_pop
          STA ZVB9+1
          BRA Start_22

Start_20  Print(NOSTORY)
          JMP quit_20
Start_22

; =================
; Set Memory Layout
; =================

          LDX #>[$D000 - Z_HEADER]
          STX Resident_Pages  ; reserved
          LDX #COLS
          STX h_screen_cols

          LDA h_config
          ORA #%0011 0001     ; fixed font / colours
;               0--- ---- 7:timed input
;                 1- ---- 5:split screen
;                  1 ---- 4:fixed  font
;                    0--- 3:italic font
;                     0-- 2:bold   font
;                      0- 1:pictures
;                       1 0:colors
          STA h_config
          LDA #%0000 1100
          ORA h_flags_lo
          STA h_flags_lo
          LDA #0
          STA h_screen_width_hi
          STA h_screen_height_hi
          LDA #COLS
          STA h_screen_width_lo
          LDA #ROWS-1
          STA h_screen_height_lo
          LDA #1
          STA h_font_width
          STA h_font_height
          LDA #8              ; 6:PC 7:C128 8:C64
          STA h_interpreter_number
          LDA #'G'
          STA h_interpreter_version

; -------- compute story location and size -----

          LDA #>EOP - >START+1 ; program
          STA Info_Pages
          LDX #>START
          LDA #>EOP
          LDY #0
          JSR Info_Print
          LDA Resident_Pages ; static
          STA Info_Pages
          CLC
          ADC #>Z_HEADER-1
          LDX #>Z_HEADER
          LDY #InfoSta-InfoPro
          JSR Info_Print
          JSR Story_Pages
          LDY #InfoSto-InfoPro
          JSR Info_Size
          Print(BITSHIFTER)
          JSR Select_Text_Window
          LDA #<InfoClr
          LDY #>InfoClr
          LDX #[InfoEnd - InfoClr]
          JSR PrintText

Start_30  JSR Load_Page       ; Load resident area
          LDA IO_STATUS
          BNE Start_50        ; EOF
          LDA Block_Lo
          AND #7
          BNE Start_40
          LDA #'.'
          JSR CHROUT
Start_40  LDA Block_Lo
          CMP Resident_Pages
          BCC Start_30

Start_50  JSR Load_Story
          JSR Wait_for_Return
          JSR Set_Mode_80
          LDA h_start_pc_hi   ; Initialize pc
          STA QI1
          LDA h_start_pc_lo
          STA QI0
          JSR Reset_Screen
          JMP PC_LOOP

; ***********
  Next_Datum
; ***********

; Input
; =====
; QD0 = Block pointer
; QD1 = Block # lo
; QD2 = Block # hi

; Output
; ======
; (A) = Byte from (Block),QD0
; Pointer QD incremented

          LDA #0
          STA RAM_BA
          LDZ QD0
          LDA QD2             ; page high
          BNE NEDA_10         ; > 64 K
          LDA QD1
          CMP Resident_Pages
          BCS NEDA_10         ; -> not resident
          ADC #>Z_HEADER
          STA RAM_HI
          BNE NEDA_50         ; -> always

; load if address is beyond resident part
; address = (QD1/2) - Resident + $040000

NEDA_10   SEC
          LDA QD1             ; page low
          SBC Resident_Pages
          STA RAM_HI          ; page low
          LDA QD2
          SBC #0              ; page high
          ADC #3              ; bank 4 = carry  + 3
          STA RAM_BA
NEDA_50   LDA [RAM_LO],Z
          INC QD0
          BNE NEDA_80
          INC QD1
          BNE NEDA_80
          INC QD2
NEDA_80   CMP #0              ; set flags
          RTS

; ****************
  Next_Instruction
; ****************

; Input
; =====
; QI0 = pointer inside page
; QI1 = page # low
; QI2 = page # high

; Output
; ======
; (A) = Byte from 24 bit address (QI)
; Pointer QI incremented
; test if address is inside resident part

          LDA #0
          STA RAM_BA
          LDZ QI0
          LDA QI2             ; page high
          BNE NEIN_10         ; > 64 K
          LDA QI1
          CMP Resident_Pages
          BCS NEIN_10         ; -> not resident
          ADC #>Z_HEADER
          STA RAM_HI
          BNE NEIN_50         ; -> always

; load if address is beyond resident part
; address = (QI1/2) - Resident + $040000

NEIN_10   SEC
          LDA QI1             ; page low
          SBC Resident_Pages
          STA RAM_HI          ; page low
          LDA QI2
          SBC #0              ; page high
          ADC #3              ; bank 4 = carry  + 3
          STA RAM_BA
NEIN_50   LDA [RAM_LO],Z
          INC QI0
          BNE NEIN_80
          INC QI1
          BNE NEIN_80
          INC QI2
NEIN_80   CMP #0              ; set flags
          RTS


; *************
  z_show_status
; *************

; save cursor coordinates, print to statusline

          JSR Save_Cursor

; push QD2, QD1, QD0, QDH, QDL

          LDX #4
zss_10    LDA QDL,X
          PHA
          DEX
          BPL zss_10

          INX                 ; X = 0
          STX Status_Col      ; switch decoder to status line
          LDA #$10            ; get location
          JSR Get_Global_Var
          LDA X0L
          LDX X0H
          JSR z_print_obj_A
          LDA #' '            ; fill rest of line with blanks
          LDX Status_Col
zss_20    STA Z_STATUS,X    ; erase rest of line
          INX
          CPX #COLS
          BCC zss_20

          LDX #$ff
          STX Status_Col      ; reset decoder
          LDA h_config
          AND #2              ; Score_Time_Flag
          BNE zss_30
          JSR Print_Score
          JMP zss_40
zss_30    JSR Print_Time
zss_40    JSR Print_Status

; pull QDL, QDH, QD0, QD1, QD2 after recursive call

          LDX #0
zss_90    PLA
          STA QDL,X
          INX
          CPX #5
          BCC zss_90

          JMP Restore_Cursor

; **********
  Fix_Colors
; **********

          LDX #7
FiCo_10   LDA FG_Color,X
          AND #15
          STA FG_Color,X
          DEX
          BPL FiCo_10
          RTS

; *********
  Cursor_On
; *********
          LDY Cursor_Col
          LDA #$A0
          STA (Scr_Adr),Y
          STA Cursor_Vis
          RTS

; **********
  Cursor_Off
; **********
          LDY Cursor_Col
          LDA #' '
          STA (Scr_Adr),Y
          LDA #0
          STA Cursor_Vis
          RTS

; *************
  Get_Character
; *************

; get a character from keyboard
; allow all ASCII characters $20 - $7e
; allow control codes CR and DEL
; compare char with CR before return

          CLI
          PHY                 ; save Y
          JSR Cursor_On
GeCh_10   JSR GETIN
          BEQ GeCh_10
          CMP #CR
          BEQ GeCh_40         ; allow CR
          CMP #DEL
          BEQ GeCh_40
          CMP #'Z'+$81        ; CBM 'Z'+1
          BCS GeCh_30         ; -> not ASCII
          CMP #'A'+$80        ; CBM 'A'
          BCC GeCh_22
          AND #$7f            ; to ASCII
          BNE GeCh_40         ; always

GeCh_22   CMP #'Z'+1          ; CBM 'z'+1
          BCS GeCh_30         ; -> not ASCII
          CMP #'A'            ; CBM 'a'
          BCC GeCh_24
          ADC #$1f            ; to ASCII
          BNE GeCh_40         ; always

GeCh_24   CMP #' '
          BCS GeCh_40

GeCh_30   ;JSR Error_Beep      ; unacceptable
          JMP GeCh_10

GeCh_40   PHA                 ; push char
          JSR Cursor_Off
          PLA
          PLY                 ; restore Y
          CMP #CR
          RTS

; ******************
  Scroll_Main_Window
; ******************

          LDX Upper_Size
          JSR Set_Screen_Pointer_X
          CLC
          LDA Scr_Adr
          ADC #COLS
          STA SAP         ; screen low
          STA TAP         ; colour low
          LDA Scr_Adr+1
          ADC #0
          STA SAP+1       ; screen high
          AND #7
          STA TAP+1       ; colour high
          LDA #$f8
          STA TAP+2       ; colour bank low
          LDA #$0f
          STA TAP+3       ; colour bank high
SMW_10    LDZ #COLS-1
SMW_20    LDA (SAP),Z
          STA (Scr_Adr),Z
          LDA [TAP],Z
          STA [Col_Adr],Z
          DEZ
          BPL SMW_20
          CLC
          LDA SAP
          STA Scr_Adr
          STA Col_Adr
          ADC #COLS           ; C=0
          STA SAP
          STA TAP
          LDA SAP+1
          STA Scr_Adr+1
          AND #7
          STA Col_Adr+1
          LDA SAP+1
          ADC #0
          STA SAP+1
          AND #7
          STA TAP+1
          INX
          CPX #ROWS-1
          BCC SMW_10
          LDZ #COLS-1
SMW_30    LDA #' '
          STA (Scr_Adr),Z
          LDA #1
          STA [Col_Adr],Z
          DEZ
          BPL SMW_30
          RTS

******************
MODULE Home_Screen
******************

          PHX
          LDX #0
          STX Cursor_Col
          STX Cursor_Row
          JSR Set_Screen_Pointer_X
          PLX
          RTS
ENDMOD

*******************
MODULE Clear_Screen
*******************

          PHX
          PHY
          LDX #ROWS-1
_loop     JSR Erase_Row
          DEX
          BPL _loop
          JSR Home_Screen
          PLY
          PLX
          RTS
ENDMOD

; *************
  Return_Screen
; *************
          PHX
          PHY
          LDY #0
          STY Cursor_Col
          LDX Cursor_Row
          INX
          CPX #ROWS
          BCC ReSc_10
          JSR Scroll_Main_Window
          LDX #ROWS-1
ReSc_10   STX Cursor_Row
          JSR Set_Screen_Pointer_X
          PLY
          PLX

; ******************
  Screen_Reverse_Off
; ******************

          LDA #0
          STA RVS
          RTS


; *****************
  Screen_Reverse_On
; *****************

          LDA #$80
          STA RVS
          RTS

*****************
Module Screen_Del
*****************
          PHY
          LDY Cursor_Col
          LDA #' '
          STA (Scr_Adr),Y
          DEC Cursor_Col
          BPL _exit
          INC Cursor_Col
_exit     PLY
          RTS
EndMod

*************
MODULE CHROUT
*************

          CMP #HOME
          BEQ Home_Screen
          CMP #CLEAR
          BEQ Clear_Screen
          CMP #CR
          BEQ Return_Screen
          CMP #REVERSE_ON
          BEQ Screen_Reverse_On
          CMP #REVERSE_OFF
          BEQ Screen_Reverse_Off
          CMP #DEL
          BEQ Screen_Del

          PHX
          PHY
          PHA
          LDZ Cursor_Col
          CPZ #COLS
          BCC CHRO_10
          JSR Return_Screen
CHRO_10   LDA COLOR
          STA [Col_Adr],Z
          PLA
          AND #$7f
          CMP #' '
          BCS CHRO_20
          LDA #'.'            ; 00-1f -> replace with dot
          BNE CHRO_80
CHRO_20   CMP #'['
          BCC CHRO_80
          AND #$1f            ; 5b-7f -> 01-1f

CHRO_80   LDY Cursor_Col
          ORA RVS
          STA (Scr_Adr),Y
          INC Cursor_Col
          PLY
          PLX
          RTS
ENDMOD

***************
Module Got_Line
***************

; Get line from keyboard with preset text

          STX X1L
          STY X1H
          LDY #0
          LDA (X1L),Y
          STA Chars_Left      ; maximum edit length
          MAC_Color(CYAN)
_loop     INY
          INY
          LDA (X1L),Y
          DEY
          CMP #' '
          BCC _set
          JSR CHROUT
          BRA _loop
_set      BIT Version
          BMI GLX_20
          INW X1L
          DEY
          BRA GLX_20
EndMod

******************
Module Get_Line_X1
******************

          LDA Upper_Size
          STA MORE_Counter
          MAC_Color(CYAN)
          LDY #0
          LDA (X1L),Y
          STA Chars_Left      ; maximum edit length
          BNE GLX_10
          DEC Chars_Left
GLX_10    BIT Version
          BPL GLX_20
          INY                 ; Version > 3

; receive character from keyboard loop

GLX_20    JSR Get_Character  ; Y = previous position
          BEQ GLX_90         ; -> CR = end of input
          CMP #DEL
          BNE GLX_40
          DEY                ; Y = prev - 1
          BMI _del_err       ; version 3 check
          BIT Version
          BPL GLX_30
          CPY #1             ; version > 3 check
          BCS GLX_30         ; -> not at 1st. column

_del_err ;JSR Error_Beep      ; No DEL at 1st. char
          INY                 ; restore Y
          JMP GLX_20

; handle DEL character

GLX_30    JSR Screen_Del       ; print DEL
          JMP GLX_20

; check edit limit

GLX_40    CPY Chars_Left      ; edit limit
          BEQ GLX_45
          BCS GLX_50          ; -> at limit
GLX_45    LDX Cursor_Col
          INX
          CPX #COLS-1         ; -> end of line
          BCC GLX_70

GLX_50    ;JSR Error_Beep      ; reached max buffer length
          JMP GLX_20

GLX_70    INY
          CMP #'A'
          BCC GLX_80
          CMP #'Z'+1
          BCS GLX_80
          ADC #$20            ; to lower case
GLX_80    STA (X1L),Y
          JSR CHROUT          ; ASCII print
          BRA GLX_20

; receivced CR: terminate buffer and return

GLX_90    INY
          STA (X1L),Y         ; store CR
          JSR CHROUT
          MAC_Color(WHITE)
          DEY                 ; don't count CR
          STY Chars_Left      ; edit length
          BIT Version
          BPL _return
          DEY
          TYA
          LDY #1
          STA (X1L),Y         ; length of string
          STA Chars_Left
_return   RTS
EndMod

; *********
  PrintText
; *********

          STA MEMUSS
          STY MEMUSS+1
          LDY #0
PrTe_A    LDA (MEMUSS),Y
          BEQ PrTe_B
          JSR CHROUT
          INY
          DEX
          BNE PrTe_A
PrTe_B    RTS

; **************
  z_split_window
; **************
          JSR Print_Buffer
          LDX X1L             ; new size
          BPL split_10
          LDX #0              ; C64 workaround
split_10  CPX #ROWS
          BCS split_err
          STX Upper_Size    ; update size
          STX Win_Top
          LDX Cursor_Row
          CPX Upper_Size
          BCS split_20
          JSR Window_Home
split_20  JSR Set_Screen_Pointer
          RTS
split_err BRK

; ************
  Reset_Screen
; ************

          JSR Clear_Screen
          JSR Return_Screen
          LDA Upper_Size
          STA MORE_Counter
          RTS

; **********
  Open_Story
; **********

          LDA Game_Unit       ; open "Z*",FA,8
          STA FA
          JSR LISTEN
          LDA #$f8
          JSR SECOND
          LDA #'Z'
          JSR CIOUT
          LDA #'*'
          JSR CIOUT
          JMP UNLSN

; **********
  ASCII_Hex
; **********

; Output: (X) = High nibble (A) = Low nibble
         PHA
         LSR A
         LSR A
         LSR A
         LSR A
         ORA #'0'
         CMP #$3a
         BCC Hex_11
         ADC #6
Hex_11   TAX
         PLA
         AND #15
         ORA #'0'
         CMP #$3a
         BCC Hex_12
         ADC #6
Hex_12   RTS

; ******************
  Select_Text_Window
; ******************

         LDA #1
         STA Win_Top
         LDA #ROWS-1
         STA Win_Bot
         RTS

; ********************
  Select_Status_Window
; ********************

         LDX #0
         STX Win_Top
         JMP Set_Screen_Pointer_X

; ***********
  Save_Cursor
; ***********

          JSR Get_Row_Col
          STX C_Save_Row
          STY C_Save_Col
          RTS


; **************
  Restore_Cursor
; **************

          LDX C_Save_Row
          LDY C_Save_Col
          JMP Set_Row_Col

; *********
  Erase_Row
; *********

          JSR Set_Screen_Pointer_X
          LDA #' '
          LDY #COLS-1
ClRo_10   STA (Scr_Adr),Y
          DEY
          BPL ClRo_10
          RTS

; ******************
  Erase_Upper_Window
; ******************
          LDX #0
EUW_10    LDY #0
          JSR Erase_Row
          INX
          CPX Upper_Size
          BCC EUW_10
          RTS

; ********
  ASCII_TS
; ********

; Convert binary number in (A) to
; two decimal digits in (X) and (A)

          LDX #'0'-1
          SEC
asts_01   INX
          SBC #10
          BCS asts_01
          ADC #$3a
          RTS

****************
Module Load_Page
****************

; Read 256 bytes of data from disk and store them in RAM
; at the address (RAM_LO) = 32 bit address.
; The I/O routines TALK, TKSA, ACPTR, and UNTLK are used
; ACPTR sets the STATUS bit 6 ($40) on error

          LDA FA
          JSR TALK            ; open channel
          LDA #$68            ; SA = 8
          JSR TKSA            ; select channel to disk buffer
          LDZ #0
          STZ IO_STATUS
_loop     JSR ACPTR
          STA [RAM_LO],Z
          LDA IO_STATUS
          BNE _eof
          INZ
          BNE _loop
_eof      JSR UNTLK           ; 256 bytes read, send untalk
          INW Block_Lo        ; increment block word Block_Lo/Hi
          INC RAM_HI          ; advance RAM pointer page
          RTS                 ; return OK
EndMod

*****************
Module Load_Story
*****************

; Continue story loading in bank 4 and bank 5

          LDX #0
          STX RAM_HI
          LDX #4             ; RAM bank
          STX RAM_BA         ; load at $040000
_loop     JSR Load_Page
          LDA IO_STATUS
          BNE _eof
          LDA Block_Lo
          AND #7
          BNE _prog
          LDA #'.'
          JSR CHROUT
_prog     LDA RAM_HI
          BNE _loop
          INC RAM_BA
          LDA RAM_BA
          CMP #6
          BCC _loop
_eof      LDA Game_Unit       ; close #8
          STA FA
          JSR LISTEN
          LDA #$e8
          JSR SECOND
          JMP UNLSN
EndMod

; *************
  Delete_Config
; *************

          LDA Game_Unit
          STA FA
          JSR LISTEN
          LDA #$6f
          JSR SECOND
          LDY #0
DeCo_10   LDA ConfigDel,Y
          JSR CIOUT
          INY
          CPY #[?ConfigDel + ?Configname]
          BCC DeCo_10
          JMP UNLSN

; ***********
  Prep_Config
; ***********

          LDA #<CONFIG_START
          LDY #>CONFIG_START
          STA A0L
          STY A0H
          LDA #<CONFIG_END
          LDY #>CONFIG_END
          STA A1L
          STY A1H
          LDA #?Configname
          LDX #<Configname
          LDY #>Configname
          JMP SETNAM

; ***********
  Save_Config
; ***********

          JSR Delete_Config
          JSR Prep_Config
          INC FNLEN           ; add ','
          INC FNLEN           ; add 'W'
          JMP Save_File

; ***********
  Load_Config
; ***********

          JSR Prep_Config
          JSR Load_File
          RTS


; *********
  Load_File
; *********

; Input : (A0) = start address
;       : (A1) = end   address
;       : SETNAM was called

          LDA FA
          JSR LISTEN
          LDA #$f3
          JSR SECOND
          LDY #0
          STY IO_STATUS
LoFi_10   LDA (FNADR),Y
          JSR CIOUT
          INY
          CPY FNLEN
          BCC LoFi_10
          JSR UNLSN
          LDA FA
          JSR TALK
          LDA #$63
          JSR TKSA
          LDY #0
          JSR ACPTR           ; read first byte
          LDX IO_STATUS
          BEQ LoFi_30
          BNE LoFi_60

LoFi_20   JSR ACPTR
LoFi_30   STA (A0L),Y
          LDA IO_STATUS
          BNE LoFi_60         ; EOF
          INW A0L
          LDA A0H
          CMP A1H
          BCC LoFi_20
          LDA A0L
          CMP A1L
          BCC LoFi_20

LoFi_60   JSR UNTLK           ; close file
          LDA FA
          JSR LISTEN
          LDA #$e3
          JSR SECOND
          JMP UNLSN

; *********
  Save_File
; *********

; Input : (A0) = start address
;       : (A1) = end   address
;       : SETNAM was called

          LDA FA
          JSR LISTEN
          LDA #$f3
          JSR SECOND
          LDY #0
          STY IO_STATUS
SaFi_10   LDA (FNADR),Y
          JSR CIOUT
          INY
          CPY FNLEN
          BCC SaFi_10
          JSR UNLSN

; TODO: check error channel

          LDA FA
          JSR LISTEN
          LDA #$63
          JSR SECOND
          LDY #0
SaFi_20   LDA (A0L),Y
          JSR CIOUT
          INW A0L
          LDA A0H
          CMP A1H
          BCC SaFi_20
          LDA A0L
          CMP A1L
          BCC SaFi_20

          JSR UNLSN           ; close file
          LDA FA
          JSR LISTEN
          LDA #$e3
          JSR SECOND
          JMP UNLSN

TEXT_WORD       .FILL  9 (0) ; unpacked ASCII word

Tokenizer_Flag  .BYTE 0
E_Attribute     .BYTE 0
Z_Mem_Flag      .BYTE 0
Z_Mem_Base      .WORD 0

; ------- data for unit dialog --------
Unit_Buf  .BYTE 2,2 ; input length
Unit_Text .BYTE "08",0
; ------- data for file name dialog----
File_Buf  .BYTE 20,20 ; input length
File_Text .BYTE "savename",0
          .FILL 12 (0)
; ------- story signature ------------
Game_Unit .BYTE 8

PRESSRET  .BYTE "\rPress <RETURN> to continue."
SAVEUNIT  .BYTE "Save to unit:"
SAVEFILE  .BYTE "Save to file:"
LOADUNIT  .BYTE "Restore from unit:"
LOADFILE  .BYTE "Restore from file:"
MORE      .BYTE REVERSE_ON,"<MORE>",REVERSE_OFF
EOS       .BYTE "End of session - press any key"
NOSTORY   .BYTE "NO Z3 STORY"


BITSHIFTER .BYTE CLEAR,"BIT SHIFTER 02-SEP-2020\r"
InfoClr    .BYTE CR
InfoPro    .BYTE 'Program: 0001 - 00FF    0 Pages\r'
InfoSta    .BYTE 'Bank  0: 0000 - 00FF    0 Pages\r'
InfoSto    .BYTE 'Story  :        Size    0 Pages\r',0
InfoEnd

BLANKS    .BYTE "            "

PRE_STATUS .BYTE HOME,REVERSE_ON," "
STAT_SCORE = 52
MOVES_COL  = 22

SCORE      .BYTE "Score: 0       Moves: 0     "
STIME      .BYTE "Time: 00:00 "
SCORE_COL  =  7
STIME_COL  =  6

Win_Bot    .BYTE 24
INTERR     .BYTE " Error 00 "

ConfigDel  .BYTE "S0:"        ; prefix for scratch command
Configname .BYTE "CONFIG"
           .BYTE ",W"         ; postfix for write command

CONFIG_START
FG_Color   .BYTE WHITE        ; f1 133
TI_Color   .BYTE YELLOW       ; f3 134
BO_Color   .BYTE BLUE         ; f5 135
BG_Color   .BYTE BLUE         ; f2 137
TB_Color   .BYTE RED          ; f4 138
           .BYTE 0            ; f6 139
           .BYTE 0            ; f8 140
CONFIG_END

; ***
  EOP
; ***

