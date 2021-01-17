*************************************
* BSZ = Bit Shifter's Z interpreter *
*       for MEGA65      26-Oct-2020 *
*************************************

.CPU 45GS02

***********************
* Commodore KEY codes *
***********************

KEY_F1       = 133
KEY_F3       = 134
KEY_F5       = 135
KEY_F7       = 136
KEY_F2       = 137
KEY_F4       = 138
KEY_F6       = 139
KEY_F8       = 140

CURSOR_DOWN  = $11
CURSOR_RIGHT = $1d
CURSOR_UP    = $91
CURSOR_LEFT  = $9d

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

CR       = $0d
DEL      = $14
INS      = $94

**************
* attributes *
**************

NORMAL    = $00
BLINK     = $10
REVERSE   = $20
BOLD      = $40
UNDERLINE = $80

**********
* Z keys *
**********

KEY_UP    = $81
KEY_DOWN  = $82
KEY_LEFT  = $83
KEY_RIGHT = $84

********************************************************
* Interpreter Zero page variables (occupy BASIC space) *
********************************************************

& = $02

; Instruction pointer     LDZ QI0  ->  LDA [RAMD],Z

Z_Code         .BSS 2           ; current code byte & previous
QI0            .BSS 1           ; Byte 0  pc = (QI0/QI1/QI2)
QI1            .BSS 1           ; Byte 1  Floppy block low
QI2            .BSS 1           ; Byte 2  Floppy block high

; Data pointer            LDZ QD0  ->  LDA [RAMD],Z

QDL            .BSS 1           ; current packed data low
QDH            .BSS 1           ; current packed data high
QD0            .BSS 1           ; Byte 0  pc = (QD0/QD1/QD2)
QD1            .BSS 1           ; Byte 1  Floppy block low
QD2            .BSS 1           ; Byte 2  Floppy block high

; keep above variables together (block push/pull)

RAMD           .BSS 4           ; 32 bit RAM address for story data

; Multi purpose local variables

LV0            .BSS 1
LV1            .BSS 1
LV2            .BSS 1

Alphabet       .BSS 1
Attribute      .BSS 1
Block_Lo       .BSS 1
Block_Hi       .BSS 1
Call_Type      .BSS 1
Charbuf_Ptr    .BSS 1
Charbuf_End    .BSS 1
Chars_Left     .BSS 1
Colour         .BSS 1
C_Save_Col     .BSS 1           ; save column
C_Save_Row     .BSS 1           ; save row
Info_Pages     .BSS 1
Left_Margin    .BSS 1
Log            .BSS 1
MORE_Counter   .BSS 1
NUMBER         .BSS 5
OP_Type        .BSS 2
ParNum         .BSS 1
Parse_Index    .BSS 1
Prop_Mask      .BSS 1
Resident_Pages .BSS 1
Right_Margin   .BSS 1
Save_Unit      .BSS 1
Status_Col     .BSS 1
Timeout        .BSS 2           ; timeout [jiffies]
Tok_Flag       .BSS 1           ; tokeniser flag
TO_Flag        .BSS 1           ; timeout flag
Upper_Size     .BSS 1           ; rows of upper window
Version        .BSS 1           ; $80 = version > 3
Vocab_Length   .BSS 1           ; length of packed vocab
Win_Bot        .BSS 1
Win_Top        .BSS 1           ; upper row of active window
Word_Length    .BSS 1           ; length of dictionary words
z_stack_ptr    .BSS 2           ; Z_STACK_LO -> Z_STACK_HI
z_frame_ptr    .BSS 2           ; save stack pointer
DPL            .BSS 1           ; dictionary pointer
DPH            .BSS 1
DPI            .BSS 1
A0L            .BSS 1           ; primary   address register
A0H            .BSS 1
A1L            .BSS 1           ; secondary address register
A1H            .BSS 1
X0L            .BSS 1           ; primary value   register
X0H            .BSS 1
X1L            .BSS 1           ; parameter register
X1H            .BSS 1           ; X1L - X4H must be contiguous
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
GLL            .BSS 1           ; Get_Line pointer
GLH            .BSS 1

Font_Sel       .BSS 1           ; font selector normal - graphics
Cursor_Col     .BSS 1
Cursor_Row     .BSS 1
Z_Mem_Base     .BSS 2           ; stream address
Z_Mem_Ptr      .BSS 2           ; Z memory pointer
ZP_CLEAR       .BSS 1

; following variables are NOT cleared at restart

Segment        .BSS 1
Scr_Adr        .BSS 2           ; screen RAM 16 bit address
Col_Adr        .BSS 4           ; colour RAM 32 bit address
DICT_WORD      .BSS 6           ; packed ZSCII dictionary word
TEXT_WORD      .BSS 9           ; unpacked     dictionary word



********************
* system variables *
********************

COLS        =  80
ROWS        =  25
R6510       = $01               ; C64 bank switching CPU port
IO_STATUS   = $90               ; used by I/O routines
Jiffy       = $a0               ; jiffy clock
Scr_Row     = $ac               ; used in screen scrolling
Col_Row     = $ae               ; used in screen scrolling
FNLEN       = $b7
FA          = $ba
FNADR       = $bb
MEMUSS      = $c3               ; string address
BLNSW       = $cc               ; cursor blink flag
BLNCT       = $cd
BLNON       = $cf
Charbuf     = $200
SCNMPG      = $288              ; screen memory page for C64 mode
CINV        = $314              ; kernal vector table
DSTATUS     = $33c              ; 40 bytes disk status
SCREEN      = $0800             ; character RAM in 80 column mode
COLRAM      = $d800             ; color     RAM
Raster      = $d012
BorderCol   = $d020
BackgCol0   = $d021
Voc1FreqLo  = $d400
Voc1FreqHi  = $d401
Voc1Control = $d404
Voc1SusRel  = $d406
FiltMode    = $d418
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

Z_VAR       = $0400             ; variables
Z_STACK_LO  = $0500             ; stack bottom
Z_STACK_HI  = $0800             ; stack top
Z_STATUS    = $033c             ; version 3
Lvar_Lo     = Z_VAR
Lvar_Hi     = Z_VAR + $10
Z_HEADER    = [EOP + $ff] & $ff00

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
h_file_size_hi        = Z_HEADER +  26; in words for version 1-3
h_file_size_lo        = Z_HEADER +  27; in words for version 1-3
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

* = 0

***************
* Print Macro *
***************

MACRO Print(lab)
        LDA  #<lab
        LDY  #>lab
        LDX  #?lab
        JSR  PrintText
ENDMAC

MACRO ERROR(num)
        LDA  #num
        JMP  INTERNAL_ERROR
ENDMAC

START = $1001                   ; *** BASIC ***

* = START

        .LOAD $2001
        .STORE START,EOC-START,"bsz-mega65"

****************
* BASIC header *
****************

        .WORD Link
        .WORD 2020              ; line number
        .BYTE $8b               ; IF   token
        .BYTE $c2               ; PEEK token
        .BYTE "(44)"
        .BYTE $b2               ; =    token
        .BYTE "8"
        .BYTE $a7               ; THEN token
        .BYTE $9e               ; SYS  token
        .BYTE "(2113):"         ; C64  start
        .BYTE $d5               ; ELSE token
        .BYTE $fe,$02           ; BANK token
        .BYTE "0:"              ; BANK argument
        .BYTE $9e               ; SYS  token
        .BYTE "(8253):"         ; C65  start
        .BYTE $8f               ; REM  token
        .BYTE " BIT SHIFTER 26-OCT-20",0
Link    .WORD 0                 ; BASIC end marker

        SEI
        JMP  Mode_65 + $1000    ; relocate for 65 mode

**************
Module Mode_64
**************

        lDA  #65                ; 40MHz CPU
        STA  0

        LDY  #0
        STY  A0L
        STY  A1L
        LDA  #>[EOC - $0800]
        STA  A0H
        LDA  #>EOC
        STA  A1H

_loop   LDA  (A0L),Y
        STA  (A1L),Y
        INY
        BNE  _loop
        DEC  A0H
        DEC  A1H
        LDA  A1H
        CMP  #>START
        BCS  _loop
        JMP  MEGA_Setup
EndMod

*****************
Module MEGA_Setup
*****************

        SEI
        LDA  #0                 ; Configure MEGA65 memory
        TAX
        TAY
        TAZ
        MAP
        EOM

        LDA  #$36               ; I/O & kernal
        STA  R6510
        LDA  #65                ; 40 MHz
        STA  0

        JSR  Init_IO
        JSR  Set_Kernal_Vectors
        LDA  #$04               ; C64 default value
        STA  SCNMPG             ; set screen memory page
        JSR  Init_Editor
        LDA  #-1                ; cursor off
        STA  BLNSW
        CLI
        LDX  #8
        STX  FA
        STX  Game_Unit
        STX  Save_Unit          ; default SAVE unit = GAME unit

        JSR  Set_Mode_80
        JSR  Load_Charset
        JSR  Screen_Setup
        JSR  Set_Attic_RAM
        LDA  BG_Color
        STA  BackgCol0
        LDA  BO_Color
        STA  BorderCol
        JSR  Clear_Screen
        JSR  Load_Config
        JSR  SETMSG             ; disable kernal messages
        JMP  z_restart
EndMod

******************
Module Set_Mode_80
******************

*       make VIC IV registers visible
*       by using the knock sequence $47 $53

        LDA  #$47
        STA  $d02f
        LDA  #$53
        STA  $d02f

*       test, whether bank 3 is write protected

        LDZ  #0                 ; address = $030000
        LDA  #3
        STZ  X7L
        STZ  X7H
        STA  X8L
        STZ  X8H
        LDA  [X7L],Z
        EOR  #$ff
        STA  [X7L],Z
        CMP  [X7L],Z
        BEQ  BATE_20            ; -> is writable

*       remove write protection from bank 2 & 3
*       bank 3 will be used for data storage

        LDA  #$70
        STA  $D640              ; toggle write protection
        NOP

BATE_20
        LDA  #$47
        STA  $d02f
        LDA  #$53
        STA  $d02f
        LDA  #$e0               ; H640, DAFAST, ATTR
        STA  $d031
        LDA  #$24               ; SCR = $0800,  CHB = $0000
        STA  $d018
        LDA  #65
        STA  0
        RTS
EndMod

*************************
Module Set_Kernal_Vectors
*************************

        LDY  #$1F               ; 16 vectors
_loop   LDA  ROM_Vectors,Y
        STA  CINV,Y
        DEY
        BPL  _loop
        RTS
EndMod

********************
Module Set_Attic_RAM
********************
        LDX  #$08               ; RAMD = $0800 0000
        LDZ  #0                 ; aka Attic RAM
        STZ  RAMD
        STZ  RAMD+1
        STZ  RAMD+2
        STX  RAMD+3
        LDA  [RAMD],Z
        EOR  #$ff
        STA  [RAMD],Z
        CMP  [RAMD],Z
        BEQ  _attic
        STZ  Segment            ; use Chip  RAM
        RTS
_attic  STX  Segment            ; use Attic RAM
        Print(Msg_Attic)
        RTS
EndMod

****************
Module Header_X1
****************

        CLC
        LDA  X1H
        ADC  #>Z_HEADER
        STA  X1H
        RTS
EndMod

**************
Module PC_LOOP
**************

* read next instruction byte from Z-program counter
* read optional operand bytes and call operator subroutine

        LDA  #0
        STA  ParNum             ; reset # of operands
        LDA  Z_Code
        STA  Z_Code+1           ; for debugging
        JSR  Next_Instruction
        STA  Z_Code             ; remember op code
        BIT  Log
        BPL  _nolog
;       JSR  Log_A
_nolog  LDA  Z_Code
        BPL  z_op_two           ; [$00 - $7f] -> codes with  2 operands
        CMP  #$b0
        BCC  z_op_one           ; [$80 - $af] -> codes with  1 operand
        CMP  #$c0
        BCC  z_op_zero          ; [$b0 - $bf] -> codes with no operand

        JSR  Next_Instruction   ; [$c0 - $ff] -> codes with 0-4 operands

; max 4 operands (11223344) for version 3
;  or 8 operands for version > 3
; ---------------------------------------
; 00 = 16 bit constant
; 01 =  8 bit constant
; 10 = 16 bit variable
; 11 = no operand (end marker)

        LDX  Z_Code
        CPX  #$ec               ; call with 8 arguments
        BEQ  _var_10
        CPX  #$fa               ; call with 8 arguments
        BNE  _var_20
_var_10 JSR Load_8_Operands
        BRA  _var_30
_var_20 JSR Load_4_Operands
_var_30 LDA Z_Code
        CMP  #$e0
        BCC  _twop           ; [$c0 - $df] -> use 2 operand table
        AND  #$1f               ; [$e0 - $ff] -> use variable operands
        CLC
        ADC  #[opcodes_var - z_opcode] >> 1
        BNE  z_execute          ; always

*********
z_op_zero
*********

; opcodes with no operand, opcode = $b0 - $bf

        SBC  #$af - [[opcodes_0op - z_opcode] >> 1] ; carry is clear
        BNE  z_execute          ; always

********
z_op_one
********

; opcode = 10tt cccc  opcodes with 1 operand
;     tt = 00 : 16 bit constant $8x
;     tt = 01 :  8 bit constant $9x
;     tt = 10 :    variable     $ax

        ASL  A
        ASL  A
        JSR  Get_Operand
        LDA  Z_Code
        AND  #15
        CLC
        ADC  #[opcodes_1op - z_opcode] >> 1
        BNE  z_execute

********
z_op_two
********

; opcode = 0fsc cccc  opcodes with 2 operands
;      f = 0 : 1st. op = short constant
;      f = 1 : 1st. op = variable
;      s = 0 : 2nd. op = short constant
;      s = 1 : 2nd. op = variable

        CLC
        AND  #$40               ; $40 if 1st. op variable
        ADC  #$40               ; $80 if 1st. op variable
        JSR  Get_Operand        ; $80 variable, $40 8 bit constant
        LDA  Z_Code
        ASL  A                  ; C=0
        AND  #$40
        ADC  #$40
        JSR  Get_Operand
        LDA  Z_Code
_twop   AND  #$1f               ; fall through

*********
z_execute
*********

; Input : A = index to opcode table

        ASL  A                  ; convert to word index
        TAX
        JSR  (z_opcode,X)
        BRA  PC_LOOP
EndMod

z_error_4 ERROR(4)

********
z_opcode
********

; $00-$1f : byte const + short const
; $20-$3f : byte const + variable
; $40-$5f : variable   + byte const
; $60-$7f : variable   + variable
; $c0-$df : 0-4 arguments defined in follow up byte

        .WORD z_error_4         ; 00 20 40 60   c0
        .WORD z_je              ; 01 21 41 61   c1
        .WORD z_jl              ; 02 22 42 62   c2
        .WORD z_jg              ; 03 23 43 63   c3
        .WORD z_dec_chk         ; 04 24 44 64   c4
        .WORD z_inc_chk         ; 05 25 45 65   c5
        .WORD z_jin             ; 06 26 46 66   c6
        .WORD z_test            ; 07 27 47 67   c7
        .WORD z_or              ; 08 28 48 68   c8
        .WORD z_and             ; 09 29 49 69   c9
        .WORD z_test_attr       ; 0a 2a 4a 6a   ca
        .WORD z_set_attr        ; 0b 2b 4b 6b   cb
        .WORD z_clear_attr      ; 0c 2c 4c 6c   cc
        .WORD z_store           ; 0d 2d 4d 6d   cd
        .WORD z_insert_obj      ; 0e 2e 4e 6e   ce
        .WORD z_loadw           ; 0f 2f 4f 6f   cf
        .WORD z_loadb           ; 10 30 50 70   d0
        .WORD z_get_prop        ; 11 31 51 71   d1
        .WORD z_get_prop_addr   ; 12 32 52 72   d2
        .WORD z_get_next_prop   ; 13 33 53 73   d3
        .WORD z_add             ; 14 34 54 74   d4
        .WORD z_sub             ; 15 35 55 75   d5
        .WORD z_mul             ; 16 36 56 76   d6
        .WORD z_div             ; 17 37 57 77   d7
        .WORD z_mod             ; 18 38 58 78   d8
        .WORD z_call_s          ; 19 39 59 79   d9
        .WORD z_call_n          ; 1a 3a 5a 7a   da
        .WORD z_set_colour      ; 1b 3b 5b 7b   db
        .WORD z_error_4         ; 1c 3c 5c 7c   dc
        .WORD z_error_4         ; 1d 3d 5d 7d   dd
        .WORD z_error_4         ; 1e 3e 5e 7e   de
        .WORD z_error_4         ; 1f 3f 5f 7f   df

***********
opcodes_1op
***********

; $80-$8f : word constant
; $90-$9f : byte constant
; $a0-$af : variable

        .WORD z_jz              ; 80 90 a0
        .WORD z_get_sibling     ; 81 91 a1
        .WORD z_get_child       ; 82 92 a2
        .WORD z_get_parent      ; 83 93 a3
        .WORD z_get_prop_len    ; 84 94 a4
        .WORD z_inc             ; 85 95 a5
        .WORD z_dec             ; 86 96 a6
        .WORD z_print_addr      ; 87 97 a7
        .WORD z_call_s          ; 88 98 a8
        .WORD z_remove_obj      ; 89 99 a9
        .WORD z_print_obj       ; 8a 9a aa
        .WORD z_ret             ; 8b 9b ab
        .WORD z_jump            ; 8c 9c ac
        .WORD z_print_paddr     ; 8d 9d ad
        .WORD z_load            ; 8e 9e ae
ZV8F    .WORD z_call_n          ; 8f 9f af  V3/4 = z_not

***********
opcodes_0op
***********

        .WORD z_rtrue           ; b0
        .WORD z_rfalse          ; b1
        .WORD z_print           ; b2
        .WORD z_print_ret       ; b3
        .WORD z_nop             ; b4
        .WORD z_save            ; b5
        .WORD z_restore         ; b6
        .WORD z_restart         ; b7
        .WORD z_ret_popped      ; b8
ZVB9    .WORD z_catch           ; b9 V3 = z_pop
        .WORD z_quit            ; ba
        .WORD z_new_line        ; bb
        .WORD z_show_status     ; bc
        .WORD Main_True         ; bd z_verify
        .WORD z_extension       ; be
        .WORD Main_True         ; bf z_piracy

***********
opcodes_var
***********

        .WORD z_call_s          ; e0 V3 call with 0-3 args
        .WORD z_storew          ; e1
        .WORD z_storeb          ; e2
        .WORD z_put_prop        ; e3
        .WORD z_read            ; e4
        .WORD z_print_char      ; e5
        .WORD z_print_num       ; e6
        .WORD z_random          ; e7
        .WORD z_push            ; e8
        .WORD z_pull            ; e9
        .WORD z_split_window    ; ea
        .WORD z_set_window      ; eb
        .WORD z_call_s          ; ec z_call_s 0-7 args
        .WORD z_erase_window    ; ed
        .WORD z_erase_line      ; ee
        .WORD z_set_cursor      ; ef
        .WORD z_get_cursor      ; f0
        .WORD z_set_text_style  ; f1
        .WORD z_buffer_mode     ; f2
        .WORD z_output_stream   ; f3
        .WORD z_error_1         ; f4 z_input_stream
        .WORD z_sound_effect    ; f5
        .WORD z_read_char       ; f6
        .WORD z_scan_table      ; f7
        .WORD z_not             ; f8
        .WORD z_call_n          ; f9 z_call_n 0-3 args
        .WORD z_call_n          ; fa z_call_n 0-7 args
        .WORD z_tokenise        ; fb
        .WORD z_encode_text     ; fc
        .WORD z_copy_table      ; fd
        .WORD z_print_table     ; fe
        .WORD z_check_argc      ; ff

z_error_1 ERROR(1)

***********
opcodes_ext
***********

        .WORD z_ext_save        ; 00
        .WORD z_ext_restore     ; 01
        .WORD z_log_shift       ; 02
        .WORD z_art_shift       ; 03
        .WORD z_set_font        ; 04
        .WORD z_error12         ; 05 z_draw_picture,
        .WORD z_error12         ; 06 z_picture_data,
        .WORD z_error12         ; 07 z_erase_picture,
        .WORD z_error12         ; 08 z_set_margins,
        .WORD z_save_undo       ; 09
        .WORD z_restore_undo    ; 0a

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


******************
Module Get_Operand
******************

; Input: (A) = tt00 0000
; ----------------------
; tt = 00 : 16 bit constant
; tt = 01 :  8 bit constant
; tt = 10 :    variable
; tt = 11 :    none

; Output: Store 1st. in X1, 2nd. in X2, etc. until X8
;         Overflow set if none

        STA  OP_Type
        LDA  #0                 ; preset high byte
        BIT  OP_Type
        BMI  _check             ; 1x -> var or end
        BVS  _const             ; 01 -> byte constant
        JSR  Next_Instruction   ; get constant high
_const  STA  X0H
        JSR  Next_Instruction   ; get constant low
        STA  X0L
        BRA  _store
_check  BVS  _return            ; 11 -> end
        JSR  Get_Var_A          ; 10 -> variable
_store  INC  ParNum             ; parameter count
        LDA  ParNum
        ASL  A
        TAX
        LDA  X0L
        STA  X0L,X
        LDA  X0H
        STA  X0H,X
        CLV                     ; not at end
_return RTS
EndMod

**********************
Module Load_4_Operands
**********************

; max 4 operands (2 type bits)
; ----------------------------
; 00 = 16 bit constant
; 01 =  8 bit constant
; 10 = 16 bit variable
; 11 = no operand (end marker)

        JSR  Get_Operand
        BVS  _return            ; V=1 -> end of args
        LDA  OP_Type
        ASL  A
        ASL  A                  ; shift next 2 type bits to 7 and 6
        LDX  ParNum
        CPX  #4
        BCC  Load_4_Operands
_return LDA  Z_Code
        RTS
EndMod

**********************
Module Load_8_Operands
**********************

; max 8 operands (2 type bits)
; ----------------------------
; 00 = 16 bit constant
; 01 =  8 bit constant
; 10 = 16 bit variable
; 11 = no operand (end marker)

        PHA                     ; 1st. type byte
        JSR  Next_Instruction   ; 2nd. type byte
        STA  OP_Type+1
        PLA
_loop   JSR  Get_Operand
        BVS  _return            ; V=1 -> end of args
        LDA  OP_Type
        ASL  OP_Type+1
        ROL  A
        ASL  OP_Type+1
        ROL  A
        LDX  ParNum
        CPX  #8
        BCC  _loop
_return LDA  Z_Code
        RTS
EndMod

******************
Module Get_Var_X1L
******************

        LDA  X1L
        BNE  GeVa_10
        JSR  z_pop
        BRA  Push_X0

*********
Get_Var_A
*********

        JSR  Next_Instruction
        LBEQ z_pop

GeVa_10 CMP  #16
        BCS  Get_Global_Var
        TAX
        LDA  Lvar_Hi,X
        STA  X0H
        LDA  Lvar_Lo,X
        STA  X0L
        RTS
EndMod

*********************
Module Get_Global_Var
*********************

        JSR  Get_Global_Var_Addr
        LDA  (A0L),Y
        STA  X0H
        INY
        LDA  (A0L),Y
        STA  X0L
        RTS
EndMod

**************
Module Put_Var
**************

        LDA  X1L                ; X1L == 0 : push  X0
        BNE  Store_Var          ; X1L != 0 : store X0
        DEW  z_stack_ptr        ; X0 replaces top of stack
        DEW  z_stack_ptr
EndMod

**************
Module Push_X0
**************

        LDA  X0L
        LDY  X0H
EndMod

**************
Module Push_AY
**************

; Input : A = low  byte
;         Y = high byte
; X register is preserved

        LDZ  #0
        STA  (z_stack_ptr),Z
        INW  z_stack_ptr
        TYA
        STA  (z_stack_ptr),Z
        INW  z_stack_ptr
        LDZ  z_stack_ptr+1
        CPZ  #>Z_STACK_HI
        BCS  z_error_6
        RTS
z_error_6 ERROR(6)
EndMod


**********
Store_Zero
**********

        LDA  #0

**********
Store_Byte
**********

        LDX  #0

********
Store_AX
********

        STX  X0H

***********
Store_A_X0H
***********

        STA  X0L

********
Store_X0
********

        JSR  Next_Instruction
        BEQ  Push_X0            ;    0: push   value

*********
Store_Var
*********

        CMP  #16                ; 1-15: local  variable
        BCS  Put_Global_Var     ; > 15: global variable
        TAX
        LDA  X0L
        STA  Lvar_Lo,X
        LDA  X0H
        STA  Lvar_Hi,X
        RTS

*********************
Module Put_Global_Var
*********************

        JSR  Get_Global_Var_Addr
        LDA  X0H                ; store in big endian order
        STA  (A0L),Y
        INY
        LDA  X0L
        STA  (A0L),Y
        RTS
EndMod

**************************
Module Get_Global_Var_Addr
**************************

;  Input: (A)  = Variable # ( 16 - 255)
; Output: (A0) = Address of Variable
;         (Y)  = 0  IMPORTANT: used by callers

; A0 = Z_HEADER + h_globals + 2 * (A)

        SEC
        SBC  #16                ; variable index = number - 16
        LDY  #0
        STY  A0H
        ASL  A
        ROL  A0H                ; (A/A0H) = index * 2 (C=0)
        ADC  h_globals_lo
        STA  A0L
        LDA  A0H                ; 0 or 1
        ADC  h_globals_hi
        ADC  #>Z_HEADER
        STA  A0H
        RTS
EndMod

; Take a jump after an instruction based on the flag, either true or
; false. The branch can be short or long; it is encoded in one or two
; bytes respectively. When bit 7 of the first byte is set, the jump
; takes place if the flag is true; otherwise it is taken if the flag
; is false. When bit 6 of the first byte is set, the branch is short;
; otherwise it is long. The offset occupies the bottom 6 bits of the
; first byte plus all the bits in the second byte for long branches.
; Uniquely, an offset of 0 means return false, and an offset of 1 is
; return true. The branch distance is offset - 2.

*****************
Module Main_False
*****************

        JSR  Next_Instruction
        BPL  _mft_20
_mft_10 AND  #$40
        BNE  _return
        JMP  Next_Instruction

*********
Main_True
*********

        JSR  Next_Instruction   ; A = specifier
        BPL  _mft_10
_mft_20 TAX                     ; X = specifier
        AND  #$40               ; bit 6: 1=short 0=long
        BEQ  _mft_30            ; ---------------------
        TXA                     ; short forward branch
        AND  #$3f               ; A = offset (6 bits)
        LDX  #0                 ; X = 0 (high offset)
        BEQ  _mft_60            ; ---------------------
_mft_30 TXA                     ; long branch
        AND  #$20               ; sign bit of offset
        BEQ  _mft_40            ; -> positive offset
        TXA
        ORA  #$c0               ; negative offset
        BNE  _mft_50            ; -> always
_mft_40 TXA
        AND  #$3f               ; positive offset
_mft_50 STA  X0H                ; high byte offset (6 bits)
        JSR  Next_Instruction   ; A = low  byte of long offset
        LDX  X0H                ; X = high byte of long offset
        BNE  Branch_XA          ; -> long branch
_mft_60 TAY                     ; Y = offset
        BEQ  z_rfalse           ; Offset 0: return false
        DEY                     ; Y = offset - 1
        BEQ  z_rtrue            ; Offset 1: return true

*********
Branch_XA
*********

; branch to IP + (A/X) - 2
; Input : (A/X) = signed word with 14 significant bits

        STX  X0H                ; offset high
        SEC
        SBC  #2
        BCS  _mft_70
        DEX
_mft_70 CLC                     ; (A/X) = offset - 2
        ADC  QI0
        STA  QI0
        TXA
        ADC  QI1
        STA  QI1
        LDA  #0                 ; A = 0
        BIT  X0H                ; check sign of offset
        BPL  _mft_80            ; -> positive
        LDA  #-1                ; A = -1
_mft_80 ADC  QI2                ; add carry and sign
        STA  QI2
_return RTS
EndMod

****************************
Module z_rtrue ; opcode # b0
****************************

        LDX  #1
        STX  X1L
        DEX
        STX  X1H
        JMP  z_ret
EndMod

*****************************
Module z_rfalse ; opcode # b1
*****************************

        LDX  #0
        STX  X1L
        STX  X1H
        JMP  z_ret
EndMod

****************************
Module z_print ; opcode # b2
****************************

; print text from instruction pointer (QI).

        LDX  #2
_id     LDA  QI0,X              ; copy QI -> QD
        STA  QD0,X
        DEX
        BPL  _id
        JSR  Decode_Text        ; print text from (QD)
        LDX  #2
_di     LDA  QD0,X              ; copy QD -> QI
        STA  QI0,X
        DEX
        BPL  _di
        RTS
EndMod

********************************
Module z_print_ret ; opcode # b3
********************************

; print text from instruction pointer (QI) add a new line and return true.

        JSR  z_print
        JSR  z_new_line
        BRA  z_rtrue
EndMod

**************************
Module z_nop ; opcode # b4
**************************

        RTS
EndMod

*********************************
Module z_ret_popped ; opcode # b8
*********************************

        JSR  z_pop
        STA  X1L
        STY  X1H
        JMP  z_ret
EndMod

**************************
Module z_pop ; opcode # b9
**************************

; pop 16 bit word from Z stack
; Output: (X0) = (A/Y)  = value
; X register preserved

        LDZ  #0
        DEW  z_stack_ptr
        LDA  (z_stack_ptr),Z
        TAY
        DEW  z_stack_ptr
        LDA  (z_stack_ptr),Z
        STY  X0H
        STA  X0L
        LDZ  z_stack_ptr+1
        CPZ  #>Z_STACK_LO
        BCC  z_error_5
        RTS
z_error_5 ERROR(5)
EndMod

****************************
Module z_catch ; opcode # b9
****************************
        ERROR(7)
EndMod

***************************
Module z_quit ; opcode # ba
***************************

        JSR  Save_Config
quit_20 Print(EOS)
quit_30 BRA  quit_30
EndMod




*******************************
Module z_jz ; opcode # 80 90 a0
*******************************

; jump if (X1 == 0)

        LDA  X1L
        ORA  X1H
        LBEQ Main_True
        JMP  Main_False
EndMod

****************************************
Module z_get_sibling ; opcode # 81 91 a1
****************************************

        LDY  #5                 ; version = 3 sibling
        BBR7 Version,_get
        LDY  #8                 ; version > 3 sibling
        BRA  _get

*******************************
z_get_child ; opcode # 82 92 a2
*******************************

        LDY  #6                 ; version = 3 child
        BBR7 Version,_get
        LDY  #10                ; version > 3 child

_get    JSR Get_Object_X1       ; X1 object's address -> A0
        JSR  Store_AX           ; (A/X) -> (X0) -> Store
        LDA  X0L
        ORA  X0H
        LBEQ Main_False         ; there is no object
        JMP  Main_True          ; there is an object
EndMod

***************************************
Module z_get_parent ; opcode # 83 93 a3
***************************************

        LDY  #4                 ; version = 3 parent
        BBR7 Version,_get
        LDY  #6                 ; version > 3 parent
_get    JSR  Get_Object_X1      ; (A/X) = parent object
        JMP  Store_AX
EndMod

*****************************************
Module z_get_prop_len ; opcode # 84 94 a4
*****************************************

; get length of property, which address is in X1
; The length info is stored one byte before (X1)

        CLC
        LDA  X1L
        ADC  #<[Z_HEADER-1]
        STA  A0L
        LDA  X1H
        ADC  #>[Z_HEADER-1]
        STA  A0H                ; (A0) = (X1) + Header - 1
        LDY  #0
        BBR7 Version,_V3
        LDA  (A0L),Y
        BMI  _mask              ; -> length = lower 6 bits
        ASL  A                  ; bit 7 = length info
        ASL  A                  ; carry = length info
        TYA                     ; A = 0
        ADC  #1                 ; A = 1 or 2
_mask   AND  #$3f               ; version > 3 mask
        JMP  Store_Byte

_V3     JSR  Property_Size
        INC  A
        JMP  Store_Byte
EndMod

********************************
Module z_inc ; opcode # 85 95 a5
********************************

        JSR  Get_Var_X1L
        INW  X0L
        JMP  Put_Var
EndMod

********************************
Module z_dec ; opcode # 86 96 a6
********************************

        JSR  Get_Var_X1L
        DEW  X0L
        JMP  Put_Var
EndMod

***************************************
Module z_print_addr ; opcode # 87 97 a7
***************************************

        LDY  X1L
        LDX  X1H
        JMP  Decode_YX
EndMod

***************************************
Module z_remove_obj ; opcode # 89 99 a9
***************************************

; Remove (unlink) object (X1)

        BBR7 Version, _V3
        LDY  #6                 ; parent offset
        JSR  Get_Object_X1      ; object's address -> A0
        LDA  A0L                ; object's address -> A1
        STA  A1L
        LDA  A0H
        STA  A1H
        TXA                     ; parent high
        ORA  (A0L),Y            ; parent low
        BEQ  _ret40             ; return if no parent

; Get parent's 1st. child

        LDA  (A0L),Y            ; parent low (X = high)
        LDY  #10                ; child offset
        JSR  Get_Object_Reg     ; parent's address -> A0
        CMP  X1L                ; object == parent's 1st. Child ?
        BNE  _ro_10             ; -> no
        CPX  X1H                ; object == parent's 1st. Child ?
        BNE  _ro_10             ; -> no

; Parent's 1st. child is this object, so
; make object's sibling the 1st. child of parent

        LDY  #8                 ; sibling offset
        LDA  (A1L),Y            ; object's sibling high
        INY
        INY                     ; Y = 10
        STA  (A0L),Y            ; parent's child high
        DEY                     ; Y =  9
        LDA  (A1L),Y            ; object's sibling low
        INY
        INY                     ; Y = 11
        STA  (A0L),Y            ; parent's child low
        BNE  _ro_20             ; always (INY)

; Parent's 1st. child is not this object
; Loop through siblings until found

_ro_10  LDY  #8                 ; sibling offset
        JSR  Get_Object_Reg     ; parent's child -> A0
        CMP  X1L                ; object == parent's child's sibling ?
        BNE  _ro_10             ; -> no
        CPX  X1H                ; object == parent's child's sibling ?
        BNE  _ro_10             ; -> no, try next sibling (A/X)

; Link younger sibling to older sibling

        LDA  (A1L),Y            ; younger sibling low
        STA  (A0L),Y            ; older   sibling low
        DEY                     ; Y = 8
        LDA  (A1L),Y            ; younger sibling high
        STA  (A0L),Y            ; older   sibling high

; clear object's parent & sibling (Y=6 .. 9)

_ro_20  LDA  #0
        LDY  #6                 ; parent offset
_ro_30  STA  (A1L),Y            ; clear parent & sibling
        INY
        CPY  #10                ; after sibling
        BCC  _ro_30
_ret40  RTS

_V3     LDY  #4                 ; parent offset
        JSR  Get_Object_X1
        LDA  A0L
        STA  A1L
        LDA  A0H
        STA  A1H
        LDA  (A0L),Y            ; A = parent object
        BEQ  _ret70             ; -> has no parent
        LDY  #6                 ; child offset
        JSR  Get_Object_Reg     ; A = child of parent
        CMP  X1L                ; is it me ?
        BNE  _ro_50             ; -> no
        LDY  #5                 ; sibling offset
        LDA  (A1L),Y            ; my sibling
        INY                     ; Y = child offset
        STA  (A0L),Y            ; is parent's cild
        BNE  _ro_60             ; always

_ro_50  LDY  #5                 ; A = sibling of parnent's
        JSR  Get_Object_Reg     ; child
        CMP  X1L                ; me ?
        BNE  _ro_50             ; -> no
        LDY  #5                 ; sibling offset
        LDA  (A1L),Y            ; my sibling is
        STA  (A0L),Y            ; parent's child sibling

_ro_60  LDA  #0
        LDY  #4                 ; parent offset
        STA  (A1L),Y            ; I have no parnet
        INY                     ; Y = sibling offset
        STA  (A1L),Y            ; I have no sibling
_ret70  RTS
EndMod

**************************************
Module z_print_obj ; opcode # 8a 9a aa
**************************************

        LDA  X1L
        LDX  X1H

*************
z_print_obj_A
*************

        LDY  #12                ; version > 3 offset
        BBS7 Version,_prio_1
        LDY  #7                 ; version = 3 offset
_prio_1 JSR  Get_Object_Reg
        BBS7 Version,_V4
        TAX                     ; object prop high
        INY
        LDA  (A0L),Y
_V4     TAY                     ; object prop low
        INY
        BNE  _decode
        INX                     ; object prop high
_decode JMP  Decode_YX
EndMod

********************************
Module z_ret ; opcode # 8b 9b ab
********************************

        LDA  z_frame_ptr        ; stack ptr = frame ptr
        STA  z_stack_ptr
        LDA  z_frame_ptr+1      ; stack ptr = frame ptr
        STA  z_stack_ptr+1

        JSR  z_pop
        STY  Z_Arg_Count        ; # of args
        TAX                     ; # of local vars
        BEQ  _noloc             ; -> no locals vars

_loop   JSR  z_pop              ; pop next local var
        STA  Lvar_Lo,X          ; and restore it
        TYA
        STA  Lvar_Hi,X
        DEX
        BNE  _loop              ; -> loop

_noloc  JSR  z_pop
        STA  Call_Type
        STY  QI0                ; restore instruction pointer L

        JSR  z_pop
        STA  QI1                ; restore instruction pointer M
        STY  QI2                ; restore instruction pointer H

        JSR  z_pop
        STA  z_frame_ptr
        STY  z_frame_ptr+1

        LDA  Call_Type
        BMI  _irqret            ; -> return from IRQ
        BNE  _return            ; -> no return value
        LDA  X1L
        LDX  X1H
        JMP  Store_AX
_irqret PLA
        PLA
_return RTS
EndMod

*********************************
Module z_jump ; opcode # 8c 9c ac
*********************************

        LDA  X1L
        LDX  X1H
        JMP  Branch_XA
EndMod

****************************************
Module z_print_paddr ; opcode # 8d 9d ad
****************************************

        LDY  X1L
        LDA  X1H
        JSR  Set_Data_YA
        ASL  QD0
        ROW  QD1
        BBR7 Version,_label
        ASL  QD0
        ROW  QD1
_label  JMP  Decode_Text
EndMod

*********************************
Module z_load ; opcode # 8e 9e ae
*********************************

        JSR  Get_Var_X1L
        JMP  Store_X0
EndMod

*********************************
Module z_not ; opcode #  8f 9f af
*********************************
        LDA  X1L
        EOR  #$ff
        TAX
        LDA  X1H
        EOR  #$ff               ; fall through
EndMod

***************
Module Store_XA
***************

        STX  X0L
        STA  X0H
        JMP  Store_X0
EndMod

*************************************
Module z_je ; opcode # 01 21 41 61 c1
*************************************

; jump if (X1 == X2 || X1 == X3 || X1 == X4)

        LDA  ParNum
        ASL  A
        TAX
_loop   CPX  #4
        LBCC Main_False
        DEX
        DEX
        LDA  X1L
        CMP  X1L,X
        BNE  _loop
        LDA  X1H
        CMP  X1H,X
        BNE  _loop
        JMP  Main_True
EndMod

*************************************
Module z_jl ; opcode # 02 22 42 62 c2
*************************************

; jump if (X1 < X2) signed 16 bit

        LDA  X1L
        CMP  X2L
        LDA  X1H
        SBC  X2H
        BVC  _sig
        EOR  #$80
_sig    LBMI Main_True
        JMP  Main_False
EndMod

*************************************
Module z_jg ; opcode # 03 23 43 63 c3
*************************************

; jump if (X1 > X2) signed 16 bit

        LDA  X2L
        CMP  X1L
        LDA  X2H
        SBC  X1H
        BVC  _sig
        EOR  #$80
_sig    LBMI Main_True
        JMP  Main_False
EndMod

******************************************
Module z_dec_chk ; opcode # 04 24 44 64 c4
******************************************

; jump if ((X0 = --Var[X1]) < X2)

        JSR  z_dec
        LDA  X0L
        CMP  X2L
        LDA  X0H
        SBC  X2H
        BVC  _sig
        EOR  #$80
_sig    LBMI Main_True
        JMP  Main_False
EndMod

******************************************
Module z_inc_chk ; opcode # 05 25 45 65 c5
******************************************

; jump if ((X0 = ++Var[X1]) > X2)

        JSR  z_inc
        LDA  X2L
        CMP  X0L
        LDA  X2H
        SBC  X0H
        BVC  _sig
        EOR  #$80
_sig    LBMI Main_True
        JMP  Main_False
EndMod

**************************************
Module z_jin ; opcode # 06 26 46 66 c6
**************************************

; jump if (X1 is child of X2)

        LDY  #4                 ; version = 3 parent
        BBR7 Version,_get
        LDY  #6                 ; version > 3 parent
_get    JSR  Get_Object_X1
        CPX  X2H
        BNE  _false
        CMP  X2L
        LBEQ Main_True
_false  JMP  Main_False
EndMod

***************************************
Module z_test ; opcode # 07 27 47 67 c7
***************************************

; jump if ((X1 & X2) == X2)

        LDX  #1
_loop   LDA  X1L,X
        AND  X2L,X
        CMP  X2L,X
        LBNE Main_False
        DEX
        BPL  _loop
        JMP  Main_True
EndMod

*************************************
Module z_or ; opcode # 08 28 48 68 c8
*************************************

; (X1 | X2)

        LDA  X1H
        ORA  X2H
        TAX
        LDA  X1L
        ORA  X2L
        JMP  Store_AX
EndMod

**************************************
Module z_and ; opcode # 09 29 49 69 c9
**************************************

; (X1 & X2)

        LDA  X1H
        AND  X2H
        TAX
        LDA  X1L
        AND  X2L
        JMP  Store_AX
EndMod

********************************************
Module z_test_attr ; opcode # 0a 2a 4a 6a ca
********************************************

; jump if attribute X2 of object X1 is set

        JSR  Get_Attr_Addr
        AND  (A0L),Y
        LBNE Main_True
        JMP  Main_False
EndMod

*******************************************
Module z_set_attr ; opcode # 0b 2b 4b 6b cb
*******************************************

; set attribute X2 of object X1

        JSR  Get_Attr_Addr
        ORA  (A0L),Y
        STA  (A0L),Y
        RTS
EndMod

*********************************************
Module z_clear_attr ; opcode # 0c 2c 4c 6c cc
*********************************************

; clear attribute X2 of object X1

        JSR  Get_Attr_Addr
        EOR  #$ff
        AND  (A0L),Y
        STA  (A0L),Y
        RTS
EndMod

****************************************
Module z_store ; opcode # 0d 2d 4d 6d cd
****************************************

; Var[X1] = X2

        LDA  X2L
        STA  X0L
        LDA  X2H
        STA  X0H
        JMP  Put_Var
EndMod

*********************************************
Module z_insert_obj ; opcode # 0e 2e 4e 6e ce
*********************************************

; insert object (X1) as 1st. child of object (X2)

        JSR  z_remove_obj       ; unlink object (X1)
        BBR7 Version,_V3
        LDY  #6                 ; parent offset
        LDA  X2H
        STA  (A1L),Y            ; X1's parent = X2 high
        TAX
        INY                     ; Y = 7
        LDA  X2L
        STA  (A1L),Y            ; X1's parent = X2 low

        LDY  #10                ; child offset
        JSR  Get_Object_Reg     ; X2 object's address -> A0
        STA  LV0                ; old child of X2 low
        LDA  X1L                ; Y = 11
        STA  (A0L),Y            ; new child low  = X1L
        DEY                     ; Y = 10
        LDA  X1H
        STA  (A0L),Y            ; new child high = X1H

        TXA                     ; X2's old child high
        ORA  LV0                ; X2's old child low
        BEQ  _return            ; -> old child was zero

        DEY                     ; Y =  9
        LDA  LV0                ; X2's old child   low
        STA  (A1L),Y            ; X1's new sibling low
        DEY                     ; Y =  8
        TXA                     ; X2's old child   high
        STA  (A1L),Y            ; X1's new sibling high
        RTS

_V3     LDA  X2L
        LDY  #4                 ; parent offset
        STA  (A1L),Y            ; object's new parent
        LDY  #6                 ; child offset
        JSR  Get_Object_Reg     ; get new parent's child
        TAX                     ; X = parent's first child
        LDA  X1L
        STA  (A0L),Y            ; object becomes parent's first child
        TXA
        BEQ  _return            ; parent had no child before
        LDY  #5                 ; sibling offset
        STA  (A1L),Y            ; old child becommes sibling
_return RTS
EndMod

*****************
Module Word_Array
*****************

; called from z_loadw
; address QD = base (X1) + index (X2 * 2)

        ASW  X2L                ; fall through
EndMod

*****************
Module Byte_Array
*****************

; called from z_loadb
; address QD = base (X1) + index (X2)

        CLC
        LDA  X1L
        ADC  X2L
        STA  QD0
        LDA  X1H
        ADC  X2H
        STA  QD1
        LDA  #0
        ROL  A                  ; add carry for address > 64K
        STA  QD2
        JMP  Next_Datum         ; get next byte
EndMod

****************************************
Module z_loadw ; opcode # 0f 2f 4f 6f cf
****************************************

; load word value from array X1[X2]

        JSR  Word_Array         ; set &X1[X2]
        STA  X0H                ; put high byte
        JSR  Next_Datum         ; get low  byte
        JMP  Store_A_X0H        ; return word
EndMod

****************************************
Module z_loadb ; opcode # 10 30 50 70 d0
****************************************

; load byte value from array X1[X2]

        JSR  Byte_Array         ; set &X1[X2]
        JMP  Store_Byte         ; return byte
EndMod

*******************************************
Module z_get_prop ; opcode # 11 31 51 71 d1
*******************************************

; get property X2 of object X1
; if X1 has no property X2 use default value
; the routine must access properties of size byte or word only

        JSR  Find_Property
        BEQ  _found             ; -> found property

        LDA  h_objects_lo       ; use default
        STA  A0L
        LDA  h_objects_hi
        ADC  #>Z_HEADER         ; carry is clear
        STA  A0H                ; A0 = address of default properties
        LDA  X2L                ; property #
        SBC  #0                 ; (C=0) : minus 1
        ASL  A                  ; default properties have word size
        TAY
        BRA  _read

_found  JSR  Property_Size      ; get size
        TAX                     ; 1: word
        BEQ  _low               ; 0: byte (X=0)

_read   LDA  (A0L),Y            ; property value high
        TAX
        INY
_low    LDA  (A0L),Y            ; property value low
        JMP  Store_AX
EndMod

************************************************
Module z_get_prop_addr ; opcode # 12 32 52 72 d2
************************************************

; get address of property X2 of object X1
; store address of property if found
; else store zero

        JSR  Find_Property
        LBNE Store_Zero
        LDA  A0L
        SBC  #<[Z_HEADER-2]     ; carry from Find_Property
        TAX                     ; set address after ID/size info
        LDA  A0H
        SBC  #>[Z_HEADER-2]
        JMP  Store_XA
EndMod

************************************************
Module z_get_next_prop ; opcode # 13 33 53 73 d3
************************************************

; for X2L == 0 find first property of object X1
; otherwise find next property after property X2L

        LDA  X2L
        BNE  _next              ; -> next prop after X2
        JSR  First_Property
        JMP  Store_Byte
_next   JSR  Find_Property
        BNE  _zero              ; -> prop X2 was not found
        JSR  Next_Property
        JMP  Store_Byte
_zero   JMP  Store_Zero
EndMod

**************************************
Module z_add ; opcode # 14 34 54 74 d4
**************************************

; (X1 + X2)

        CLC
        LDA  X1L
        ADC  X2L
        TAX
        LDA  X1H
        ADC  X2H
        JMP  Store_XA
EndMod

**************************************
Module z_sub ; opcode # 15 35 55 75 d5
**************************************

; (X1 - X2)

        SEC
        LDA  X1L
        SBC  X2L
        TAX
        LDA  X1H
        SBC  X2H
        JMP  Store_XA
EndMod

**************************************
Module z_mul ; opcode # 16 36 56 76 d6
**************************************

; (X1 * X2) signed 16 bit

        LDX  #16
        LDA  #0
        STA  X0L
        STA  X0H
        CLC
_loop   ROR  X0H
        ROR  X0L
        ROR  X2H
        ROR  X2L
        BCC  _zero
        CLC
        LDA  X1L
        ADC  X0L
        STA  X0L
        LDA  X1H
        ADC  X0H
        STA  X0H
_zero   DEX
        BPL  _loop
        LDX  X2L
        LDA  X2H
        JMP  Store_XA
EndMod

**********************
Module Divide_Unsigned
**********************

; Quotient : X1 = X1 / X2
; Remainder: X0 = X1 % X2

        LDA  X2L
        ORA  X2H
        BEQ  z_error_8          ; Divisor is zero
        LDX  #16
        LDA  #0
        STA  X0L
        STA  X0H
        CLC
_loop   ROL  X1L
        ROL  X1H
        ROL  X0L
        ROL  X0H
        LDA  X0L
        SEC
        SBC  X2L
        TAY
        LDA  X0H
        SBC  X2H
        BCC  _next
        STY  X0L
        STA  X0H
_next   DEX
        BNE  _loop
        ROL  X1L
        ROL  X1H
        RTS
z_error_8 ERROR(8)              ; Divide by zero
EndMod

********************
Module Divide_Signed
********************

; (X1) = (X1) / (X2)
; (X0) = (X1) % (X2)

        LDA  X1H
        PHA                     ; sign of remainder
        EOR  X2H
        PHA                     ; sign of quotient
        LDX  #X1L
        LDA  X1H
        JSR  Sign_ZPX           ; make dividend positive
        LDX  #X2L
        LDA  X2H
        JSR  Sign_ZPX           ; make divisor positive
        JSR  Divide_Unsigned
        LDX  #X1L
        PLA                     ; sign of quotient
        JSR  Sign_ZPX
        LDX  #X0L
        PLA                     ; sign of remainder
EndMod                          ; fall through

***************
Module Sign_ZPX
***************

; negate word at (0,X) if N flag is set

        BPL  _return
        SEC
        LDA  #0
        SBC  0,X
        STA  0,X
        LDA  #0
        SBC  1,X
        STA  1,X
_return RTS
EndMod

**************************************
Module z_div ; opcode # 17 37 57 77 d7
**************************************

        JSR  Divide_Signed      ; X1 = X1 / X2
        LDX  X1L
        LDA  X1H
        JMP  Store_XA
EndMod

**************************************
Module z_mod ; opcode # 18 38 58 78 d8
**************************************

        JSR  Divide_Signed      ; X0 = X1 % X2
        JMP  Store_X0
EndMod

*****************************************
Module z_call_s ; opcode # 19 39 59 79 d9
*****************************************

        LDA  #0
        BRA  call_00            ; always
EndMod

*****************************************
Module z_call_n ; opcode # 1a 3a 5a 7a da
*****************************************

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

        LDA  #1
call_00 STA  Call_Type
        LDA  X1L
        ORA  X1H
        BNE  call_20
        LDA  Call_Type
        BEQ  call_10
        RTS
call_10 JMP  Store_Byte

call_20 LDA  z_frame_ptr        ; push frame pointer
        LDY  z_frame_ptr+1
        JSR  Push_AY

        LDY  QI2                ; push instruction pointer H
        LDA  QI1                ; push instruction pointer M
        JSR  Push_AY

        LDY  QI0                ; push instruction pointer L
        LDA  Call_Type          ; push call type
        JSR  Push_AY

        LDA  X1L                ; compute call address
        ASL  A
        STA  QI0
        LDA  X1H
        ROL  A
        STA  QI1
        LDA  #0
        ROL  A
        STA  QI2                ; IP = (X1) * 2
        BBR7 Version,call_25
        ASL  QI0
        ROW  QI1                ; IP = (X1) * 4
call_25 JSR  Next_Instruction
        STA  A1H                ; number of local variables
        BEQ  call_50            ; no local variables
        LDX  #1

call_30 LDY  Lvar_Hi,X
        LDA  Lvar_Lo,X
        JSR  Push_AY            ; push local var of caller
;-----------------------------
        LDA  Version
        CMP  #$85               ; version 5
        BCC  call_32
        LDA  #0                 ; initialize with zero
        STA  Lvar_Hi,X
        STA  Lvar_Lo,X
        BRA  call_36
;-----------------------------
call_32 JSR  Next_Instruction   ; initialize with default value
        STA  Lvar_Hi,X
        JSR  Next_Instruction   ; from instruction stream
        STA  Lvar_Lo,X
;-----------------------------
call_36 CPX  ParNum             ; index > # of args ?
        BCS  call_40            ; -> stay with default value

        TXA
        ASL  A                  ; or replace with arg value
        TAY
        LDA  X1L,Y
        STA  Lvar_Lo,X
        LDA  X1H,Y
        STA  Lvar_Hi,X
call_40 CPX  A1H                ; # of vars to process
        INX
        BCC  call_30

call_50 LDY  Z_Arg_Count
        LDA  A1H                ; push # of local vars
        JSR  Push_AY
        LDY  ParNum
        DEY
        STY  Z_Arg_Count
        LDA  z_stack_ptr
        STA  z_frame_ptr        ; set new frame pointer
        LDA  z_stack_ptr+1
        STA  z_frame_ptr+1      ; set new frame pointer
        RTS
EndMod

*********************************************
Module z_set_colour ; opcode # 1b 3b 5b 7b db
*********************************************

; Input : X1 = foreground colour
;         X2 = background colour

        JSR  Print_Buffer
        LDX  X1L
        CPX  #13
        BCS  _ret
        LDA  _coltab,X
        STA  Colour
        LDX  X2L
        CPX  #13
        BCS  _ret
        LDA  _coltab,X
        STA  BorderCol
        STA  BackgCol0
_ret    RTS

_coltab .BYTE $00               ;  0 current
        .BYTE $00               ;  1 default
        .BYTE $00               ;  2 black
        .BYTE $02               ;  3 red
        .BYTE $05               ;  4 green
        .BYTE $07               ;  5 yellow
        .BYTE $06               ;  6 blue
        .BYTE $04               ;  7 magenta
        .BYTE $03               ;  8 cyan
        .BYTE $01               ;  9 white
        .BYTE $0f               ; 10 light  grey
        .BYTE $0c               ; 11 medium grey
        .BYTE $0b               ; 12 dark   grey
EndMod

***********************
Module z_log_shift ; 02
***********************

; Input : X1 = number
;         X2 = shifts

        LDA  X1L
        LDY  X2L
        BMI  _right
_left   ASL  A
        ROL  X1H
        DEY
        BNE  _left
        BEQ  _store

_right  LSR  X1H
        ROR  A
        INY
        BNE  _right

_store  LDX  X1H
        JMP  Store_AX
EndMod

***********************
Module z_art_shift ; 03
***********************

; Input : X1 = number
;         X2 = shifts

        LDA  X1L
        LDY  X2L
        BMI  _right
_left   ASL  A
        ROL  X1H
        DEY
        BNE  _left
        BEQ  _store

_right  LDA  X1H
        ASL  A
        ROR  X1H
        ROR  X1L
        INY
        BNE  _right
        LDA  X1L

_store  LDX  X1H
        JMP  Store_AX
EndMod

*****************
Module z_set_font
*****************

; 0 : previous font
; 1 : normal   font
; 2 : picture  font
; 3 : character graphics
; 4 : fixed pitch

        JSR  Print_Buffer
        LDX  Z_Active_Window    ; 0 = main, 1 = upper
        LDA  #0                 ; default font
        LDY  X1L                ; new font
        CPY  #3                 ; graphics ?
        BNE  _offset            ; -> no
        LDA  #$80               ; graphics offset
_offset STA  Font_Sel           ; graphics font
        LDA  Z_Active_Font,X    ; old font
        STY  Z_Active_Font,X    ; new font
        JMP  Store_Byte
EndMod

***********************
Module X1_PLUS_X2_TO_A0
***********************

        CLC
        LDA  X2L
        ADC  X1L
        STA  A0L
        LDA  X2H
        ADC  X1H
        ADC  #>Z_HEADER
        STA  A0H
        LDY  #0
        RTS
EndMod

********************
Module z_storew ; e1
********************

; X1[X2] = X3

        ASW  X2L
        JSR  X1_PLUS_X2_TO_A0
        LDA  X3H
        STA  (A0L),Y
        INY
        LDA  X3L
        STA  (A0L),Y
        RTS
EndMod

********************
Module z_storeb ; e2
********************

; X1[X2] = X3

        JSR  X1_PLUS_X2_TO_A0
        LDA  X3L
        STA  (A0L),Y
        RTS
EndMod

**********************
Module z_put_prop ; e3
**********************

; object (X1) property (X2) = X3

        JSR  Find_Property
        BNE  z_error10
        JSR  Property_Size
        BEQ  _byte              ; 0: byte
        CMP  #1                 ; 1: word
        BNE  z_error11
        LDA  X3H
        STA  (A0L),Y
        INY
_byte   LDA  X3L
        STA  (A0L),Y
        RTS
z_error10 ERROR(10)
z_error11 ERROR(11)
EndMod

******************
Module z_read ; e4
******************

; read an input line and call parser

; X1 = text  buffer
; X2 = parse buffer
; X3 = timeout interval [1/10 seconds]
; X4 = timeout interrupt routine

        JSR  Set_Timeout        ; for Borderzone
        BBS7 Version,_V4
        JSR  z_show_status      ; only in version 3
_V4     JSR  Print_Buffer       ; print prompt
        LDX  #0
        STX  Tok_Flag
        JSR  Header_X1
        JSR  Get_Line_X1        ; read input from user
        LDA  TO_Flag            ; timeout ?
        LBNE Store_Zero
        DEC  ParNum
        BEQ  _skip              ; -> no parse buffer
        LDA  X2L
        ORA  X2H
        BEQ  _skip              ; -> no parse buffer
        JSR  Parse_Input
_skip   LDA  Version
        CMP  #$85
        BCC  _return
        LDA  #CR
        JMP  Store_Byte
_return RTS
EndMod

************************
Module z_print_char ; e5
************************

        LDA  X1L
        JMP  Print_Formatted
EndMod

***********************
Module z_print_num ; e6
***********************

        LDX  #X1L
        LDA  X1H
        BPL  _pos
        JSR  Sign_ZPX
        LDA  #'-'
        JSR  Print_Formatted
_pos    LDY  X1L
        LDZ  X1H
        JSR  Format_YZ
        LDX  #0
_loop   LDA  NUMBER,X
        CMP  #' '
        BEQ  _next
        PHX
        JSR  Print_Formatted
        PLX
_next   INX
        CPX  #5
        BCC  _loop
        RTS
EndMod

********************
Module z_random ; e7
********************

        LDA  X1L
        STA  X2L
        LDA  X1H
        STA  X2H
        LDA  Random
        LDX  Raster
        STX  X1L
        AND  #$7f
        STA  X1H
        JSR  Divide_Signed
        INW  X0L
        JMP  Store_X0
EndMod

******************
Module z_push ; e8
******************

        LDA  X1L
        LDY  X1H
        JMP  Push_AY
EndMod

******************
Module z_pull ; e9
******************

        JSR  z_pop
        JMP  Put_Var
EndMod

**************************
Module z_split_window ; ea
**************************

        JSR  Print_Buffer
        LDX  X1L                ; new size
        CPX  #ROWS
        BCS  _return
        STX  Upper_Size         ; update size
        STX  Win_Top
        LDX  Cursor_Row
        CPX  Upper_Size
        BCS  _set
        JSR  Window_Home
_set    JSR Set_Screen_Pointer
_return RTS
EndMod

************************
Module z_set_window ; eb
************************

        JSR  Print_Buffer
        LDX  X1L                ; new active window
        STX  Z_Active_Window
        LDA  FG_Color,X
        STA  Colour
        LDA  #0
        LDY  Z_Active_Font,X
        CPY  #3
        BNE  _font
        LDA  #$80
_font   STA Font_Sel
        TXA
        BEQ  _lower

        LDA  #0                 ; window 1: upper
        LDX  Upper_Size
        DEX
        BRA  _set

_lower  LDA  Upper_Size         ; window 0: lower
        LDX  #ROWS-1

_set    STA  Win_Top
        STX  Win_Bot
        LDY  #0
        JMP  Set_Row_Col
EndMod

**************************
Module z_erase_window ; ed
**************************

        JSR  Print_Buffer
        LDX  X1L
        BEQ  _lower             ;  0: erase lower window
        CPX  #-1                ; -1: unsplit
        BEQ  _unspl
        CPX  #-2                ; -2: clear screen
        BEQ  _clear
        LDX  #0                 ;  1: erase upper window
_uloop  JSR  Erase_Row
        INX
        CPX Upper_Size
        BCC  _uloop
        RTS

_lower  LDX  Upper_Size
_lloop  JSR  Erase_Row
        INX
        CPX  #ROWS
        BCC  _lloop
        RTS

_unspl  LDA  #0
        STA  Upper_Size
        STA  Win_Top
        STA  Z_Active_Window
        LDA  #ROWS-1
        STA  Win_Bot
_clear  JMP  Clear_Screen

EndMod

************************
Module z_erase_line ; ee
************************

        LDX  X1L
        CPX  #1
        BNE  _return
        LDY  Cursor_Col
        LDA  #' '
_loop   STA  (Scr_Adr),Y
        INY
        CPY  #COLS
        BCC  _loop
_return RTS
EndMod

************************
Module z_set_cursor ; ef
************************

; Input : X1 = new row position relative to window top
;         X2 = new column position
;         X3 = window (version>= 6)
;         the Z  cursor home is [1:1]
;         the OS cursor home is [0:0]

        LDA  Upper_Size
        STA  MORE_Counter
        JSR  Print_Buffer
        LDA  X1L
        DEC  A
        CLC
        ADC  Win_Top
        TAX
        LDY  X2L
        DEY
        JMP  Set_Row_Col
EndMod

************************
Module z_get_cursor ; f0
************************

; Input : X1 = receiving word array
;         the Z  cursor home is [1:1]
;         the OS cursor home is [0:0]

        LDA  #0
        TAY
        STA  (X1L),Y            ; row high
        LDA  Cursor_Row
        INC  A
        INY                     ; Y = 1
        STA  (X1L),Y            ; row low
        LDA  #0
        INY
        STA  (X1L),Y            ; col high
        LDA  Cursor_Col
        INC  A
        INY                     ; Y = 1
        STA  (X1L),Y            ; col low
        RTS
EndMod

*****************
Module Reverse_On
*****************

        LDA  #REVERSE
        STA  Attribute
        RTS
EndMod

*******************
Module Underline_On
*******************

        LDA  #UNDERLINE
        STA  Attribute
        RTS
EndMod

**************
Module Bold_On
**************

        LDA  #BOLD
        STA  Attribute
        RTS
EndMod

****************************
Module z_set_text_style ; f1
****************************

; % 76543210 Z                C128
;       ^--- 8: fixed pitch   ignore (font is fixed anyway)
;        ^-- 4: Italic        Attribute = UNDERLINE
;         ^- 2: Bold          Attribute = BOLD
;          ^ 1: Reverse       Attribute = REVERSE
;            0: Normal        Attribute = 0

;       JSR Print_Buffer
;       LDA #'{'
;       JSR CHROUT
;       LDA X1L
;       ORA #'0'
;       JSR CHROUT
;       LDA #'}'
;       JSR CHROUT

        JSR  Print_Buffer
        LDA  X1L
        LSR  A
        BCS  Reverse_On
        LSR  A
        BCS  Bold_On
        LSR  A
        BCS  Underline_On
        LDA  #0
        STA  Attribute
        RTS
EndMod

*************************
Module z_buffer_mode ; f2
*************************

        JSR  Print_Buffer
        LDA  X1L
        STA  Z_Buffer_Mode
        RTS
EndMod

***************************
Module z_output_stream ; f3
***************************

; X1 = stream number  3:  select memory
;                    -3:deselect memory
; X2 = table address
; X3 = width

        JSR  Print_Buffer       ; flush buffer
        LDA  X1L                ; stream number
        CMP  #-3                ; deselect memory stream ?
        BEQ  _close             ; -> do
        CMP  #3                 ; select memory stream ?
        BNE  _return             ; no -> return
        LDA  #-1
        STA  Z_Mem_Flag         ; open memory channel
        CLC
        LDA  X2H
        ADC  #>Z_HEADER
        STA  Z_Mem_Base+1       ; Z_Mem_Base = TABLE
        STA  Z_Mem_Ptr+1
        LDA  X2L
        STA  Z_Mem_Base
        STA  Z_Mem_Ptr          ; Z_Mem_Ptr = TABLE
_return RTS

; close memory channel
; store length of TABLE in first word of (Z_Mem_Base)

_close  LDY  #1
        SEC
        LDA  Z_Mem_Ptr
        SBC  Z_Mem_Base
        STA  (Z_Mem_Base),Y
        DEY                     ; Y = 0
        STY  Z_Mem_Flag         ; close channel
        LDA  Z_Mem_Ptr+1
        SBC  Z_Mem_Base+1
        STA  (Z_Mem_Base),Y
        RTS
EndMod

**********
Error_Beep
**********


************
Module Click
************

        LDA  #$3c
        STA  Voc1FreqLo
        LDA  #$32
        STA  Voc1FreqHi
        LDA  #$fc
        STA  Jiffy+2
        LDA  #$f0
        STA  Voc1SusRel
        LDA  #$8f
        STA  FiltMode
        LDA  #$41
        STA  Voc1Control
_wait   LDA  Jiffy+2
        BNE  _wait
        STA  Voc1Control
        LDA  #$80
        STA  FiltMode
        RTS
EndMod

**************************
Module z_sound_effect ; f5
**************************

; X1  = sound effect number
;       1: high pitched beep
;       2: low  pitched beep
; X2L = volume
; X2H = repeats
; X3  = routine to be called after finish

        LDX  X1L
        DEX
        BEQ  Click
        DEX
        BEQ  Error_Beep
        RTS
EndMod

***********************
Module z_read_char ; f6
***********************

        JSR  Set_Timeout
        JSR  Print_Buffer
        LDA  Upper_Size
        STA  MORE_Counter
        JSR  Cursor_On
_loop   JSR  Get_Character
        CMP  #0
        BEQ  _loop
        JSR  Cursor_Off
        CMP  #DEL
        BNE  _exit
        LDA  #8
_exit   JMP  Store_Byte
EndMod

******************
Module Set_Data_YA
******************

        STY  QD0
        STA  QD1
        LDY  #0
        STY  QD2
        RTS

************************
Module z_scan_table ; f7
************************

; Input : X1 = search value
;         X2 = address of table
;         X3 = number of table entries
;         X4 = type (default = $82)

        LDA  X3H
        BMI  zst_fa             ; length  < 0 : false
        ORA  X3L
        BEQ  zst_fa             ; length == 0 : false

        LDA  ParNum
        CMP  #4                 ; type parameter given ?
        BEQ  _all               ; -> yes
        LDA  #$82               ; default: word table, size=2
        STA  X4L                ; word/byte flag

_all    BBS7 X4L,_addr          ; -> word
        LDA  X1L
        STA  X1H                ; byte to search

_addr   LDY  X2L                ; setup table address
        LDA  X2H
        JSR  Set_Data_YA

_loop   LDA  QD0                ; remember address
        STA  X0L
        LDA  QD1
        STA  X0H

        JSR  Next_Datum         ; next table item high (or byte)
        CMP  X1H                ; compare
        BNE  _next              ; -> no match
        BBR7 X4L,zst_tr         ; -> compare bytes
        JSR  Next_Datum         ; next table item low
        CMP  X1L                ; compare
        BEQ  zst_tr             ; -> match

_next   LDA  X4L
        AND  #$7f               ; length
        CLC
        ADC  X0L                ; QD = X0 + item length
        STA  QD0
        LDA  #0
        ADC  X0H
        STA  QD1
        DEW  X3L                ; countdown
        BNE  _loop

zst_fa  JSR  Store_Zero         ; Store 0 and return false
        JMP  Main_False

zst_tr  JSR  Store_X0           ; store X and return true
        JMP  Main_True
EndMod

**********************
Module z_tokenise ; fb
**********************

;         X1 = text input buffer
;         X2 = parse buffer
;         X3 = dictionary
;         X4 = flag

        JSR  Header_X1
        LDX  ParNum
        DEX
        DEX
        LBEQ  Parse_Input       ; standard dictionary
        DEX
        STX  Tok_Flag     ; 0 or 1
        LDA  X3L
        LDY  X3H
        JMP  Parse_AY_Dict
EndMod

*************************
Module z_encode_text ; fc
*************************

; Input : X1 = text buffer
;         X2 = length of word
;         X3 = start index
;         X4 = encoded text

        JSR  Header_X1
        LDA  #0
        LDX  #9                 ; word length
_clear  STA  TEXT_WORD-1,X
        DEX
        BNE  _clear

        LDY  X3L                ; copy word from buffer
_loop   LDA  (X1L),Y            ; to TEXT_WORD
        PHX
        PHY
        JSR  Sep_All
        PLY
        PLX
        BCS  _next
        STA  TEXT_WORD,X
        INY
        INX
        CPX  Word_Length
        BCS  _next
        CPX  X2L
        BCC  _loop

_next   JSR  Encode
        CLC
        LDA  X4H
        ADC  #>Z_HEADER
        STA  X4H
        LDY  Vocab_Length
        DEY
_copy   LDA  DICT_WORD,Y
        STA  (X4L),Y
        DEY
        BPL  _copy
        RTS
EndMod

******************
Module Clear_Table
******************

        JSR  Header_X1
        LDY  #0                 ; Y = 0
ClTa_10 LDA  #0                 ; A = 0
        STA  (X1L),Y
        INY
        BNE  ClTa_20
        INC  X1H
ClTa_20 DEW  X3L
        BNE  ClTa_10
        RTS
EndMod

************************
Module z_copy_table ; fd
************************

; Input : X1 = source
;         X2 = target
;         X3 = size

        LDA  X2L
        ORA  X2H
        BEQ  Clear_Table        ; no copy
        LDA  X3L
        ORA  X3H
        BEQ  _return            ; size = 0

        LDX  #X3L
        LDA  X3H
        BPL  _safe              ; -> safe copy
        JSR  Sign_ZPX           ; make size positive
        BRA  _fwd               ; -> forced forward

_safe   LDA  X1L
        CMP  X2L
        LDA  X1H
        SBC  X2H
        BCC  _bwd               ; -> (X1 < X2) backward

; forward copy (X1 may point to high memory)

_fwd    LDY  X1L                ; data pointer = source
        LDA  X1H
        JSR  Set_Data_YA

        CLC
        LDA  X2H
        ADC  #>Z_HEADER
        STA  X2H

_floop  JSR  Next_Datum         ; LDA (source)
        LDY  #0
        STA  (X2L),Y            ; STA (target)
        INW  X2L                ; ++X2
        DEW  X3L                ; --X3 (counter)
        BNE  _floop             ; -> loop
_return RTS

; backward copy

_bwd    CLC                     ; set source end
        LDA  X1L
        ADC  X3L
        STA  X1L
        LDA  X1H
        ADC  X3H
        ADC  #>Z_HEADER
        STA  X1H
        CLC                     ; set target end
        LDA  X2L
        ADC  X3L
        STA  X2L
        LDA  X2H
        ADC  X3H
        ADC  #>Z_HEADER
        STA  X2H
        LDY  #0
_bloop  DEW  X1L                ; --X1
        DEW  X2L                ; --X2
        LDA  (X1L),Y
        STA  (X2L),Y            ; copy byte
        DEW  X3L                ; --X3
        BNE  _bloop             ; loop
        RTS
EndMod

*************************
Module z_print_table ; fe
*************************

; Input : X1 = zscii text
;         X2 = width
;         X3 = height
;         X4 = skip

        JSR  Print_Buffer
        LDY  X1L                ; set text address
        LDA  X1H
        JSR  Set_Data_YA
        LDA  ParNum
        CMP  #3
        BCS  _start
        LDA  #1                 ; default height = 1
        STA  X3L
_start  JSR  Save_Cursor
_rloop  JSR  Restore_Cursor
        LDX  X2L                ; width
_cloop  JSR  Next_Datum
        JSR  CHROUT
        DEX                     ; column countdown
        BNE  _cloop
        INC  C_Save_Row         ; next row
        DEC  X3L                ; row countdown
        BNE  _rloop
        RTS
EndMod

*******************************
Module z_check_argc ; opcode ff
*******************************

        LDA  Z_Arg_Count
        CMP  X1L
        LBCS Main_True
        JMP  Main_False
EndMod

******************
Module Set_Timeout
******************
        LDY  #0
        STY  Timeout
        STY  Timeout+1
        LDA  ParNum
        CMP  #3
        BCC  _return
        LDA  X4L                ; timeout routine
        STA  Z_Timeout
        LDA  X4H
        STA  Z_Timeout+1
        LDA  X3L                ; timeout value
        STA  Timeout            ; [1/10 seconds]
        LDX  X3H
        STX  Timeout+1
        ROW  Timeout            ; * 2
        ADC  Timeout
        STA  Timeout
        TXA
        ADC  Timeout+1
        STA  Timeout+1          ; * 3
        ROW  Timeout            ; * 6 = Jiffies
        SEI
        STY  Jiffy              ; reset clock
        STY  Jiffy+1
        STY  Jiffy+2
        CLI
_return RTS
EndMod

************
Module Log_A
************
        PHA
        LDA  Z_Active_Window
        BNE  _return
        LDA  #'{'
        JSR  CHROUT
        PLA
        PHA
        JSR  ASCII_Hex
        PHA
        TXA
        JSR  CHROUT
        PLA
        JSR  CHROUT
        LDA  #'}'
        JSR  CHROUT
        LDA  Cursor_Col
        CMP  #76
        BCC  _return
        JSR  Return_Screen
_return PLA
        RTS
EndMod

*************
Module Dump_A
*************
        JSR  ASCII_Hex
        PHA
        TXA
        JSR  CHROUT
        PLA
        JMP  CHROUT
EndMod

****************
Module Dump_Word
****************
; Input: X = ZP address
        LDA  #'{'
        JSR  CHROUT
        LDA  1,Y
        JSR  Dump_A
        LDA  0,Y
        JSR  Dump_A
        LDA  #'}'
        JSR  CHROUT
        RTS
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

        LDA  h_dictionary_lo
        LDY  h_dictionary_hi

*************
Parse_AY_Dict
*************

        STA  DPL
        STY  DPH
        CLC
        LDA  X2H                ; X2 += Header
        ADC  #>Z_HEADER         ; parse buffer
        STA  X2H

; limit buffer capacity to 59 items

        LDY  #0
        LDA  (X2L),Y
        BEQ  Parse_10           ; -> use default
        CMP  #60
        BCC  Parse_15           ; -> OK (1..59)
Parse_10  LDA #59
        STA  (X2L),Y            ; use default
Parse_15  STA X6H               ; copy of parse buffer limit
        BBR7 Version,Parse_17
        INY                     ; Y = 1
        LDA  (X1L),Y
        STA  Chars_Left         ; Version > 3
Parse_17  LDY #2                ; Y = 2
        STY  Parse_Index        ; start in parse buffer
        BBS7 Version,Parse_18
        DEY                     ; start at 1 for Version = 3
Parse_18  STY X5L               ; buffer index
        LDY  #0
        STY  X6L                ; items parsed
        STY  X5H                ; word size = 0

; parse loop for items (words or delimiters)

Parse_20  LDX X6L               ; items parsed
        CPX  X6H                ; items max.
        BCS  Parse_25           ; -> exceeded # of words
        LDA  Chars_Left
        ORA  X5H                ; word size
        BNE  Parse_30

Parse_25  LDY #1                ; finish
        TXA
        STA  (X2L),Y            ; items parsed
        RTS                     ; exit

; continue parsing word

Parse_30  LDA X5H               ; word size
        CMP  Word_Length
        BCC  Parse_35           ; -> word length < max
        JSR  Skip_Surplus       ; skip to next delimiter

Parse_35  LDA X5H               ; word size
        BNE  Parse_45           ; -> search word in dictionary

        LDX  #8                 ; clear Word buffer
Parse_40  STA TEXT_WORD,X
        DEX
        BPL  Parse_40

; not a word, is this character a delimiter ?

        LDY  X5L                ; buffer index
        STY  X7L+3              ; X8H = word position
        LDA  (X1L),Y
        JSR  Sep_Dict           ; dot, comma, quote
        BCS  Parse_50           ; -> in dictionary
        JSR  Sep_Std            ; standard separators
        BCC  Parse_45
        DEC  Chars_Left         ; ignore ! or ?
        INC  X5L                ; ++buffer index
        BNE  Parse_20           ; always

Parse_45  LDA Chars_Left
        BEQ  Parse_55
        LDY  X5L                ; buffer index
        LDA  (X1L),Y
        JSR  Sep_All            ; check all separators
        BCS  Parse_55
        LDX  X5H                ; word size
        STA  TEXT_WORD,X
        DEC  Chars_Left
        INC  X5H                ; ++word size
        INC  X5L                ; ++buffer index
        BRA  Parse_20

Parse_50  STA TEXT_WORD         ; word is a separator
        DEC  Chars_Left
        INC  X5H                ; ++word size
        INC  X5L                ; ++buffer index

; start parsing

Parse_55  LDA X5H               ; word size
        BEQ  Parse_20           ; -> nothing to parse
        STA  X7L+2              ; X8L = # of letters
        JSR  Encode             ; convert to packed format
        JSR  Dictionary_Search  ; result -> X7
        INC  X6L                ; Inc # of items
        LDX  #0
        STX  X5H                ; word size
        LDY  Parse_Index
        LDA  Tok_Flag     ; 1 : store matches only
        BEQ  Parse_60           ; -> store anyhow
        LDA  X7L
        ORA  X7H
        BNE  Parse_60
        INY                     ; skip this entry
        INY                     ; no synonym replacement
        INY
        INY
        BNE  Parse_70           ; 0 : Dict address hi
Parse_60  LDA X7L,X             ; 1 : Dict address lo
        STA  (X2L),Y            ; 2 : # of letters
        INY                     ; 3 : text buffer position
        INX
        CPX  #4
        BCC  Parse_60
Parse_70  STY Parse_Index       ; Parse_Index += 4
        JMP  Parse_20
EndMod

************
Skip_Surplus
************

        LDA  Chars_Left
        BEQ  SkSu_Ret
SkSu_10 LDY  X5L                ; buffer index
        LDA  (X1L),Y
        JSR  Sep_All
        BCS  SkSu_Ret
        INC  X5H                ; word size
        INC  X5L                ; buffer index
        DEC  Chars_Left
        BNE  SkSu_10
SkSu_Ret  RTS

*******
Sep_All
*******

; Input : (A) = character
; Output: C=1 is separator, C=0 is not

; check standard list and dictionary list

        JSR  Sep_Std
        BCS  SeDi_Ret           ; -> is separator

********
Sep_Dict
********

; Input : (A) = character
; Output: C=1 is separator, C=0 is not

; typical list: , . "

        PHA
        STA  SeDi_10+4          ; search character
        LDY  h_dictionary_lo
        LDA  h_dictionary_hi
        JSR  Set_Data_YA
        JSR  Next_Datum
        TAX                     ; X = # of separators
SeDi_10 JSR  Next_Datum
        CMP  #' '               ; modified !
        BEQ  SeDi_20            ; yes: return with C=1
        DEX
        BNE  SeDi_10
        CLC                     ; no : return with C=0
SeDi_20 PLA
SeDi_Ret  RTS

*******
Sep_Std
*******

; Input : (A) = character
; Output: C=1 is separator, C=0 is not

; standard list: ! ? , . CR SPACE

        LDX  #?Sep_Std_List-1   ; size of separator list
SeSt_10 CMP  Sep_Std_List,X
        BEQ  SeSt_Ret           ; yes: return with C=1
        DEX
        BPL  SeSt_10
        CLC                     ; no : return with C=0
SeSt_Ret  RTS

****************
Module Skip_Data
****************

; Input : A = # of bytes to skip
; Output: QD pointer updated

        CLC
        ADC  QD0
        STA  QD0
        BCC  _return
        INW  QD1
_return RTS
EndMod

*****************
Dictionary_Search
*****************

; Input : DP = dictionary
;         RA = search token

        LDY  DPL                ; start of dictionary
        LDA  DPH
        JSR  Set_Data_YA

        JSR  Next_Datum         ; size of separator list
        JSR  Skip_Data          ; skip list
        JSR  Next_Datum         ; item size
        STA  DPI
        JSR  Next_Datum         ; item count hi
        STA  X4H
        JSR  Next_Datum         ; item count lo
        STA  X4L
        ORA  X4H
        BEQ  DiSe_70            ; -> empty dictionary
        LDA  X4H
        BMI  DiSe_20            ; -> entries are unsorted

        SEC
        LDA  #0                 ; positive countdown is sorted
        SBC  X4L                ; make count down negative
        STA  X4L
        LDA  #0
        SBC  X4H
        STA  X4H

DiSe_20 LDA  QD0                ; X7 = address of next item
        STA  X7H
        LDA  QD1
        STA  X7L
        LDX  #0                 ; X = token byte index
DiSe_30 JSR  Next_Datum
        INX
        CMP  DICT_WORD-1,X
        BNE  DiSe_50
        CPX  Vocab_Length
        BCC  DiSe_30
        RTS                     ; -> match

DiSe_50 JSR  Next_Datum         ; skip to next item
        INX
        CPX  DPI                ; item_Size
        BCC  DiSe_50

        INW  X4L                ; count up to zero
        BNE  DiSe_20

        LDA  #0
DiSe_70 STA  X7H                ; not found: return (X7) = 0
        STA  X7L
DiSe_Ret  RTS


;                6789abcdef0123456789abcdef
;                --------------------------
;         .BYTE "abcdefghijklmnopqrstuvwxyz"
;         .BYTE "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ZSCII_2 .BYTE "\0\r0123456789.,!?_#'\"/\\-:()"

*********
Decode_YX
*********

; print 5bit encoded text from address (Y/X)

        STY  QD0
        STX  QD1
        LDA  #0
        STA  QD2

***********
Decode_Text
***********

;         LDA #'{'
;         JSR Print_Formatted
;         LDA QD2
;         JSR Dump_For
;         LDA QD1
;         JSR Dump_For
;         LDA QD0
;         JSR Dump_For
;         LDA #'}'
;         JSR Print_Formatted

        LDX  #0
        STX  QDH
DeTe_10 LDX  #0
        STX  Alphabet
DeTe_15 JSR  Get_Packed_Char
        BCS  DiSe_Ret           ; return
        BNE  DeTe_20
        LDA  #' '               ; 0 : space
        BNE  DeTe_40
DeTe_20 CMP  #4                 ; 1,2,3 : abbreviations
        BCC  DeTe_55
        CMP  #6                 ; shift 4:capital, 5:numeric
        BCS  DeTe_25
        STA  Alphabet
        BCC  DeTe_15
DeTe_25 LDX  Alphabet
        CPX  #5                 ; numeric
        BNE  DeTe_30
        TAX
        LDA  ZSCII_2-6,X
        BNE  DeTe_40
        BEQ  DeTe_50

DeTe_30 CPX  #4                 ; upper case
        BNE  DeTe_35
        ADC  #$3a               ; carry is set   from CPX
        BNE  DeTe_40
DeTe_35 ADC  #$5b               ; carry is clear from CPX
DeTe_40 JSR  Print_Formatted
        JMP  DeTe_10

DeTe_50 JSR  Get_Packed_Char    ; 10 bit ZSCII
        ASL  A
        ASL  A
        ASL  A
        ASL  A
        ASL  A
        STA  LV0
        JSR  Get_Packed_Char
        ORA  LV0
        JMP  DeTe_40

; process code for abbreviations (1,2,3) plus abbr. index

DeTe_55 SBC  #0                 ; subtract 1 (C=0)
        ASL  A                  ; 0, 1, 2
        ASL  A
        ASL  A
        ASL  A
        ASL  A
        ASL  A
        STA  LV2                ; -> $00, $40, $80
        JSR  Get_Packed_Char
        ASL  A
        CLC
        ADC  LV2                ; add offset
        TAY                     ; index to abbr. table

; push QD2, QD1, QD0, QDH, QDL for recursive call

        LDX  #4
DeTe_60 LDA  QDL,X
        PHA
        DEX
        BPL  DeTe_60

; recursive call of Decode_Text for abbreviation

        CLC
        LDA  h_abbreviations_lo
        STA  A0L
        LDA  h_abbreviations_hi
        ADC  #>Z_HEADER
        STA  A0H                ; (A0) = start of abbr. pointer
        INY
        LDA  (A0L),Y            ; address low
        ASL  A                  ; x 2
        STA  QD0
        DEY
        LDA  (A0L),Y            ; address high
        ROL  A                  ; x 2
        STA  QD1
        LDA  #0
        ROL  A                  ; high mem
        STA  QD2

; recursive call

        JSR  Decode_Text

; pull QDL, QDH, QD0, QD1, QD2 after recursive call

        LDX  #0
DeTe_65 PLA
        STA  QDL,X
        INX
        CPX  #5
        BCC  DeTe_65
        JMP  DeTe_10
        .SIZE

***************
Get_Packed_Char
***************

        LDA  QDH
        BEQ  GPC_20             ; load next word
        CMP  #%100 00000        ; EOS flag
        BEQ  GPC_Ret            ; return with C=1 : End-Of-String
        BIT  QDH                ; valid 2nd. char ?
        BVC  GPC_10             ; goto  3rd. cahr
        AND  #%100 11111        ; save EOS clear bit 6
        ORA  #%001 00000        ; make non zero
        STA  QDH
        AND  #%000 11111        ; extract data
        CLC
        RTS                     ; return with 2nd. char

GPC_10  AND  #%100 00000        ; save EOS only
        STA  QDH
        LDA  QDL
        AND  #%000 11111        ; extract 5 bit
        CLC
        RTS                     ; return with 3rd. char

GPC_20  JSR  Next_Datum
        TAX                     ; x = first half
        AND  #%0000 0011        ; extract part of 2nd. char
        STA  QDH
        JSR  Next_Datum
        STA  QDL
        ASL  A
        ROL  QDH
        ASL  A
        ROL  QDH
        ASL  A
        ROL  QDH
        TXA
        AND  #%1000 0000        ; save EOS flag
        ORA  #%0100 0000        ; set valid 2nd. char flag
        ORA  QDH                ; add value
        STA  QDH                ; save 2nd. char

        TXA
        LSR  A
        LSR  A
        AND  #%000 11111
        CLC
GPC_Ret RTS                     ; return with 1st. char

******
Encode
******

; The encode takes 9 ASCII characters (6 in version 3) and
; converts them to packed ZSCII. This routine is called from the parser
; only, which already converts uppercase to lowercase letters.
; Also the output length is restricted to 6 characters (4 in bersion 3)
; because this is the size of a dictionary entry.

; Input:  6-9 ASCII charcaters in TEXT_WORD
; Output: 4-6 Z     characters in DICT_WORD

        LDY  #0                 ; TEXT_WORD[Y]
        LDX  #0                 ; DICT_WORD[X]
        BEQ  Enco_30

Enco_10 LDA  #5                 ; void (numeric shift)
Enco_20 STA  DICT_WORD,X        ; store next ZSCII
        INX
        CPX  Word_Length
        BCS  Enco_70

        INY
Enco_30 LDA  TEXT_WORD,Y        ; get next ASCII char
        BEQ  Enco_10            ; insert void char

        CMP  #'Z'+1             ; (A-Z) upper range
        BCS  Enco_35
        CMP  #'A'               ; (A-Z) lower range
        BCC  Enco_35
        SBC  #$3b               ; (A-Z) -> $06-$1f
        BNE  Enco_20            ; -> loop always

Enco_35 CMP  #'z'+1             ; (a-z) upper range
        BCS  Enco_40
        CMP  #'a'               ; (a-z) lower range
        BCC  Enco_40
        SBC  #$5b               ; (a-z) -> $06-$1f
        BNE  Enco_20            ; -> loop always

Enco_40 STY  L_Encode_Y
        STA  L_Encode_A
        LDA  #5
        STA  DICT_WORD,X
        INX
        LDA  L_Encode_A
        LDY  #25
Enco_50 CMP  ZSCII_2,Y
        BEQ  Enco_60            ; found ZSCII in table 3
        DEY
        BNE  Enco_50

        LDA  #6                 ; shift to 10 bit char
        STA  DICT_WORD,X
        INX
        LDA  L_Encode_A
        LSR  A
        LSR  A
        LSR  A
        LSR  A
        LSR  A
        AND  #3
        STA  DICT_WORD,X
        INX
        LDA  L_Encode_A
        AND  #$1f
        LDY  L_Encode_Y
        JMP  Enco_20

Enco_60 TYA                     ; use index to table 3 as value
        LDY  L_Encode_Y         ; restore read index
        ADC  #5                 ; add offset 6 (5 + carry)
        BNE  Enco_20            ; loop always

Enco_70 LDX  #0
        LDY  #0
Enco_80 LDA  DICT_WORD+1,X
        ASL  A
        ASL  A
        ASL  A
        ASL  A
        ROL  DICT_WORD,X
        ASL  A
        ROL  DICT_WORD,X
        ORA  DICT_WORD+2,X
        PHA
        LDA  DICT_WORD,X
        STA  DICT_WORD,Y
        PLA
        STA  DICT_WORD+1,Y
        INX
        INX
        INX
        INY
        INY
        CPX  Word_Length
        BCC  Enco_80
        LDA  DICT_WORD-2,Y
        ORA  #$80               ; end of string flag
        STA  DICT_WORD-2,Y
        RTS

*************
Module A0_x_8
*************

        STA  A0L
        STX  A0H                ; (A0) = object
        ASL  A
        ROL  A0H                ; (A/A0H) = object *  2
        ASL  A
        ROL  A0H                ; (A/A0H) = object *  4
        ASL  A
        ROL  A0H                ; (A/A0H) = object *  8
        RTS
EndMod

********************
Module Object_Offset
********************

; V3 Address = (h_objects) + 53 + 9 * object

        BBS7 Version,_V4
        LDX  #0
        JSR  A0_x_8
        ADC  A0L
        BCC  _label
        INC  A0H                ; (A/A0H) = object *  9
        CLC
_label  ADC  #53                ; V3 offset 53
        RTS

; V4 Address = (h_objects) + 112 + 14 * object

_V4     JSR  A0_x_8
        SEC
        SBC  A0L
        PHA                     ; low  of  (object *  7)
        LDA  A0H
        STX  A0H
        SBC  A0H
        STA  A0H                ; high of  (object *  7)
        PLA
        ASL  A
        ROL  A0H                ; (A/A0H) = object * 14
        ADC  #112               ; V4 offset 112
        RTS
EndMod

********************
Module Get_Object_X1
********************

        LDA  X1L
        LDX  X1H

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

        JSR  Object_Offset
        BCC  _label
        INC  A0H
        CLC
_label  ADC  h_objects_lo
        STA  A0L
        LDA  A0H
        ADC  h_objects_hi
        ADC  #>Z_HEADER
        STA  A0H
        LDX  #0                 ; object hi for V3
        BBR7 Version,_objlow
        LDA  (A0L),Y
        TAX                     ; object hi
        INY
_objlow LDA  (A0L),Y            ; object lo
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

        LDY  #7                 ; version = 3 offset to property pointer
        BBR7 Version,_fips_1
        LDY  #12                ; version > 3 offset to property pointer
_fips_1 JSR  Get_Object_X1
        BBS7 Version,_fips_2
        TAX                     ; property pointer high
        INY
        LDA  (A0L),Y            ; property pointer low
_fips_2 STA  A0L                ; property table low
        TXA                     ; property table high
        ADC  #>Z_HEADER         ; C=0 from Get_Object
        STA  A0H                ; (A0) = property table
        LDY  #0                 ; offset to NAME property
        LDA  (A0L),Y            ; size of name (words)
        ASL  A                  ; size of name (bytes)
        SEC                     ; add size + 1
        ADC  A0L
        STA  A0L
        BCC  FiPr_10
        INC  A0H
FiPr_10 LDA  (A0L),Y
        AND  Prop_Mask          ; A = property ID
        RTS
EndMod

********************
Module Property_Size
********************

        BBR7 Version,_V3
        LDA  (A0L),Y
        BPL  _one
        INY
        LDA  (A0L),Y            ; 2nd. size byte
        INY
        AND  Prop_Mask
        RTS

_one    INY                     ; Y points after size
        ASL  A                  ; bit 7 = length info
        ASL  A                  ; carry = length info
        ROL  A                  ; bit 0 = length info
        AND  #1
        RTS

_V3     LDA  (A0L),Y
        INY
        LSR  A
        LSR  A
        LSR  A
        LSR  A
        LSR  A
        RTS
EndMod

*************
Next_Property
*************

; use size info and point to next property (V3-V5)

; Input : (A0) = pointer to current property
; Output: (A0) = pointer to next property
;            Y = 0

        JSR  Property_Size
        CLC
        ADC  #2
        ADC  A0L
        STA  A0L
        BCC  NePr_20
        INC  A0H
NePr_20 LDY  #0
        LDA  (A0L),Y
        AND  Prop_Mask
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

        JSR  First_Property     ; -> (A0) and Y = 0
_loop   CMP  X2L
        BEQ  _match
        BCC  _return            ; -> not in table (Z=0)
        JSR  Next_Property
        BRA  _loop
_match  CLC
        BBR7 Version,_ok
        LDA  (A0L),Y            ; prepare pointer advancement
        ASL  A                  ; C=1 for 2 byte header
_ok     LDA  #0                 ; set zero flag
_return RTS                     ; Z=1 success, Z=0 not found
EndMod

*************
Get_Attr_Addr
*************

; Input
; -----
; X1L = object #
; X2L = flag   #
;
; Output
; ------
; (A0L),Y = Address of attribute
; (A)     = Attribute mask

        JSR  Get_Object_X1
        LDA  X2L                ; flag #
        LSR  A
        LSR  A
        LSR  A
        TAY                     ; offset = flag / 8
        LDA  X2L
        AND  #7                 ; flag mod 8
        TAX
        LDA  #$80               ; flag 0
        BNE  gaa_02
gaa_01  LSR  A
gaa_02  DEX
        BPL  gaa_01
        RTS

****************
Module Dump_Code
****************

        LDY  #1
_loop   LDA  Z_Code,Y
        JSR  ASCII_Hex
        PHA
        TXA
        JSR  CHROUT
        PLA
        JSR  CHROUT
        LDA  #' '
        JSR  CHROUT
        DEY
        BPL  _loop
        RTS
EndMod


**************
INTERNAL_ERROR
**************

        JSR  ASCII_TS
        STX  INTERR+7
        STA  INTERR+8
        JSR  z_new_line
        JSR  Dump_Code
        Print(INTERR)
        LDX  #2
INER_10 LDA  0,X
        STA  $4000,X
        INX
        BNE  INER_10
        JMP  z_quit

***************
Print_To_Status
***************

        STA  Z_STATUS,Y
        CPY  #COLS-1
        BCS  PTS_01
        INC  Status_Col
PTS_01  RTS

****************
Module Print_Mem
****************

        LDY  #2
        STA  (Z_Mem_Ptr),Y
        INW  Z_Mem_Ptr
        RTS
EndMod

****************
Print_Unbuffered
****************

        JMP  CHROUT

**********************
Module Print_Formatted
**********************

        BBR7 Version,PrFo_10
        BIT  Z_Mem_Flag
        BMI  Print_Mem          ; print to stream # 3
        LDY  Z_Buffer_Mode
        BEQ  Print_Unbuffered
        LDY  Z_Active_Window
        BNE  Print_Unbuffered
        BEQ  PrFo_20
PrFo_10 LDY  Status_Col         ; Version 3
        BPL  Print_To_Status
PrFo_20 CMP  #CR
        BEQ  z_new_line
        CMP  #' '               ; not printable ?
        BCC  PrFo_25
        LDY  Charbuf_Ptr
        STA  Charbuf,Y
        TAX                     ; X = char
        CLC
        TYA
        ADC  Cursor_Col
        CMP  #COLS
        BCS  PrFo_30            ; -> buffer full: print row


        INC  Charbuf_Ptr
PrFo_25 RTS

PrFo_30 LDA  #' '               ; scan backwards for blank
        STY  Charbuf_End
PrFo_35 CMP  Charbuf,Y
        BEQ  PrFo_40
        DEY
        BNE  PrFo_35
        LDY  Charbuf_End        ; no blanks found
PrFo_40 STY  Charbuf_Ptr        ; print line before Charbuf_Ptr
        TYA
        PHA                     ; save line break col
        JSR  z_new_line         ; print line
        PLA
        TAX                     ; line break col
        LDY  #0
        BEQ  PrFo_50

PrFo_45 LDA  Charbuf,X          ; scroll rest of buffer
        STA  Charbuf,Y
        INY
PrFo_50 CPX  Charbuf_End
        INX
        BCC  PrFo_45
        STY  Charbuf_Ptr
        RTS
EndMod

*******************************
Module z_new_line ; opcode # bb
*******************************

        INC  MORE_Counter
        LDX  MORE_Counter
        CPX  #ROWS
        BCC  Terminate_Buffer
        LDA  Upper_Size
        STA  MORE_Counter
        JSR  Empty_Keyboard_Queue
        JSR  Save_Cursor
        LDX  #ROWS-1
        LDY  #0
        JSR  Set_Row_Col
        LDA  Colour
        PHA
        LDA  Attribute
        PHA
        JSR  Reverse_On
        LDA  #YELLOW
        STA  Colour
        Print(MORE)
        PLA
        STA  Attribute
        PLA
        STA  Colour
_wait   JSR  GETIN
        TAX
        BEQ  _wait
        JSR  Restore_Cursor
        Print(BLANKS)
        JSR  Restore_Cursor
EndMod

****************
Terminate_Buffer
****************

        LDX  Charbuf_Ptr
        LDA  #CR
        STA  Charbuf,X
        INC  Charbuf_Ptr

************
Print_Buffer
************

        LDA  Charbuf_Ptr
        BEQ  PrBu_Ret           ; -> nothing to print
        LDX  #0
PrBu_10 LDA  Charbuf,X          ; flush buffer
        JSR  CHROUT
        INX
        CPX  Charbuf_Ptr
        BCC  PrBu_10
        LDX  #0
        STX  Charbuf_Ptr
PrBu_Ret  RTS

*********************
Module Format_Integer
*********************

        LDY  X0L
        LDZ  X0H

*********
Format_YZ
*********

        LDX  #0
_loop   LDA  #'0'
        STA  NUMBER,X
_sub    SEC
        TYA
        SBC  FormLo,X
        PHA
        TZA
        SBC  FormHi,X
        BCC  _next
        INC  NUMBER,X
        TAZ
        PLY
        BRA  _sub
_next   PLA
        INX
        CPX  #4
        BCC  _loop
        TYA
        ORA  #'0'
        STA  NUMBER+4
        LDY  #' '
        LDA  #'0'
        LDX  #0
_blanks CMP  NUMBER,X
        BNE  _return
        STY  NUMBER,X
        INX
        CPX  #4
        BCC  _blanks
_return RTS

FormLo  .BYTE <10000
        .BYTE  <1000
        .BYTE   <100
        .BYTE    <10
FormHi  .BYTE >10000
        .BYTE  >1000
        .BYTE   >100
        .BYTE    >10
EndMod

********************
Module Status_Number
********************

; This routine is used in version 3 stories
; Insert number (score, moves, time) into status line
; Imput : (A) = global variable
;         (Y) = status line column

        PHY
        JSR  Get_Global_Var
        JSR  Format_Integer
        PLY
        LDX  #0
StNu_10 LDA  NUMBER,X
        CMP  #' '
        BEQ  StNu_20
        STA  SCORE,Y
        INY
StNu_20 INX
        CPX  #5
        BCC  StNu_10
        RTS
EndMod

*****************
Module Print_Time
*****************

        LDA  #$11               ; hours var
        JSR  Get_Global_Var
        JSR  Format_Integer
        LDY  #STIME_COL
        LDA  NUMBER+3
        STA  STIME,Y
        LDA  NUMBER+4
        STA  STIME+1,Y

        LDA  #$12               ; minutes var
        JSR  Get_Global_Var
        JSR  Format_Integer
        LDY  #STIME_COL
        LDA  NUMBER+3
        ORA  #'0'               ; blank -> '0'
        STA  STIME+3,Y
        LDA  NUMBER+4
        STA  STIME+4,Y

        LDX  #0
_loop   LDA  STIME,X
        STA  Z_STATUS+67,X
        INX
        CPX  #12
        BCC  _loop
        RTS
EndMod

******************
Module Print_Score
******************

        LDA  #' '
        LDY  #SCORE_COL+4
_clr_sc STA  SCORE,Y            ; clear score
        DEY
        CPY  #SCORE_COL
        BNE  _clr_sc
        LDA  #$11               ; score var
        JSR  Status_Number      ; print score
        LDA  #' '
        LDY  #MOVES_COL+4
_clr_mv STA  SCORE,Y            ; clear score
        DEY
        CPY  #MOVES_COL
        BNE  _clr_mv
        LDA  #$12               ; moves var
        JSR  Status_Number

        LDX  #0
_loop   LDA  SCORE,X
        STA  Z_STATUS+STAT_SCORE,X
        INX
        CPX  #COLS-STAT_SCORE
        BCC  _loop
        RTS
EndMod

*******************
Module Print_Status
*******************

        JSR  Select_Status_Window
        LDA  #YELLOW
        STA  Colour
        JSR  Home_Screen
        JSR  Reverse_On
        LDA  #<(Z_STATUS-1)
        LDY  #>(Z_STATUS-1)
        LDX  #COLS
        JSR  PrintText
        JMP  Select_Text_Window
EndMod

******************
Module z_extension
******************
        PLA                     ; remove return address
        PLA
        JSR  Next_Instruction   ; extendedcode
        STA  Z_Code
        JSR  Next_Instruction   ; operand type
        JSR  Load_4_Operands
        LDA  Z_Code
        AND  #$1f
        CMP  #$0b
        BCS  z_error12
        ASL  A
        TAX
        JSR  (opcodes_ext,X)
        JMP  PC_LOOP
z_error12 ERROR(12)
EndMod


******************
Module z_save_undo
******************

        LDA  #-1
        TAX
        JMP  Store_AX
EndMod

*********************
Module z_restore_undo
*********************

        JMP Store_Zero
EndMod

********************
Empty_Keyboard_Queue
********************

        JSR  GETIN
        BNE  Empty_Keyboard_Queue
        RTS

***********
Decode_Unit
***********
        CMP  #'0'
        BCS  DeUn_10
        TXA                     ; one digit
        BNE  DeUn_20
DeUn_10 CPX  #'1'
        BNE  DeUn_20
        ADC  #9                 ; two digits 10 - 15
DeUn_20 AND  #15
        RTS


***********
Unit_Dialog
***********

        LDX  #<Unit_Buf
        LDY  #>Unit_Buf
        JSR  Got_Line
        LDX  Unit_Text          ; 10
        LDA  Unit_Text+1        ;  1
        JSR  Decode_Unit
        STA  Save_Unit
        RTS

***************
Wait_for_Return
***************

        LDA  #<PRESSRET
        LDY  #>PRESSRET
        LDX  #[SAVEUNIT-PRESSRET]
        JSR  PrintText
        JSR  Empty_Keyboard_Queue
wfret_10  JSR Get_Character
        CMP  #CR
        BEQ  wfret_99
        JSR  Error_Beep
        BRA  wfret_10
wfret_99  RTS

***********
File_Dialog
***********

        LDX  #<File_Buf
        LDY  #>File_Buf
        JMP  Got_Line

****************
Read_Disk_Status
****************
        TXA
        PHA
        LDA  FA
        JSR  TALK
        LDA  #$6f
        JSR  TKSA
        LDY  #0
        STY  IO_STATUS
RDS_10  JSR  ACPTR              ; read status
        LDX  IO_STATUS
        BNE  RDS_30
        STA  DSTATUS,Y
        CMP  #' '
        BCC  RDS_20
        INY
        CPY  #40
        BCC  RDS_10
RDS_20  JSR  UNTLK              ; close channel
RDS_30  LDA  #CR
        STA  DSTATUS,Y
        LDA  #0
        STA  DSTATUS+1,Y
        PLA
        TAX
        LDA  DSTATUS
        AND  #15                ; return with 1st. value
        RTS

**********
z_ext_save
**********
        JSR  Save_Game
        LDA  #1
        JMP  Store_Byte
******
z_save
******

        JSR  Save_Game
        JMP  Main_True

****************
Module Save_Game
****************
        Print(SAVEUNIT)
        JSR  Unit_Dialog
        Print(SAVEFILE)
        JSR  File_Dialog

        LDA  Z_HEADER+2         ; save config
        STA  Z_VAR+$20
        LDA  Z_HEADER+3
        STA  Z_VAR+$21
        LDX  #3
WG_05   LDA  z_stack_ptr,X      ; save stack pointer
        STA  Z_VAR+$22,X        ; and  frame pointer
        DEX
        BPL  WG_05
        LDX  #2
WG_10   LDA  QI0,X              ; save IP
        STA  Z_VAR+$26,X
        DEX
        BPL  WG_10
        INX                     ; X = 0
        STX  IO_STATUS          ; clear status
        LDA  Save_Unit
        STA  FA
        JSR  LISTEN             ; open Pos,Device,3
        LDA  #$f3
        JSR  SECOND
        LDY  #0
WG_20   LDA  File_Text,Y
        CMP  #' '
        BCC  WG_30
        CMP  #'a'
        BCC  WG_25
        SBC  #$20
WG_25   JSR  CIOUT
        INY
        BNE  WG_20

WG_30   LDA  #','
        JSR  CIOUT
        LDA  #'W'
        JSR  CIOUT
        JSR  UNLSN
        LDA  IO_STATUS
        BNE  WG_Err

        LDA  FA
        JSR  LISTEN
        LDA  #$63
        JSR  SECOND
        LDA  #>Z_VAR            ; Save variables & stack
        STA  RAMD+1
        LDX  #>[Z_STACK_HI - Z_VAR]; # of blocks
WG_40   JSR  Write_Block
        LDA  IO_STATUS
        BNE  WG_Err
        DEX
        BNE  WG_40
        LDA  #>Z_HEADER
        STA  RAMD+1
        LDX  h_dynamic_size_hi
        INX                     ; # of blocks
WG_50   JSR  Write_Block
        LDA  IO_STATUS
        BNE  WG_Err
        DEX
        BNE  WG_50
        JSR  UNLSN
        JMP  Close_File

WG_Err  JSR  UNLSN
        JSR  Read_Disk_Status
        Print(DSTATUS)
        JMP  Close_File
EndMod

***********
Write_Block
***********

        LDY  #0
        STY  X0L
        STY  IO_STATUS
WrBl_10 LDA  (RAMD),Y
        JSR  CIOUT
        INY
        BNE  WrBl_10
        INC  RAMD+1
        LDA  #'.'
        JMP  CHROUT

*********
z_restore
*********

        JSR  Restore_Game
        JMP  Main_True

*************
z_ext_restore
*************

        JSR  Restore_Game
        LDA  #2
        JMP  Store_Byte

*******************
Module Restore_Game
*******************

        Print(LOADUNIT)
        JSR  Unit_Dialog
        Print(LOADFILE)
        JSR  File_Dialog

        LDX  #$1f               ; Save local variables
_saveloc  LDA Z_VAR,X           ; to stack bottom -
        STA  Charbuf,X          ; if restoring fails
        DEX                     ; we can retrieve them
        BPL  _saveloc
        LDA  Save_Unit
        STA  FA
        JSR  LISTEN             ; open Pos,Unit,3
        LDA  #$f3
        JSR  SECOND
        LDY  #0
_fn_loop  LDA File_Text,Y
        CMP  #' '
        BCC  _fn_done
        CMP  #'a'
        BCC  _no_conv
        SBC  #$20               ; convert to lower case
_no_conv  JSR CIOUT
        INY
        BPL  _fn_loop
_fn_done  JSR UNLSN

; Check drive error channel

        LDA  Save_Unit
        STA  FA
        JSR  TALK
        LDA  #$63
        JSR  TKSA
        LDA  #>Z_VAR
        STA  RAMD+1
        JSR  Read_Block         ; Read Z_VAR
        LDA  Z_VAR+$20
        CMP  Z_HEADER+2         ; correct version ?
        BNE  _cancel            ; -> nope
        LDA  Z_VAR+$21
        CMP  Z_HEADER+3
        BEQ  _ver_ok            ; OK -> continue restoring
_cancel LDX  #$1f               ; Restore local variables
_resloop  LDA Charbuf,X         ; and abort restoring
        STA  Z_VAR,X
        DEX
        BPL  _resloop
        JSR  Reset_Screen
        JMP  Main_False

_ver_ok LDA  h_flags_hi         ; save flags
        PHA
        LDA  h_flags_lo
        PHA

        LDZ  #>[Z_STACK_HI - Z_STACK_LO] ; stack pages
_st_loop  JSR Read_Block
        DEZ
        BNE  _st_loop

        LDA  #>Z_HEADER
        STA  RAMD+1
        LDZ  h_dynamic_size_hi
        INZ                     ; # of blocks
_hd_loop  JSR Read_Block
        DEZ
        BNE  _hd_loop

        PLA                     ; restore flags
        STA  h_flags_lo
        PLA
        STA  h_flags_hi

        LDX  #3
_stack  LDA  Z_VAR+$22,X
        STA  z_stack_ptr,X
        DEX
        BPL  _stack

        LDX  #2
_PC     LDA  Z_VAR+$26,X
        STA  QI0,X
        DEX
        BPL  _PC
        JSR  UNTLK              ; fall through
EndMod


*****************
Module Read_Block
*****************

        LDY  #0
        STY  IO_STATUS
_loop   JSR  ACPTR
        STA  (RAMD),Y
        INY
        BNE  _loop
        INC  RAMD+1
        LDA  #'.'
        JMP  CHROUT
EndMod


********
* DATA *
********

Row_Lo  .BYTE <[SCREEN]
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

Row_Hi  .BYTE >[SCREEN]
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

L_Encode_A  .BYTE 0             ;
L_Encode_Y  .BYTE 0             ;

; data for separators / delimiters in parsing

Sep_Std_List    .BYTE "!?,.\r " ; built in standard
Z_Arg_Count     .BYTE 0         ; argument count
Z_Buffer_Mode   .BYTE 1         ; output buffering on or off
Z_Active_Window .BYTE 0
Z_Active_Font   .BYTE 0,0

******************************
* Cursor, Windows and Screen *
******************************

******************
Set_Screen_Pointer
******************

        LDX  Cursor_Row

********************
Set_Screen_Pointer_X
********************

        LDA  Row_Lo,X
        STA  Scr_Adr
        STA  Col_Adr
        LDA  Row_Hi,X
        STA  Scr_Adr+1
        AND  #7
        STA  Col_Adr+1
        RTS


***********
Window_Home
***********

        LDX  Upper_Size
        LDY  #0

***********
Set_Row_Col
***********

; Input : X = screen row    (0 .. 24)
;         Y = screen column (0 .. 79)

        CPX  #ROWS
        BCC  SRC_10
        LDX  #ROWS-1
SRC_10  CPY  #COLS
        BCC  SRC_20
        LDY  #COLS-1

SRC_20  STX  Cursor_Row
        STY  Cursor_Col
        JSR  Set_Screen_Pointer

***********
Get_Row_Col
***********

; Output: X = screen row    (0 .. 24)
;         Y = screen column (0 .. 79)

        LDX  Cursor_Row
        LDY  Cursor_Col
        RTS

*********
Info_Size
*********
        PHY
        JSR  Format_Integer
        PLY
        LDX  #1
InSi_10 LDA  NUMBER,X
        STA  InfoPro+21,Y
        INY
        INX
        CPX  #5
        BCC  InSi_10
        RTS

*******************
Module Screen_Setup
*******************

        LDA  #$0F               ; colour RAM: $FF80000
        STA  Col_Adr+3
        LDA  #$F8
        STA  Col_Adr+2
        RTS
EndMod

******************
Module Story_Pages
******************

; Leave story size in X0L/X0H for printing

        LDA  #0
        STA  X0H
        LDA  h_file_size_hi     ; size in words high
        STA  X0L
        LDA  h_file_size_lo     ; size in words low
        ASL  A
        ROW  X0L                ; size * 2 (version 3)
        BBR7 Version,_round
        ASL  A
        ROW  X0L                ; size * 4 (version 4 and 5)
_round  TAX
        BEQ  _return            ; at page boundary
        INW  X0L                ; add 1 to round up
_return RTS
EndMod

****************
Module Modify_Op
****************
        LDA  #<z_not
        STA  ZV8F
        LDA  #>z_not
        STA  ZV8F+1
        LDA  #<z_pop
        STA  ZVB9
        LDA  #>z_pop
        STA  ZVB9+1
        RTS
EndMod

**********************
Module Version_4_Setup
**********************
        JSR  Modify_Op
        RTS
EndMod

**********************
Module Version_5_Setup
**********************
        LDA  #9
        STA  Word_Length
        LDA  #6
        STA  Vocab_Length
        LDA  #$3f
        STA  Prop_Mask
        LDA  #ROWS-1
        STA Win_Bot
        RTS
EndMod

****************
Module z_restart
****************

        CLD
        LDX  #$fb               ; default stack initialisation
        TXS
        LDA  #$36
        STA  R6510
        LDA  #0                 ; clear ZP variables
        LDX  #Z_Code            ; start of interpreter variables
_clear  STA  0,X
        INX
        CPX  #ZP_CLEAR
        BCC  _clear
        LDA  FG_Color
        STA  Colour
        LDA  #1
        STA  Upper_Size         ; status line for version 3
        LDA  #>Z_STACK_LO
        STA  z_stack_ptr+1
        STA  z_frame_ptr+1
        DEC  Status_Col         ; -1
        JSR  Version_5_Setup    ; default settings
        JSR  Open_Story         ; open  8,8,8,"z*"
        LDA  #>Z_HEADER
        STA  RAMD+1
        JSR  Load_Page          ; load first block to Z_HEADER

; version dependant initialisation

        LDA  h_version          ; allow 3 >= version <= 5
        CMP  #4
        BCC  _V3
        ORA  #$80               ; version > 3
_V3     STA  Version
        CMP  #$85
        BCS  _config            ; version 5 = default
        CMP  #$84
        BNE  _check3
        JSR  Version_4_Setup
        BRA  _config
_check3 CMP  #3
        BEQ  _set3
        Print(NOSTORY)
        JMP  quit_20

; Version 3 initialisation

_set3   LDA  #6
        STA  Word_Length
        LDA  #4
        STA  Vocab_Length
        LDA  #$1f
        STA  Prop_Mask
        JSR  Modify_Op
        LDA  #' '
        STA  Z_STATUS-1

_config LDX  #>[$D000 - Z_HEADER]
        STX  Resident_Pages     ; reserved
        LDX  #COLS
        STX  h_screen_cols
        STX  h_screen_width_lo
        LDX  #ROWS
        STX  h_screen_rows
        STX  h_screen_height_lo
        LDA  #0
        STA  h_screen_width_hi
        STA  h_screen_height_hi

        LDA  h_config
        BBS7 Version,_V4_Flags
        ORA  #%0011 0000
;               0--- ---- 7:unused
;                0-- ---- 6:prop font
;                 1- ---- 5:split screen
;                  1 ---- 4:status available
;                    0--- 3:unused
;                     0-- 2:file split(1)
;                      0- 1:score(0) or time(1)
;                       0 0:unused
        BRA  _setconf
_V4_Flags ORA #%1001 1001
;               1--- ---- 7:timed input
;                0-- ---- 6:unused
;                 0- ---- 5:sound
;                  1 ---- 4:fixed  font
;                    0--- 3:italic font
;                     0-- 2:bold   font
;                      0- 1:pictures
;                       1 0:colors
_setconf  STA h_config
        LDA  #%0000 1100
        ORA  h_flags_lo
        STA  h_flags_lo
        LDA  #1
        STA  h_font_width
        STA  h_font_height
        LDA  #7                 ; 6:PC 7:C128 8:C64
        STA  h_interpreter_number
        LDA  #'G'
        STA  h_interpreter_version

; -------- compute story location and size -----

        JSR  Story_Pages
        LDY  #InfoSto-InfoPro
        JSR  Info_Size
        JSR  Clear_Screen
        Print(BITSHIFTER)
        JSR  Select_Text_Window
        LDA  #<InfoClr
        LDY  #>InfoClr
        LDX  #[InfoEnd - InfoClr]
        JSR  PrintText

_res    JSR Load_Page           ; Load resident area
        LDA  IO_STATUS
        BNE  _story             ; EOF
        LDA  Block_Lo
        AND  #15
        BNE  _dot
        LDA  #'.'
        JSR  CHROUT
_dot    LDA  Block_Lo
        CMP  Resident_Pages
        BCC  _res

        JSR  Set_Attic_RAM
_story  JSR  Load_Story
        JSR  Wait_for_Return
        JSR  Set_Mode_80
        JSR  Screen_Setup
        LDA  h_start_pc_hi      ; Initialize pc
        STA  QI1
        LDA  h_start_pc_lo
        STA  QI0
        JSR  Reset_Screen
        JMP  PC_LOOP
EndMod

*****************
Module Next_Datum
*****************

; Input
; =====
; QD0 = Block pointer
; QD1 = Block # lo
; QD2 = Block # hi

; Output
; ======
; (A) = Byte from (Block),QD0
; 24 bit Pointer QD incremented

        PHX
        LDZ  QD0                ; page index
        LDA  QD1                ; page low
        LDX  QD2                ; page high
        BNE  _banked            ; > 64 K
        STX  RAMD+2             ; bank = 0
        STX  RAMD+3             ; segment = 0
        CMP  Resident_Pages
        BCS  _banked            ; -> not resident
        ADC  #>Z_HEADER
        STA  RAMD+1
        BRA  _get

; load if address is beyond resident part
; address = (QD1/2) - Resident + bank

_banked STA  RAMD+1             ; page low
        LDA  Segment
        STA  RAMD+3             ; segment
        LDA  Bank_Order,X
        STA  RAMD+2
        CMP  #1
        BNE  _get
        LDA  RAMD+1
        SBC  #8                 ; don't use $1F800 -> $1FFFF
        STA  RAMD+1
_get    LDA  [RAMD],Z
        INC  QD0
        BNE  _exit
        INW  QD1
_exit   PLX
        CMP  #0                 ; set flags
        RTS

***********************
Module Next_Instruction
***********************

; Input
; =====
; QI0 = pointer inside page
; QI1 = page # low
; QI2 = page # high

; Output
; ======
; (A) = Byte from 24 bit address (QI)
; 24 bit Pointer QI incremented

        PHX
        PHZ
        LDZ  QI0                ; page index
        LDA  QI1                ; page low
        LDX  QI2                ; page high
        BNE  _banked            ; > 64 K
        STX  RAMD+2             ; bank    = 0
        STX  RAMD+3             ; segment = 0
        CMP  Resident_Pages
        BCS  _banked            ; -> not resident
        ADC  #>Z_HEADER
        STA  RAMD+1
        BRA  _get

; load if address is beyond resident part
; address = (QI1/2) - Resident + bank

_banked STA  RAMD+1             ; page low
        LDA  Segment
        STA  RAMD+3             ; segment
        LDA  Bank_Order,X
        STA  RAMD+2
        CMP  #1
        BNE  _get
        LDA  RAMD+1
        SBC  #8
        STA  RAMD+1
_get    LDA  [RAMD],Z
        INC  QI0
        BNE  _exit
        INW  QI1
_exit   PLZ
        PLX
        CMP  #0                 ; set flags
        RTS

*************
z_show_status
*************

; save cursor coordinates, print to statusline

        JSR  Save_Cursor

; push QD2, QD1, QD0, QDH, QDL

        LDX  #4
zss_10  LDA  QDL,X
        PHA
        DEX
        BPL  zss_10

        INX                     ; X = 0
        STX  Status_Col         ; switch decoder to status line
        LDA  #$10               ; get location
        JSR  Get_Global_Var
        LDA  X0L
        LDX  X0H
        JSR  z_print_obj_A
        LDA  #' '               ; fill rest of line with blanks
        LDX  Status_Col
zss_20  STA  Z_STATUS,X         ; erase rest of line
        INX
        CPX  #COLS
        BCC  zss_20

        LDX  #$ff
        STX  Status_Col         ; reset decoder
        LDA  h_config
        AND  #2                 ; Score_Time_Flag
        BNE  zss_30
        JSR  Print_Score
        JMP  zss_40
zss_30  JSR  Print_Time
zss_40  JSR  Print_Status

; pull QDL, QDH, QD0, QD1, QD2 after recursive call

        LDX  #0
zss_90  PLA
        STA  QDL,X
        INX
        CPX  #5
        BCC  zss_90

        JMP  Restore_Cursor

****************
Module Cursor_On
****************
        PHA
        PHZ
        LDZ  Cursor_Col
        LDA  [Col_Adr],Z
        ORA  #UNDERLINE
        STA  [Col_Adr],Z
        PLZ
        PLA
        RTS
EndMod

*****************
Module Cursor_Off
*****************
        PHP
        PHA
        PHZ
        LDZ  Cursor_Col
        LDA  [Col_Adr],Z
        AND  #$7f
        STA  [Col_Adr],Z
        PLZ
        PLA
        PLP
        RTS
EndMod

************
Special_Keys
************

        .BYTE CURSOR_UP         ; $81
        .BYTE CURSOR_DOWN       ; $82
        .BYTE CURSOR_LEFT       ; $83
        .BYTE CURSOR_RIGHT      ; $84

*********************
Module Translate_Keys
*********************

        LDY  #3
CSK_10  CMP  Special_Keys,Y
        BEQ  CSK_20
        DEY
        BPL  CSK_10
        CLC                     ; unchanged
        RTS
CSK_20  TYA                     ; C=1
        ADC  #$80               ; translate
        SEC                     ; translated
        RTS
EndMod

********************
Module Get_Character
********************

; get a character from keyboard
; allow all ASCII characters $20 - $7e
; allow control codes CR, INS,DEL and cursor keys
; return 0 for timeout (Timeout != 0)

        PHY                     ; save Y
_loop   JSR  GETIN
        BNE  _got               ; read a key
        LDA  Timeout
        ORA  Timeout+1
        BEQ  _loop
        LDA  Timeout
        CMP  Jiffy
        LDA  Timeout+1
        SBC  Jiffy+1
        BCS  _loop
        LDA  #0                 ; timeout
        BRA  _exit

_got    CMP  #CR
        BEQ  _exit              ; allow CR
        CMP  #DEL
        BEQ  _exit              ; allow DEL
        CMP  #INS
        BEQ  _exit              ; allow INS
        JSR  Translate_Keys
        BCS  _exit
        CMP  #'Z'+$81           ; CBM 'Z'+1
        BCS  _error             ; -> not ASCII
        CMP  #'A'+$80           ; CBM 'A'
        BCC  _lower
        AND  #$7f               ; to ASCII
        BRA  _exit

_lower  CMP  #'Z'+1             ; CBM 'z'+1
        BCS  _error             ; -> not ASCII
        CMP  #'A'               ; CBM 'a'
        BCC  _ctrl
        ADC  #$1f               ; to ASCII
        BRA  _exit

_ctrl   CMP  #' '
        BCS  _exit

_error  JSR  Error_Beep         ; unacceptable
        BRA  _loop

_exit   PLY                     ; restore Y
        RTS
EndMod

*************************
Module Scroll_Main_Window
*************************

        LDA  #$f8
        STA  Col_Row+2          ; colour bank low
        LDA  #$0f
        STA  Col_Row+3          ; colour bank high
        LDX  Upper_Size         ; top row

_row_loop JSR Set_Screen_Pointer_X
        INX
        LDA  Row_Lo,X
        STA  Scr_Row
        STA  Col_Row
        LDA  Row_Hi,X
        STA  Scr_Row+1          ; screen high
        AND  #7
        STA  Col_Row+1          ; colour high
        LDZ  #COLS-1

_col_loop LDA (Scr_Row),Z
        STA  (Scr_Adr),Z
        LDA  [Col_Row],Z
        STA  [Col_Adr],Z
        DEZ
        BPL  _col_loop

        CPX  #ROWS-1
        BCC  _row_loop
        JMP  Erase_Row
EndMod

******************
Module Home_Screen
******************

        PHX
        LDX  #0
        STX  Cursor_Col
        STX  Cursor_Row
        JSR  Set_Screen_Pointer_X
        PLX
        RTS
ENDMOD

*******************
Module Clear_Screen
*******************

        PHX
        LDX  #ROWS-1
_loop   JSR  Erase_Row
        DEX
        BPL  _loop
        JSR  Home_Screen
        PLX
        RTS
ENDMOD

*************
Return_Screen
*************
        PHX
        PHY
        LDY  #0
        STY  Font_Sel
;         STY Attribute
        STY  Cursor_Col
        LDX  Cursor_Row
        INX
        CPX  #ROWS
        BCC  ReSc_10
        JSR  Scroll_Main_Window
        LDX  #ROWS-1
ReSc_10 STX  Cursor_Row
        JSR  Set_Screen_Pointer_X
        PLY
        PLX
        RTS

****************
Module Edit_Left
****************
        DEY
        JSR  Cursor_Off
        DEC  Cursor_Col
        JMP  Cursor_On
EndMod

*****************
Module Edit_Right
*****************
        INY
        JSR  Cursor_Off
        INC  Cursor_Col
        JMP  Cursor_On
EndMod

***************
Module Edit_Del
***************
        LDZ  Cursor_Col
        JSR  Edit_Left

_loop   LDA  (Scr_Adr),Z
        DEZ
        STA  (Scr_Adr),Z
        INZ
        INZ
        CPZ  #COLS
        BCC  _loop
        DEZ
        LDA  #' '
        STA  (Scr_Adr),Z
        RTS
EndMod

***************
Module Edit_Ins
***************
        LDZ  Right_Margin
_loop   DEZ
        LDA  (Scr_Adr),Z
        INZ
        STA  (Scr_Adr),Z
        DEZ
        CPZ  Cursor_Col
        BNE  _loop
        LDA  #' '
        STA  (Scr_Adr),Z
        RTS
EndMod

*************
Module CHROUT
*************

        CMP  #CR
        BEQ  Return_Screen

        PHX
        PHY
        PHZ
        PHA
        LDZ  Cursor_Col
        CPZ  #COLS
        BCC  CHRO_10
        LDZ  #COLS-1
        STZ  Cursor_Col
CHRO_10 LDA  Colour
        ORA  Attribute
        STA  [Col_Adr],Z
        PLA
        ORA  Font_Sel
        STA  (Scr_Adr),Z
        INC  Cursor_Col
        PLZ
        PLY
        PLX
        RTS
ENDMOD

*******************
Module Prompt_Color
*******************
        LDA  #CYAN
        BRA  Set_Colour
EndMod

*****************
Module Text_Color
*****************
        LDA  #WHITE
EndMod

*****************
Module Set_Colour
*****************
        BBS7 Version,_return
        STA  Colour
_return RTS
EndMod

**********************
Module Timeout_Routine
**********************
        PHY
        PHZ
        SEI
        LDX  #0
        STX  Jiffy
        STX  Jiffy+1
        STX  Jiffy+2
        CLI
_save   LDA  X1L,X              ; push X1L,X1H,X2L,X2H
        PHA
        INX
        CPX  #4
        BCC  _save
        LDA  ParNum
        PHA                     ; push ParNum
        LDA  #1
        STA  ParNum
        LDA  Cursor_Col
        PHA                     ; push Cursor_Col

        JSR  Cursor_Off
        LDA  Z_Timeout
        STA  X1L
        LDA  Z_Timeout+1
        STA  X1H
        LDA  #$80               ; call type
        JSR  call_00
        JSR  PC_LOOP
        LDY  X1L                ; return value

        PLA                     ; pull Cursor_Col
        STA  Cursor_Col
        JSR  Cursor_On
        PLA                     ; pull ParNum
        STA  ParNum
        LDX  #3
_rest   PLA                     ; pull X2H,X2L,X1H,X1L
        STA  X1L,X
        DEX
        BPL  _rest
        TYA                     ; return value
        PLZ
        PLY
        RTS
EndMod

***************
Module Got_Line
***************

; Get line from keyboard with preset text

        LDA  #0
        STA  Timeout
        STA  Timeout+1
        STX  X1L
        STY  X1H
        LDA  Version
        PHA
        LDY  Cursor_Col
        LDZ  #1
        STZ  Version
        JSR  Prompt_Color
_loop   LDA  (X1L),Z
        CMP  #' '
        BCC  _call
        JSR  CHROUT
        INZ
        BPL  _loop
_call   STY  Cursor_Col
        JSR  Get_Line_X1
        PLA
        STA  Version
        RTS
EndMod

; Simple line editor, which gets a line and stores it in (X1)
; On entry byte 0 is expected to contain the maximum string length
; On exit byte 1 holds the actual string length for versions > 3
; The string is zero terminated.
; Chars_Left is set to the actual string length

; RETURN  : editing stops, the string is created from screen row
; DEL     : delete character under cursor and scroll remaining row
; INS     : insert blank at cursor
; LEFT    : move cursor left
; RIGHT   : move cursor right
; A - Z   : converted to a - z

******************
Module Get_Line_X1
******************

        LDA  Upper_Size
        STA  MORE_Counter
        LDA  X1L                ; buffer pointer
        STA  GLL
        LDA  X1H
        STA  GLH
        INW  GLL                ; point to content
        JSR  Prompt_Color
        BBR7 Version,_lm
        INW  GLL                ; point to content
_lm     LDA  Cursor_Col
        STA  Left_Margin
        TAY                     ; Y = left margin
        LDZ  #0
        STZ  TO_Flag
        CLC
        ADC  (X1L),Z            ; add maximum length
        CMP  Left_Margin        ; added zero ?
        BEQ  _max
        CMP  #COLS-1
        BCC  _rm
_max    LDA  #COLS-1
_rm     STA  Right_Margin
        BRA  _loop

_error  JSR  Error_Beep

; receive character from keyboard

_loop   JSR  Cursor_On
        JSR  Get_Character
        JSR  Cursor_Off
        CMP  #CR
        BEQ  _end               ; -> CR = end of input
        CMP  #0
        BNE  _del

; timeout

        JSR  Timeout_Routine
        CMP  #0
        BEQ  _loop              ; carry on
        LDA  #0
        STA  Chars_Left
        JSR  Text_Color
        LDA  #0
        LDY  #1
        STY  TO_Flag
        STA  (X1L),Y            ; length of string
        RTS

; DEL key

_del    CMP  #DEL
        BNE  _ins
        CPY  Left_Margin
        BEQ  _error
        JSR  Edit_Del
        BRA  _loop

; INS key

_ins    CMP  #INS
        BNE  _left
        CPY  Right_Margin
        BCS  _error
        JSR  Edit_Ins
        BRA  _loop

; cursor left

_left   CMP  #KEY_LEFT
        BNE  _limit
        CPY  Left_Margin
        BEQ  _error
        JSR  Edit_Left
        BRA  _loop

; check edit limit

_limit  CPY  Right_Margin
        BCS  _error             ; -> at limit

; cursor right

        CMP  #KEY_RIGHT
        BNE  _valid
        JSR  Edit_Right
        BRA  _loop

; unsupported keys

_valid  CMP  #' '
        BCC  _error
        CMP  #$7f
        BCS  _error

; upper case to lower case

        CMP  #'A'
        BCC  _store
        CMP  #'Z'+1
        BCS  _store
        ADC  #$20               ; to lower case
_store  JSR  CHROUT
        INY
        BPL  _loop

; skip trailing blanks

_end    LDY  Right_Margin
        LDA  #' '
_skip   CMP  (Scr_Adr),Y
        BNE  _buffer
        DEY
        CPY  Left_Margin
        BCS  _skip

; copy screen row to buffer

_buffer TYA                     ; Y = last non blank
        INC  A
        SEC
        SBC  Left_Margin        ; A = string length
        STA  Chars_Left
        TAZ                     ; Z = terminator pos
        LDA  #0
        STA  (GLL),Z            ; terminator
        DEZ
        BMI  _empty
_copy   LDA  (Scr_Adr),Y
        STA  (GLL),Z
        DEY
        DEZ
        BPL  _copy

_empty  JSR  Text_Color
        BBR7 Version,_return
        LDA  Chars_Left
        LDY  #1
        STA  (X1L),Y            ; length of string
_return JMP  Return_Screen
EndMod

*********
PrintText
*********

        STA  MEMUSS
        STY  MEMUSS+1
        LDY  #0
PrTe_A  LDA  (MEMUSS),Y
        BEQ  PrTe_B
        JSR  CHROUT
        INY
        DEX
        BNE  PrTe_A
PrTe_B  RTS

************
Reset_Screen
************

        JSR  Clear_Screen
        JSR  Return_Screen
        LDA  Upper_Size
        STA  MORE_Counter
        RTS

*****************
Module Open_Story
*****************

        LDA  Game_Unit          ; open "Z*",FA,8
        STA  FA
        JSR  LISTEN
        LDA  #$f8
        JSR  SECOND
        LDA  #'Z'
        JSR  CIOUT
        LDA  #'*'
        JSR  CIOUT
        JSR  UNLSN
        LDA  FA
        JSR  TALK               ; open channel
        LDA  #$68               ; SA = 8
        JMP  TKSA               ; select channel to disk buffer
EndMod

**********
ASCII_Hex
**********

; Output: (X) = High nibble (A) = Low nibble
        PHA
        LSR  A
        LSR  A
        LSR  A
        LSR  A
        ORA  #'0'
        CMP  #$3a
        BCC  Hex_11
        ADC  #6
Hex_11  TAX
        PLA
        AND  #15
        ORA  #'0'
        CMP  #$3a
        BCC  Hex_12
        ADC  #6
Hex_12  RTS

*************************
Module Select_Text_Window
*************************

        LDA  #1
        STA  Win_Top
        LDA  #ROWS-1
        STA  Win_Bot
        LDA  #0
        STA  Attribute
        JMP  Text_Color
EndMod

********************
Select_Status_Window
********************

        LDX  #0
        STX  Win_Top
        JMP  Set_Screen_Pointer_X

***********
Save_Cursor
***********

        JSR  Get_Row_Col
        STX  C_Save_Row
        STY  C_Save_Col
        RTS


**************
Restore_Cursor
**************

        LDX  C_Save_Row
        LDY  C_Save_Col
        JMP  Set_Row_Col

****************
Module Erase_Row
****************

        PHZ
        JSR  Set_Screen_Pointer_X
        LDZ  #COLS-1
_loop   LDA  #' '
        STA  (Scr_Adr),Z
        LDA  #WHITE
        STA  [Col_Adr],Z
        DEZ
        BPL  _loop
        PLZ
        RTS
EndMod

********
ASCII_TS
********

; Convert binary number in (A) to
; two decimal digits in (X) and (A)

        LDX  #'0'-1
        SEC
asts_01 INX
        SBC  #10
        BCS  asts_01
        ADC  #$3a
        RTS

****************
Module Load_Page
****************

; Read 256 bytes of data from disk and store them in RAM
; at the address (RAMD) = 32 bit address.
; The I/O routines TALK, TKSA, ACPTR, and UNTLK are used
; ACPTR sets the STATUS bit 6 ($40) on error

        PHX
;  LDA RAMD+3
;  JSR Dump_A
;  LDA RAMD+2
;  JSR Dump_A
;  LDA RAMD+1
;  JSR Dump_A
;  LDA #' '
;  JSR CHROUT
;  LDA Cursor_Col
;  CMP #75
;  BCC _weiter
;  JSR Return_Screen
;_weiter
        LDZ  #0
        STZ  IO_STATUS
_loop   JSR  ACPTR
        STA  [RAMD],Z
        LDA  IO_STATUS
        BNE  _eof
        INZ
        BNE  _loop
_eof    INW  Block_Lo           ; increment block word Block_Lo/Hi
        INC  RAMD+1             ; advance RAM pointer page
        PLX
        RTS                     ; return OK
EndMod

*****************
Module Load_Story
*****************

; Continue story loading in banks

        LDA  Segment
        STA  RAMD+3
_next   LDA  Block_Lo
        STA  RAMD+1
        LDX  Block_Hi
        LDA  Bank_Order,X
        STA  RAMD+2
        CMP  #1
        BNE  _loop
        LDA  RAMD+1
        SBC  #8
        STA  RAMD+1
_loop   JSR  Load_Page
        LDA  IO_STATUS
        BNE  _eof
        LDA  Block_Lo
        AND  #15
        BNE  _prog
        LDA  #'.'
        JSR  CHROUT
_prog   LDX  Block_Hi
        CPX  #8
        BCC  _next
_eof    JSR  UNTLK
        LDA  Game_Unit          ; close #8
        STA  FA
        JSR  LISTEN
        LDA  #$e8
        JSR  SECOND
        JMP  UNLSN
EndMod

**********
Close_File
**********

        LDA  FA
        JSR  LISTEN
        LDA  #$e3
        JSR  SECOND
        JMP  UNLSN

*************
Delete_Config
*************

        LDA  Game_Unit
        STA  FA
        JSR  LISTEN
        LDA  #$6f
        JSR  SECOND
        LDY  #0
DeCo_10 LDA  ConfigDel,Y
        JSR  CIOUT
        INY
        CPY  #[?ConfigDel + ?Configname]
        BCC  DeCo_10
        JMP  UNLSN

***********
Prep_Config
***********

        LDA  #<CONFIG_START
        LDY  #>CONFIG_START
        STA  A0L
        STY  A0H
        LDA  #<CONFIG_END
        LDY  #>CONFIG_END
        STA  A1L
        STY  A1H
        LDA  #?Configname
        LDX  #<Configname
        LDY  #>Configname
        JMP  SETNAM

***********
Save_Config
***********

        JSR  Delete_Config
        JSR  Prep_Config
        INC  FNLEN              ; add ','
        INC  FNLEN              ; add 'W'
        JMP  Save_File

********************
Module Send_Filename
********************

        LDA  FA
        JSR  LISTEN
        LDA  #$f3
        JSR  SECOND
        LDY  #0
        STY  IO_STATUS
_fname  LDA  (FNADR),Y
        JSR  CIOUT
        INY
        CPY  FNLEN
        BCC  _fname
        JMP  UNLSN
EndMod

******************
Module Load_Config
******************

        JSR  Prep_Config
EndMod

****************
Module Load_File
****************

; Input : (A0) = start address
;       : (A1) = end   address
;       : SETNAM was called

        JSR  Send_Filename
        LDA  FA
        JSR  TALK
        LDA  #$63
        JSR  TKSA
        LDY  #0
_loop   JSR  ACPTR
        LDX  IO_STATUS
        BNE  _stop
        STA  (A0L),Y
        INW  A0L
        LDA  A0L
        CMP  A1L
        LDA  A0H
        SBC  A1H
        BCC  _loop

_stop   JSR  UNTLK
        JMP  Close_File
EndMod

****************
Module Save_File
****************

; Input : (A0) = start address
;       : (A1) = end   address
;       : SETNAM was called

        JSR  Send_Filename
        LDA  FA
        JSR  LISTEN
        LDA  #$63
        JSR  SECOND
        LDY  #0
_loop   LDA  (A0L),Y
        JSR  CIOUT
        INW  A0L
        LDA  A0L
        CMP  A1L
        LDA  A0H
        SBC  A1H
        BCC  _loop

        JSR  UNLSN
        JMP  Close_File
EndMod

Bank_Order .BYTE 1,4,5,3,6,7,8,9

Z_Mem_Flag      .BYTE 0
Z_Timeout       .WORD 0

; ------- data for unit dialog --------
Unit_Buf  .BYTE 2               ; input length
Unit_Text .BYTE "08",0
; ------- data for file name dialog----
File_Buf  .BYTE 20              ; input length
File_Text .BYTE "savename",0
          .FILL 12 (0)
Game_Unit .BYTE 8

PRESSRET  .BYTE "\rPress <RETURN> to continue."
SAVEUNIT  .BYTE "Save to unit:"
SAVEFILE  .BYTE "Save to file:"
LOADUNIT  .BYTE "Restore from unit:"
LOADFILE  .BYTE "Restore from file:"
MORE      .BYTE " MORE "
EOS       .BYTE "End of session - power off"
NOSTORY   .BYTE "NO Z STORY"
Msg_Attic .BYTE "\rUsing attic RAM at $0800-0000\r"
BITSHIFTER .BYTE "BIT SHIFTER 26-OCT-2020\r"
InfoClr .BYTE CR
InfoPro .BYTE 'Program: '
        .HEX4 START
        .BYTE ' - '
        .HEX4 EOP
        .BYTE ' '
        .DEC4 [Z_HEADER - START + 1] >> 8
        .BYTE ' Pages\r'

Bank0   .BYTE 'Bank  0: '
        .HEX4 Z_HEADER
        .BYTE ' - '
        .HEX4 $CFFF
        .BYTE ' '
        .DEC4 [$D000 -Z_HEADER] >> 8
        .BYTE ' Pages\r'

Bank1   .BYTE 'Bank  1: '
        .HEX4 $C800 - Z_HEADER
        .BYTE ' - '
        .HEX4 $F7FF
        .BYTE ' '
        .DEC4 [Z_HEADER + $3000] >> 8
        .BYTE ' Pages\r'

InfoSto .BYTE 'Story  :        Size    0 Pages\r',0
InfoEnd

BLANKS  .BYTE "            "

STAT_SCORE = 52
MOVES_COL  = 22

SCORE   .BYTE "Score: 0       Moves: 0     "
STIME   .BYTE "Time: 00:00 "
SCORE_COL  =  7
STIME_COL  =  6

INTERR  .BYTE " Error 00 "

ConfigDel  .BYTE "S0:"          ; prefix for scratch command
Configname .BYTE "CONFIG"
        .BYTE ",W"              ; postfix for write command

CONFIG_START
FG_Color   .BYTE WHITE          ; f1 133
TI_Color   .BYTE YELLOW         ; f3 134
BO_Color   .BYTE BLUE           ; f5 135
BG_Color   .BYTE BLUE           ; f2 137
TB_Color   .BYTE RED            ; f4 138
        .BYTE 0                 ; f6 139
        .BYTE 0                 ; f8 140
CONFIG_END

***
EOP
***

*******************
Module Load_Charset
*******************

* Copy charset from program to $0FF7 E000

        LDA  #<Charset
        STA  LV0
        LDA  #>Charset
        STA  LV1
        LDA  #$00
        STA  X7L
        LDA  #$E0
        STA  X7H
        LDA  #$F7
        STA  X8L
        LDA  #$0F
        STA  X8H
        LDX  #8
        LDZ  #0
_loop   LDA  (LV0),Z
        STA  [X7L],Z
        INZ
        BNE  _loop
        INC  LV1
        INC  X7H
        DEX
        BNE  _loop
        RTS
EndMod

*******
Charset
*******
        .INCLUDE "Zchar.asm"

**************
Module Mode_65
**************

; move complete program from $2000 -> $1000

        LDA  #65                ; 40MHz CPU
        STA  0

        LDY  #0
        STY  A0L
        STY  A1L
        LDA  #$20               ; loaded at $2001
        STA  A0H
        LDA  #>START
        STA  A1H

_loop   LDA  (A0L),Y
        STA  (A1L),Y
        INY
        BNE  _loop
        INC  A0H
        INC  A1H
        LDA  A1H
        CMP  #>EOC+1
        BCC  _loop
        JMP  MEGA_Setup
EndMod

***
EOC
***
