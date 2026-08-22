; nasm itsy-linux.asm -fbin -l itsy-linux.lst -o itsy-linux && chmod +x itsy-linux

%define link 0
%define immediate 080h

%macro head 4
%%link dd link
%define link %%link
%strlen %%count %1
db %3 + %%count, %1
xt_ %+ %2 dd %4
%endmacro

%macro primitive 2-3 0
head %1, %2, %3, $ + 4
%endmacro

%macro colon 2-3 0
head %1, %2, %3, docolon
%endmacro

%macro variable 3
head %1, %2, 0, dovar
val_ %+ %2 dd %3
%endmacro

%define TEXTORG 0x00400000
%define MEMSIZE 1048576
%define TIBSIZE 256
%define INBUFSIZE 256               ; private kernel input buffer
%define STACKSIZE 4096
%define TIBPTR TEXTORG + MEMSIZE - TIBSIZE
%define INBUF  TIBPTR - INBUFSIZE   ; private buffer just below TIB
%define SP0    INBUF - 4
%define RP0    SP0 - STACKSIZE

BITS 32
        org     TEXTORG

ehdr:                           ; Elf32_Ehdr
        db   0x7F, "ELF", 1, 1, 1, 0     ; e_ident
        times 8 db   0
        dw   2                  ; e_type
        dw   3                  ; e_machine
        dd   1                  ; e_version
        dd   xt_abort + 4       ; e_entry
        dd   phdr - $$  ; e_phoff
        dd   0                  ; e_shoff
        dd   0                  ; e_flags
        dw   ehdrsize   ; e_ehsize
        dw   phdrsize   ; e_phentsize
        dw   1                  ; e_phnum
        dw   0x28               ; e_shentsize without setting this size 'file' complains that elf header is corrupted
        dw   0                  ; e_shnum
        dw   0                  ; e_shstrndx

ehdrsize   equ   $ - ehdr

phdr:                           ; Elf32_Phdr
        dd   1                  ; p_type
        dd   0                  ; p_offset
        dd   $$                 ; p_vaddr
        dd   $$                 ; p_paddr
        dd   filesize   ; p_filesz
        dd   MEMSIZE    ; p_memsz
        dd   7                  ; p_flags
        dd   0x1000             ; p_align

phdrsize   equ   $ - phdr

; esp - data stack pointer
; ebp - return stack pointer
; esi - Forth instruction pointer
; ebx - TOS (top of data stack)

    variable 'state', state, 0
    variable '>in', to_in, 0
    variable '#tib', number_t_i_b, 0
    variable 'dp', dp, freemem
    variable 'base', base, 10
    variable 'last', last, final
    variable 'tib', t_i_b, TIBPTR

    ; Private input buffer state (not exposed as Forth words)
    ; in_pos  = current read index into INBUF
    ; in_len  = number of valid bytes currently in INBUF
in_pos  dd 0
in_len  dd 0

    primitive 'bye', bye
        xor ebx, ebx            ; exit code 0
        xor eax, eax
        inc eax                 ; sys_exit = 1
        int 80h

    primitive 'execute', execute
        ; ebx is TOS, so it contains address we will jump to
        mov eax, ebx ; eax is important here, it is used by docolon and dovar
        pop ebx ; we used TOS so we need to pop new value from the data stack
        jmp dword[eax] ; now we jump to the address that is stored in the eax

    primitive 'abort', abort
        mov eax, dword[val_number_t_i_b]
        mov dword[val_to_in], eax
        xor ebp, ebp
        mov dword[val_state], ebp
        ; keep private input buffer (in_pos/in_len) so already-read
        ; lines are not discarded after a failed word
        mov esp, SP0
        mov ebp, RP0
        mov esi, xt_interpret + 4
        jmp next

    primitive ',', comma
        xchg eax, ebx
        mov ebx, val_dp
        mov edi, [ebx]
        stosd
        mov [ebx], edi
        pop ebx
        jmp next

    primitive 'lit', lit
        push ebx
        lodsd
        xchg eax, ebx
        jmp next

    primitive 'drop', drop
        pop ebx
        jmp next

    primitive 'dup', dupe
        push ebx
        jmp next

    primitive 'swap', swap
        xchg ebx, [esp]
        jmp next

    primitive '+', plus
        pop eax
        add ebx, eax
        jmp next

    primitive '-', minus
        pop eax
        sub eax, ebx
        mov ebx, eax
        jmp next

    primitive '*', star
        pop eax
        imul ebx, eax
        jmp next

    primitive 'and', and_
        pop eax
        and ebx, eax
        jmp next

    primitive 'or', or_
        pop eax
        or ebx, eax
        jmp next

    primitive 'invert', invert
        not ebx
        jmp next

    primitive '0<', zero_less
        sar ebx, 31             ; arithmetic smear sign → 0 or -1
        jmp next

    ; >r ( x -- )   (R: -- x)
    primitive '>r', to_r
        xchg ebp, esp
        push ebx
        xchg ebp, esp
        pop ebx
        jmp next

    ; r> ( -- x )   (R: x -- )
    primitive 'r>', r_from
        push ebx
        xchg ebp, esp
        pop ebx
        xchg ebp, esp
        jmp next

    ; c@ ( addr -- char )
    primitive 'c@', c_fetch
        movzx ebx, byte [ebx]
        jmp next

    ; c! ( char addr -- )
    primitive 'c!', c_store
        pop eax
        mov [ebx], al
        pop ebx
        jmp next

    primitive 'exit', exit
        xchg ebp, esp
        pop esi
        xchg ebp, esp
next    lodsd
        jmp dword[eax] ; eax is later used by docolon and dovar

    primitive '=', equals
        pop eax
        sub ebx, eax
        sub ebx, 1
        sbb ebx, ebx
        jmp next

    primitive '@', fetch
        mov ebx, dword[ebx]
        jmp next

    primitive '!', store
        pop dword[ebx]
        pop ebx
        jmp next

    primitive '0branch', zero_branch
        lodsd
        test ebx, ebx
        jne zerob_z
        xchg eax, esi
zerob_z pop ebx
        jmp next

    primitive 'branch', branch
        mov esi, dword[esi]
        jmp next

    ; accept ( addr len -- len' )
    ; Buffered, line-oriented input.
    ; Stops at \n (not stored), EOF or max len.
    ; Returns n >= 0 = chars stored (0 = empty line);
    ;         -1     = true EOF.
    primitive 'accept', accept
        pop edi                 ; edi = destination buffer
        mov ecx, ebx            ; ecx = max chars to accept
        xor ebx, ebx            ; ebx = chars written so far
        test ecx, ecx
        jz accept_done

accept_next_char:
        mov eax, [in_pos]
        cmp eax, [in_len]
        jb accept_have_data

        ; Private buffer empty → refill
        mov dword [in_pos], 0
        mov dword [in_len], 0
        push ebx
        push ecx
        push edi
        mov eax, 3              ; sys_read
        xor ebx, ebx            ; stdin
        mov ecx, INBUF
        mov edx, INBUFSIZE
        int 80h
        pop edi
        pop ecx
        pop ebx
        test eax, eax
        jg accept_got_data
        ; EOF: keep gathered chars, or -1 if none
        test ebx, ebx
        jnz accept_done
        dec ebx                 ; 0 → -1
        jmp accept_done
accept_got_data:
        mov [in_len], eax

accept_have_data:
        mov eax, [in_pos]
        movzx edx, byte [INBUF + eax]
        inc eax
        mov [in_pos], eax
        cmp dl, 13              ; CR — skip
        je accept_next_char
        cmp dl, 10              ; LF — end of line
        je accept_done
        mov [edi], dl
        inc edi
        inc ebx
        dec ecx
        jnz accept_next_char
accept_done:
        jmp next

    ; type ( addr len -- )
    primitive 'type', type
        mov edx, ebx            ; edx = len
        pop ecx                 ; ecx = addr
        pop ebx                 ; restore TOS
        push ebx                ; save TOS across syscall
        mov eax, 4              ; sys_write
        mov ebx, 1              ; stdout
        int 80h
        pop ebx
        jmp next

    primitive '>number', to_number
        pop edi
        pop ecx
        pop eax
to_numl test ebx, ebx
        je to_numz
        push eax
        movzx eax, byte[edi]
        cmp al, 'a'
        jc to_nums
        sub al, 32
to_nums cmp al, '9' + 1
        jc to_numg
        cmp al, 'A'
        jc to_numh
        sub al, 7
to_numg sub al, 48
        cmp al, byte[val_base]
        jnc to_numh
        xchg eax, edx
        pop eax
        push edx
        xchg eax, ecx
        mul dword[val_base]
        xchg eax, ecx
        mul dword[val_base]
        add ecx, edx
        pop edx
        add eax, edx
        dec ebx
        inc edi
        jmp to_numl
to_numz push eax
to_numh push ecx
        push edi
        jmp next

    primitive 'word', word
        mov edi, dword[val_dp]
        push edi
        mov edx, ebx
        mov ebx, dword[val_t_i_b]
        mov ecx, ebx
        add ebx, dword[val_to_in]
        add ecx, dword[val_number_t_i_b]
wordf   cmp ecx, ebx
        je wordz
        mov al, byte[ebx]
        inc ebx
        cmp dl, 32
        jne word_exact1
        cmp al, dl
        jbe wordf
        jmp wordc
word_exact1        cmp al, dl
        je wordf
wordc   inc edi
        mov byte[edi], al
        cmp ecx, ebx
        je wordz
        mov al, byte[ebx]
        inc ebx
        cmp dl, 32
        jne word_exact2
        cmp al, dl
        jbe wordz
        jmp wordc
word_exact2        cmp al, dl
        jne wordc
wordz   mov byte[edi + 1], 32
        mov eax, dword[val_dp]
        xchg eax, edi
        sub eax, edi
        mov byte[edi], al
        sub ebx, dword[val_t_i_b]
        mov dword[val_to_in], ebx
        pop ebx
        jmp next

    primitive 'find', find
        mov edi, val_last
findl   push edi
        push ebx
        movzx ecx, byte[ebx]
        inc ecx
findc   mov al, byte[edi + 4]
        and al, 07Fh
        cmp al, byte[ebx]
        je findm
        pop ebx
        pop edi
        mov edi, dword[edi]
        test edi, edi
        jne findl
findnf  push ebx
        xor ebx, ebx
        jmp next
findm   inc edi
        inc ebx
        loop findc
        pop ebx
        pop edi
        xor ebx, ebx
        inc ebx
        lea edi, [edi + 4]
        mov al, byte[edi]
        test al, immediate
        jne findi
        neg ebx
findi   and eax, 31
        add edi, eax
        inc edi
        push edi
        jmp next

    ; rot ( x y z -- y z x )
    primitive 'rot', rote
        pop eax                 ; eax = y ; [esp]=x, ebx=z
        push ebx                ; [esp]=z, [esp+4]=x
        mov ebx, [esp+4]        ; ebx = x
        mov [esp+4], eax        ; [esp+4]=y → (y z x)
        jmp next

    ; count ( addr -- addr+1 len )
    primitive 'count', count
        movzx eax, byte [ebx]
        inc ebx
        push ebx
        mov ebx, eax
        jmp next

    colon ':', colon
        dd xt_lit, -1
        dd xt_state
        dd xt_store
        dd xt_create
        dd xt_do_semi_code

docolon xchg ebp, esp
        push esi
        xchg ebp, esp
        lea esi, [eax + 4] ; eax value is set by next
        jmp next

    colon ';', semicolon, immediate
        dd xt_lit, xt_exit
        dd xt_comma
        dd xt_lit, 0
        dd xt_state
        dd xt_store
        dd xt_exit

    colon 'create', create
        dd xt_dp, xt_fetch
        dd xt_last, xt_fetch
        dd xt_comma
        dd xt_last, xt_store
        dd xt_lit, 32
        dd xt_word
        dd xt_count
        dd xt_plus
        dd xt_dp, xt_store
        dd xt_lit, 0
        dd xt_comma
        dd xt_do_semi_code

dovar   push ebx
        lea ebx, [eax + 4] ; eax value is set by next
        jmp next

    primitive '(;code)', do_semi_code
        mov edi, dword[val_last]
        mov al, byte[edi + 4]
        and eax, 31
        add edi, eax
        mov dword[edi + 5], esi
        xchg ebp, esp
        pop esi
        xchg esp, ebp
        jmp next

final:

    colon 'interpret', interpret
interpt dd xt_number_t_i_b
        dd xt_fetch
        dd xt_to_in
        dd xt_fetch
        dd xt_equals
        dd xt_zero_branch
        dd intpar                 ; if >in != #tib → process next word
        ; need a new line
        dd xt_t_i_b
        dd xt_fetch
        dd xt_lit, 255            ; max line length
        dd xt_accept              ; ( len | -1 )
        dd xt_dupe
        dd xt_lit, -1
        dd xt_equals
        dd xt_zero_branch
        dd not_eof                ; if not -1 → continue
        dd xt_bye                 ; true EOF → exit
not_eof dd xt_dupe
        dd xt_zero_branch
        dd empty_line             ; len=0 → blank line, do not WORD
        dd xt_number_t_i_b
        dd xt_store               ; #tib = len
        dd xt_lit, 0
        dd xt_to_in
        dd xt_store               ; >in = 0
        dd xt_branch
        dd intpar
empty_line:
        dd xt_drop                ; drop zero length
        dd xt_branch
        dd interpt                ; read another line
intpar  dd xt_lit, 32
        dd xt_word
        dd xt_dupe
        dd xt_c_fetch            ; length of token
        dd xt_zero_branch
        dd empty_tok             ; empty token → drop addr, continue
        dd xt_find
        dd xt_dupe
        dd xt_zero_branch
        dd intnf
        dd xt_state
        dd xt_fetch
        dd xt_equals
        dd xt_zero_branch
        dd intexc
        dd xt_comma
        dd xt_branch
        dd intdone
empty_tok:
        dd xt_drop                ; drop c-addr of empty token
        dd xt_branch
        dd intdone
intexc  dd xt_execute
        dd xt_branch
        dd intdone
intnf   dd xt_dupe
        dd xt_rote
        dd xt_count
        dd xt_to_number
        dd xt_zero_branch
        dd intskip
        dd xt_state
        dd xt_fetch
        dd xt_zero_branch
        dd intnc
        dd xt_last
        dd xt_fetch
        dd xt_dupe
        dd xt_fetch
        dd xt_last
        dd xt_store
        dd xt_dp
        dd xt_store
intnc   dd xt_abort
intskip dd xt_drop
        dd xt_drop
        dd xt_state
        dd xt_fetch
        dd xt_zero_branch
        dd intdone
        dd xt_lit
        dd xt_lit
        dd xt_comma
        dd xt_comma
intdone dd xt_branch
        dd interpt

freemem:

filesize   equ   $ - $$
