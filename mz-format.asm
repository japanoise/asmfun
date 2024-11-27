;;; This is a program I wrote to understand MZ exe, and how much
;;; memory you have available on startup with default (as much as you
;;; want) settings.  It turns out the answer is "quite a lot," at
;;; least on Dosbox; when this program is run there it outputs:

        ;; env segment   0188
        ;; psp start     0192
        ;; cseg          01A2
        ;; dseg          01AE
        ;; fseg          01B6
        ;; dcseg         01B7
        ;; stack seg     01B8
        ;; end of memory 9FFF

;;; Don't worry about the exact values of the segment addresses here -
;;; the important thing is the amount of memory we have & how it is
;;; allocated.  We can see that each segment defined in the code is
;;; allocated as much as it needs, which in the cases of most of these
;;; is just one paragraph (16 bytes).  Note that the stack is, by
;;; default, pointed to the paragraph after the end of the last
;;; segment.  This means we can calculate the total memory available
;;; in paragraphs, see below.

;;; Note also that fasm is fussy about segment names - I had trouble
;;; when using the `.name` convention, it was trying to find
;;; non-existant local labels.  It seems to really want the segments
;;; to be named in capital letters.  Calling them xSEG, where x is a
;;; mnemonic in capital letters, seems to be the best way to do it -
;;; you won't get it confused with anything else that way.

;;; Ignore the names I've chosen here, except for CSEG and DSEG
;;; (code/data segment) - they're from another, incomplete program and
;;; aren't important except to demonstrate what your memory map will
;;; look like with multiple segments.

;;; Assemble this source for DOS as follows:
        ;; fasm mz-format.asm MZTEST.EXE

;;; Format directive - use mz exe format.  This allows us to have DOS
;;; set up our segments for us, and also not limit our program to 64k.
format MZ
entry CSEG:start                ; Define the program's entry point

;;; Start state (according to https://wiki.osdev.org/MZ)
;;;
;;; ES = DS; point to the segment containing the PSP structure.
;;; CS equals value specified in the header, i.e. CSEG
;;; IP equals value specified in the header. i.e. start
;;; SS equals value specified in the header, relocated.
;;;    In practice, this appears to be the paragraph after the end of
;;;    the last segment defined in the source, so it works as a
;;;    pointer to the end of that segment. Though, if this is
;;;    important to you, I'd recommend using a label instead.
;;; SP equals value specified in the header.
;;; AL is:
;;;    - 0x00 if the first FCB in the PSP has a valid drive identifier
;;;    - 0xFF otherwise.
;;;    AH is the same as AL, but for the second FCB in the PSP.
;;;
;;; Other registers are undefined.
;;;
;;; We can detect the segment after the end of memory from the PSP -
;;; it should be in offset 0x02:
        ;; mov ax, [es:0x02]
;;; This means we can do some arithmetic (EoM - SS) to work out how
;;; many 16-byte paragraphs we have available.

segment CSEG
start:
        cld

        mov ax, DSEG
        mov ds, ax

        mov dx, eseglabel
        mov ah, 9h
        int 21h
        mov cx, [es:0x2c]
        call printword

        mov dx, psplabel
        mov ah, 9h
        int 21h
        mov cx, es
        call printword

        mov dx, cseglabel
        mov ah, 9h
        int 21h
        mov cx, CSEG
        call printword

        mov dx, dseglabel
        mov ah, 9h
        int 21h
        mov cx, DSEG
        call printword

        mov dx, fseglabel
        mov ah, 9h
        int 21h
        mov cx, FSEG
        call printword

        mov dx, dcseglabel
        mov ah, 9h
        int 21h
        mov cx, DCSEG
        call printword

        mov dx, stacklabel
        mov ah, 9h
        int 21h
        mov cx, ss
        call printword

        ;; es:02h-03h should contain the segment beyond our memory
        ;; this means we can use it to guess how much memory we have
        mov dx, eomlabel
        mov ah, 9h
        int 21h
        mov cx, [es:0x02]
        call printword

        ;; exit(0)
        mov ah, 0x4c
        mov al, 0
        int 21h

printword:                      ; cx = word to print
        mov al, ch
        call printbyte
        mov al, cl
        call printbyte
        call newline
        ret

printbyte:                      ; al = byte to print
        push ax
        ;; Print high nybble
        shr al, 4
        call printnybble
        pop ax
        ;; Print low nybble
        call printnybble
        ret

printnybble:                    ; al = nybble to print (will mask)
        mov ah, 0x02
        and al, 0x0F
        cmp al, 9
        jg @f
        ;; Number is 0-9, so print that
        add al, '0'
        mov dl, al
        int 21h
        ret
        @@:
        ;; Number is greater than 9, so print A-F
        sub al, 10
        add al, 'A'
        mov dl, al
        int 21h
        ret

newline:
        mov ah, 0x02
        mov dl, 0x0D
        int 21h
        mov ah, 0x02
        mov dl, 0x0A
        int 21h
        ret

segment DSEG
        eseglabel db   'env segment   $'
        dseglabel db   'dseg          $'
        cseglabel db   'cseg          $'
        fseglabel db   'fseg          $'
        dcseglabel db  'dcseg         $'
        stacklabel db  'stack seg     $'
        psplabel db    'psp start     $'
        eomlabel db    'end of memory $'
        db 0

segment FSEG
        db 0

segment DCSEG
        db 0
