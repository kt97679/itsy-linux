; Itsy Forth - Macros
;   Written by John Metcalf
;   Commentary by Mike Adams
;
; Itsy Forth was written for use with NASM, the "Netwide Assembler"
; (http://www.nasm.us/). It uses a number of macros to deal with the tedium
; of generating the headers for the words that are defined in Itsy's source
; code file. The macros, and the explanations of what they're doing, are
; listed below:

;--------------------------------------------------------------------------
; First, two variables are defined for use by the macros:
        ; link is the initial value for the first link field that'll
        ; be defined. It's value will be updated with each header
        ; that's created.
        %define link 0

        ; A bitmask that'll be called "immediate" will be used to
        ; encode the flag into the length bytes of word names in order
        ; to indicate that the word will be of the immediate type.
        %define immediate 080h

;--------------------------------------------------------------------------
; The first macro defined is the primary one used by the others, "head".
; It does the lion's share of the work for the other macros that'll be
; defined afterwards. Its commands perform the following operations:

        ; The first line of the macro declares it's name as "head".
        ; The 4 in this line signifies that it expects to receive
        ; 4 parameters when it's invoked: the string that will be the
        ; word's name and will be encoded into the header along with
        ; the string's name; an "execution tag" name that will have the
        ; prefix "xt_" attached to it and will be used as a label for
        ; the word's code field; a flag that will be 080h if the word
        ; will be immediate and a 0 otherwise; and the label for the
        ; word's runtime code, whose address will be put into the
        ; word's code field.
        %macro head 4

        ; Okay, what we're doing in this odd-looking bit of code is
        ; declaring a variable called "%%link" that's local only to this
        ; macro and is independent of the earlier variable we declared
        ; as "link". It's a label that will represent the current
        ; location in the object code we're creating. Then we lay down
        ; some actual object code, using the "dw" command to write the
        ; current value of "link" into the executable file.
        %%link dw link

        ; Here's one of the tricky parts. We now redefine the value of
        ; "link" to be whatever the current value of "%%link" is, which
        ; is basically the address of the link field that was created
        ; during this particular use of this macro. That way, the next
        ; time head is called, the value that will be written into the
        ; code in the "dw" command above will be whatever the value of
        ; "%%link" was during THIS use of the macro. This way, each time
        ; head is called, the value that'll be written into the new
        ; link field will be the address that was used for the link
        ; field the previous time head was called, which is just how
        ; we want the link fields to be in a Forth dictionary. Note that
        ; the first time that head is called, the value of link was
        ; predefined as 0, so that the link field of the first word in
        ; the dictionary will contain the value of 0 to mark it as
        ; being the first word in the dictionary.
        %define link %%link

        ; Now the name field. The first argument passed to head is the
        ; string defining the new word's name. The next line in the macro
        ; measures the length of the string (the "%1" tells it that it's
        ; supposed to look at argument #1) and assigns it to a macro-local
        ; variable called "%%count".
        %strlen %%count %1

        ; In this next line, we're writing data into the object code on
        ; a byte-by-byte basis. We first write a byte consisting of the
        ; value of argument 3 (which is 080h if we're writing the header
        ; for an immediate word or a 0 otherwise) added to the length of
        ; the name string to produce the length byte in the header. Then
        ; we write the name string itself into the file.
        db %3 + %%count,%1

        ; Okay, don't get confused by the "+" in this next line. Take
        ; careful note of the spaces; the actual command is "%+", which
        ; is string concatenation, not numeric addition. We're going to
        ; splice a string together. The first part consists of the "xt_",
        ; then we splice the macro's 2nd argument onto it. The resulting
        ; string is used as the head's "execution tag", the address of
        ; it's code field. This label is then used for the "dw" command
        ; that writes the value of argument #4 (the address of the word's
        ; runtime code) into the header's code field.
        xt_ %+ %2 dw %4

        ; As you might guess, the next line marks the end of the
        ; macro's definition. The entire header's been defined at this
        ; point, and we're now ready for the data field, whether it's
        ; composed of assembly code, a list of Forth words, or the
        ; numeric data for a variable or constant.
        %endmacro

; For example, calling head with the following line:
;
;      head,'does>',does,080h,docolon
;
; will produce the following header code...
;
;               dw (address of link of previous header)
;               db 085h,'does>'
;      xt_does  dw docolon
;
; ...and records the address of this header's link field so that it can
; be written into the link field of the next word, just as the address
; of the previous link field was written into this header.
; This method saves the programmer a lot of tedium in manually generating
; the code for word headers when writing a Forth system's kernel in
; assembly language. Note that argument #2 is surrounded by single quotes.
; That's the format that the assembler expects to see when being told to
; lay down a string of characters byte-by-byte in a db command, so they
; have to be present when they're given as an arg to this macro so that
; the macro puts them in their proper place.

;--------------------------------------------------------------------------
; The next macro is called "primitive", and is used for setting up a header
; for a word written in assembly language.
;
        ; Here we declare the definition of the macro called "primitive".
        ; Note, though, the odd manner in which the number of required
        ; arguments is stated. Yes, that really does mean that it can
        ; take from 2 to 3 arguments. Well, what does it do if the user
        ; only gives it 2? That's what that 0 is: the default value that's
        ; to be used for argument #3 if the user doesn't specify it. Most
        ; of the time he won't; the only time arg #3 will be specifically
        ; given will be if the user is defining an immediate word.
        %macro primitive 2-3 0

        ; All primitive does is to pass its arguments on to head, which
        ; does most of the actual work. It passes on the word name and
        ; the execution tag name as-is. Parameter #3 will be given the
        ; default value of 0 unless the user specifically states it.
        ; This is meant to allow the user to add "immediate" to the
        ; macro invocation to create an immediate word. The 4th arg,
        ; "$+2", means that when head goes to write the address of the
        ; run-time code into the code field, the address it's going to
        ; use will be 2 bytes further along than the code field address,
        ; i.e. the address of the start of the code immediately after
        ; the code field. (The "$" symbol is used by most assemblers
        ; to represent the address of the code that's currently being
        ; assembled.)
        head %1,%2,%3,$+2

        ; End of the macro definition.
        %endmacro

;--------------------------------------------------------------------------
; The macro "colon" operates very similarly to "primitive", except that
; it's used for colon definitions:
;
        ; Declare the macro, with 2 to 3 arguments, using 0 for the default
        ; value of arg #3 if one isn't specifically given.
        %macro colon 2-3 0

        ; Pass the args on to head, using docolon as the runtime code.
        head %1,%2,%3,docolon

        ; End of macro definition.
        %endmacro

;--------------------------------------------------------------------------
; The rest of the macros all require a specific number of arguments, since
; none of them have the option of being immediate. This one defines
; a constant:

        ; Macro name is, unsurprisingly, "constant", and gets 3 arguments.
        ; As with head and primitive, the first 2 are the word's name and
        ; the label name that'll be used for the word. The third argument
        ; is the value that we want the constant to hold.
        %macro constant 3

        ; Use the head macro. Args 1 and 2, the names, get passed on as-is.
        ; Constants are never defined as immediate (though it's an intriguing
        ; idea; a constant whose value is one thing when compiling and
        ; another when interpreting might be useful for something), so arg #3
        ; passed on to head is always a 0, and arg #4 will always be doconst,
        ; the address of the runtime code for constants.
        head %1,%2,0,doconst

        ; Similar to the way that the label is created for the execution
        ; tags, here we create a label for the data field of the constant,
        ; though this time we're prefixing the name with "val_" instead
        ; of the "xt_" used for the execution tags. Then we use a dw to
        ; write constant's arg #3, the constant's value, into the code.
        val_ %+ %2 dw %3

        ; End of the definition.
        %endmacro

;--------------------------------------------------------------------------
; The macro for variables is very similar to the one for constants.

        ; Macro name "variable", 3 arguments, with arg #3 being the
        ; initial value that will be given to the variable.
        %macro variable 3

        ; Just like in "constant", except that the runtime code is dovar.
        head %1,%2,0,dovar

        ; Exact same line as used in "constant", with the same effects.
        val_ %+ %2 dw %3

        ; End of the definition.
        %endmacro

;--------------------------------------------------------------------------
;
; That's the last of the macros. They're accessed through the
; "%include macros.asm" command near the beginning of Itsy's
; source code file. Or, if you prefer, you can remove the
; %include command and splice the above code directly
; into itsy.asm in its place.
;
;--------------------------------------------------------------------------

