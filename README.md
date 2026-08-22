# itsy-linux

32-bit Linux port of [Itsy Forth](http://www.retroprogramming.com/) by John Metcalf — a minimal Forth used to bootstrap a larger system.

Original articles:

- [Itsy Forth 1K Tiny Compiler](http://www.retroprogramming.com/2012/03/itsy-forth-1k-tiny-compiler.html)
- [Dictionary and Inner Interpreter](http://www.retroprogramming.com/2012/04/itsy-forth-dictionary-and-inner.html)
- [Primitives](http://www.retroprogramming.com/2012/04/itsy-forth-primitives.html)
- [Compiler](http://www.retroprogramming.com/2012/06/itsy-forth-compiler.html)
- [Bit-twiddling notes](http://www.retroprogramming.com/2012/09/itsy-documenting-bit-twiddling-voodoo.html)

The original 8086 code (`.com`) lives in `msdos/`. This tree is a Linux ELF32 port (x86, `int 0x80` syscalls).

## Build

```bash
make
# or: nasm itsy-linux.asm -fbin -o itsy-linux && chmod +x itsy-linux
```

Requires [NASM](https://www.nasm.us/). Produces a standalone ELF binary (about 1.8 KB).

## What's in the kernel

Direct-threaded Forth with a small set of primitives, including:

| Area | Words |
|------|--------|
| Stack | `dup` `drop` `swap` `rot` `>r` `r>` |
| Memory | `@` `!` `c@` `c!` `,` `count` |
| Arithmetic / logic | `+` `-` `*` `=` `and` `or` `invert` `0<` |
| Control / compile | `lit` `0branch` `branch` `:` `;` `create` `execute` `exit` |
| Outer interpreter | `word` `find` `>number` `interpret` `abort` |
| I/O | `accept` (buffered, line-oriented) `type` `bye` |
| Variables | `state` `>in` `#tib` `dp` `base` `last` `tib` |

Higher-level words (`if`/`then`, `emit`, `variable`, comments, …) are loaded from Forth sources.

## Bootstrap with `extend.4`

```bash
./itsy-linux < extend.4          # batch only
cat extend.4 - | ./itsy-linux    # then type interactively
```

`extend.4` defines `immediate`, `\`, `(`, arithmetic helpers, `emit` (via `type`), control flow, `'`, `[char]`, etc.

Example session:

```text
$ cat extend.4 - | ./itsy-linux
65 emit 10 emit
A
bye
# press Ctrl+D so `cat` can exit (see below)
```

### Pipeline note (`cat file - |`)

`bye` exits **Itsy** only. With `cat extend.4 - | ./itsy-linux`, `cat` is still blocked on the terminal. After `bye`, press **Ctrl+D** (EOF) so `cat` finishes and the shell regains the prompt. A second `bye` only unblocks `cat` (SIGPIPE); it is not required by Itsy.

## Tests

```bash
cat extend.4 tester-min.fr | ./itsy-linux
```

Expect 27 asterisks (`*`) and exit status 0. `tester-min.fr` is a small Hayes-style suite (stack, arithmetic, memory, colon definitions, `if`/`begin`).

## Files

| File | Role |
|------|------|
| `itsy-linux.asm` | Kernel sources |
| `extend.4` | Bootstrap Forth (Core subset) |
| `tester-min.fr` | Minimal tests |
| `say_hi.f` | Tiny sample |
| `msdos/` | Original Itsy for DOS |

## Design notes

- **Threading:** direct-threaded; `esi` = IP, `esp` = data stack, `ebp` = return stack, `ebx` = TOS.
- **Input:** private kernel buffer + line-oriented `ACCEPT` (handles long pipes, CRLF, EOF as −1).
- **Output:** `TYPE` is the primitive; `EMIT` is defined in `extend.4` using a one-cell buffer.
- **Size:** kernel is intentionally small; prefer new words in Forth (`extend.4`) over new primitives unless they are required early or are performance-critical.

## License / credit

Based on John Metcalf's Itsy Forth. Linux port and later fixes/extensions are part of this repository.
