






`r64` - a c64/6502 assembler in R
==============================================================================


Rationale
------------------------------------------------------------------------------

* Who *doesn't* want a compiler for a *1 MHz* 8-bit computer?
* Writing my own 6502 assembler in R will allow for preparing/calculating 
  data in R and then incorporating directly into the assembly code.


Features
------------------------------------------------------------------------------

General features

* Basic syntax only. Similar to TASS64 syntax.
* Settable program counter
    * e.g. `* = $0801`
* Defined variables
    * e.g. `border = $d020`
* Low/High byte extraction from symol (similar to TASS)
    * e.g. `lda #<routine` will store the low byte of the address of symbol `routine` in the `A` register

For integration with R

* include an `R` string as text using the `.rtext` directive
* include an `R` integer vector as bytes using the `.rbytes` directive

Limitations
------------------------------------------------------------------------------

This is the very definition of a *toy assembler*.  There is enough support for
the things I want to do, but it is lacking a lot of niceties.

Things which are **NOT** included, but could be implemented when the need arises:

* Direct computation on symbols
    * e.g. `lda message+80`
* Computation/manipulation of the program counter 
    * e.g. `sta *-1`
* Support for decimal and binary literals.
    * e.g. `lda #127` or `lda #b01111111`


Installation
------------------------------------------------------------------------------

`r64` depnds on a lot of tidyverse packages as well as the [minilexer](https://bitbucket.org/coolbutuseless/minilexer)
package (for splitting the c64 asm code into tokens).


```r
devtools::install_bitbucket('coolbutuseless/minilexer') # for lexing the 6502 assembly into tokens
devtools::install_bitbucket('coolbutuseless/r64')
```




A simple 6502 program 
------------------------------------------------------------------------------

The following c64/6502 ASM code will clear the screen and then 
write `Hello #rstats!` at the top


```r
asm <- '
*=$0801
  .byte $0c, $08, $0a, $00, $9e, $20  ; 10 SYS 2080
  .byte $32, $30, $38, $30, $00, $00
  .byte $00

*=$0820
      lda #$93        ; Clear the screen
      jsr $ffd2

      ldx #$00        ; initialise the offset pointer into our message
loop  lda message,x   ; load a character and write it to screen 
      and #$3f        ; Manually place chars on screen
      sta $0400,x
      inx
      cpx #$0e
      bne loop

      rts

message
    .text "Hello #rstats!"
'
```


Compile the program and show the compiled program bytes
------------------------------------------------------------------------------


```r
prg_df <- r64::compile(asm)
r64::extract_prg_bytes(prg_df)
```

```
##  [1] 01 08 0c 08 0a 00 9e 20 32 30 38 30 00 00 00 00 00 00 00 00 00 00 00
## [24] 00 00 00 00 00 00 00 00 00 00 a9 93 20 d2 ff a2 00 bd 35 08 29 3f 9d
## [47] 00 04 e8 e0 0e d0 f3 60 c8 45 4c 4c 4f 20 23 52 53 54 41 54 53 21
```


Compare `r64` output to `TASS64` output
------------------------------------------------------------------------------

TASS64 is a well known 6502 assembler.  Here i will use it to compile the same code and 
compare the output. It should be identical. I'm using an [R6](https://cran.r-project.org/package=R6)
wrapper around the command line executable to just make things a bit easier.


```r
tass <- r64::TASS$new(asm = asm)$compile()
```

```
## Error messages:    None
## Warning messages:  None
```

```r
identical(r64::extract_prg_bytes(prg_df), tass$get_prg())
```

```
## [1] TRUE
```


Run c64 programs using VICE
------------------------------------------------------------------------------


```r
prg_filename <- tempfile()
r64::save_prg(prg_df, prg_filename)
system(paste("/usr/local/opt/vice/x64.app/Contents/MacOS/x64", prg_filename), wait=FALSE)
```

![helloworld output](img/hello.gif)


Breakdown of assembly process
------------------------------------------------------------------------------

The compiler makes a few passes through the data to resolve symbol values.

The `r64::compile()` function is just a wrapper which calls the following 4 functions

1. `line_tokens <- r64::create_line_tokens(asm)`
    * For each line in the input break it into tokens.
    * Filter any rows that contain no instructions
2. `prg_df <- r64::create_prg_df(line_tokens)`
    * Create a data.frame from `line_tokens`
    * This is the key data structure for the compilation process
    * The compilation process is just a matter of manipulating this data.frame and merging with information about the instructions
3. `prg_df <- r64::process_xrefs(prg_df)`
    * Resolve labels to their actual addresses
    * Replace any defined variables with their values
4. `prg_df <- r64::process_zero_padding(prg_df)`
    * If there are gaps between blocks of code, insert zero bytes

An example of the final form of the `prg_df` data.frame is show below.  The actual contents of the c64 `prg` file
is just the sequence of values in the `bytes` column. 


```r
prg_df %>%
  select(addr, label, xref, line, opcommand, op, ophex, nbytes, bytes, hexbytes) %>%
  knitr::kable()
```



| addr|label   |xref    |line                          |opcommand     |op  |ophex | nbytes|bytes                                                      |hexbytes                                               |
|----:|:-------|:-------|:-----------------------------|:-------------|:---|:-----|------:|:----------------------------------------------------------|:------------------------------------------------------|
| 2049|NA      |        |* = $0801                     |NA            |NA  |NA    |      0|integer(0)                                                 |                                                       |
| 2049|NA      |        |.byte $0c $08 $0a $00 $9e $20 |NA            |NA  |NA    |      6|c(12, 8, 10, 0, 158, 32)                                   |0c, 08, 0a, 00, 9e, 20                                 |
| 2055|NA      |        |.byte $32 $30 $38 $30 $00 $00 |NA            |NA  |NA    |      6|c(50, 48, 56, 48, 0, 0)                                    |32, 30, 38, 30, 00, 00                                 |
| 2061|NA      |        |.byte $00                     |NA            |NA  |NA    |      1|0                                                          |0                                                      |
| 2062|NA      |NA      |(zero padding)                |NA            |NA  |NA    |     18|c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)    |                                                       |
| 2080|NA      |        |* = $0820                     |NA            |NA  |NA    |      0|integer(0)                                                 |                                                       |
| 2080|NA      |        |lda #$93                      |lda #$93      |lda |a9    |      2|c(169, 147)                                                |a9, 93                                                 |
| 2082|NA      |        |jsr $ffd2                     |jsr $ffd2     |jsr |20    |      3|c(32, 210, 255)                                            |20, d2, ff                                             |
| 2085|NA      |        |ldx #$00                      |ldx #$00      |ldx |a2    |      2|c(162, 0)                                                  |a2, 00                                                 |
| 2087|loop    |message |loop lda message x            |lda message x |lda |bd    |      3|c(189, 53, 8)                                              |bd, 35, 08                                             |
| 2090|NA      |        |and #$3f                      |and #$3f      |and |29    |      2|c(41, 63)                                                  |29, 3f                                                 |
| 2092|NA      |        |sta $0400 x                   |sta $0400 x   |sta |9d    |      3|c(157, 0, 4)                                               |9d, 00, 04                                             |
| 2095|NA      |        |inx                           |inx           |inx |e8    |      1|232                                                        |e8                                                     |
| 2096|NA      |        |cpx #$0e                      |cpx #$0e      |cpx |e0    |      2|c(224, 14)                                                 |e0, 0e                                                 |
| 2098|NA      |loop    |bne loop                      |bne loop      |bne |d0    |      2|c(208, 243)                                                |d0, f3                                                 |
| 2100|NA      |        |rts                           |rts           |rts |60    |      1|96                                                         |60                                                     |
| 2101|message |        |message                       |NA            |NA  |NA    |      0|integer(0)                                                 |                                                       |
| 2101|NA      |        |.text "Hello #rstats!"        |NA            |NA  |NA    |     14|c(200, 69, 76, 76, 79, 32, 35, 82, 83, 84, 65, 84, 83, 33) |c8, 45, 4c, 4c, 4f, 20, 23, 52, 53, 54, 41, 54, 53, 21 |


















