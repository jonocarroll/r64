






`r64` - a c64/6502 assembler in R
==============================================================================

Full documentation is available here: [[coolbutuseless.bitbucket.io/package/r64](https://coolbutuseless.bitbucket.io/package/r64)]


Rationale
------------------------------------------------------------------------------

* Who *doesn't* want a compiler for a **1 MHz** 8-bit computer with 16 colours and a max resolution of 320x200?
* A 6502 assembler in R will allow for preparing/calculating 
  data in R and then incorporating directly into the assembly code. e.g. 
    * creating character sets 
    * computing animation paths


Features
------------------------------------------------------------------------------

General features

* Basic syntax only. Similar to TASS64 syntax.
* Settable program counter
    * e.g. `* = $0801`
* Defined variables
    * e.g. `border = $d020`
* Low/High byte extraction from symbol (similar to TASS)
    * e.g. `lda #<routine` will store the low byte of the address of symbol `routine` in the `A` register

For integration with R

* `.rtext` directive to include an `R` string as text data
* `.rbyte` directive to include an `R` integer vector as bytes 
* `{...}` to delimit code to be evaluated at run time to manipulate labels and variables e.g. `lda {border + 1}`


Limitations
------------------------------------------------------------------------------

This is the very definition of a *toy assembler*.  There is enough support for
writing simple code, but it is lacking a lot of niceties.

* For most basic assembly code, the syntax is very similar to TASS64
* Errors will be thrown for most syntax errors, but there are still corner cases
  that are untested. It might just turn some things into code even if
  it doesn't make sense.  e.g. a `jmp` to a location more than 128 bytes away.
* `indirect indexed` and `indexed indirect` modes using symbolic addresses seems
  to work, but need more tests.
* No Support for decimal and binary literals. e.g. Currently can't do `lda #127` or `lda #b01111111`
* All bytes must be in 2-character hexadecimal and preceded by a `$`.  I.e. `$01` is ok, but `$1` won't work.
* All word values by in 4-character hexadecimal and preceded by a `$`.  I.e. `$0820` is ok, but `$820` won't work.
* You may need to force zero page addressing modes with symbols/variables by explicitly noting that only the low byte should
  be used. e.g.
    * `BUFPNT = $A6`  is the pointer to the tape i/o buffer
    * `lda <BUFPNT` should be used to force zero page addressing mode, otherwise `lda BUFPNT` would assume absolute addressing mode.
    * The actual result would be the same, but zero page addressing takes fewer bytes and is faster than absolute.


Installation
------------------------------------------------------------------------------

`r64` depends on a lot of tidyverse packages as well as the [minilexer](https://bitbucket.org/coolbutuseless/minilexer)
package (for splitting the c64 asm code into tokens).


```r
devtools::install_bitbucket('coolbutuseless/minilexer') # for lexing the 6502 assembly into tokens
devtools::install_bitbucket('coolbutuseless/r64')
```


Documentation
------------------------------------------------------------------------------

The documentation for the package is available online at [coolbutuseless.bitbucket.io](https://coolbutuseless.bitbucket.io/package/r64) 
(thanks to [pkgdown](https://github.com/r-lib/pkgdown))



Vignettes
------------------------------------------------------------------------------

* `helloworld` - write text to the screen
* `helloborder` - colour cycling in the border as fast as possible
* `helloworld_details` - a look at the intermediate assembly outputs during compilation
* `ascii` - Using R-specific `.rbyte` directive to put characters from an R variable into the assembly code
* `custom_character_set` - Building a custom character set in R and passing it into the assemblly code with the `.rbyte` directive.
* `symbol-arithmetic` - How to do arithmetic on the program counter and address labels

The code for the vignettes is also available in the `prg/` directory of this repository.



Running c64 programs
------------------------------------------------------------------------------

* Online emulators (both of which will let you drag-and-drop)
    * [c64js.com](http://www.c64js.com/)
    * [virtualconsoles.com](https://virtualconsoles.com/online-emulators/c64/)

* Native
    * [VICE](http://vice-emu.sourceforge.net/)

    
    

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
compare the output. It should be identical. `r64::TASS` is a an [R6](https://cran.r-project.org/package=R6)
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

![helloworld output](vignettes/img/helloworld.gif)


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
3. `prg_df <- r64::process_symbols(prg_df)`
    * Resolve labels to their actual addresses
    * Replace any defined variables with their values
4. `prg_df <- r64::process_zero_padding(prg_df)`
    * If there are gaps between blocks of code, insert zero bytes

An example of the final form of the `prg_df` data.frame is show below.  The actual contents of the c64 `prg` file
is just the sequence of values in the `hexbytes` column. 


```r
prg_df %>%
  select(addr, label, line, opcommand, op, ophex, nbytes, hexbytes) %>%
  knitr::kable()
```



| addr|label   |line                          |opcommand     |op  |ophex | nbytes|hexbytes                                               |
|----:|:-------|:-----------------------------|:-------------|:---|:-----|------:|:------------------------------------------------------|
| 2049|NA      |* = $0801                     |NA            |NA  |NA    |      0|                                                       |
| 2049|NA      |.byte $0c $08 $0a $00 $9e $20 |NA            |NA  |NA    |      6|0c, 08, 0a, 00, 9e, 20                                 |
| 2055|NA      |.byte $32 $30 $38 $30 $00 $00 |NA            |NA  |NA    |      6|32, 30, 38, 30, 00, 00                                 |
| 2061|NA      |.byte $00                     |NA            |NA  |NA    |      1|0                                                      |
| 2062|NA      |(zero padding)                |NA            |NA  |NA    |     18|                                                       |
| 2080|NA      |* = $0820                     |NA            |NA  |NA    |      0|                                                       |
| 2080|NA      |lda #$93                      |lda #$93      |lda |a9    |      2|a9, 93                                                 |
| 2082|NA      |jsr $ffd2                     |jsr $ffd2     |jsr |20    |      3|20, d2, ff                                             |
| 2085|NA      |ldx #$00                      |ldx #$00      |ldx |a2    |      2|a2, 00                                                 |
| 2087|loop    |loop lda message x            |lda message x |lda |bd    |      3|bd, 35, 08                                             |
| 2090|NA      |and #$3f                      |and #$3f      |and |29    |      2|29, 3f                                                 |
| 2092|NA      |sta $0400 x                   |sta $0400 x   |sta |9d    |      3|9d, 00, 04                                             |
| 2095|NA      |inx                           |inx           |inx |e8    |      1|e8                                                     |
| 2096|NA      |cpx #$0e                      |cpx #$0e      |cpx |e0    |      2|e0, 0e                                                 |
| 2098|NA      |bne loop                      |bne loop      |bne |d0    |      2|d0, f3                                                 |
| 2100|NA      |rts                           |rts           |rts |60    |      1|60                                                     |
| 2101|message |message                       |NA            |NA  |NA    |      0|                                                       |
| 2101|NA      |.text "Hello #rstats!"        |NA            |NA  |NA    |     14|c8, 45, 4c, 4c, 4f, 20, 23, 52, 53, 54, 41, 54, 53, 21 |


















