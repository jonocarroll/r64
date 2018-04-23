

`r64` - a c64/6502 assembler in R
------------------------------------------------------------------------------






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
  .byte $0c, $08, $0a, $00, $9e, $20
  .byte $32, $30, $38, $30, $00, $00
  .byte $00

*=$0820
      lda #$93   ; Clear the screen
      jsr $ffd2

      lda #$0e   ; set hi/lo case
      jsr $ffd2

      ldx #$00        ; initialise the offset pointer into our message
loop  lda message,x   ; load a caracter and write it to screen 
      and #$3f        ; Doing manually rather than with kernal $ffd2
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
#>  [1] 01 08 0c 08 0a 00 9e 20 32 30 38 30 00 00 00 00 00 00 00 00 00 00 00
#> [24] 00 00 00 00 00 00 00 00 00 00 a9 93 20 d2 ff a9 0e 20 d2 ff a2 00 bd
#> [47] 3a 08 29 3f 9d 00 04 e8 e0 0e d0 f3 60 c8 45 4c 4c 4f 20 23 52 53 54
#> [70] 41 54 53 21
```


Compare `r64` output to `TASS64` output
------------------------------------------------------------------------------

TASS64 is a well known 6502 assembler.  Here i will use it to compile the same code and 
compare the output. It should be identical. I'm using an [R6](https://cran.r-project.org/package=R6)
wrapper around the command line executable to just make things a bit easier.


```r
tass <- r64::TASS$new(asm = asm)$compile()

identical(r64::extract_prg_bytes(prg_df), tass$get_prg())
```


Run c64 programs using VICE
------------------------------------------------------------------------------


```r
prg_filename <- tempfile()
r64::save_prg(prg_df, prg_filename)
system(paste("/usr/local/opt/vice/x64.app/Contents/MacOS/x64", prg_filename), wait=FALSE)
```

![helloworld output](img/hello.gif)



```r
# r64::compile() runs the following steps:
# line_tokens <- r64::create_line_tokens(asm)
# prg_df      <- r64::create_prg_df(line_tokens)
# prg_df      <- r64::process_xrefs(prg_df)
# prg_df      <- r64::process_zero_padding(prg_df)
```

