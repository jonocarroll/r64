context("compare_to_tass")

#-----------------------------------------------------------------------------
# The ASM files in the vignettes/asm directory are basic programs which
# should compile to the same bytes in both `r64` and TASS64
#-----------------------------------------------------------------------------
test_that("compare to TASS", {
  asm_files <- list.files("asm/", pattern = "asm$", full.names = TRUE)

  for (asm_file in asm_files) {
    asm <- readLines(asm_file) %>% paste(collapse="\n")

    prg_df    <- r64::compile(asm)
    prg_bytes <- r64::extract_prg_bytes(prg_df)

    tass <- r64::TASS$new(asm = asm)

    if (!file.exists(tass$tass_bin)) {
      warning("set 'option(TASS_BIN=...)' to your TASS64 path to run compiler tests")
    } else {
      print(asm_file)
      tass$compile()
      expect_identical(prg_bytes, tass$get_prg())
    }

  }
})
