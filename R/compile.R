



#-----------------------------------------------------------------------------
#' Compile ASM to prg_df
#'
#' All the intermediate assembler steps are still works-in-progress and can
#' be quite fragile.
#'
#' This function is only recommneded if you're sure that the code compiles OK,
#' otherwise tracking down errors is going to be pretty tough
#'
#' @param asm single character string containing all ASM (including newlines)
#' @return prg_df main assembler data.frame
#'
#' @export
#-----------------------------------------------------------------------------
compile <- function(asm) {
  line_tokens <- create_line_tokens(asm)
  prg_df      <- create_prg_df(line_tokens)
  prg_df      <- process_xrefs(prg_df)
  prg_df      <- process_zero_padding(prg_df)

  prg_df
}