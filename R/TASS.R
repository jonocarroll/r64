

suppressPackageStartupMessages({
  library(glue)
  library(R6)
})


#-----------------------------------------------------------------------------
#' Class wrapping the 64TASS assembler for easier testing of my assembler
#'
#' @examples
#' \dontrun{
#' tass <- TASS$new("./asm/border.asm")
#' tass$dump_asm()
#' tass$compile()
#' tass$get_prg()
#' tass$get_asm()
#' tass$compile_and_run()
#' }
#-----------------------------------------------------------------------------
TASS <- R6::R6Class(
  "TASS",
  public = list(
    asm_filename    = NULL,
    asm             = NULL,
    bytes           = NULL,
    prg_filename    = NULL,
    tass_bin        = NULL,
    compile_result  = NULL,
    x64_bin         = NULL,

    initialize = function(asm_filename, tass_bin = "./bin/64tass", x64_bin = '/usr/local/opt/vice/x64.app/Contents/MacOS/x64') {
      self$asm_filename <- asm_filename
      self$prg_filename <- paste0(tools::file_path_sans_ext(asm_filename), ".prg")

      self$tass_bin <- tass_bin
      self$x64_bin  <- x64_bin


      invisible(self)
    },


    compile = function() {
      # /usr/local/bin/64tass -C -a -i SOURCEFILE -o OUTFILE
      # open /Applications/Vice64/x64.app OUTFILE
      unlink(self$prg_filename)
      command = glue("{self$tass_bin} -C -a -i {self$asm_filename} -o {self$prg_filename}")
      self$compile_result <- system(command, intern = TRUE)

      short_result <- grep("messages:", tass$compile_result, value=TRUE)
      cat(paste(short_result, collapse="\n"), "\n")

      invisible(self)
    },

    run = function() {
      command = glue("{self$x64_bin} {self$prg_filename}")
      system(command)

      invisible(self)
    },

    dump_asm = function() {
      cat(self$get_asm(), "\n")
      invisible(self)
    },

    get_asm = function() {
      paste(readLines(self$asm_filename), collapse="\n")
    },

    get_prg = function() {
      readBin(self$prg_filename, what='raw', n=file.info(self$prg_filename)$size)
    },

    compile_and_run = function() {
      self$compile()
      self$run()
    }

  )
)


if (FALSE) {
  tass <- TASS$new("./asm/border.asm")
  tass$dump_asm()
  tass$compile()
  tass$get_prg()
  tass$get_asm()
  tass$compile_and_run()
}
