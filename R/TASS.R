


#-----------------------------------------------------------------------------
#' Class wrapping the 64TASS assembler for easier testing of the R64 assembler
#'
#' Set `option(TASS_BIN=...)` and `option(X64_BIN=...)` to set the assembler
#' and emulator executable locations, or pass in as arguments
#' `TASS$new(tass_bin=..., x64_bin=...)`
#'
#' @examples
#' \dontrun{
#' tass <- TASS$new("./asm/border.asm", tass_bin = "~/bin/64tass", x64_bin = "~/bin/x64")
#' tass$dump_asm()
#' tass$compile()
#' tass$get_prg()
#' tass$get_asm()
#' tass$compile_and_run()
#' }
#'
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @export
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


    #-------------------------------------------------------------------------
    # Can specify either an asm_filename, or pass in asm directly, which
    # will be saved to a temp file for compilation
    #-------------------------------------------------------------------------
    initialize = function(asm_filename=NULL, asm=NULL, prg_filename=NULL, tass_bin = "~/bin/64tass", x64_bin = '/usr/local/opt/vice/x64.app/Contents/MacOS/x64 -VICIIfilter 0  -silent') {

      if (!is.null(asm)) {
        self$asm_filename <- tempfile(fileext=".asm", pattern = "r64-")
        writeLines(asm, self$asm_filename)
      } else {
        self$asm_filename <- asm_filename
      }

      if (is.null(prg_filename)) {
        self$prg_filename <- paste0(tools::file_path_sans_ext(self$asm_filename), ".prg")
      } else {
        self$prg_filename <- prg_filename
      }

      self$tass_bin <- getOption("TASS_BIN", tass_bin)
      self$x64_bin  <- getOption("X64_BIN" , x64_bin)


      invisible(self)
    },


    #-----------------------------------------------------------------------------
    # Compile asm with 64TASS
    #-----------------------------------------------------------------------------
    compile = function() {

      # remove PRG file if it already exists
      unlink(self$prg_filename)
      command             <- glue::glue("{self$tass_bin} -C -a -i {self$asm_filename} -o {self$prg_filename}")
      self$compile_result <- system(command, intern = TRUE)

      short_result <- grep("messages:", self$compile_result, value=TRUE)
      cat(paste(short_result, collapse="\n"), "\n")

      invisible(self)
    },

    #-----------------------------------------------------------------------------
    # Run program in VICE emulator
    #-----------------------------------------------------------------------------
    run = function(wait=FALSE, intern=FALSE) {
      command = glue::glue("{self$x64_bin} {self$prg_filename}")
      system(command, wait = wait, intern=intern)

      invisible(self)
    },

    #-----------------------------------------------------------------------------
    # Dump the ASM to screen
    #-----------------------------------------------------------------------------
    dump_asm = function() {
      cat(self$get_asm(), "\n")
      invisible(self)
    },

    #-----------------------------------------------------------------------------
    # get a single string representing the entire ASM
    #-----------------------------------------------------------------------------
    get_asm = function() {
      paste(readLines(self$asm_filename), collapse="\n")
    },

    #-----------------------------------------------------------------------------
    # Get a raw vector of bytes representing the PRG
    #-----------------------------------------------------------------------------
    get_prg = function() {
      readBin(self$prg_filename, what='raw', n=file.info(self$prg_filename)$size)
    },

    #-----------------------------------------------------------------------------
    # Compile and then run
    #-----------------------------------------------------------------------------
    compile_and_run = function(wait=FALSE, intern=FALSE) {
      self$compile()
      self$run(wait=wait, intern=intern)
    }

  )
)




#-------------------------------------------------------------------------------
# Testing
#-------------------------------------------------------------------------------
if (FALSE) {
  tass <- Tass$new("./asm/border0.asm")
  tass$dump_asm()
  tass$compile()
  tass$get_prg()
  tass$get_asm()
  tass$compile_and_run()
}
