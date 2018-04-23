
globalVariables(c('opcodes', 'opbyte', 'init_addr', 'label', 'xref',
                  'line', 'opcommand', 'op', 'opmode', 'ophex', 'nopbytes',
                  'nargbytes'))


#-----------------------------------------------------------------------------
#' Determine the opcode, address mode etc for a sequence of tokens representing a single instruction
#'
#' @param tokens A sequence of ASM tokens. This *must* start with a valid opcode, and
#'               not be a `symbol` or an assembler directive like `.text`
#'
#' @return single row data.frame containing information about the instruction
#'
#' @importFrom dplyr filter data_frame
#' @importFrom rlang "%||%"
#-----------------------------------------------------------------------------
get_opcode_info <- function(tokens) {

  #---------------------------------------------------------------------------
  # What are the token types present? This is the main way I'll determine
  # the addressing mode of the op
  #---------------------------------------------------------------------------
  types    <- names(tokens)

  #---------------------------------------------------------------------------
  # Default values
  #---------------------------------------------------------------------------
  command  <- paste(tokens, collapse=" ")
  xref     <- ''
  argbytes <- list(NULL)

  #---------------------------------------------------------------------------
  # If we don't start with an opcode, then this is a failure situation.
  # Put in an 'unknown' opmode for now and catch this error later
  #---------------------------------------------------------------------------
  if (!identical(types[1], 'opcode')) {
    return(data_frame(opmode = 'unknown', nopbytes = NA_integer_, nargbytes = NA_integer_, opcommand=command, xref=xref, argbytes=argbytes))
  }

  #---------------------------------------------------------------------------
  # Get the opcode_info from the reference table for this op
  # and extract the list of available modes
  #---------------------------------------------------------------------------
  opcode_info <- opcodes %>% filter(op == tokens[1])
  modes       <- opcode_info$opmode


  #---------------------------------------------------------------------------
  # Decode what we can about any 'symbol' used in this line
  # e.g.
  #   - Is this an 'immediate' value?
  #   - Do we only wnat the low byte of the value?
  #   - What's the actual symbol underneath any other directives?
  #     i.e. '#<int' is the symbol 'int', in 'immediate' mode, and
  #     we only want the low byte
  #---------------------------------------------------------------------------
  symbol <- list()
  if ('symbol' %in% types) {
    symbol_actual <- tokens[types=='symbol']
    if (grepl('^#', symbol_actual)) {
      symbol$mode   <- 'immediate'
      symbol_actual <- gsub("^#", '', symbol_actual)
    }
    if (grepl('^<', symbol_actual)) {
      symbol$mode   <- symbol$mode %||% 'zero page'
      symbol$op     <- 'low'
      symbol_actual <- gsub("^<", '', symbol_actual)
    } else if (grepl('^>', symbol_actual)) {
      symbol$mode   <- symbol$mode %||% 'zero page'
      symbol$op     <- 'high'
      symbol_actual <- gsub("^>", '', symbol_actual)
    }
    symbol$actual <- symbol_actual
  }


  #---------------------------------------------------------------------------
  # Use the 'types' of the 'tokens' as a template for determining what the
  # addressing mode is.  This is also conditional on what actual modes
  # are available for this op.
  #
  # It gets trickier if the op references a symbol.
  #
  # NOTE: Not all symbol cases are handled. e.g.
  #    - Not handled symbol use:  indirect, indirect x, indirect y
  #---------------------------------------------------------------------------
  if (length(tokens) == 1) {
    opmode    <- 'implied'
    nargbytes <- 0L
  } else if (identical(types, c('opcode', 'immediate')) && ('immediate' %in% modes)) {
    opmode    <- 'immediate'
    argbytes  <- list(h2i(tokens[2]))
  } else if (identical(types[-3], c('opcode', 'lbracket', 'rbracket')) && ('indirect' %in% modes)) {
    opmode    <- 'indirect'
    argbytes  <- list(address_to_bytes(h2i(tokens[3])))
  } else if (identical(types[-3], c('opcode', 'lbracket', 'x', 'rbracket')) && ('indirect x' %in% modes)) {
    opmode    <- 'indirect x'
    argbytes  <- list(address_to_bytes(h2i(tokens[3])))
  } else if (identical(types[-3], c('opcode', 'lbracket', 'rbracket', 'y')) && ('indirect y' %in% modes)) {
    opmode    <- 'indirect y'
    argbytes  <- list(address_to_bytes(h2i(tokens[3])))
  } else if (identical(types, c('opcode', 'word')) && ('absolute' %in% modes)) {
    opmode    <- 'absolute'
    argbytes  <- list(address_to_bytes(h2i(tokens[2])))
  } else if (identical(types, c('opcode', 'word', 'x')) && ('absolute x' %in% modes)) {
    opmode    <- 'absolute x'
    argbytes  <- list(address_to_bytes(h2i(tokens[2])))
  } else if (identical(types, c('opcode', 'word', 'y')) && ('absolute y' %in% modes)) {
    opmode    <- 'absolute y'
    argbytes  <- list(address_to_bytes(h2i(tokens[2])))
  } else if (identical(types, c('opcode', 'byte')) && ('zero page' %in% modes)) {
    opmode    <- 'zero page'
    argbytes  <- list(h2i(tokens[2]))
  } else if (identical(types, c('opcode', 'byte', 'x')) && ('zero page x' %in% modes)) {
    opmode    <- 'zero page x'
    argbytes  <- list(h2i(tokens[2]))
  } else if (identical(types, c('opcode', 'byte', 'y')) && ('zero page y' %in% modes)) {
    opmode    <- 'zero page y'
    argbytes  <- list(h2i(tokens[2]))
  } else if (identical(types, c('opcode', 'byte'))  && ('relative' %in% modes)) {
    opmode    <- 'relative'
    argbytes  <- list(h2i(tokens[2]))
  } else if (identical(types, c('opcode', 'symbol')) && ('relative' %in% modes)) {
    opmode    <- 'relative'
    xref      <- symbol$actual
    argbytes  <- list(c(0L))
  } else if (identical(types, c('opcode', 'symbol')) && ('absolute' %in% modes)) {
    opmode    <- symbol$mode %||% 'absolute'
    xref      <- symbol$actual
    argbytes  <- ifelse(grepl('absolute', opmode), list(c(0L, 0L)), list(c(0L)))
  } else if (identical(types, c('opcode', 'symbol', 'x')) && ('absolute x' %in% modes)) {
    opmode    <- symbol$mode %||% 'absolute'
    opmode    <- paste(opmode, 'x')
    xref      <- symbol$actual
    argbytes  <- ifelse(grepl('absolute', opmode), list(c(0L, 0L)), list(c(0L)))
  } else if (identical(types, c('opcode', 'symbol', 'y')) && ('absolute y' %in% modes)) {
    opmode    <- symbol$mode %||% 'absolute'
    opmode    <- paste(opmode, 'y')
    xref      <- symbol$actual
    argbytes  <- ifelse(grepl('absolute', opmode), list(c(0L, 0L)), list(c(0L)))
  } else {
    print(tokens)
    stop("not handled")
  }


  #---------------------------------------------------------------------------
  # By now, we know the 'op' and its addressing mode, which means that
  # we should only have one possible row from the reference 'opcode_info' table
  #---------------------------------------------------------------------------
  opcode_info <- opcode_info[opcode_info$opmode == opmode, , drop=FALSE]
  if (nrow(opcode_info) != 1) {
    print("=======================================")
    print("Should only have one possible mode")
    print(tokens)
    print(opcode_info)
    stop()
  }


  #---------------------------------------------------------------------------
  # Construct a full table of opcode info for this set of tokens
  #---------------------------------------------------------------------------
  data_frame(
    op        = tokens[1],
    opbyte    = opcode_info$dec,
    ophex     = as.character(as.hexmode(opcode_info$dec)),
    opmode    = opmode,
    nopbytes  = 1L,
    nargbytes = length(argbytes[[1]]),
    opcommand = command,
    xref      = xref,
    argbytes  = argbytes,
    symbolop  = ifelse(xref != '', symbol$op %||% opmode, NA_character_)
  )
}



#-----------------------------------------------------------------------------
#' Determine the opcode, address mode etc for a sequence of tokens representing a single instruction
#'
#' @param tokens A sequence of ASM tokens for one row of the ASM file. This may be a row containing
#'               (1) only a label
#'               (2) a PC(*) assignment
#'               (3) a variable assignment
#'               (4) an assembler directive e.g. '.text', '.byte'
#'               (5) an instruction
#'               (6) any of the above preceded by a label
#'
#' @return single row data.frame containing information about the row
#'
#' @importFrom dplyr bind_cols
#-----------------------------------------------------------------------------
create_prg_df_row <- function(tokens) {
  types <- names(tokens)

  res <- data_frame(line = paste(tokens, collapse=" "), value=list(c()))

  if (identical(types, 'symbol')) {
    res$label <- tokens[1]
    return(res)
  } else if (identical(types, c('symbol', 'equals', 'word'))) {
    res$label <- tokens[1]
    res$value <- list(address_to_bytes(h2i(tokens[3])))
    return(res)
  } else if (identical(types[1], 'symbol')) {
    res$label <- tokens[1]
    types     <- types[-1]
    tokens    <- tokens[-1]
  }

  if (identical(types, c('PC', 'equals', 'word'))) {
    res$init_addr <- h2i(tokens[3])
    return(res)
  } else if (identical(types[1], 'byte_inst')) {
    res$argbytes  <- list(h2i(tokens[-1]))
    res$nargbytes <- length(tokens[-1])
    return(res)
  } else if (identical(types, c('text_inst', 'text'))) {
    res$argbytes  <- list(s2i(tokens[2]))
    res$nargbytes <- length(s2i(tokens[2]))
    return(res)
  } else if (identical(types, c('rtext_inst', 'symbol'))) {
    rtext         <- get(tokens[2])
    res$argbytes  <- list(s2i(rtext))
    res$nargbytes <- length(s2i(rtext))
    return(res)
  } else if (identical(types, c('rbyte_inst', 'symbol'))) {
    rbytes        <- get(tokens[2])
    res$argbytes  <- list(rbytes)
    res$nargbytes <- length(rbytes)
    return(res)
  }

  opcode_info <- get_opcode_info(tokens)
  res         <- bind_cols(res, opcode_info)

  res
}



#-----------------------------------------------------------------------------
#' Create the main compiler datastructure 'prg_df' from a list of `line_tokens`
#'
#' @param line_tokens list of line tokens created by `create_line_tokens()`
#'
#' @return prg_df the main compiler datastructure
#'
#' @importFrom dplyr select mutate everything
#' @importFrom tidyr replace_na
#' @importFrom purrr map_dfr
#' @export
#-----------------------------------------------------------------------------
create_prg_df <- function(line_tokens) {
  prg_df <- line_tokens %>%
    purrr::map_dfr(create_prg_df_row) %>%
    select(init_addr, label, xref, line, opcommand, op, opmode, opbyte, ophex, everything()) %>%
    tidyr::replace_na(list(nopbytes=0L, nargbytes=0L, xref='')) %>%
    mutate(
      nbytes = nopbytes + nargbytes
    )

  if (any(prg_df$opmode == 'unknown', na.rm = TRUE)) {
    warning("Unknown opcodes")
  }

  prg_df
}
