
globalVariables(c('nbytes', 'addr', 'bytes_so_far', 'value',
                  'xref_address', 'xref_bytes', 'xref_relative',
                  'symbolop', 'argbytes', 'bytes', 'nbytes',
                  'hexbytes', 'next_addr'))


#-----------------------------------------------------------------------------
#' Process any cross-referenced symbols to insert actual addresse/values
#'
#' @param prg_df main assembler data.frame created by `create_prg_df()`
#'
#' @return udpated prg_df with actual addresses added for each row, and
#'         cross-referenced values resolved
#'
#' @import dplyr
#' @importFrom purrr map2 map_chr map_lgl
#' @importFrom tidyr fill
#' @export
#-----------------------------------------------------------------------------
process_xrefs <- function(prg_df) {
  if (any(prg_df$opmode == 'unknown', na.rm = TRUE)) {
    stop("Unknown opcodes")
  }


  #-----------------------------------------------------------------------------
  # Address calculation by using
  # - known start PC
  # - nbytes for each instruction
  #-----------------------------------------------------------------------------
  prg_df %<>%
    tidyr::fill(init_addr, .direction='down') %>%
    tidyr::fill(init_addr, .direction='up'  ) %>%
    mutate(addr = init_addr) %>%
    group_by(init_addr) %>%
    mutate(
      bytes_so_far = cumsum(nbytes),
      addr         = addr + lag(bytes_so_far, default = 0)
    ) %>%
    ungroup() %>%
    select(-bytes_so_far) %>%
    select(addr, everything())


  #-----------------------------------------------------------------------------
  # Address cross-reference resolution.
  # Some xrefs are for addresses,
  # others are for defined constants
  #-----------------------------------------------------------------------------
  xref <- prg_df %>%
    filter(!is.na(label)) %>%
    select(xref = label, xref_address = addr, value) %>%
    mutate(
      xref_bytes = map(xref_address, address_to_bytes),
      xref_bytes = ifelse(map_lgl(value, is.null), xref_bytes, value),
      xref_chr   = xref_bytes %>% map_chr(~paste(.x, collapse=", "))
    )

  xref %<>% select(xref, xref_address, xref_bytes)



  prg_df %<>%
    left_join(xref, by='xref') %>%
    mutate(
      xref_relative = xref_address - addr - 2L,  # not sure where the offset sould be calculated from
      xref_relative = ifelse(xref_relative < 0, 256L + xref_relative, xref_relative),
      xref_relative = as.list(xref_relative),
      argbytes      = if_else(xref!='' & symbolop == 'relative', xref_relative, argbytes, argbytes),
      argbytes      = if_else(xref!='' & symbolop %in% c('absolute', 'absolute x', 'absolute y'), xref_bytes   , argbytes, argbytes),
      argbytes      = if_else(xref!='' & symbolop == 'low'     , map(xref_bytes, 1), argbytes, argbytes),
      argbytes      = if_else(xref!='' & symbolop == 'high'    , map(xref_bytes, 2), argbytes, argbytes)
    )

  prg_df %<>% select(-xref_bytes, -xref_relative)


  #-----------------------------------------------------------------------------
  # Recalc 'bytes' and produce a hexbytes representation for debugging
  #-----------------------------------------------------------------------------
  prg_df %<>% mutate(
    bytes        = purrr::map2(opbyte, argbytes, function(.x, .y) {res <- c(.x, .y); res[!is.na(res)]}),
    hexbytes     = purrr::map_chr(bytes, ~paste(as.character(as.hexmode(.x)), collapse=", "))
  )

  prg_df
}



#-----------------------------------------------------------------------------
#' Pad out with zero bytes between address blocks
#'
#' @param prg_df main assembler data.frame after `process_xrefs()` has been run
#'
#' @return prg_df updated with zero padding rows between address blocks so that
#'         every byte from start to finish is accounted for
#'
#' @import dplyr
#' @importFrom magrittr "%<>%"
#' @importFrom tidyr replace_na
#' @export
#-----------------------------------------------------------------------------
process_zero_padding <- function(prg_df) {

  zeros <- prg_df %>%
    mutate(
      next_addr = addr + nbytes,
      nbytes    = lead(addr) - next_addr
    ) %>% tidyr::replace_na(list(nbytes = 0L))

  zeros %<>% filter(nbytes > 0)

  if (nrow(zeros) > 0) {
    zeros %<>%
      mutate(
        bytes     = purrr::map(nbytes, ~rep(0L, .x)),
        argbytes  = list(NA_integer_),
        line      = '(zero padding)',
        init_addr = next_addr,
        hexbytes  = ''
      ) %>%
      select(init_addr, addr = next_addr, bytes, nbytes, argbytes, line, hexbytes)

    prg_df %<>%
      bind_rows(zeros) %>%
      arrange(addr)
  }

  prg_df
}



#-----------------------------------------------------------------------------
#' Extract the raw bytes for the assembled PRG
#'
#' @param prg_df main assembler data.frame
#'
#' @return raw vector of bytes
#'
#' @importFrom purrr flatten_int
#' @export
#-----------------------------------------------------------------------------
extract_prg_bytes <- function(prg_df) {
  as.raw(c(address_to_bytes(prg_df$addr[1]), purrr::flatten_int(prg_df$bytes)))
}



#-----------------------------------------------------------------------------
#' Save the PRG to file
#'
#' @param prg_df main assembler data.frame
#' @param filename c64 PRG file to write to
#'
#' @export
#-----------------------------------------------------------------------------
save_prg <- function(prg_df, filename) {
  prg_bytes <- extract_prg_bytes(prg_df)
  writeBin(prg_bytes, filename)
}



