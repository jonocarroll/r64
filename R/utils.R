

#-----------------------------------------------------------------------------
#' Reference list of opcode information
#'
#' A dataset containing opcode information for 6502 instructions
#'
#' @source \url{http://www.oxyron.de/html/opcodes02.html}
#-----------------------------------------------------------------------------
"opcodes"



#-----------------------------------------------------------------------------
#' Hex (character strings) to integer values
#'
#' @param ... can be a vector of values or multiple arguments
#'
#' @return vector of integers
#'
#' @export
#-----------------------------------------------------------------------------
h2i <- function(...) {
  h <- unlist(list(...))
  stopifnot(is.character(h))
  h       <- gsub('#', '', h)
  h       <- gsub('^\\$', '0x', h)
  oxstart <- grepl("^0x", h)
  h       <- ifelse(grepl("^0x", h), h, paste0("0x", h))
  i       <- strtoi(h)
  if (any(is.na(i))) {
    stop("Can't convert hex2int: ", paste(h, collapse=', '))
  }
  i
}


#-----------------------------------------------------------------------------
#' String to ASM for ".text" code
#'
#' This is a complete hack to create CBMASCII bytes from plain text
#'
#' @param string single string
#' @return Vector of integers
#'
#' @importFrom purrr map_int
#' @export
#-----------------------------------------------------------------------------
s2i <- function(string) {

  # Remove quotes
  string <- gsub('\\"', '', string)

  # Split into individual chars
  vals <- (strsplit(string, '')[[1]])

  # Convert from chars to int
  vals <- purrr::map_int(vals, utf8ToInt)

  # Recode upper case
  uppercase <- (vals >= 65) & (vals <= 90)
  vals[uppercase] <- vals[uppercase] + 128L

  # recode lower case
  lowercase <- (vals >= 97) & (vals <=122)
  vals[lowercase] <- vals[lowercase] - 32L


  vals
}




#-----------------------------------------------------------------------------
#' Convert an address to 2 bytes (lo_byte, hi_byte) as is the 6502 way
#'
#' @param address address from $0000 to $ffff
#'
#' @return integer vector of length 2 with (lo_byte, hi_byte)
#'
#' @export
#-----------------------------------------------------------------------------
address_to_bytes <- function(address) {
  address <- as.integer(address)
  c(address %% 256L, address %/% 256L)
}
