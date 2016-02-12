#' Downloads synonyms from mycobank.org
#' 
#' Searches and downloads synonmys from the mycobank.org website for a given fungal species name
#' 
#' @param taxon a \code{character} containing fungal species name
#' 
#' 
#' @details The function searches all synonym entries on the MycoBank on-line database. The data
#' may then be further processes, e.g. a search which of the synonmys are present at GenBank
#' (syns_on_ncbi). 
#' 
#' Mycobank devides synonyms into obligate (=) and facultative (≡) synonyms. Obligate
#' synonyms have 2 different materials, while facultative synonyms refer to the same material. The
#' output of this function produces two lists according to this definition. 
#' 
#' @return list with two vectors of class \code{character}
#' 
#' @author Franz-Sebastian Krah
#' 
#' @examples
#' syns <- syno_mycobank(taxon = "Heterobasidion annosum")

syno_mycobank <- function(taxon){
  require("XML"); require("stringr")
  tax <- strsplit(taxon, "\\s|_")[[1]]
  url <- paste("http://www.mycobank.org/Services/Generic/SearchService.svc/",
    "rest/xml?layout=14682616000000161&filter=e4060%20CONTAINS%20%22",
    tax[1], "%20", tax[2], "%22", sep ="")
  # parse data from mycobank.org
  par <- xmlTreeParse(url, useInternal = TRUE)
  # extract obligate and facultative synonyms
  syn <- getNodeSet(par, "//e4060")[1]
  syn <- strsplit(as.character(xmlValue(syn[[1]])), "\\n")
  syn <- syn[[1]][!unlist(lapply(syn[[1]], nchar)) == 0]
  obl <- syn[grep("=", syn)]; obl <- gsub("=", "", obl)
  fac <- syn[grep("≡", syn)]; fac <- gsub("≡", "", fac)
  # extract species data
  syns_derive <- function(x) {
    x <- do.call(rbind, lapply(x, word, start = 1, end = 4))
    x <- lapply(x, function(x) {
      if(!length(grep("var\\.|spec|sp\\.|ssp\\.|\\bf\\.", x))>0) { x <- word(x, 1, 2) }
      else {x}
    })
    return(x)
  }
  obl <- syns_derive(obl)
  fac <- syns_derive(fac)
  syns <- list(obligate = unlist(obl), facultative = unlist(fac))
  return(syns)
}
