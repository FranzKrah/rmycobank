#' Downloads synonyms from mycobank.org
#' 
#' Searches and downloads synonmys from the mycobank.org website for a given fungal species name
#' 
#' @param taxon a \code{character} containing fungal species name
#' 
#' 
#' @details The function searches all synonym entries on the MycoBank on-line database. The data may then be further processes, e.g. a search which of the synonmys are present at GenBank (syns_on_ncbi). 
#' 
#' 
#' @return vector of class \code{character}. 
#' 
#' @author Franz-Sebastian Krah
#' 
#' @examples
#' syns <- syno.mycobank("Heterobasidion annosum")

syno_mycobank <- function(taxon){
  require("XML"); require(stringr)
  taxon <- strsplit(taxon, "\\s|_")[[1]]
  url <- paste("http://www.mycobank.org/Services/Generic/SearchService.svc/",
               "rest/xml?layout=14682616000000161&filter=e4060%20CONTAINS%20%22",
               taxon[1], "%20", taxon[2], "%22", sep ="")
  par <- xmlTreeParse(url, useInternal = TRUE)
  syn <- getNodeSet(par, "//e4060")[[1]]
  syn <- strsplit(as.character(xmlValue(syn)), "=")
  syn <- syn[[1]][!(lapply(syn[[1]], nchar) == 0)]
  syn <- do.call(rbind, lapply(syn, word, start = 1, end = 4))
  syn <- lapply(syn, function(x) {
    if(!length(grep("var\\.|spec|sp\\.|ssp\\.|f\\.", x))>0) { x <- word(x, 1, 2) }
    else {x}
  })
  syn <- as.data.frame(do.call(rbind, syn)); names(syn) <- "synonyms"
  return(syn)
}
