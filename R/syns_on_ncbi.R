#' Checks synonyms against GenBank 
#' 
#' Searches which of the synonyms of an input species list are present on GenBank
#' 
#' @param synonyms a vector of class \code{character} containing fungal species names
#' 
#' 
#' @details The function searches which of the synonmys are present at GenBank. If you have a species list e.g. from a field study and want to derive sequences from GenBank but only get a subset of the species; then you might ignore the synonyms.
#' 
#' 
#' @return an data.frame with the synomyms and the GIs, if present.
#' 
#' @author Franz-Sebastian Krah
#' 
#' @examples 
#' # Use the synonyms derived from mycobank (syno_mycobank)
#' syns <- syno_mycobank(taxon = "Heterobasidion annosum")
#' gis <- lapply(syns, syns_on_ncbi)
#' gis
#'
#' # or any vector of synonyms
#' synonyms <- c("Heterobasidion annosum", "Polyporus annosus","Polyporus subpileatus",
#' "Polyporus scoticus","Polyporus makraulos","Polyporus macraulos","Trametes radiciperda",
#' "Poria macraula","Poria macraula","Polyporus irregularis","Polystictoides fuscus",
#' "Polyporus atramosus","Polyporus marginatoides","Polyporus atrannosus",
#' "Heterobasidion annosum f. macraulos")
#' gi <- syns_on_ncbi(synonyms)

syns_on_ncbi <- function(synonyms) {
  if (is.null(synonyms)) warning("  argument not of class character ")
  if (is.null(synonyms)) {res <- NA}
  if (!is.null(synonyms)) {
    require("taxize")
    res <- get_uid(synonyms)
    res <- cbind.data.frame(synonym = synonyms, GI = as.numeric(res))
    if(length(grep("\\d", res$GI))==0){res <- NA}
    else {res <- res[grep("\\d", res$GI),]
    res$synonym <- as.character(res$synonym)}
  }
  return(res)
}
