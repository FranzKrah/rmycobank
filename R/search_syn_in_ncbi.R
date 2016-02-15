#' Searches for the synonym on GenBank
#' 
#' Searches first for the synonyms and then searches for the UID code of the synonym present on GenBank
#' 
#' @param taxon a \code{character} containing fungal species name
#' 
#' 
#' @details The function is a wraper function of syno_mycobank and syns_on_ncbi.
#' 
#' @return data.frame with UIDs for the synonyms on GenBank
#' 
#' @author Franz-Sebastian Krah
#' 
#' @examples
#' taxon <- "Friesia annosa"
#' search_syn_in_ncbi(taxon = taxon)

taxon <- "Velutarina_rufoolivacea"
search_syn_in_ncbi <- function(taxon){
  message("... downloading synonyms from MycoBank ...")
  synonyms <- syno_mycobank(taxon = taxon)
  res <- lapply(synonyms, syns_on_ncbi)
  res <- do.call(rbind, res)
  return(res)
}
