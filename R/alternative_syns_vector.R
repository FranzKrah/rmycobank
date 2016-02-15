#' Produces alternative species names vector
#' 
#' Take the output from search_syn_in_ncbi and produces and alternative species vector 
#' substituting the species not present on NCBI GenBank by synonyms that are present
#' 
#' @param search_syn_in_ncbi_out takes the output from \code{search_syn_in_ncbi}
#' 
#' 
#' @details This function is motivated as a helper for producing an input for MEGAPTERA. MEGAPTERA takes an input taxa dataset and automatically procecces downloaded sequences. However, it does not look for synonyms, which is a problem - especially for fungi. alternative_syns_vector, as the name says takes synonyms as input and produces a new vector of species names but subsitures "blind" species, thus species not present on GenBank but the synonyms are.
#' 
#' @return data.frame cols: facultative synonnmys, obligate synonyms and a combined new vector
#' 
#' @author Franz-Sebastian Krah
#' 
#' @examples
#' taxa <- c("Heterobasidion annosum","Schizopora paradoxa")
#' s <- search_syn_in_ncbi(taxa)
#' spec <- alternative_syns_vector(search_syn_in_ncbi_out = s)
#' spec$syns

alternative_syns_vector <- function(search_syn_in_ncbi_out){
  s = search_syn_in_ncbi_out
  # seperate obligate and facultative synonyms
  obl <- lapply(s, function(x) x[grep("obligate", rownames(x)), ])
  fac <- lapply(s, function(x) x[grep("facultative", rownames(x)), ])
  # choose either the original name if found on ncbi or a synonmy if found
  get_any <- function(syn_type, number){
    syn_any <- foreach(x = seq_along(taxa)) %do% {
      if((names(syn_type)[[x]] %in% syn_type[[x]]$synonym) == TRUE)
      {names(syn_type)[[x]]}
      else {syn_type[[x]]$synonym[number]}}
    # if(is.na(syn_type[[x]]$synonym) == TRUE) {NA}}
    names(syn_any) <- taxa
    return(syn_any)
  }
  any_fac <- get_any(fac, number = 1)
  any_obl <- get_any(obl, number = 1)
  # alternative vector with synonyms on genbank for synonyms types
  alt_vec <- function(syn_list){
    res <- data.frame(syns = do.call(rbind, syn_list))
    res$new <- res$syns
    res$new <- as.character(res$new); res$syns <- as.character(res$syns)
    res$new[is.na(res$new)] <- rownames(res)[is.na(res$new)]
    return(res)
  }
  abl_alt <- alt_vec(syn_list = any_obl)
  fac_alt <- alt_vec(syn_list = any_fac)
  # combined alternative vector for 
  alt <- data.frame(syns_fac = fac_alt$syns, syns_abl = abl_alt$syns)
  rownames(alt) <- rownames(fac_alt)
  alt$syns <- alt$syns_fac
  alt$syns[is.na(alt$syns)] <- abl_alt$syns[is.na(alt$syns)]
  alt[] <- lapply(alt, as.character)
  return(alt)
}
