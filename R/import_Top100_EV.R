#' Downloads Top100 protein list and converts to protein accession ids
#'
#' @return
#' @export
#'
#'
import_Top100_EV <- function() {

  # Message for deprecated function
  message("Please use setup_EV_TOP_100() in the future. This function will be removed later.")


  # Download list
  data <- read.delim("http://microvesicles.org/Archive/EV_TOP_100.txt")


  # Translate Gene symbols to UniProt ids
  data <- select_org(keys = data[[1]],
                     columns = "UNIPROT",
                     output = "vector.rm",
                     keytype = "SYMBOL",
                     OrgDb = "org.Hs.eg.db")


  # Add new database entry
  add_database(database = data,
               id = "EV_TOP_100",
               type = "Protein_lists",
               replace = T)

}
