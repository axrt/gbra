#' Reads a processed best hit file for a pair of genomes given (see params).
#' Will install dplyr, tidyr and data.table if not avaliable yet
#' 
#' @param \code{qgen.id} query genome id
#' @param \code{tgen.id} target genome id
#' @param \code{bh.folder} from a given bh folder
#' @param \code{ending} with a given file ending, defualt "80.0.short" which means "score cutoff 80"
#' @param \code{sep} specific character that separates the genome ids and the margin, default "_"
#' @param \code{margin} a margin symbol that separates the geniome ids, default "VS"
#' @param \code{bit.score.cutoff} a minimal cutoff bit score for a hit to be considered, default=80
#' @return a plain data.table representation of the file with "QUERY_ORF_ID", "COMULATIVE_BITSCORE", "ID_QUERY_GENOME" and "ID_TARGET_GENOME"
#' @examples 
#' system.time(all.tables<-lapply(genome.ids, function(i){return(read.short.file(i[1],i[3],bh.folder))}))
#' 
read.bh.file<-function(qgen.id, tgen.id, bh.folder, ending="80.0.short", sep="_", margin="VS", bit.score.cutoff=80){
  if(!require("tidyr")){
    install.packages("tidyr")
  }
  if(!require("data.table")){
    install.packages("data.table")
  }
  if(!require("plyr")){
    install.packages("plyr")
  }
  if(!require("dplyr")){
    install.packages("dplyr")
  }
  input<-paste(bh.folder,paste(qgen.id, margin, tgen.id, ending, sep = sep),sep = "/")
  message(paste("Reading", input))
  table.bh<-fread(input = input, 
                  showProgress = TRUE,
                  sep = "\t", 
                  header = TRUE, 
                  stringsAsFactors = FALSE, 
                  data.table = TRUE, 
                  select=c("QUERY_ORF_ID", "COMULATIVE_BITSCORE")) %>% 
    filter(COMULATIVE_BITSCORE>=bit.score.cutoff) %>%
    mutate(ID_QUERY_GENOME=qgen.id, ID_TARGET_GENOME=tgen.id)
  
  return(table.bh)
}