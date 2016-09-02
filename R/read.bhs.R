#' Reads best hit files from the given folder and extracts a hit matrix (non-normalized)
#' 
#' @param \code{bh.folder} that contains the shortened bh files
#' @param \code{ext} shortened files extension, default ".short"
#' @param \code{sep} separator symbol(s) that split the file names in query and target genomes
#' @param \code{bit.score.cutoff} a minimal cutoff bit score for a hit to be considered, default=80
#' @return a data.frame representation of the hit matrix where rows are orfs and columns are target genomes
#' @examples
#' bh.folder<-"gBLASTer/bh"
#' head(test.table<-read.bhs(bh.folder = bh.folder))
#' 
read.bhs<-function(bh.folder, ext=".short", sep="_", bit.score.cutoff=80){
  
  bh.files<-list.files(path = bh.folder, 
                       pattern = ext, 
                       full.names = FALSE)
  genome.ids<-strsplit(x = bh.files, 
                       split = sep, 
                       fixed = TRUE)
  all.tables<-lapply(genome.ids, function(i){
    
    return(
      read.bh.file(qgen.id=i[1], 
                   tgen.id=i[3], 
                   bh.folder=bh.folder, 
                   bit.score.cutoff=bit.score.cutoff)
      )
    })
  
  bh.table<-rbind_all(all.tables) %>% spread(ID_TARGET_GENOME, COMULATIVE_BITSCORE, fill = 0)
  return(bh.table)
}