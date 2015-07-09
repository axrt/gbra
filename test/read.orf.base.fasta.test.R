file<-"/home/alext/Documents/Research/gBLASTer/orfs/Acidilobus_saccharovorans"
test.fasta<-readAAStringSet(filepath = file)
test.fasta.df<-data.table(test.fasta, stringsAsFactors=FALSE)
length(rownames(test.fasta.df))
tast.fasta.df<-data.table(test.fasta.df,ids=rownames(test.fasta.df))

test.fasta.df<-read.orf.base.fasta.single(file = file)
