conn<-connect.derby(db = "/home/alext/Documents/Research/gBLASTer/db/gblasterdb", usr = "APP",
                    derby.jar = "/home/alext/Developer/gBLASTer/out/artifacts/gBLASTer_jar/derby-10.9.1.0.jar")

dbListTables(conn$conn)
dbGetQuery(conn$conn, "select * from genomes")
dbGetQuery(conn$conn, "select * from genomes where name='Takifugu_rubripes'")
dbSendUpdate(conn$conn, "update genomes set name='Methanosarcina_barkeri_str_Fusaro' where name='Takifugu_rubripes'")
dbSendUpdate(conn$conn, "update genomes set name='Takifugu_rubripes' where id_genome=17")

dbGetQuery(conn$conn, "select * from blasts FETCH FIRST 1 ROWS ONLY")
dbGetQuery(conn$conn, "select count(id_genome) from genomes")

system.time(c40s30<-dbGetQuery(conn$conn, "select ID_QUERY_GENOME, O.ID_QUERY_ORF, ID_TARGET_GENOME, SCORE 
           from 
           (select ID_QUERY_ORF from blasts group by ID_QUERY_ORF having count(ID_QUERY_ORF)=40) as O 
                               inner join blasts on O.ID_QUERY_ORF=blasts.ID_QUERY_ORF
                               FETCH FIRST 1000000 ROWS ONLY"))

c0s0<-dbGetQuery(conn$conn, "select ID_QUERY_GENOME, ID_QUERY_ORF, ID_TARGET_ORF, ID_TARGET_GENOME, SCORE 
           from blasts")

system.time(c2s80<-dbGetQuery(conn$conn, "select ID_QUERY_GENOME, ID_QUERY_ORF, ID_TARGET_ORF, ID_TARGET_GENOME, SCORE
                               from blasts where score >= 80
                               group by ID_QUERY_GENOME, ID_QUERY_ORF, ID_TARGET_ORF, ID_TARGET_GENOME, SCORE 
                               having count(ID_QUERY_ORF)>=40
                               "))

system.time(cAs80<-dbGetQuery(conn$conn, "select ID_QUERY_GENOME, ID_QUERY_ORF, ID_TARGET_ORF, ID_TARGET_GENOME, SCORE 
           from blasts where SCORE > 80"))

dbListConnections(conn$drv)
dbGetInfo(conn$drv)
dbDisconnect(conn=conn$conn)

dbGetQuery(conn$conn, "select ID_GENOME, NAME from genomes")
