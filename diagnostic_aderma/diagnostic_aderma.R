#
aderma_diag$nomproduit <- ifelse(regexpr("dermalibour",aderma_diag$nomproduit,fixed=T) != -1,"dermalibour",aderma_diag$nomproduit) 
aderma_diag$nomproduit <- ifelse(regexpr("epitheliale",aderma_diag$nomproduit,fixed=T) != -1,"epitheliale",aderma_diag$nomproduit) 
aderma_diag$nomproduit <- ifelse(regexpr("rheacalm",aderma_diag$nomproduit,fixed=T) != -1,"rheacalm",aderma_diag$nomproduit) 
aderma_diag$nomproduit <- ifelse(regexpr("sensiphas",aderma_diag$nomproduit,fixed=T) != -1,"sensiphase",aderma_diag$nomproduit) 
aderma_diag$nomproduit <- ifelse(regexpr("exomega",aderma_diag$nomproduit,fixed=T) != -1,"xeramega",aderma_diag$nomproduit) 

ind_vir <- which(nchar(aderma_diag$nomproduit) > 12)
aderma_diag <- aderma_diag[-ind_vir,]

aderma_diag$brucha <- ifelse(regexpr("brule+|chaud+",aderma_diag$com,perl=T) != -1,1,0)
aderma_diag$demang <- ifelse(regexpr("demang",aderma_diag$com,perl=T) != -1,1,0)
aderma_diag$pictir <- ifelse(regexpr("picot+|tirail+",aderma_diag$com,perl=T) != -1,1,0)
aderma_diag$rouge <- ifelse(regexpr("rouge",aderma_diag$com,perl=T) != -1,1,0)
aderma_diag$seche <- ifelse(regexpr("Seche",aderma_diag$peau,perl=T) != -1,1,0)
aderma_diag$douleur <- ifelse(regexpr("douleur",aderma_diag$com,perl=T) != -1,0,1)

for(c in names(aderma_diag)) aderma_diag[,c] <- ifelse(is.na(aderma_diag[,c]),0,aderma_diag[,c])

res$verif_rdf <- ifelse(res[,1] == res[,2],"OK","KO")
res$verif_svm <- ifelse(res[,1] == res[,3],"OK","KO")

i <- 3
pred_nnet$predi <- ""
for (i in 1:length(pred_nnet[,1])) {
    pred_nnet[i,"predi"] <- names(pred_nnet)[which(pred_nnet[i,] == max(pred_nnet[i,1], pred_nnet[i,2],pred_nnet[i,3], pred_nnet[i,4], pred_nnet[i,5]))]
}


pred_rpart$predi <- ""
for (i in 1:length(pred_rpart[,1])) {
  pred_rpart[i,"predi"] <- names(pred_rpart)[which(pred_rpart[i,] == max(pred_rpart[i,1], pred_rpart[i,2],pred_rpart[i,3], pred_rpart[i,4], pred_rpart[i,5]))]
}

res$verif_rdf <- ifelse(res[,1] == res[,2],"OK","KO")
res$verif_svm <- ifelse(res[,1] == res[,3],"OK","KO")
res$verif_nnet <- ifelse(res[,1] == res[,4],"OK","KO")
res$verif_rpart <- ifelse(res[,1] == res[,5],"OK","KO")
