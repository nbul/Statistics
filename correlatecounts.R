require(SummarizedExperiment)
require(DESeq2)
require(magrittr)


MyData <- read.csv(file="/Volumes/DataGurdon/Joshua Greig/mass-spec/180710result.csv", header=TRUE, sep=",")
MyData2 <- MyData[,c(7,13:20)]
MyData2[MyData2==""] <- NA
MyData2 <- MyData2[is.na(MyData2[,1]) == 0,]
MyData3 <- aggregate(MyData2[,2:9], list(MyData2[,1]), function(x) sum(unique(x)))

raw_counts_p120 <- as.matrix(MyData3[MyData3[,1] == "p120ctn", 2:9])
raw_counts <- as.matrix(MyData3[, 2:9])
cor_mat <- cor(as.data.frame(t(raw_counts)),as.data.frame(array(raw_counts_p120)), use='pairwise.complete.obs')
cor_mat2 <- as.data.frame(cor_mat)
rownames(cor_mat2) <- MyData3[,1]
library(psych)
cor_pvl <- corr.p(cor_mat, n = nrow(cor_mat), adjust="fdr", alpha=.05)
tbl <- as.data.frame(cbind(rownames(cor_mat2),as.numeric(cor_pvl$r),as.numeric(cor_pvl$p)))
colnames(tbl) <- c("name", "cor", "p-value")
tbl <- tbl[order(tbl$cor, decreasing=TRUE),]
keep <- tbl[as.numeric(as.character(tbl$cor)) >= 0.6,]
keep <- keep[is.na(keep[,1]) == 0,]
write.csv(keep, "/Volumes/DataGurdon/Joshua Greig/mass-spec/180710correlation.csv")
