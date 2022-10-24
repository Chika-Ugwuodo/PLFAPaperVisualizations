dataset <- read.csv("C:/Users/cju1004/OneDrive - University of New Hampshire/Desktop/PLFA_final_data_and_plots/Individual_PLFAs_heatmap.csv",
                    row.names = "PLFA") 
data <- as.matrix(dataset) #convert dataset to matrix using as.matrix function

library(pheatmap) #install first, then run this line of code to load in the package
library(RColorBrewer) #install, then run this to load in package
breaksList = seq(0, 40, by = 0.01)#set a sequence of numbers that go from 0 to 15, with 
#increments of I


library(vegan)
# distances = vegdist(t(data), method="bray")
# dendrogram <- hclust(distances, method="complete")
# plot(dendrogram)
library(ggpubr)

?pheatmap #let's you read heatmap documentation
PLFALabels <- c("cyclo C17:0", "cyclo C16:0", "ai17:0", "4,8,12triMe16:0", "i16:0", "ai15:0",
                "C22:6\u03c93c", "C22:5\u03c93c", "C22:5\u03c96", "C22:4\u03c96c",
                "C20:4\u03c93", "C20:4\u03c96",  "C20:3\u03c99c","C20:2\u03c96c",
                "C18:4\u03c93",  "C18:3\u03c93", "C18:3\u03c96",
                "C18:2\u03c96", "C18:2\u03c94", "C16:2\u03c96", "C22:1\u03c99c",
                "C20:1\u03c99c", "C18:1\u03c99", "C18:1\u03c95t", "C18:1\u03c97c",
                "C17:1\u03c97c", "C16:1\u03c99c", "C16:1\u03c97c", "C15:1\u03c95c",
                "C14:1\u03c95c", "C22:0", "C18:0", "C17:0", "C16:0",
                "C15:0", "C14:0", "C13:0", "C12:0", "C11:0")
annotation_row = data.frame(
Class = factor(rep(c("SFA", "MUFA", "PUFA", "BCFA", "CyclicFA"), c(9, 10, 14, 4, 2)), ordered = TRUE))

rownames(annotation_row) = rownames(data)


p <- pheatmap(data, cluster_rows=F, cellwidth=50, cellheight=15, 
              cluster_cols = F,
              #color=colorRampPalette(c("blue", "white", "red"))(100),
              breaks=c(seq(0, 5, length=60), seq(6, 30, length=20), seq(31, 50, length=20)),
              angle_col=45,labels_row = rev(PLFALabels), annotation_row = annotation_row, annotation_legend = F)


