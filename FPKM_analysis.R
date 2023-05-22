# Script to manipulate the gene expression data by ggplot

setwd("D:/RNA seq - analysis/gene expression/GSE232487")

# loading the library 
library(dplyr)
library(tidyverse)
library(GEOquery)
library(data.table)
library(readxl)

# loading the data set 
geo_data <- read_excel("GSE232487_FPKM_all_compare.xls")

# data dimension and columns name

dim(geo_data)
colnames(geo_data)

#getting the GSE entity which has a metadata section 
#In GSE entity, GEO Data Table has no tables

gse<- getGEO(GEO = 'GSE232487', GSEMatrix = TRUE)
# printing the entities
gse

length(gse)

# Print the samples id 
sampleNames(gse)


#Getting the metadata from GSE entities 

meta_df<-pData(phenoData(gse[[1]]))
head(meta_df)
colnames(meta_df)

# selecting the columns which are necessary and rename
meta_modified <- meta_df %>%
  select(1,10,11) %>%
  rename(tissue = characteristics_ch1) %>%
  rename(treatment = characteristics_ch1.1)


#  removing the tissues and treatments in each row of  3,4 column
meta_changes <- meta_modified%>%
  mutate(tissue=gsub("tissue: ","",tissue)) %>%
  mutate(treatment= gsub("treatment: ","",treatment))
meta_changes  


# selecting the from geo_data set 
geo_modified <- geo_data %>%
  select(19,8,9,10,11,12,13)%>%
  rename(Sham1 = Sham1_fpkm, Sham2 = Sham2_fpkm, Sham3 =Sham3_fpkm )%>%
  rename(IANX1 = IANX1_fpkm , IANX2=IANX2_fpkm, IANX3=IANX3_fpkm) %>%
  rename(gene=gene_name)

# wide to long format 
geo_long <- geo_modified %>%
  gather(key = 'samples', value ='FPKM', -gene)

# join the data frames Geo_long and meta changes 
geo_join <- geo_long %>%
  left_join(.,meta_changes, by = c("samples" = "title"))

# scatter plot without filter the data

ggplot(data = geo_join) +
  geom_point(mapping = aes(x = samples,y = FPKM ))

ggplot(data =geo_join)+
  geom_point(mapping = aes(x = FPKM,y = samples))

selcted_gene <- geo_join %>%
  filter(gene =='U6' | gene == "Rpl39" |  gene == "U6atac" |  gene == "SNORA61"| gene == "pRNA" )


# Non - filterning datad

ggplot(data = geo_join) +
  geom_point(mapping = aes(x= FPKM,y= samples,color = gene, shape = gene ))

ggplot(data = geo_join) +
  geom_point(mapping = aes(x= FPKM,y= samples,color = gene, shape = gene )) +


#  Plotting the filtering Data 

# "stat='identity', which is basically telling ggplot2 you will provide the y-values for the barplot, rather than counting the aggregate number of rows for each x value, which is the default stat=count

ggplot(selcted_gene, aes(x= samples, y= FPKM, fill= gene,color = gene,)) +
  geom_point(stat='identity') +
  facet_wrap(~ gene
             )
ggplot(selcted_gene, aes(x= samples, y= FPKM, fill= gene,color = gene)) +
  geom_bar(stat='identity') 



