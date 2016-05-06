### Load Data 
    setwd("~/Documents/GitHub/Method_Project_Data/")
    data <- read.delim("Project1_Data_Nohead.txt")
    
### Packages
    require(reshape2)
    require(dplyr)
    
### Reshape Data
    df <- melt(data, id=c("locus", "chr", "start", "end", "strand"))
    df$strand <- NULL
    # grab cell types, signal types, and replicate (1,2) from previous var name
    id <- strsplit(as.character(df$variable), split="_")
    id <- matrix(unlist(id), ncol = 3, byrow = TRUE)
    colnames(id) <- c("cell", "signal.type", "sample")
    df <- cbind(df, id)
    df$sample <- as.character(df$sample)
    df$sample <- substr(df$sample, nchar(df$sample), nchar(df$sample))
    # delete old variable
    df$variable <- NULL 
    colnames(df)[colnames(df)=="value"] <- "signal"

### Medium Width Data Format
    df2 <- dcast(df, chr+locus+start+end+cell+signal.type ~ sample, value.var = "signal")
    colnames(df2)[7:8] <- c("sample_1", "sample_2")
    
### Wide Data Format
    df3 <- dcast(df, locus+chr+signal.type ~ cell+sample, value.var="signal")
    
### Write CSV
    save(df, file="Clean_Data_Narrow.Robj")
    save(df2, file="Clean_Data.Robj")
    save(df3, file="Clean_Data_Wide.Robj")
    