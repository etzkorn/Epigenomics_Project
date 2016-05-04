### Load Data 
    setwd("~/Documents/GitHub/Methods_Project/")
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

### Wide Data Format
    df2 <- dcast(df, locus+chr+signal.type~cell+sample, value.var="signal")
    
### Write CSV
    write.csv(df, file="Clean_Data.csv")
    write.csv(df2, file="Clean_Data_Wide.csv")
    