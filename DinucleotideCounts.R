
DinucleotideCounts <- function(fasta) {
  
  #program required for converting FASTA To vector
  library(seqinr)
  
  #import FASTA file
  fastaCsv <- read.fasta(file = (fasta), seqtype = "DNA", forceDNAtolower = FALSE)
  #create data frame from FASTA file
  fastaDf <- as.data.frame(lapply(fastaCsv, as.vector))
  #convert to vector format
  data <- apply(fastaDf, length(fastaDf), as.vector)
  
  
  #variable to keep track of nucleotide corresponding to i-1
  check <- "N"  
  
  #variables to store cumulative count of each nucleotide
  Acount <- 0
  Ccount <- 0
  Tcount <- 0
  Gcount <- 0
  
  #variables to store cumulative count of each dinucleotide
  AA <- 0
  AC <- 0
  AT <- 0
  AG <- 0
  CA <- 0
  CC <- 0
  CT <- 0
  CG <- 0
  TA <- 0
  TC <- 0
  TT <- 0
  TG <- 0
  GA <- 0
  GC <- 0
  GT <- 0
  GG <- 0
  
  
  for(i in data[1:(length(data))])         {
    
    #i=A
    if( i == "A" )                  {
      if(check == "A") {
        AA <- AA + 1    }
      if(check == "C") {
        CA <- CA + 1    }
      if(check == "T") {
        TA <- TA + 1    }
      if(check == "G") {
        GA <- GA + 1    }
      
      check <- "A"
      Acount <- Acount + 1        }
    
    #i=C
    if( i == "C" )                  {
      if(check == "A") {
        AC <- AC + 1    }
      if(check == "C") {
        CC <- CC + 1    }
      if(check == "T") {
        TC <- TC + 1    }
      if(check == "G") {
        GC <- GC + 1    }
      
      check <- "C"
      Ccount <- Ccount + 1        }
    
    #i=T
    if( i == "T" )                  {
      if(check == "A") {
        AT <- AT + 1    }
      if(check == "C") {
        CT <- CT + 1    }
      if(check == "T") {
        TT <- TT + 1    }
      if(check == "G") {
        GT <- GT + 1    }
      
      check <- "T"
      Tcount <- Tcount + 1        }
    
    #i=G
    if( i == "G" )                  {
      if(check == "A") {
        AG <- AG + 1    }
      if(check == "C") {
        CG <- CG + 1    }
      if(check == "T") {
        TG <- TG + 1    }
      if(check == "G") {
        GG <- GG + 1    }
      
      check <- "G"
      Gcount <- Gcount + 1        }
    
  }
  
  #output
  return(list(
    TotalNucleotides=length(data), 
    Acount=Acount, 
    Ccount=Ccount, 
    Tcount=Tcount, 
    Gcount=Gcount, 
    AA=AA, 
    AC=AC, 
    AT=AT, 
    AG=AG, 
    CA=CA, 
    CC=CC, 
    CT=CT, 
    CG=CG, 
    TA=TA, 
    TC=TC, 
    TT=TT, 
    TG=TG, 
    GA=GA, 
    GC=GC, 
    GT=GT, 
    GG=GG))
  
}