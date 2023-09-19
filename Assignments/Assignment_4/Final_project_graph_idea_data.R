library(tidyverse)



N <- c(1:50)

DNA_Samples <- paste0('DNA_',N)
#DNA will be separated based on E-value 
E_value <- runif(50, min = 0, max = 1)
#in this case an E-value < .5 would be considered DNA
#of unknown origin however in a real data set the cutoff evalue 
# would probably be closer to around 1e-20~30
acceptable <- E_value<.5

df <- cbind(DNA_Samples,E_value, acceptable)

df_2 <- data.frame(df)


ggplot(df_2, aes(x="", fill=factor(acceptable))) +
         geom_bar(width = 1) +
         coord_polar("y", start=0)
# Idealy I would then compare all remaining 'non human' DNA fragments from each species against each other 
#to identify if any of the DNA is shared between any, or all, of the 3 specimens 
#each segment is around 300 base pairs long, so the odds that any two segments match up
#perfectly by random chance should be 1/4^300th. 
#however the DNA probably wont be neatly cut so that the first nucleotide of the search DNA strand lines up with the first 
#nucleotide I'm searching against so I'll have to compare the search DNA against the other DNA starting from each position
#its also possible that the reverse complements may be what is present in the search DNA so it may be nessicary to generate the
#reverse complement and also check that against the other strands in the same way
# I have no idea how to do any of that in R aside from generatting the reverse complement.


#generating reverse compliment
DNA<- c('AGAATGATGTAAGATTTGCTTATCAGAATGAGTATTGTGAGAACTGTATAAGTGGTAGGGTAGCCATATGAGGATGGTGAAACTTTAAGAACAGAGACCAGCGAGGAGGCGATTACAGTAATCCAGGTAAAAGATGAAGAAGGAAAGAGTGAGTTTCATTCTAGATCCTTCCCCCTCCCTTACCCCCTACATTTAATCAATAACTCCTGTTAATTCTAATTCTTAATATCCAACAACATATTTCCTCATCTGTCTTCACTGACACTACCAGACTCTTTCCTTCTTCATCTTTTACCTGGA')
D_N_A <- unlist(strsplit(DNA,split = ""))
A_N_D <- rev(D_N_A)
a<-A_N_D=='A'
t<-A_N_D=='T'
c<-A_N_D=='C'
g<-A_N_D=='G'

A_N_D<- replace(A_N_D,a,'T')
A_N_D<-replace(A_N_D,t,'A')
A_N_D<-replace(A_N_D,c,'G')
A_N_D<-replace(A_N_D,g,'C')
#this could probably be done more efficiently for large amounts of data in a forloop 