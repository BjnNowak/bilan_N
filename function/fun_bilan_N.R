library(tidyverse)

data<-tibble(
  plot=c("test"),          # name of the plot
  crop=c("Ble"),         # type of crop
  cultivar=c("Apache"),    # cultivar name (only for wheat)
  yield=c(80),             # yield target (numeric variable)
  tillers=c(2),            # number of tillers
  soil=c("Terres noires"), # soil type
  previous=c("Mais grain"),      # previous crop
  residues=c("Enfouis")   # crop residues management
)           

data

bilan_N<-function(data){
  balance <- data%>%
    left_join(
      read_delim('https://github.com/BjnNowak/bilan_N/raw/refs/heads/main/data/b_wheat.csv',delim=";"),
      by='cultivar' 
    )%>%
    left_join(
      read_delim('https://github.com/BjnNowak/bilan_N/raw/refs/heads/main/data/Rf.csv',delim=";"),
      by='soil'         
    )%>%
    left_join(
      read_delim('https://github.com/BjnNowak/bilan_N/raw/refs/heads/main/data/Ri.csv',delim=";"),
      by=c('soil','previous')         
    )%>%
    left_join(
      read_delim('https://github.com/BjnNowak/bilan_N/raw/refs/heads/main/data/Mh.csv',delim=";"),
      by=c('soil','residues')         
    )%>%
    left_join(
      read_delim('https://github.com/BjnNowak/bilan_N/raw/refs/heads/main/data/Mr.csv',delim=";"),
      by=c('previous','residues')         
    )%>%
    mutate(Pf=yield*b)%>%
    mutate(Pi=10+5*tillers)%>%
    select(plot,Pf,Rf,Pi,Ri,Mh,Mr)%>%
    mutate(X=Pf+Rf-(Pi+Ri+Mh+Mr))
  
  return(balance)
}

bilan_N(data)

data_2<-tibble(
  plot=c("test","test2"),          # name of the plot
  crop=c("Ble","Ble"),         # type of crop
  cultivar=c("Apache","Uli 12"),    # cultivar name (only for wheat)
  yield=c(80,80),             # yield target (numeric variable)
  tillers=c(2,1),            # number of tillers
  soil=c("Terres noires","Alluvions"), # soil type
  previous=c("Mais grain","Colza"),      # previous crop
  residues=c("Enfouis","Enfouis")   # crop residues management
) 

bilan_N(data_2)


tst<-data%>%
  left_join(
    read_delim('https://github.com/BjnNowak/bilan_N/raw/refs/heads/main/data/Mh.csv',delim=";"),
    by=c('soil','residues')         
  )

tst<-read_delim('https://github.com/BjnNowak/bilan_N/raw/refs/heads/main/data/Mh.csv',delim=";")
tst
tst$soil
tst$previous
