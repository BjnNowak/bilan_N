# Function to compute nitrogen budget
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
    # Compute amount of nitrogen
    mutate(X=Pf+Rf-(Pi+Ri+Mh+Mr))
  
  return(balance)
}
