setwd("C:\\Users\\wxf\\Desktop")
adaptive_threthold=function(input){
  w=nrow(input)
  h=ncol(input)
  intimg=matrix(data=NA,nrow=w,ncol=h)
  output=matrix(data=NA,nrow=w,ncol=h)
  t=15
  s=w/8
  for (i in 1:w){
    sum=0
    for (j in 1:h){
      sum=sum+input[i,j]
      if (i==1){
        intimg[i,j]=sum
      }
      else{
        intimg[i,j]=intimg[i-1,j]+sum
      }
    }
  }
  for(i in 1:w){
    for (j in 1:h){
      x1=max(i-s/2,2)
      x2=min(i+s/2,w)
      y1=max(j-s/2,2)
      y2=min(j+s/2,h)
      count=(x2-x1)*(y2-y1)
      sum=intimg[x2,y2]-intimg[x2,y1-1]-intimg[x1-1,y2]+intimg[x1-1,y1-1]
      if (input[i,j]*count <= sum*(100-t)/100){
        output[i,j]=0
      }
      else{
        output[i,j]=1
      }
    }
  }
  return(output)
}

