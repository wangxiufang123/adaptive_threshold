adaptive_threthold_3=function(input){
  w=dim(input)[1]
  l=dim(input)[2]
  h=dim(input)[3]
  intimg=array(0,dim=c(w,l,h))
  output=array(0,dim=c(w,l,h))
  t=15
  s=w/8
  for (i in 1:w){
    sum=0
    for (j in 1:l){
      for (k in 1:h){
        sum=sum+input[i,j,k]
        if (i==1){
          intimg[i,j,k]=sum
        }
        else{
          intimg[i,j,k]=intimg[i-1,j,k]+sum
        }
      }
      
    }
  }
  
  for(i in 1:w){
    for (j in 1:l){
      for (k in 1:h){
        x1=max(i-s/2,2)
        x2=min(i+s/2,w)
        y1=max(j-s/2,2)
        y2=min(j+s/2,l)
        z1=max(k-s/2,2)
        z2=min(k+s/2,h)
        count=(x2-x1)*(y2-y1)*(z2-z1)
        sum=intimg[x2,y2,z2]-intimg[x2,y1-1,z2]-intimg[x1-1,y2,z2]-intimg[x2,y2,z1-1]+intimg[x1-1,y1-1,z2]+intimg[x1-1,y2,z1-1]+intimg[x2,y1-1,z1-1]-intimg[x1-1,y1-1,z1-1]
        if ( input[i,j,k]*count <= sum*(100-t)/100 ){
          output[i,j,k]=0
        }
        else{
          output[i,j,k]=1
        }
      }
      
    }
  }
  return(output)
}

