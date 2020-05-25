
#####»¹Ô­ÎÄ×ÖÍ¼Æ¬
library(png)
fig=readPNG("fig.png")
newfig=0.435*fig[,,1]+0.282*fig[,,2]+0.168*fig[,,3]+fig[,,4]
outfig=adaptive_threthold(newfig)
writePNG(outfig,"outfig.png")


#####3D RNA Sample
library(R.matlab)
volume <- readMat('sample.mat')$vol
label  <- readMat('true_label.mat')$md
label[label!=0] = 1
out.data=adaptive_threthold_3(volume)
library(caret)
predict=as.factor(out.data)
true_label=as.factor(label)
confusionMatrix(predict,true_label)
table=table(predict,true_label)
TPR=table[1]/(table[1]+table[2])
FPR=table[4]/(table[4]+table[3])
TNR=1-FPR
FNR=1-TPR
