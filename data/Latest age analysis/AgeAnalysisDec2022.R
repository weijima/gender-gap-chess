
####### age analysis

# input files (same as in overleaf document)
name2=c(
'C:/Users/rls/aug20/research/ChessRatings/PermTests/RLS_Code/SupplementaryTableWithJuniorsNoInactives-RLS.csv',
'C:/Users/rls/aug20/research/ChessRatings/PermTests/RLS_Code/SupplementaryTableNoJuniorsNoInactives-RLS.csv',
'C:/Users/rls/aug20/research/ChessRatings/PermTests/RLS_Code/SupplementaryTableNoJuniorsWithInactives-RLS.csv',
'C:/Users/rls/aug20/research/ChessRatings/PermTests/RLS_Code/SupplementaryTableWithJuniorsWithInactives-RLS.csv')
# output files
name3=c(
'C:/Users/rls/aug20/research/ChessRatings/Meetings/12-05-22/AgePlotWithJNoI.csv',
'C:/Users/rls/aug20/research/ChessRatings/Meetings/12-05-22/AgePlotNoJNoI.csv',
'C:/Users/rls/aug20/research/ChessRatings/Meetings/12-05-22/AgePlotNoJWithI.csv',
'C:/Users/rls/aug20/research/ChessRatings/Meetings/12-05-22/AgePlotWithJWithI.csv')
# cycle through j=1,2,3,4

j=1

tab2=read.csv(name2[j])
nn=nrow(tab2)
out1=matrix(nrow=nn,ncol=6)
xx=tab2$AGEDIFFOVERALL[2:nn]
yy=tab2$MEAN_OBS[2:nn]
ww=1/tab2$MEAN_PTSD[2:nn]^2
ww=ww[!is.na(xx)]
yy=yy[!is.na(xx)]
xx=xx[!is.na(xx)]
yy=yy[order(xx)]
xx=xx[order(xx)]
# print out regression results
lm(yy~xx,weights=ww)$coef
summary(lm(yy~xx,weights=ww))
su1=summary(lm(yy~xx,weights=ww))$coef
out1[1:(nn-1),1]=xx
out1[1:(nn-1),2]=yy
out1[nn,1:2]=lm(yy~xx,weights=ww)$coef
xx=tab2$AGEDIFFMAX10[2:nn]
yy=tab2$MAX10_OBS[2:nn]-tab2$MAX10_PTMEAN[2:nn]
ww=1/tab2$MAX10_PTSD[2:nn]^2
ww=ww[!is.na(xx)]
yy=yy[!is.na(xx)]
xx=xx[!is.na(xx)]
yy=yy[order(xx)]
xx=xx[order(xx)]
# print out regression results
lm(yy~xx,weights=ww)$coef
summary(lm(yy~xx,weights=ww))
su2=summary(lm(yy~xx,weights=ww))$coef
out1[1:(nn-1),3]=xx
out1[1:(nn-1),4]=yy
out1[nn,3:4]=lm(yy~xx,weights=ww)$coef
xx=tab2$AGEDIFFMAX1[2:nn]
yy=tab2$MAX1_OBS[2:nn]-tab2$MAX1_PTMEAN[2:nn]
ww=1/tab2$MAX1_PTSD[2:nn]^2
u1=!is.na(xx)
ww=ww[!is.na(xx)]
yy=yy[!is.na(xx)]
xx=xx[!is.na(xx)]
yy=yy[order(xx)]
xx=xx[order(xx)]
# print out regression results
lm(yy~xx,weights=ww)$coef
summary(lm(yy~xx,weights=ww))
su3=summary(lm(yy~xx,weights=ww))$coef
out1[(1:(nn-1))[u1],5]=xx
out1[(1:(nn-1))[u1],6]=yy
out1[nn,5:6]=lm(yy~xx,weights=ww)$coef
out1=cbind(c(tab2[2:nn,1],'COEF'),out1)
out1=rbind(c('Country','AgeDiffAll','AdjRD-All','AgeDiffMax10','AdjRD-Max10','AgeDiffMax1','AdjRD-Max1'),out1)
write.csv(out1,name3[j])
# displaying weighted least squares output in latex notation (for supplementary tables 4,5,6,7)
su1=round(su1,3)
su2=round(su2,3)
su3=round(su3,3)
print(paste0('All & ',su1[1,1],'&',su1[1,2],'&',su1[1,3],'&',su1[1,4],'&',su1[2,1],'&',su1[2,2],'&',su1[2,3],'&',su1[2,4],'\\'))
print(paste0('Top 10 & ',su2[1,1],'&',su2[1,2],'&',su2[1,3],'&',su2[1,4],'&',su2[2,1],'&',su2[2,2],'&',su2[2,3],'&',su2[2,4],'\\'))
print(paste0('Top 1 & ',su3[1,1],'&',su3[1,2],'&',su3[1,3],'&',su3[1,4],'&',su3[2,1],'&',su3[2,2],'&',su3[2,3],'&',su3[2,4],'\\'))

