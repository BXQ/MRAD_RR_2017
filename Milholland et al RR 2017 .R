#Code to reproduce the analysis in the Rejuvenation Research article "'Best-guess' MRAD provides robust evidence for a limit to human lifespan" by Milholland, Dong & Vijg (2017)
#Article can be found at https://dx.doi.org/10.1089/rej.2017.2008
#For more context, see the Nature article "Evidence for a limit to human lifespan" at https://dx.doi.org/10.1038/nature19793
best_guess_MRAD_no_1997=best_guess_MRAD[best_guess_MRAD$Year!=1997,]
best_guess_MRAD_no_1997_or_77=best_guess_MRAD_no_1997[best_guess_MRAD_no_1997$Year!=1977,]
best_guess_MRAD_no_1997_or_77_or_99=best_guess_MRAD_no_1997_or_77[best_guess_MRAD_no_1997_or_77$Year!=1999,]
lin_mod=lm(Age~Year,data=best_guess_MRAD)
seg_mod=segmented(lin_mod,seg.Z=~Year)
lin_mod_no_1997=lm(Age~Year,data=best_guess_MRAD_no_1997)
lin_mod_no_1997_or_1977=lm(Age~Year,data=best_guess_MRAD_no_1997_or_77)
lin_mod_no_1997_77_or_99=lm(Age~Year,data=best_guess_MRAD_no_1997_or_77_or_99)
seg_mod_no_1997=segmented(lin_mod_no_1997,seg.Z=~Year)
seg_mod_no_1997_or_1977=segmented(lin_mod_no_1997_or_1977,seg.Z=~Year)
seg_mod_no_1997_77_or_99=segmented(lin_mod_no_1997_77_or_99,seg.Z=~Year)
plot(seg_mod,col=c("blue","orange"),ylim=c(108,124),lwd=3,ylab="\"Best-guess\" MRAD")
points(best_guess_MRAD,col=c(rep("blue",30),rep("orange",19)),pch=19)
plot(seg_mod_no_1997,col=c("blue","orange"),ylim=c(108,124),lwd=3,ylab="\"Best-guess\" MRAD")
points(best_guess_MRAD_no_1997,col=c(rep("blue",30),rep("orange",18)),pch=19)
plot(seg_mod_no_1997_or_1977,col=c("blue","orange"),ylab="\"Best-guess\" MRAD",ylim=c(108,124),lwd=3)
points(best_guess_MRAD_no_1997_or_77,col=c(rep("blue",29),rep("orange",20)),pch=19)
plot(seg_mod_no_1997_or_77_or_99,col=c("blue","orange"),ylab="\"Best-guess\" MRAD",ylim=c(108,124),lwd=3)
plot(seg_mod_no_1997_or_77_or_99,col=c("blue","orange"),ylim=c(108,124),lwd=3,ylab="\"Best-guess\" MRAD")
points(best_guess_MRAD_no_1997_or_77_or_99,col=c(rep("blue",27),rep("orange",20)),pch=19,ylim=c(108,124),ylab="\"Best-guess\" MRAD")

best_guess_MRAD_no_1997_or_77_or_99_pre1995=best_guess_MRAD_no_1997_or_77_or_99[best_guess_MRAD_no_1997_or_77_or_99$Year<=1995,]
best_guess_MRAD_no_1997_or_77_or_99_post1995=best_guess_MRAD_no_1997_or_77_or_99[best_guess_MRAD_no_1997_or_77_or_99$Year>1995,]

pre1995lm=(lm(Age~Year,data=best_guess_MRAD_no_1997_or_77_or_99_pre1995))
post1995lm=(lm(Age~Year,data=best_guess_MRAD_no_1997_or_77_or_99_post1995))
pre1995preds=predict(pre1995lm,interval = 'confidence')
post1995preds=predict(post1995lm,interval = 'confidence')
plot(best_guess_MRAD_no_1997_or_77_or_99,col=c(rep("blue",27),rep("orange",20)),pch=19,ylim=c(108,124),ylab="\"Best-guess\" MRAD")
polygon(c(rev(best_guess_MRAD_no_1997_or_77_or_99_post1995$Year), best_guess_MRAD_no_1997_or_77_or_99_post1995$Year), c(rev(post1995preds[ ,3]), post1995preds[ ,2]), col = 'tan1', border = NA)
polygon(c(rev(best_guess_MRAD_no_1997_or_77_or_99_post1995$Year), best_guess_MRAD_no_1997_or_77_or_99_post1995$Year), c(rev(post1995preds[ ,3]), post1995preds[ ,2]), col = 'yellow', border = NA)
polygon(c(rev(best_guess_MRAD_no_1997_or_77_or_99_pre1995$Year), best_guess_MRAD_no_1997_or_77_or_99_pre1995$Year), c(rev(pre1995preds[ ,3]), pre1995preds[ ,2]), col = 'lightblue', border = NA)

points(post1995preds[,1]~best_guess_MRAD_no_1997_or_77_or_99_post1995$Year,type="l",lwd=3,col="orange")
points(pre1995preds[,1]~best_guess_MRAD_no_1997_or_77_or_99_pre1995$Year,type="l",lwd=3,col="blue")
points(best_guess_MRAD_no_1997_or_77_or_99,col=c(rep("blue",27),rep("orange",20)),pch=19,ylim=c(108,124),ylab="\"Best-guess\" MRAD")
post_1995_extapolation_preds=predict(lm(Age~Year,data=best_guess_MRAD_no_1997_or_77_or_99_pre1995),newdata=best_guess_MRAD_no_1997_or_77_or_99_post1995)
lines(post1995extrapolation~best_guess_MRAD_no_1997_or_77_or_99_post1995$Year,lty=2,lwd=3,col="blue")

x=best_guess_MRAD$Year-min(best_guess_MRAD$Year)
y=best_guess_MRAD$Age-min(best_guess_MRAD$Age)
exp_mod=nls(y~(L*(1-exp(0-k*x))+b),start = list(L=1,k=1,b=1))

x=best_guess_MRAD_no_1997$Year-min(best_guess_MRAD_no_1997$Year)
y=best_guess_MRAD_no_1997$Age-min(best_guess_MRAD_no_1997$Age)
exp_mod_no_1997=nls(y~(L*(1-exp(0-k*x))+b),start = list(L=1,k=1,b=1))

x=best_guess_MRAD_no_1997_or_77$Year-min(best_guess_MRAD_no_1997_or_77$Year)
y=best_guess_MRAD_no_1997_or_77$Age-min(best_guess_MRAD_no_1997_or_77$Age)
exp_mod_no_1997_or_77=nls(y~(L*(1-exp(0-k*x))+b),start = list(L=1,k=1,b=1))

x=best_guess_MRAD_no_1997_or_77_or_99$Year-min(best_guess_MRAD_no_1997_or_77_or_99$Year)
y=best_guess_MRAD_no_1997_or_77_or_99$Age-min(best_guess_MRAD_no_1997_or_77_or_99$Age)
exp_mod_no_1997_or_77_or_99=nls(y~(L*(1-exp(0-k*x))+b),start = list(L=1,k=1,b=1))

plot(best_guess_MRAD$Age~best_guess_MRAD$Year,pch=19,ylim=c(108,124),ylab="\"Best-guess\" MRAD",xlab="Year")
lines(predict(exp_mod)+min(best_guess_MRAD$Age)~best_guess_MRAD$Year,lwd=3)
lines(predict(exp_mod_no_1997)+min(best_guess_MRAD_no_1997$Age)~best_guess_MRAD_no_1997$Year,lwd=3,col="red")
lines(predict(exp_mod_no_1997_or_77)+min(best_guess_MRAD_no_1997_or_77$Age)~best_guess_MRAD_no_1997_or_77$Year,lwd=3,col="blue")
lines(predict(exp_mod_no_1997_or_77_or_99)+min(best_guess_MRAD_no_1997_or_77_or_99$Age)~best_guess_MRAD_no_1997_or_77_or_99$Year,lwd=3,col="green")
legend("topleft",lty=1,col=c("black","red","blue","green"),legend=c("All years","1997 removed","1977 removed","1999 removed"),lwd=3)

print(summary(lin_mod))
print(summary(seg_mod))
print(summary(exp_mod))

print(summary(lin_mod_no_1997))
print(summary(seg_mod_no_1997))
print(summary(exp_mod_no_1997))

print(summary(lin_mod_no_1997_or_1977))
print(summary(seg_mod_no_1997_or_1977))
print(summary(exp_mod_no_1997_or_77))

print(summary(lin_mod_no_1997_77_or_99))
print(summary(seg_mod_no_1997_77_or_99))
print(summary(exp_mod_no_1997_or_77_or_99))

print("All points")
print(AIC(lin_mod))
print(AIC(seg_mod))
print(AIC(exp_mod))

print("1997 removed")
print(AIC(lin_mod_no_1997))
print(AIC(seg_mod_no_1997))
print(AIC(exp_mod_no_1997))

print("1977 removed")
print(AIC(lin_mod_no_1997_or_1977))
print(AIC(seg_mod_no_1997_or_1977))
print(AIC(exp_mod_no_1997_or_77))

print("1999 removed")
print(AIC(lin_mod_no_1997_77_or_99))
print(AIC(seg_mod_no_1997_77_or_99))
print(AIC(exp_mod_no_1997_or_77_or_99))

