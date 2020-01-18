par( mai = c(0.55,0.55,0.15,0.15), mfrow=c(1,3), mao = c(0.5,0.5,0.5,0.5))

# SIM1
alphaTtestAlphaPre1N = c(0.068, 0.059, 0.051, 0.050)
alphaJZSBFAlphaPre1N = c(0.026, 0.026, 0.019, 0.019)
alphaTtestAlphaPre05N = c(0.043, 0.056, 0.062, 0.050)
alphaJZSBFAlphaPre05N = c(0.017, 0.024, 0.015, 0.019)
alphaTtestAlphaPre01N = c(0.051, 0.048, 0.055, 0.056)
alphaJZSBFAlphaPre01N = c(0.016, 0.021, 0.023, 0.021)
alphaTtestAlphaPreNoPreN = c(0.043, 0.046, 0.038, 0.048)
alphaJZSBFAlphaPreNoPreN = c(0.021, 0.020, 0.018, 0.022)

plot(c(10,20,30,40),alphaTtestAlphaPre1N,ty="l",main="",ylab=expression(alpha),xlab="Sample size",ylim=c(0,0.15),xaxt='n', lwd=2)
lines(c(10,20,30,40),alphaJZSBFAlphaPre1N,lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTtestAlphaPre05N,col="blue",lwd=2)
lines(c(10,20,30,40),alphaJZSBFAlphaPre05N,col="blue",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTtestAlphaPre01N,col="green",lwd=2)
lines(c(10,20,30,40),alphaJZSBFAlphaPre01N,col="green",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTtestAlphaPreNoPreN ,col="red",lwd=2)
lines(c(10,20,30,40),alphaJZSBFAlphaPreNoPreN,col="red",lty="dotted",lwd=2)

axis(1, at=seq(0,40,by=10), labels=seq(0,40,by=10))

legend("topright", legend=c(expression(paste("t-test, ", alpha[pre], " = ", .10)),
                            expression(paste("t-test, ", alpha[pre], " = ", .05)),
                            expression(paste("t-test, ", alpha[pre], " = ", .01)),
                            expression(paste("t-test, no pretest")),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .10)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .05)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .01)),
                            expression(paste(BF[10], "no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)








# SIM2
alphaWilcoxAlphaPre1N = c(0.042, 0.068, 0.060, 0.056)
alphaRankSumBFAlphaPre1N = c(0.009, 0.010, 0.016, 0.024)
alphaWilcoxAlphaPre05N = c(0.054, 0.066, 0.056, 0.053)
alphaRankSumBFAlphaPre05N = c(0.012, 0.016, 0.019, 0.010)
alphaWilcoxAlphaPre01N = c(0.068, 0.080, 0.070, 0.070)
alphaRankSumBFAlphaPre01N = c(0.010, 0.014, 0.018, 0.012)
alphaWilcoxAlphaPreNoPreN = c(0.039, 0.051, 0.040, 0.056)
alphaRankSumBFAlphaPreNoPreN = c(0.005, 0.022, 0.010, 0.014)

plot(c(10,20,30,40),alphaWilcoxAlphaPre1N,ty="l",main="",ylab=expression(alpha),xlab="Sample size",ylim=c(0,0.15),xaxt="n",lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPre1N,lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaWilcoxAlphaPre05N,col="blue",lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPre05N,col="blue",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaWilcoxAlphaPre01N,col="green",lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPre01N,col="green",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaWilcoxAlphaPreNoPreN,col="red",lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPreNoPreN,col="red",lty="dotted",lwd=2)

axis(1, at=seq(0,40,by=10), labels=seq(0,40,by=10))

legend("topright", legend=c(expression(paste("Mann-Whitney-U-test, ", alpha[pre], " = ", .10)),
                            expression(paste("Mann-Whitney-U-test, ", alpha[pre], " = ", .05)),
                            expression(paste("Mann-Whitney-U-test, ", alpha[pre], " = ", .01)),
                            expression(paste("Mann-Whitney-U-test, no pretest")),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .10)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .05)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .01)),
                            expression(paste(BF[10], " no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)






# SIM3
alphaTtestAlphaPre05E = c(0.072, 0.150, 0.164, 0.000)
alphaJZSBFAlphaPre05E = c(0.037, 0.075, 0.094, 0.000)
alphaTtestAlphaPre1E = c(0.063, 0.131, 0.154, 0.200)
alphaJZSBFAlphaPre1E = c(0.025, 0.065, 0.083, 0.103)
alphaTtestAlphaPre01E = c(0.046, 0.097, 0.119, 0.145)
alphaJZSBFAlphaPre01E = c(0.023, 0.039, 0.067, 0.070)
alphaTtestAlphaPreNoPreE = c(0.037, 0.047, 0.051, 0.049)
alphaJZSBFAlphaPreNoPreE = c(0.018, 0.025, 0.018, 0.012)

plot(c(10,20,30,40),alphaTtestAlphaPre1E,ty="l",main="",ylab=expression(alpha),xlab="Sample size",ylim=c(0,0.3),col="black",xaxt='n',lwd=2)
lines(c(10,20,30,40),alphaJZSBFAlphaPre1E,col="black",lty="dotted",lwd=2)

points(c(10,20,30),alphaTtestAlphaPre05E[1:3],ty="l",col="blue",lwd=2)
points(c(10,20,30),alphaJZSBFAlphaPre05E[1:3],ty="l",lty="dotted",col="blue",lwd=2)

lines(c(10,20,30,40),alphaTtestAlphaPre01E,col="green",lwd=2)
lines(c(10,20,30,40),alphaJZSBFAlphaPre01E,col="green",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTtestAlphaPreNoPreE ,col="red",lwd=2)
lines(c(10,20,30,40),alphaJZSBFAlphaPreNoPreE,col="red",lty="dotted",lwd=2)

axis(1, at=seq(0,40,by=10), labels=seq(0,40,by=10))

legend("topright", legend=c(expression(paste("t-test, ", alpha[pre], " = ", .10)),
                            expression(paste("t-test, ", alpha[pre], " = ", .05)),
                            expression(paste("t-test, ", alpha[pre], " = ", .01)),
                            expression(paste("t-test, no pretest")),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .10)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .05)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .01)),
                            expression(paste(BF[10], "no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)



# SIM4
alphaWilcoxAlphaPre1E = c(0.043, 0.041, 0.053, 0.061)
alphaRankSumBFAlphaPre1E = c(0.008, 0.014, 0.014, 0.015)
alphaWilcoxAlphaPre05E = c(0.046, 0.059, 0.044, 0.056)
alphaRankSumBFAlphaPre05E = c(0.006, 0.012, 0.013, 0.015)
alphaWilcoxAlphaPre01E = c(0.031, 0.051, 0.057, 0.049)
alphaRankSumBFAlphaPre01E = c(0.005, 0.013, 0.015, 0.011)
alphaWilcoxAlphaPreNoPreE = c(0.038, 0.047, 0.032, 0.051)
alphaRankSumBFAlphaPreNoPreE = c(0.003, 0.017, 0.004, 0.008)

plot(c(10,20,30,40),alphaWilcoxAlphaPre1E,ty="l",main="",ylab=expression(alpha),xlab="Sample size",ylim=c(0,0.15),lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPre1E,lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaWilcoxAlphaPre05E,col="blue",lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPre05E,col="blue",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaWilcoxAlphaPre01E,col="green",lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPre01E,col="green",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaWilcoxAlphaPreNoPreE,col="red",lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPreNoPreE,col="red",lty="dotted",lwd=2)

legend("topright", legend=c(expression(paste("Mann-Whitney-U-test, ", alpha[pre], " = ", .10)),
                            expression(paste("Mann-Whitney-U-test, ", alpha[pre], " = ", .05)),
                            expression(paste("Mann-Whitney-U-test, ", alpha[pre], " = ", .01)),
                            expression(paste("Mann-Whitney-U-test, no pretest")),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .10)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .05)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .01)),
                            expression(paste(BF[10], " no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)



# SIM5
alphaTtestAlphaPre1U = c(0.031, 0.048, 0.039, 0.034)
alphaJZSBFAlphaPre1U = c(0.015, 0.022, 0.012, 0.009)
alphaTtestAlphaPre05U = c(0.045, 0.047, 0.037, 0.030)
alphaJZSBFAlphaPre05U = c(0.019, 0.018, 0.011, 0.007)
alphaTtestAlphaPre01U = c(0.051, 0.051, 0.050, 0.048)
alphaJZSBFAlphaPre01U = c(0.023, 0.025, 0.017, 0.018)
alphaTtestAlphaPreNoPreU = c(0.054, 0.052, 0.060, 0.052)
alphaJZSBFAlphaPreNoPreU = c(0.028, 0.023, 0.030, 0.020)

plot(c(10,20,30,40),alphaTtestAlphaPre1U,ty="l",main="",ylab=expression(alpha),xlab="Sample size",ylim=c(0,0.1),lwd=2)
lines(c(10,20,30,40),alphaJZSBFAlphaPre1U,lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTtestAlphaPre05U,col="blue",lwd=2)
lines(c(10,20,30,40),alphaJZSBFAlphaPre05U,col="blue",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTtestAlphaPre01U,col="green",lwd=2)
lines(c(10,20,30,40),alphaJZSBFAlphaPre01U,col="green",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTtestAlphaPreNoPreU ,col="red")
lines(c(10,20,30,40),alphaJZSBFAlphaPreNoPreU,col="red",lty="dotted")

legend("topright", legend=c(expression(paste("t-test, ", alpha[pre], " = ", .10)),
                            expression(paste("t-test, ", alpha[pre], " = ", .05)),
                            expression(paste("t-test, ", alpha[pre], " = ", .01)),
                            expression(paste("t-test, no pretest")),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .10)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .05)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .01)),
                            expression(paste(BF[10], "no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)


# SIM6
alphaWilcoxAlphaPre1U = c(0.062, 0.069, 0.062, 0.038)
alphaRankSumBFAlphaPre1U = c(0.015, 0.020, 0.019, 0.005)
alphaWilcoxAlphaPre05U = c(0.086, 0.067, 0.053, 0.052)
alphaRankSumBFAlphaPre05U = c(0.020, 0.018, 0.016, 0.010)
alphaWilcoxAlphaPre01U = c(0.159, 0.108, 0.065, 0.066)
alphaRankSumBFAlphaPre01U = c(0.028, 0.028, 0.020, 0.022)
alphaWilcoxAlphaPreNoPreU = c(0.042, 0.053, 0.040, 0.046)
alphaRankSumBFAlphaPreNoPreU = c(0.005, 0.009, 0.014, 0.006)

plot(c(10,20,30,40),alphaWilcoxAlphaPre1U,ty="l",main="",ylab=expression(alpha),xlab="Sample size",ylim=c(0,0.25),lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPre1U,lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaWilcoxAlphaPre05U,col="blue",lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPre05U,col="blue",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaWilcoxAlphaPre01U,col="green",lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPre01U,col="green",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaWilcoxAlphaPreNoPreU,col="red",lwd=2)
lines(c(10,20,30,40),alphaRankSumBFAlphaPreNoPreU,col="red",lty="dotted",lwd=2)

legend("topright", legend=c(expression(paste("Mann-Whitney-U-test, ", alpha[pre], " = ", .10)),
                            expression(paste("Mann-Whitney-U-test, ", alpha[pre], " = ", .05)),
                            expression(paste("Mann-Whitney-U-test, ", alpha[pre], " = ", .01)),
                            expression(paste("Mann-Whitney-U-test, no pretest")),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .10)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .05)),
                            expression(paste(BF[10], ", ", alpha[pre], " = ", .01)),
                            expression(paste(BF[10], "no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)


# SIM7
alphaTwoStageAlphaPre1N = c(0.0514, 0.0544, 0.0558, 0.0452)
alphaTwoStageAlphaPre1NBF = c(0.0230, 0.0210, 0.0198, 0.0132)
alphaTwoStageAlphaAlphaPre05N = c(0.0450, 0.0524, 0.0526, 0.0518)
alphaTwoStageAlphaAlphaPre05NBF = c(0.0190, 0.0204, 0.0182, 0.0170)
alphaTwoStageAlphaAlphaPre01N = c(0.0484, 0.0512, 0.0546, 0.0516)
alphaTwoStageAlphaAlphaPre01NBF = c(0.0226, 0.0198, 0.0176, 0.0160)
alphaTwoStageAlphaAlphaPreNoPreN = c(0.0518, 0.0506, 0.0488, 0.0514)
alphaTwoStageAlphaAlphaPreNoPreNBF = c(0.0232, 0.0196, 0.0176, 0.0154)


plot(c(10,20,30,40),alphaTwoStageAlphaPre1N,ty="l",main="",ylab=expression(alpha),xlab="Sample size",ylim=c(0,0.1),xaxt='n',lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaPre1NBF,lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre05N,col="blue",lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre05NBF,col="blue",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre01N,col="green",lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre01NBF,col="green",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPreNoPreN ,col="red",lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPreNoPreNBF,col="red",lty="dotted",lwd=2)

axis(1, at=seq(0,40,by=10), labels=seq(0,40,by=10))

legend("topright", legend=c(expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .10)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .05)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .01)),
                            expression(paste("t-test, no pretest")),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .10)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .05)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .01)),
                            expression(paste("Bayesian param. two-sample test, ", "no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)


# SIM8
alphaTwoStageAlphaPre1E = c(0.0422, 0.0484, 0.0488, 0.0472)
alphaTwoStageAlphaPre1EBF = c(0.0140, 0.0134, 0.0158, 0.0142)
alphaTwoStageAlphaAlphaPre05E = c(0.0554, 0.0502, 0.0430, 0.0450)
alphaTwoStageAlphaAlphaPre05EBF = c(0.0186, 0.0146, 0.0130, 0.0138)
alphaTwoStageAlphaAlphaPre01E = c(0.0524, 0.0604, 0.0496, 0.0504)
alphaTwoStageAlphaAlphaPre01EBF = c(0.0196, 0.0182, 0.0152, 0.0150)
alphaTwoStageAlphaAlphaPreNoPreE = c(0.0372, 0.0476, 0.0458, 0.0518)
alphaTwoStageAlphaAlphaPreNoPreEBF = c(0.0154, 0.0174, 0.0184, 0.0152)

plot(c(10,20,30,40),alphaTwoStageAlphaPre1E,ty="l",main="",ylab=expression(alpha),xlab="Sample size",ylim=c(0,0.1),xaxt='n',lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaPre1EBF,lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre05E,col="blue",lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre05EBF,col="blue",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre01E,col="green",lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre01EBF,col="green",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPreNoPreE ,col="red",lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPreNoPreEBF,col="red",lty="dotted",lwd=2)

axis(1, at=seq(0,40,by=10), labels=seq(0,40,by=10))

legend("topright", legend=c(expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .10)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .05)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .01)),
                            expression(paste("t-test, no pretest")),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .10)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .05)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .01)),
                            expression(paste("Bayesian param. two-sample test, ", "no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)


# SIM9
alphaTwoStageAlphaPre1U = c(0.0496, 0.0470, 0.0528, 0.0514)
alphaTwoStageAlphaPre1UBF = c(0.0166, 0.0144, 0.0156, 0.0180)
alphaTwoStageAlphaAlphaPre05U = c(0.0464, 0.0528, 0.0490, 0.0504)
alphaTwoStageAlphaAlphaPre05UBF = c(0.0174, 0.0162, 0.0144, 0.0158)
alphaTwoStageAlphaAlphaPre01U = c(0.0510, 0.0514, 0.0544, 0.0494)
alphaTwoStageAlphaAlphaPre01UBF = c(0.0240, 0.0190, 0.0174, 0.0130)
alphaTwoStageAlphaAlphaPreNoPreU = c(0.0514, 0.0474, 0.0532, 0.0502)
alphaTwoStageAlphaAlphaPreNoPreUBF = c(0.0246, 0.0192, 0.0204, 0.0150)


plot(c(10,20,30,40),alphaTwoStageAlphaPre1U,ty="l",main="",ylab=expression(alpha),xlab="Sample size",ylim=c(0,0.1),xaxt='n',lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaPre1UBF,lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre05U,col="blue",lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre05UBF,col="blue",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre01U,col="green",lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPre01UBF,col="green",lty="dotted",lwd=2)

lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPreNoPreU ,col="red",lwd=2)
lines(c(10,20,30,40),alphaTwoStageAlphaAlphaPreNoPreUBF,col="red",lty="dotted",lwd=2)

axis(1, at=seq(0,40,by=10), labels=seq(0,40,by=10))

legend("topright", legend=c(expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .10)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .05)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .01)),
                            expression(paste("t-test, no pretest")),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .10)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .05)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .01)),
                            expression(paste("Bayesian param. two-sample test, ", "no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)





# SIM10 
betaTwoStageAlphaPre1N = c(0.7054, 0.4872, 0.3026, 0.1820, 0.1030, 0.0604, 0.0354, 0.0144)
betaTwoStageAlphaPre1NBF = c(0.8300, 0.6432, 0.4624, 0.3300, 0.2202, 0.1408, 0.0890, 0.0568)
betaTwoStageAlphaAlphaPre05N = c(0.7302, 0.4944, 0.2988, 0.1832, 0.1122, 0.0644, 0.0310, 0.0200)
betaTwoStageAlphaAlphaPre05NBF = c(0.8294, 0.6498, 0.4560, 0.3334, 0.2174, 0.1416, 0.0952, 0.0562)
betaTwoStageAlphaAlphaPre01N = c(0.7322, 0.4922, 0.3026, 0.1804, 0.1108, 0.0572, 0.0294, 0.0156)
betaTwoStageAlphaAlphaPre01NBF = c(0.8334, 0.6442, 0.4674, 0.3246, 0.2306, 0.1396, 0.0878, 0.0582)
betaTwoStageAlphaAlphaPreNoPreN = c(0.7266, 0.4756, 0.2960, 0.1738, 0.1060, 0.0606, 0.0306, 0.0162)
betaTwoStageAlphaAlphaPreNoPreNBF = c(0.8342, 0.6276, 0.4668, 0.3116, 0.2186, 0.1430, 0.0842, 0.0580)


plot(seq(from=10,to=80,by=10),betaTwoStageAlphaPre1N,ty="l",main="",ylab=expression(beta),xlab="Sample size",ylim=c(0,1.3),xaxt='n',lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaPre1N,pch=17)
lines(seq(from=10,to=80,by=10),betaTwoStageAlphaPre1NBF,lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaPre1NBF,pch=2)

lines(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre05N,col="blue",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre05N,pch=17,col="blue")
lines(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre05NBF,col="blue",lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre05NBF,pch=2,col="blue")

lines(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre01N,col="green",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre01N,pch=17,col="green")
lines(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre01NBF,col="green",lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre01NBF,pch=2,col="green")

lines(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPreNoPreN ,col="red",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPreNoPreN,pch=17,col="red")
lines(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPreNoPreNBF,col="red",lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPreNoPreNBF,pch=2,col="red")

axis(1, at=seq(from=10,to=80,by=10), labels=seq(from=10,to=80,by=10))

legend("topright", legend=c(expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .10)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .05)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .01)),
                            expression(paste("t-test, no pretest")),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .10)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .05)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .01)),
                            expression(paste("Bayesian param. two-sample test, ", "no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)





# SIM11
betaTwoStageAlphaPre1E = c(0.7626, 0.5478, 0.3672, 0.2490, 0.1634, 0.0976, 0.0634, 0.0376)
betaTwoStageAlphaPre1EBF = c(0.8864, 0.7372, 0.5546, 0.4184, 0.3104, 0.2110, 0.1476, 0.0982)
betaTwoStageAlphaAlphaPre05E = c(0.7426, 0.5448, 0.3930, 0.2598, 0.1688, 0.0980, 0.0618, 0.0370)
betaTwoStageAlphaAlphaPre05EBF = c(0.8760, 0.7390, 0.5804, 0.4204, 0.3110, 0.2144, 0.1378, 0.0938)
betaTwoStageAlphaAlphaPre01E = c(0.7450, 0.5334, 0.3888, 0.2560, 0.1642, 0.0984, 0.0612, 0.0440)
betaTwoStageAlphaAlphaPre01EBF = c(0.8756, 0.7108, 0.5722, 0.4320, 0.3076, 0.1972, 0.1380, 0.1006)
betaTwoStageAlphaAlphaPreNoPreE = c(0.7964, 0.4958, 0.2902, 0.1552, 0.0758, 0.0420, 0.0220, 0.0078)
betaTwoStageAlphaAlphaPreNoPreEBF = c(0.8792, 0.6680, 0.4728, 0.3226, 0.1876, 0.1220, 0.0664, 0.0360)


plot(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaPre1E,ty="l",main="",ylab=expression(beta),xlab="Sample size",ylim=c(0,1.3),xaxt='n',lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaPre1E,pch=17,col="black")
lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaPre1EBF,lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaPre1EBF,pch=17,col="black")


lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPre05E,col="blue",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre05E,pch=17,col="blue")
lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPre05EBF,col="blue",lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre05EBF,pch=17,col="blue")


lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPre01E,col="green",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre01E,pch=17,col="green")
lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPre01EBF,col="green",lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre01EBF,pch=17,col="green")

lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPreNoPreE ,col="red",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPreNoPreE,pch=17,col="red")
lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPreNoPreEBF,col="red",lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPreNoPreEBF,pch=17,col="red")

axis(1, at=seq(0,80,by=10), labels=seq(0,80,by=10))

legend("topright", legend=c(expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .10)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .05)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .01)),
                            expression(paste("t-test, no pretest")),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .10)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .05)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .01)),
                            expression(paste("Bayesian param. two-sample test, ", "no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)





# SIM12
betaTwoStageAlphaPre1U = c(0.0620, 0.0026, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000)
betaTwoStageAlphaPre1UBF = c(0.1428, 0.0044, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000)
betaTwoStageAlphaAlphaPre05U = c(0.0484, 0.0014, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000)
betaTwoStageAlphaAlphaPre05UBF = c(0.1102, 0.0024, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000)
betaTwoStageAlphaAlphaPre01U = c(0.0382, 0.0010, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000)
betaTwoStageAlphaAlphaPre01UBF = c(0.0918, 0.0020, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000)
betaTwoStageAlphaAlphaPreNoPreU = c(0.0330, 0.0002, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000)
betaTwoStageAlphaAlphaPreNoPreUBF = c(0.0898, 0.0006, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000)


plot(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaPre1U,ty="l",main="",ylab=expression(beta),xlab="Sample size",ylim=c(0,1.3),xaxt='n',lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaPre1U,pch=17,col="black")
lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaPre1UBF,lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaPre1UBF,pch=17,col="black")

lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPre05U,col="blue",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre05U,pch=17,col="blue")
lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPre05UBF,col="blue",lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre05UBF,pch=17,col="blue")

lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPre01U,col="green",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre01U,pch=17,col="green")
lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPre01UBF,col="green",lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPre01UBF,pch=17,col="green")

lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPreNoPreU ,col="red",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPreNoPreU,pch=17,col="red")
lines(c(10,20,30,40,50,60,70,80),betaTwoStageAlphaAlphaPreNoPreUBF,col="red",lty="dotted",lwd=2)
points(seq(from=10,to=80,by=10),betaTwoStageAlphaAlphaPreNoPreUBF,pch=17,col="red")

axis(1, at=seq(0,80,by=10), labels=seq(0,80,by=10))

legend("topright", legend=c(expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .10)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .05)),
                            expression(paste("t-test & Mann-Whitney's U, ", alpha[pre], " = ", .01)),
                            expression(paste("t-test, no pretest")),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .10)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .05)),
                            expression(paste("Bayesian two-sample tests, ", alpha[pre], " = ", .01)),
                            expression(paste("Bayesian param. two-sample test, ", "no pretest"))),
       col=c("black", "blue", "green","red",
             "black", "blue", "green","red"), lty=c(1,1,1,1,2,2,2,2), cex=1.2)

