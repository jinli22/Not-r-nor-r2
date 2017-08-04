######## This file is for paper: Assessing the accuracy of predictive models for 
######## numerical data: Not r nor r2, why not? Then what? 
######## Journal: PLOS ONE ########
# Title: Predictive errors and accuracy for PONE supporting information 2.R
# Author: Jin Li
#         Geoscience Australia
#         jin.li@ga.gov.au
# Date:   July 2017
# Abstract: R functions and scripts for assessing the measurements of predictive 
# erorr and accuracy.
###############################################################################

# Accuracy measures
    vecv <- function (obs, pred) {(1-
        sum((obs-pred)^2) / sum((obs-mean(obs))^2)) * 100
        }
       
    e1 <- function (obs, pred) {(1-
        sum(abs(obs-pred)) / sum(abs(obs-mean(obs)))) * 100
        }
    
    dr <- function (obs, pred) {
        if (sum(abs(obs-pred)) / sum(abs(obs-mean(obs))) <= 1 )
        (1- sum(abs(obs-pred)) / sum(abs(obs-mean(obs)))) * 100 else
        (sum(abs(obs-mean(obs))) / sum(abs(obs-pred)) - 1) * 100
        }

# Generate data for simulation
    set.seed(168)
    n = 30
    x<-sample(1:n, 30)
    e<-rnorm(30, sd = 2)
    
    y000 <- x
    y0001 <- 0.6 * x
    y0001_1 <- mean(x) + 0.6 * (x - mean(x))

    lm000 <- lm(y000 ~ x)
    lm0001 <- lm(y0001 ~ x)
    lm0001_1 <- lm(y0001_1 ~ x)
    
# Figure 1    
 # Figure 1a
    tiff("Fig 1a.tif", width = 8, height = 8, units = 'in', res = 300, 
      compression = "lzw")
    par(mfrow = c(1,1))
    par (font.axis = 2, font.lab = 2)
    plot(y000 ~ x,  typ = "l", xlab = "Observed values (x)", 
      ylab = "Predicted values (y)")
    abline(lm000, col = "black", lty = 1)
    abline(lm0001, col = "black", lty = 1)
    points(25, 25, col = "blue")
    points(25, 15, pch = 19, col = "blue")
    #lines(c(25,25), c(25,15))
    arrows(25,25,25,15, length = 0.1, code = 3, lty = 2, col = "red")
    text(16, 17.5, expression(hat(y)[a] == x))
    text(17, 8.5, expression(hat(y)[b] == beta * x), col="black")
    text(c(23, 23.6, 24.1, 24.9, 25.5), c(26.2, 26.2, 26.2, 26.2,26.2,26.2), 
      expression ("(", x[1], ",", hat(y)[a1], ")"))
    text(c(25.5, 26.1, 26.6, 27.4, 28.5), c(14.7, 14.7,14.7, 14.7, 14.7), 
      expression ("(", x[1], ",", hat(y)[b1], ") or"), col="black")
    text(c(25.5, 26.1, 26.6, 27.4, 27.9), c(13.5, 13.5,13.5, 13.5, 13.5), 
      expression ("(", x[1], ",", y[1], ")"), col = "black")
    text(26.9, 20, expression (abs(hat(y)[a1] - y[1])), col="red")

    points(10, 3, pch = 19, col = "blue")
    points(10, 6, col = "blue")
    points(10, 10, col = "blue")
    arrows(10,3,10,6, length = 0.1, code = 3, col = "green")
    arrows(10,3,10,10, length = 0.1, code = 3, lty = 2, col = "red")
    text(c(8, 8.6, 9.1, 9.9, 10.6), c(11.2, 11.2, 11.2, 11.2,11.2,11.2), 
      expression ("(", x[3], ",", hat(y)[a3], ")"))
    text(c(10.5, 11.1, 11.6, 12.4, 13), c(5.8, 5.8,5.8, 5.8, 5.8), 
      expression ("(", x[3], ",", hat(y)[b3], ")"), col="black")
    text(c(10.5, 11.1, 11.6, 12.4, 13), c(3, 3,3, 3, 3), 
      expression ("(", x[3], ",", y[3], ")"), col="black")
    text(11.8, 8.6, expression (abs(hat(y)[a3] - y[3])), col="red")
    text(11.8, 4.5, expression (abs(hat(y)[b3] - y[3])), col="green")

    points(20, 20, col = "blue")
    points(20, 12, col = "blue")
    points(20, 15, pch = 19, col = "blue")
    arrows(20,12,20,15, length = 0.1, code = 3, col = "green")
    arrows(20,15,20,20, length = 0.1, code = 3, lty = 2, col = "red")
    text(c(18, 18.6, 19.1, 19.9, 20.6), c(21.2, 21.2, 21.2, 21.2,21.2,21.2), 
      expression ("(", x[2], ",", hat(y)[a2], ")"))
    text(c(20.5, 21.1, 21.6, 22.4, 23), c(11.8, 11.8,11.8, 11.8, 11.8), 
      expression ("(", x[2], ",", hat(y)[b2], ")"), col = "black")
    text(c(20.5, 21.1, 21.6, 22.4, 23), c(15, 15,15, 15, 15), 
      expression ("(", x[2], ",", y[2], ")"), col = "black")
    text(21.8, 17.3, expression (abs(hat(y)[a2] - y[2])), col = "red")
    text(21.8, 13.5, expression (abs(hat(y)[b2] - y[2])), col = "green")
    dev.off()

  # Figure 1b
    tiff("Fig 1b.tif", width = 8, height = 8, units = 'in', res = 300, 
      compression = "lzw")
    par(mfrow = c(1,1))
    par (font.axis = 2, font.lab = 2)
    plot(y000 ~ x,  typ = "l", xlab = "Observed values (x)", 
      ylab = "Predicted values (y)")
    abline(lm000, col = "black", lty = 1)
    abline(lm0001_1, col = "black", lty = 1)
    points(25, 25, col = "blue")
    points(25, 21.2, pch = 19, col = "blue")
    arrows(25,25,25,21.2, length = 0.1, code = 3, lty = 2, col = "red")
    text(19.5, 21, expression(hat(y)[a] == x))
    text(21, 16.7, expression(hat(y)[b] == beta[0] + beta[1] * x), col="black")
    text(c(23, 23.6, 24.1, 24.9, 25.5), c(26.2, 26.2, 26.2, 26.2,26.2,26.2), 
      expression ("(", x[1], ",", hat(y)[a1], ")"))
    text(c(25.5, 26.1, 26.6, 27.4, 28.5), c(20.5, 20.5,20.5, 20.5, 20.5), 
      expression ("(", x[1], ",", hat(y)[b1], ") or"), col = "black")
    text(c(25.5, 26.1, 26.6, 27.4, 27.9), c(19.1, 19.1, 19.1, 19.1, 19.1), 
      expression ("(", x[1], ",", y[1], ")"), col = "black")
    text(26.5, 23.7, expression (abs(hat(y)[a1] - y[1])), col = "red")
 
    #mean(x)+0.6*(10-mean(x))
    points(10, 12.2, col = "blue")
    points(10, 15, pch = 19, col = "blue")
    points(10, 10,  col = "blue")
    arrows(10,12.2,10,15, length = 0.1, code = 3, col = "green")
    arrows(10,10,10,15, length = 0.1, code = 3, lty = 2, col = "red")
    text(c(7.1, 7.7, 8.2, 8.9, 9.6), c(12.8, 12.8, 12.8, 12.8,12.8,12.8), 
      expression ("(", x[2], ",", hat(y)[b2], ")"))
    text(c(10.5, 11.1, 11.6, 12.4, 13), c(9.8, 9.8,9.8, 9.8, 9.8), 
      expression ("(", x[2], ",", hat(y)[a2], ")"), col = "black")
    text(c(10.5, 11.1, 11.6, 12.4, 13), c(15.5, 15.5, 15.5, 15.5, 15.5), 
      expression ("(", x[2], ",", y[2], ")"), col = "black")
    text(11.8, 11.5, expression (abs(hat(y)[a2] - y[2])), col = "red")
    text(11.8, 14, expression (abs(hat(y)[b2] - y[2])), col = "green")

    #mean(x)+0.6*(2-mean(x))
    points(2, 2, col = "blue")
    points(2, 5, pch = 19, col = "blue")
    points(2, 7.4, col = "blue")
    arrows(2,2,2,5, length = 0.1, code = 3, lty = 2, col = "red")
    arrows(2,5,2,7.4, length = 0.1, code = 3, col = "green")
    text(c(2.5, 3.1, 3.6, 4.4, 5), c(1.8, 1.8,1.8, 1.8, 1.8), 
      expression ("(", x[3], ",", hat(y)[a3], ")"), col = "black")
    text(c(2.5, 3.1, 3.6, 4.4, 5), c(5.3, 5.3, 5.3, 5.3, 5.3), 
      expression ("(", x[3], ",", y[3], ")"), col = "black")
    text(c(0, 0.6, 1.1, 1.9, 2.6), c(8.4, 8.4, 8.4, 8.4, 8.4, 8.4), 
      expression ("(", x[3], ",", hat(y)[b3], ")"))
    text(3.8, 3.5, expression (abs(hat(y)[a3] - y[3])), col = "red")
    text(3.8, 6.5, expression (abs(hat(y)[b3] - y[3])), col = "green")
    dev.off()

# Appendix 1
  # Appendix 1a
  # r and VEcv for predictions without errors, no intercept but different slopes
    y00 <- 1 * x
    y001 <- 0.9 * x
    y002 <- 0.6 * x
    y003 <- 0.3 * x
    data001 <- cbind(x, e, y00, y001, y002, y003)
    data001 <- as.data.frame(data001[order(x),])

    lm00 <- lm(y00 ~ x, data = data001)
    lm001 <- lm(y001 ~ x, data = data001)
    lm002 <- lm(y002 ~ x, data = data001)
    lm003 <- lm(y003 ~ x, data = data001)

    tiff("Appendix 1a.tif", width = 8, height = 8, units = 'in', res = 300, 
      compression = "lzw")
    par (font.axis = 2, font.lab = 2)
    plot(y00 ~ x, type = "l", data = data001, xlab = "Observed values (x)", 
      ylab = "Predicted values(y)")
    abline(lm00, col = "black", lty = 1)
    abline(lm001, col = "green", lty = 2)
    abline(lm002, col = "blue", lty = 3)
    abline(lm003, col = "red", lty = 4)
    legend(0.5,31, lty = 1:4, col = c("black", "green", "blue", "red"), 
      legend = c(
      "y1=1*x",
      "y0.9=0.9*x",
      "y0.6=0.6*x",
      "y0.3=0.3*x"), bty = "n")
    dev.off()

  # Appendix 1b
  # generate predictions without errors, same intercept but different slopes
    y00 <- 1 * x
    y1.01<- mean(x) + 0.9 * (x - mean(x))
    y2.01<- mean(x) + 0.6 * (x - mean(x))
    y3.01<- mean(x) + 0.3 * (x - mean(x))

    data1.01 <- cbind(x, y1.01, y2.01, y3.01, y00)
    data1.01 <- as.data.frame(data1.01[order(x),])

    lm1.01 <- lm(y1.01 ~ x, data = data1.01)
    lm2.01 <- lm(y2.01 ~ x, data = data1.01)
    lm3.01 <- lm(y3.01 ~ x, data = data1.01)
    lm00 <- lm(y00 ~ x, data = data1.01)

    tiff("Appendix 1b.tif", width = 8, height = 8, units = 'in', res = 300, 
      compression = "lzw")
        par(mfrow = c(1,1))
        par (font.axis = 2, font.lab = 2)
    plot(y00 ~ x, data = data1.01, type = "l", xlab = "Observed values (x)", 
      ylab = "Predicted values (y)")
    abline(lm00, col = "black", lty = 1)
    abline(lm1.01, col = "green", lty = 2)
    abline(lm2.01, col = "blue", lty = 3)
    abline(lm3.01, col = "red", lty = 4)
    legend(0.5,31, lty = 1:4, col = c("black", "green", "blue", "red"), 
      legend = c(
      "y1=1*x",
      "y0.9=mean(x)+0.9*(x-mean(x))",
      "y0.6=mean(x)+0.6*(x-mean(x))",
      "y0.3=mean(x)+0.3*(x-mean(x))"), bty="n")
    dev.off()

  # Appendix 1c
  # generate predictions with the same errors, no intercept but different slopes
    y0 <- 1 * x + e
    y01 <- 0.9 * x + e
    y02 <- 0.6 * x + e
    y03 <- 0.3 * x + e

    data01 <- cbind(x, e, y0, y01, y02, y03)
    data01 <- as.data.frame(data01[order(x),])

    lm0 <- lm(y0 ~ x, data = data01)
    lm01 <- lm(y01 ~ x, data = data01)
    lm02 <- lm(y02 ~ x, data = data01)
    lm03 <- lm(y03 ~ x, data = data01)

    tiff("Appendix 1c.tif", width = 8, height = 8, units = 'in', res = 300, 
      compression = "lzw")
        par(mfrow = c(1,1))
        par (font.axis = 2, font.lab = 2)
    plot(y0 ~ x, data = data01, xlab = "Observed values (x)", 
      ylab = "Predicted values (y)")
    abline(lm0, col = "black", lty = 1)
    points(x, y01, col = "green", pch = 19)
    abline(lm01, col = "green", lty = 2)
    points(x, y02, col = "blue", pch = 2)
    abline(lm02, col = "blue", lty = 3)
    points(x, y03, col = "red", pch = 17)
    abline(lm03, col = "red", lty = 4)
    legend(0.5,33, lty = 1:4, col = c("black", "green", "blue", "red"), 
      legend = c(
      expression (paste("y1 = 1*x+",epsilon)),
      expression (paste("y0.9 = 0.9*x+",epsilon)),
      expression (paste("y0.6 = 0.6*x+",epsilon)),
      expression (paste("y0.3 = 0.3*x+",epsilon))), bty="n")
    dev.off()

  # Appendix 1d
  # generate predictions with the same errors, but different intercept and slope
    y1_0 <- 1*x+e
    y1.1<- mean(x)+0.9*(x-mean(x))+e
    y2.1<- mean(x)+0.6*(x-mean(x))+e
    y3.1<- mean(x)+0.3*(x-mean(x))+e

    data1.1 <- cbind(x, e, y1.1, y2.1, y3.1, y1_0)
    data1.1 <- as.data.frame(data1.1[order(x),])

    lm1.1 <- lm(y1.1~x, data=data1.1)
    lm2.1 <- lm(y2.1~x, data=data1.1)
    lm3.1 <- lm(y3.1~x, data=data1.1)
    lm1_0 <- lm(y1_0~x, data=data1.1)

    tiff("Fig Appendix 1d.tif", width = 8, height = 8, units = 'in', res = 300, 
      compression = "lzw")
        par(mfrow = c(1,1))
        par (font.axis = 2, font.lab = 2)
    plot(y1_0 ~ x, data = data1.1, xlab = "Observed values", ylab = "Predicted values")
    abline(lm1_0, col = "black", lty = 1)
    points(x, y1.1, col = "green", pch = 19)
    abline(lm1.1, col = "green", lty = 2)
    points(x, y2.1, col = "blue", pch = 2)
    abline(lm2.1, col = "blue", lty = 3)
    points(x, y3.1, col = "red", pch = 17)
    abline(lm3.1, col = "red", lty = 4)
    legend(0.5, 33, lty = 1:4, col = c("black", "green", "blue", "red"), 
      legend = c(
      expression (paste("y1 = 1*x+",epsilon)),
      expression (paste("y0.9 = mean(x)+0.9*(x-mean(x))+",epsilon)),
      expression (paste("y0.6 = mean(x)+0.6*(x-mean(x))+",epsilon)),
      expression (paste("y0.3 = mean(x)+0.3*(x-mean(x))+",epsilon))), bty="n")
    dev.off()

# Figure 2
  # Figure 2a
  # no intercept, no noise
    slope <- seq(0.1, 1.2, 0.05)
    VEcv1 <- NULL
    R1 <- NULL
    E1 <- NULL
    Dr <- NULL
    for (i in 1:length(slope)) {
    pred1 <- slope[i] * x
    VEcv1[i] <- vecv(x, pred1)
    R1[i] <- cor(x, pred1)
    E1 [i] <- e1(x, pred1)
    Dr[i] <- dr(x, pred1)
    }

    library(plotrix)
    tiff("Fig 2a.tif", width = 8, height = 8, units = 'in', res = 300, 
      compression = "lzw")
    par (font.axis = 2, font.lab = 2)
    twoord.plot(slope, VEcv1, slope, R1, rylim = c(0,1.037), xlim = c(0, 1.2),
    type = "l", rcol = "red", lwd = 1, ylab = expression(bold(paste("Accuracy measures", 
      " (%)", sep = ""))), xlab = "Slope", rylab = "Correlation coefficient (r)")
    abline(h = 0, col = "blue")
    abline(v = 1.0, lty = 1, col = "blue")
    abline(v = 0.9, lty = 2, col = "green")
    abline(v = 0.6, lty = 3, col = "blue")
    abline(v = 0.3, lty = 2, col = "red")
    lines(Dr~slope, col = "orange")
    lines(E1~slope, col = "black", lty = 2)   
    legend(0,98, lty = c(1, 1, 1, 2), col = c("red", "black", "orange", "black"), 
      legend = c("r", expression(paste(VEcv, " (%)", sep = "")),
    expression(paste(d[r], " (%)", sep = "")), expression(paste(E[1], " (%)", 
      sep = ""))), bty = "n")
    dev.off()
    
    options(digits=3)
    > E1
    [1] -86.00 -75.67 -65.33 -55.00 -44.67 -34.33 -24.00 -13.67  -3.33   7.00
    [11]  17.33  27.67  38.00  48.33  58.67  69.00  79.33  89.67 100.00  89.67
    [21]  79.33  69.00  58.67

    > VEcv1
    [1] -240.76 -203.95 -169.24 -136.64 -106.14  -77.74  -51.45  -27.26   -5.17
    [10]   14.81   32.69   48.47   62.14   73.71   83.17   90.53   95.79   98.95
    [19]  100.00   98.95   95.79   90.53   83.17

    > Dr
    [1] -46.24 -43.07 -39.52 -35.48 -30.88 -25.56 -19.35 -12.02  -3.23   7.00
    [11]  17.33  27.67  38.00  48.33  58.67  69.00  79.33  89.67 100.00  89.67
    [21]  79.33  69.00  58.67

  # Figure 2b
  # with intercepts that are changing with slope and with a fixed mean, 
  # without noise
    options(digits=20)
    slope <- seq(0, 1.2, 0.05)
    VEcv2 <- NULL
    R2 <- NULL
    E2 <- NULL
    Dr2 <- NULL
    for (i in 1:length(slope)){
    pred1 <- mean(x) + slope[i] * (x-mean(x))
    VEcv2[i] <- vecv(x, pred1)
    R2[i] <- cor(pred1, x)
    E2 [i] <- e1(x, pred1)
    Dr2[i] <- dr(x, pred1)
    }

    tiff("Fig 2b.tif", width = 8, height = 8, units = 'in', res = 300, 
      compression = "lzw")
    par (font.axis = 2, font.lab = 2)
    twoord.plot(slope, VEcv2, slope, R2, rylim = c(0, 1.037), xlim = c(0, 1.2),
    type = "l", rcol = "red", lwd = 1, ylab = expression(bold(paste("Accuracy measures", 
      " (%)", sep = ""))), xlab = "Slope", rylab = "Correlation coefficient (r)")
    abline(h = 0, col = "blue")
    abline(v = 1.0, lty = 1, col = "blue")
    abline(v = 0.9, lty = 2, col = "green")
    abline(v = 0.6, lty = 3, col = "blue")
    abline(v = 0.3, lty = 2, col = "red")
    lines(Dr2 ~ slope, col = "orange")
    lines(E2 ~ slope, col = "black", lty = 2)   
    legend(0,98, lty = c(1, 1, 1, 2), col = c("red", "black", "orange", "black"), 
      legend = c("r", expression(paste(VEcv, " (%)", sep = "")),
    expression(paste(d[r], " (%)", sep = "")), expression(paste(E[1], " (%)", 
      sep=""))), bty = "n")
    dev.off()

    >   R2
    [1] NA  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
    [25]  1 
      # NA was due to that: the standard deviation is zero. However, this would 
      # not prevent us to simulate for: slope <- seq(0.05, 1.2, 0.05) 
    > E2
    [1]   0   5  10  15  20  25  30  35  40  45  50  55  60  65  70  75  80  85
    [19]  90  95 100  95  90  85  80
    > VEcv2
    [1]   0.00   9.75  19.00  27.75  36.00  43.75  51.00  57.75  64.00  69.75
    [11]  75.00  79.75  84.00  87.75  91.00  93.75  96.00  97.75  99.00  99.75
    [21] 100.00  99.75  99.00  97.75  96.00
    > Dr2
    [1]   0   5  10  15  20  25  30  35  40  45  50  55  60  65  70  75  80  85
    [19]  90  95 100  95  90  85  80


# Figure 2c
    # no intercept, with noise
    slope <- seq(0.1, 1.2, 0.05)
    VEcv3 <- NULL
    R3 <- NULL
    E3 <- NULL
    Dr3 <- NULL
    for (i in 1:length(slope)){
    pred1 <- slope[i] * x + e
    VEcv3[i] <- vecv(x, pred1)
    R3[i] <- cor(x, pred1)
    E3 [i] <- e1(x, pred1)
    Dr3[i] <- dr(x, pred1)
    }

    tiff("Fig 2.c.tif", width = 8, height = 8, units = 'in', res = 300, 
      compression = "lzw")
    par (font.axis = 2, font.lab = 2)
    twoord.plot(slope,VEcv3,slope,R3,rylim = c(0,1.037), xlim = c(0, 1.2),
    type = "l",rcol = "red",lwd = 1, ylab = expression(bold(paste("Accuracy measures", 
      " (%)", sep = ""))), xlab = "Slope", rylab = "Correlation coefficient (r)")
    abline(h = 0, col = "blue")
    abline(v = 1.0, lty = 1, col = "blue")
    abline(v = 0.9, lty = 2, col = "green")
    abline(v = 0.6, lty = 3, col = "blue")
    abline(v = 0.3, lty = 2, col = "red")
    lines(Dr3~slope, col =  "orange")
    lines(E3~slope, col =  "black", lty  = 2)   
    legend(0,98, lty =  c(1, 1, 1, 2), col = c("red", "black", "orange", "black"), 
      legend = c("r", expression(paste(VEcv, " (%)", sep = "")),
    expression(paste(d[r], " (%)", sep = "")), expression(paste(E[1], " (%)", 
      sep = ""))), bty = "n")
    dev.off()
    
    > R3
    [1] 0.525 0.628 0.705 0.764 0.808 0.842 0.868 0.889 0.905 0.918 0.929 0.938
    [13] 0.945 0.951 0.956 0.960 0.964 0.968 0.970 0.973 0.975 0.977 0.979
    >  E3
    [1] -89.28 -78.95 -68.61 -58.28 -47.95 -37.61 -27.28 -16.95  -6.61   3.43
    [11]  13.37  23.30  33.23  43.17  52.81  61.94  70.31  73.21  73.65  71.91
    [21]  69.02  62.14  52.83
    > VEcv3
    [1] -246.00 -209.32 -174.73 -142.25 -111.88  -83.61  -57.44  -33.37  -11.41
    [10]    8.45   26.20   41.85   55.40   66.84   76.19   83.42   88.56   91.59
    [19]   92.51   91.34   88.06   82.68   75.19
    > Dr3
    [1] -47.17 -44.12 -40.69 -36.82 -32.41 -27.33 -21.43 -14.49  -6.20   3.43
    [11]  13.37  23.30  33.23  43.17  52.81  61.94  70.31  73.21  73.65  71.91
    [21]  69.02  62.14  52.83
    
  # Figure 2d
    # with intercepts that are changing with slope and with a fixed mean, with 
    # noise
    slope <- seq(0, 1.2, 0.05)
    VEcv4 <- NULL
    R4 <- NULL
    E4 <- NULL
    Dr4 <- NULL
    for (i in 1:length(slope)){
    pred1 <- mean(x)+slope[i] * (x-mean(x)) + e
    VEcv4[i] <- vecv(x, pred1)
    R4[i] <- cor(x, pred1)
    E4 [i] <- e1(x, pred1)
    Dr4[i] <- dr(x, pred1)
    }
    
    tiff("Fig 2d.tif", width = 8, height = 8, units = 'in', res = 300, 
      compression = "lzw")
    par (font.axis = 2, font.lab = 2)
    twoord.plot(slope,VEcv4,slope,R4,lylim = c(0,100), rylim = c(0,1.037), 
      xlim = c(0, 1.2),
    type = "l",rcol = "red",lwd = 1, ylab = expression(bold(paste("Accuracy measures", 
      " (%)", sep = ""))), xlab = "Slope", rylab = "Correlation coefficient (r)")
    abline(h = 0, col = "blue")
    abline(v = 1.0, lty = 1, col = "blue")
    abline(v = 0.9, lty = 2, col = "green")
    abline(v = 0.6, lty = 3, col = "blue")
    abline(v = 0.3, lty = 2, col = "red")
    lines(Dr4~slope, col =  "orange")
    lines(E4~slope, col =  "black", lty  = 2)   
    legend(0,95, lty =  c(1, 1, 1, 2), col = c("red", "black", "orange", "black"), 
      legend = c("r", expression(paste(VEcv, " (%)", sep = "")),
    expression(paste(d[r], " (%)", sep = "")), expression(paste(E[1], " (%)", 
      sep = ""))), bty = "n")
    dev.off()

    > R4
    [1] 0.233 0.394 0.525 0.628 0.705 0.764 0.808 0.842 0.868 0.889 0.905 0.918
    [13] 0.929 0.938 0.945 0.951 0.956 0.960 0.964 0.968 0.970 0.973 0.975 0.977
    [25] 0.979
    > E4
    [1]  2.64  7.62 12.60 17.58 22.54 27.37 32.03 36.70 41.37 46.03 50.57 54.57
    [13] 58.57 62.57 66.28 69.79 72.20 73.57 74.53 74.90 73.65 71.43 69.21 66.42
    [25] 63.44
    > VEcv4
    [1]  5.18 14.30 22.92 31.03 38.65 45.77 52.38 58.50 64.11 69.23 73.85 77.96
    [13] 81.58 84.70 87.31 89.43 91.05 92.16 92.78 92.90 92.51 91.63 90.25 88.36
    [25] 85.98
    > Dr4
    [1]  2.64  7.62 12.60 17.58 22.54 27.37 32.03 36.70 41.37 46.03 50.57 54.57
    [13] 58.57 62.57 66.28 69.79 72.20 73.57 74.53 74.90 73.65 71.43 69.21 66.42
    [25] 63.44
    
# Figure 3
    tiff("Fig 3.tif", width = 8, height = 8, units = 'in', res = 300, compression = "lzw")
    par (font.axis = 2, font.lab = 2)
    plot(R1, VEcv1, type = 'l', col = 1, lwd = 2, xlab  =  expression (bold(paste(
      "Correlation coefficient (r) or ",  r^2))), ylab = "VEcv (%)", xlim = c(0, 1), 
        ylim = c(-250, 100))
    lines(R2, VEcv2, type = "l", col = 2, lwd = 2, lty = 2)
    lines(R3, VEcv3, type = "l", col = 3, lwd = 2, lty = 3)
    lines(R4, VEcv4, type = "l", col = 4, lwd = 2, lty = 4)
    lines(R3^2, VEcv3, type = "l", col = 5, lwd = 2, lty = 3)
    lines(R4^2, VEcv4, type = "l", col = 6, lwd = 2, lty = 4)

    legend(0,0, lty = c(1:4, 3,4), col = c(1:6), lwd = 2, legend = c(
    expression (paste("Scenario 1: r (or ", r^2, ") vs VEcv")),
    expression (paste("Scenario 2: r (or ", r^2, ") vs VEcv")),
    "Scenario 3: r vs VEcv","Scenario 4: r vs VEcv",
    expression (paste("Scenario 3: ", r^2, " vs VEcv")),
    expression (paste("Scenario 4: ", r^2, " vs VEcv"))), bty = "n")
    dev.off()

# Figure 4 
    # data with different sample size and data variance   
    set.seed(1234)
    n = 30
    x2 <-sample(1:n, 300, replace  = T)
    e2 <-rnorm(300, sd = 2)
    slope <- seq(0, 1.2, 0.05)
    E5 <- NULL
    VEcv5 <- NULL
    
    for (i in 1:length(slope)){
    pred1 <- mean(x2) + slope[i] * (x2 - mean(x2)) + e2
    E5 [i] <- e1(x2, pred1)
    VEcv5[i] <- vecv(x2, pred1)
    }
   
    set.seed(1234)
    n = 30
    x2 <-sample(1:n, 300, replace  = T)
    e2 <-rnorm(300, sd = 3)
    slope <- seq(0, 1.2, 0.05)
    E6 <- NULL
    VEcv6 <- NULL
    for (i in 1:length(slope)){
    pred1 <- mean(x2) + slope[i] * (x2 - mean(x2)) + e2
     E6 [i] <- e1(x2, pred1)
     VEcv6[i] <- vecv(x2, pred1)
    }    

   VEcv <- c(VEcv4, VEcv5, VEcv6)
   E <- c(E4, E5, E6)
   VEcv_p <- VEcv [VEcv > = 0]
   E_p <- E[E> = 0]
   
   tiff("Fig 4.tif", width = 8, height = 8, units = 'in', res = 300, compression = "lzw")
   par (font.axis = 2, font.lab = 2)
   plot(VEcv_p, E_p, type = "n", xlab  =  "VEcv (%)", ylab  =  expression(E[1]))
   points(VEcv4 [VEcv4 > = 0], E4[E4> = 0], col = 1, pch = 1)
   points(VEcv5 [VEcv5 > = 0], E5[E5> = 0], col = 2, pch = 2)
   points(VEcv6 [VEcv6 > = 0], E6[E6> = 0], col = 3, pch = 3)
   
   legend(0,100, pch = c(1, 2, 3), col = c(1:3), legend = c(
     "Scenario 4, n=30, sd=2", "Scenario 4, n=300, sd=2", "Scenario 4, n=300, 
      sd=3"), bty = "n")
   dev.off()
