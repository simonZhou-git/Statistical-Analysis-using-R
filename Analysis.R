
number <- rnorm(n=24, mean = 0, sd = 1);
matrixA <- matrix(data = round(number,1), nrow = 4, ncol = 6 );
print(matrixA);


matrixA1 <- matrixA[,c(1,3,4,6)]
print(matrixA1);
Y <- matrix(1:4, nrow = 4, ncol = 1);
X <- solve(matrixA1,Y);
print(t(X));


Answer <- matrixA1 %*% X;
print(Answer);

numberExpo <- rexp(n = 200, rate = 2);
sampleQ <- quantile(numberExpo, probs = seq(0.05,0.95,0.05));
print(round(sampleQ,3));


theoQ <- qexp(seq(0.05,0.95,0.05), rate = 1);
print(theoQ);
plot(theoQ, sampleQ,xlab = "Theoretical Quantile", ylab = "Sample Quantile", main = "Sample quantile vs. Theoretical quantile", type = "p");


print("The plot indicates a linear relation between sample quantiles and theoretical quantiles");
predictX <- seq(0,3,0.5);
predictY <- 0.5 * predictX;
lines(predictX,predictY);


dat <- iris;
dat;
head(dat);
par(mfrow = c(2,2));
hist(dat$Sepal.Length);
hist(dat$Sepal.Width);
hist(dat$Petal.Length);
hist(dat$Petal.Width);

attach(iris);
par(mfrow = c(2,2));
boxplot(Sepal.Length ~ Species, main = "Speal length vs. Species");
boxplot(Sepal.Width ~ Species, main = 'Sepal width Vs. Species');
boxplot(Petal.Length ~ Species, main = 'Petal length Vs. Species');
boxplot(Petal.Width ~ Species, main = 'Petal width vs. Species');
print("Observing outliers of each species. ");


datS <- dat[dat$Species == 'setosa', ]; #matrix with species = setosa
datVe <- dat[dat$Species == 'versicolor', ];#matrix with species = versicolor
datVi <- dat[dat$Species == 'virginica', ];#matrix with species = virginica
par(mfrow = c(1,3));
qqnorm(datS$Petal.Width, main = 'QQ plot for setosa');
qqline(datS$Petal.Width);
qqnorm(datVe$Petal.Width, main = 'QQ plot for versicolor');
qqline(datVe$Petal.Width);
qqnorm(datVi$Petal.Width, main = 'QQ plot for virginica');
qqline(datVi$Petal.Width);


#we first need to seperate this matrix in to three submatrixes based on species.
datS <- dat[dat$Species == 'setosa', ]; #matrix with species = setosa
datVe <- dat[dat$Species == 'versicolor', ];#matrix with species = versicolor
datVi <- dat[dat$Species == 'virginica', ];#matrix with species = virginica

remove_outlier <- function(data){
  counter = 0;
  outlier <- vector(mode = "numeric", 0);
  Q1 <- quantile(data[, 'Petal.Width'], probs = 0.25);
  Q3 <- quantile(data[, 'Petal.Width'], probs = 0.75);
  IQR <- (Q3 - Q1);
  upperend <- Q3 + 1.5 * (IQR);
  lowerend <- Q1 - 1.5 * (IQR);
  for (x in data[, 'Petal.Width']){
    if ((x > upperend) | (x < lowerend)){
      counter = counter + 1;
      outlier[counter] <- x;
    }
  }
  return(outlier);
  
  if (length(outlier) == 0)
    outlier = NULL;
  
}

boxplot(datS); #visualized by plots for each species
boxplot(datVe);
boxplot(datVi);


numberSe <- remove_outlier(datS);
if(length(numberSe) == 0){
  cat('There are no outliers.');
}else{
  cat('The outliers are: ', numberSe);
}

numberVe <- remove_outlier(datVe);
if (length(numberVe) == 0){
  cat('There are no outliers.');
}else{
  cat('The ouliers are: ', numberVe);
}

numberVi <- remove_outlier(datVi);
if(length(numberVi) == 0){
  cat('There are no outliers.');
}else{
  cat('The outliers are: ', numberVi);
}
