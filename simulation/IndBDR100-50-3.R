source("PC.txt")

R = 500
#set.seed(1000)
#precision = BD2(50, 3, tau2 = 1.2, rho = c(0.2, 0.4, 0.8, 1))
#precision = BD3(50, 3, tau2 = 2, rho = c(0.4, 1.6))
precision = BD4(50, 3, tau2 = 1, rho = c(0.25, 0.8))
eigen(precision)$values

Sigma = solve(precision)
n = 100; p = dim(Sigma)[1]
precisionMatrix = precision
IndMatrix = matrix(1, p, p) - diag(rep(1, p))
STrue = 1 * (abs(precisionMatrix) > 10^(-6)); NoNSTrue = 1 * (STrue == 0)
STrueNum = sum(STrue) - p

rho = 0.5
FDPprop = c(); powerprop = c(); FDPLiu = c(); powerLiu = c()

for (rep in 1 : R){
	X = matrix(0, n, p)
	Epsilon = mvrnorm(n, rep(0, p), Sigma)
	X[1, ] = Epsilon[1, ]
	for (i in 2 : n){
		X[i, ] = rho * X[i - 1, ] + sqrt(1 - rho^2) * Epsilon[i, ]
	}
	p = dim(X)[2]
	
	Res  = individual(X)
	ResProp = Res[, c(1 : p)]
	ResLiu = Res[, c((p + 1) : (2 * p))]

	TruePositive = sum(ResProp * STrue) - p; FalsePositive = sum(ResProp * NoNSTrue)
	powerprop1 = TruePositive / STrueNum
	FDPprop1 = FalsePositive / max(1, (sum(ResProp) - p))
	FDPprop = c(FDPprop, FDPprop1)
	powerprop = c(powerprop, powerprop1)

	TruePositiveLiu = sum(ResLiu * STrue) - p; FalsePositiveLiu = sum(ResLiu * NoNSTrue)
	powerLiu1 = TruePositiveLiu / STrueNum
	FDPLiu1 = FalsePositiveLiu / max(1, (sum(ResLiu) - p))
	FDPLiu = c(FDPLiu, FDPLiu1)
	powerLiu = c(powerLiu, powerLiu1)
	
	cat("iteration = ", c(rep, FDPprop1, powerprop1, FDPLiu1, powerLiu1), "\n")	
}

Res05 = cbind(FDPprop, powerprop, FDPLiu, powerLiu)
write.table(Res05, "IndBD4-100-50-3-05.txt", row.names = FALSE, col.names = FALSE)

rho = 0.3
FDPprop = c(); powerprop = c(); FDPLiu = c(); powerLiu = c()

for (rep in 1 : R){
  X = matrix(0, n, p)
  Epsilon = mvrnorm(n, rep(0, p), Sigma)
  X[1, ] = Epsilon[1, ]
  for (i in 2 : n){
    X[i, ] = rho * X[i - 1, ] + sqrt(1 - rho^2) * Epsilon[i, ]
  }
  p = dim(X)[2]
  
  Res  = individual(X)
  ResProp = Res[, c(1 : p)]
  ResLiu = Res[, c((p + 1) : (2 * p))]
  
  TruePositive = sum(ResProp * STrue) - p; FalsePositive = sum(ResProp * NoNSTrue)
  powerprop1 = TruePositive / STrueNum
  FDPprop1 = FalsePositive / max(1, (sum(ResProp) - p))
  FDPprop = c(FDPprop, FDPprop1)
  powerprop = c(powerprop, powerprop1)
  
  TruePositiveLiu = sum(ResLiu * STrue) - p; FalsePositiveLiu = sum(ResLiu * NoNSTrue)
  powerLiu1 = TruePositiveLiu / STrueNum
  FDPLiu1 = FalsePositiveLiu / max(1, (sum(ResLiu) - p))
  FDPLiu = c(FDPLiu, FDPLiu1)
  powerLiu = c(powerLiu, powerLiu1)
  
  cat("iteration = ", c(rep, FDPprop1, powerprop1, FDPLiu1, powerLiu1), "\n")	
}

Res03 = cbind(FDPprop, powerprop, FDPLiu, powerLiu)
write.table(Res03, "IndBD4-100-50-3-03.txt", row.names = FALSE, col.names = FALSE)

rho = 0
FDPprop = c(); powerprop = c(); FDPLiu = c(); powerLiu = c()

for (rep in 1 : R){
  X = matrix(0, n, p)
  Epsilon = mvrnorm(n, rep(0, p), Sigma)
  X[1, ] = Epsilon[1, ]
  for (i in 2 : n){
    X[i, ] = rho * X[i - 1, ] + sqrt(1 - rho^2) * Epsilon[i, ]
  }
  p = dim(X)[2]
  
  Res  = individual(X)
  ResProp = Res[, c(1 : p)]
  ResLiu = Res[, c((p + 1) : (2 * p))]
  
  TruePositive = sum(ResProp * STrue) - p; FalsePositive = sum(ResProp * NoNSTrue)
  powerprop1 = TruePositive / STrueNum
  FDPprop1 = FalsePositive / max(1, (sum(ResProp) - p))
  FDPprop = c(FDPprop, FDPprop1)
  powerprop = c(powerprop, powerprop1)
  
  TruePositiveLiu = sum(ResLiu * STrue) - p; FalsePositiveLiu = sum(ResLiu * NoNSTrue)
  powerLiu1 = TruePositiveLiu / STrueNum
  FDPLiu1 = FalsePositiveLiu / max(1, (sum(ResLiu) - p))
  FDPLiu = c(FDPLiu, FDPLiu1)
  powerLiu = c(powerLiu, powerLiu1)
  
  cat("iteration = ", c(rep, FDPprop1, powerprop1, FDPLiu1, powerLiu1), "\n")	
}

Res0 = cbind(FDPprop, powerprop, FDPLiu, powerLiu)
write.table(Res0, "IndBD4-100-50-3-0.txt", row.names = FALSE, col.names = FALSE)