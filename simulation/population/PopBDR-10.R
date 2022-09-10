source("PC-population.txt")

set.seed(3000)
R = 200
precision = BD3(50, 3, rho = c(0.05, 0.25))
Sigma = solve(precision)
n = 200; m = 10; p = dim(Sigma)[1]; rho = 0.5
precisionMatrix = solve(Sigma)
IndMatrix = matrix(1, p, p) - diag(rep(1, p))
STrue = 1 * (abs(precisionMatrix) > 10^(-6)); NoNSTrue = 1 * (STrue == 0)
STrueNum = sum(STrue) - p

Index0 = diag(rep(1, p))
for (i in 1 : p){
	for (j in 1 : p){
		if (abs(i - j) <= 5) Index0[i, j] = 1
	}
}
diag(Index0) = rep(0, p)

FDPprop = c(); powerprop = c(); FDPLiu = c(); powerLiu = c(); FDPMin = c(); powerMin = c()
SigmaAll = array(dim = c(p, p, m))
for (sub in 1 : m){
	RE = matrix(rnorm(p^2, 0, sqrt(2) * 0.05), p, p) * Index0
	#RE = matrix(0, p, p)
	RE1 = (RE + t(RE)) / 2
	#diag(RE1) = diag(abs(RE1))
	precisionMatrixInd = precisionMatrix + RE1
	SigmaInd = solve(precisionMatrixInd)
	SigmaAll[, , sub] = SigmaInd
}

#set.seed(435346)
for (rep in 1 : R){
	Z = array(dim = c(n, p, m))
	for (sub in 1 : m){
		SigmaInd = SigmaAll[, , sub]
		Xtemp = matrix(0, n, p)
		Epsilon = mvrnorm(n, rep(0, p), SigmaInd)
		Xtemp[1, ] = Epsilon[1, ]
		for (i in 2 : n){
			Xtemp[i, ] = rho * Xtemp[i - 1, ] + sqrt(1 - rho^2) * Epsilon[i, ]
		}
		Z[, , sub] = Xtemp
	}	
	Res  = population(Z)
	ResProp = Res[, c(1 : p)]
	ResLiu = Res[, c((p + 1) : (2 * p))]
	ResMin = Res[, c((2 * p + 1) : (3 * p))]

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
	
	TruePositiveMin = sum(ResMin * STrue) - p; FalsePositiveMin = sum(ResMin * NoNSTrue)
	powerMin1 = TruePositiveMin / STrueNum
	FDPMin1 = FalsePositiveMin / max(1, (sum(ResMin) - p))
	FDPMin = c(FDPMin, FDPMin1)
	powerMin = c(powerMin, powerMin1)

	cat("iteration = ", c(rep, FDPprop1, powerprop1, FDPLiu1, powerLiu1, FDPMin1, powerMin1), "\n")	
}

Res5 = cbind(FDPprop, powerprop, FDPLiu, powerLiu, FDPMin, powerMin)


FDPprop = c(); powerprop = c(); FDPLiu = c(); powerLiu = c(); FDPMin = c(); powerMin = c()
rho = 0.3
for (rep in 1 : R){
	Z = array(dim = c(n, p, m))
	for (sub in 1 : m){
		SigmaInd = SigmaAll[, , sub]
		Xtemp = matrix(0, n, p)
		Epsilon = mvrnorm(n, rep(0, p), SigmaInd)
		Xtemp[1, ] = Epsilon[1, ]
		for (i in 2 : n){
			Xtemp[i, ] = rho * Xtemp[i - 1, ] + sqrt(1 - rho^2) * Epsilon[i, ]
		}
		Z[, , sub] = Xtemp
	}	
	Res  = population(Z)
	ResProp = Res[, c(1 : p)]
	ResLiu = Res[, c((p + 1) : (2 * p))]
	ResMin = Res[, c((2 * p + 1) : (3 * p))]

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
	
	TruePositiveMin = sum(ResMin * STrue) - p; FalsePositiveMin = sum(ResMin * NoNSTrue)
	powerMin1 = TruePositiveMin / STrueNum
	FDPMin1 = FalsePositiveMin / max(1, (sum(ResMin) - p))
	FDPMin = c(FDPMin, FDPMin1)
	powerMin = c(powerMin, powerMin1)

	cat("iteration = ", c(rep, FDPprop1, powerprop1, FDPLiu1, powerLiu1, FDPMin1, powerMin1), "\n")	
}

Res3 = cbind(FDPprop, powerprop, FDPLiu, powerLiu, FDPMin, powerMin)



FDPprop = c(); powerprop = c(); FDPLiu = c(); powerLiu = c(); FDPMin = c(); powerMin = c()
rho = 0
for (rep in 1 : R){
	Z = array(dim = c(n, p, m))
	for (sub in 1 : m){
		SigmaInd = SigmaAll[, , sub]
		Xtemp = matrix(0, n, p)
		Epsilon = mvrnorm(n, rep(0, p), SigmaInd)
		Xtemp[1, ] = Epsilon[1, ]
		for (i in 2 : n){
			Xtemp[i, ] = rho * Xtemp[i - 1, ] + sqrt(1 - rho^2) * Epsilon[i, ]
		}
		Z[, , sub] = Xtemp
	}	
	Res  = population(Z)
	ResProp = Res[, c(1 : p)]
	ResLiu = Res[, c((p + 1) : (2 * p))]
	ResMin = Res[, c((2 * p + 1) : (3 * p))]

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
	
	TruePositiveMin = sum(ResMin * STrue) - p; FalsePositiveMin = sum(ResMin * NoNSTrue)
	powerMin1 = TruePositiveMin / STrueNum
	FDPMin1 = FalsePositiveMin / max(1, (sum(ResMin) - p))
	FDPMin = c(FDPMin, FDPMin1)
	powerMin = c(powerMin, powerMin1)

	cat("iteration = ", c(rep, FDPprop1, powerprop1, FDPLiu1, powerLiu1, FDPMin1, powerMin1), "\n")	
}

Res0 = cbind(FDPprop, powerprop, FDPLiu, powerLiu, FDPMin, powerMin)


c1 = c(mean(Res0[, 1] >= 0.1), mean(Res0[, 1]), mean(Res0[, 2]), mean(Res0[, 3] > 0.1), mean(Res0[, 3]), mean(Res0[, 4]), mean(Res0[, 5] >= 0.1), mean(Res0[, 5]), mean(Res0[, 6]))
c2 = c(mean(Res3[, 1] >= 0.1), mean(Res3[, 1]), mean(Res3[, 2]), mean(Res3[, 3] > 0.1), mean(Res3[, 3]), mean(Res3[, 4]), mean(Res3[, 5] >= 0.1), mean(Res3[, 5]), mean(Res3[, 6]))
c3 = c(mean(Res5[, 1] >= 0.1), mean(Res5[, 1]), mean(Res5[, 2]), mean(Res5[, 3] > 0.1), mean(Res5[, 3]), mean(Res5[, 4]), mean(Res5[, 5] >= 0.1), mean(Res5[, 5]), mean(Res5[, 6]))
matrix(round(c(c1, c2, c3), 4), 3, 9, byrow = TRUE)


write.table(Res5, "Pop0416BD3200-50-10-0525-5-05.txt", row.names = FALSE, col.names = FALSE)
write.table(Res3, "Pop0416BD3200-50-10-0525-5-03.txt", row.names = FALSE, col.names = FALSE)
write.table(Res0, "Pop0416BD3200-50-10-0525-5-0.txt", row.names = FALSE, col.names = FALSE)

