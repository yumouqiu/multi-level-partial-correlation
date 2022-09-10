A1 = read.table("IndARR2-200-100-0.txt", head = FALSE)
A2 = read.table("IndARR2-200-100-03.txt", head = FALSE)
A3 = read.table("IndARR2-200-100-05.txt", head = FALSE)

R1 = c(mean(A1[, 1] > 0.09), colMeans(A1[, c(1, 2)]), mean(A1[, 3] > 0.1), colMeans(A1[, c(3, 4)]))
R2 = c(mean(A2[, 1] > 0.09), colMeans(A2[, c(1, 2)]), mean(A2[, 3] > 0.1), colMeans(A2[, c(3, 4)]))
R3 = c(mean(A3[, 1] > 0.09), colMeans(A3[, c(1, 2)]), mean(A3[, 3] > 0.1), colMeans(A3[, c(3, 4)]))

Q1 = rbind(R1, R2, R3)
Q1 = as.data.frame(Q1)
names(Q1) = c("FDP>0.1", "FDR", "Power", "FDP>0.1", "FDR", "Power")
round(Q1, 3)



A4 = read.table("IndBD4-200-100-3-0.txt", head = FALSE)
A5 = read.table("IndBD4-200-100-3-03.txt", head = FALSE)
A6 = read.table("IndBD4-200-100-3-05.txt", head = FALSE)

R4 = c(mean(A4[, 1] > 0.1), colMeans(A4[, c(1, 2)]), mean(A4[, 3] > 0.1), colMeans(A4[, c(3, 4)]))
R5 = c(mean(A5[, 1] > 0.1), colMeans(A5[, c(1, 2)]), mean(A5[, 3] > 0.1), colMeans(A5[, c(3, 4)]))
R6 = c(mean(A6[, 1] > 0.1), colMeans(A6[, c(1, 2)]), mean(A6[, 3] > 0.1), colMeans(A6[, c(3, 4)]))

Q2 = rbind(R4, R5, R6)
Q2 = as.data.frame(Q2)
names(Q2) = c("FDP>0.1", "FDR", "Power", "FDP>0.1", "FDR", "Power")
Q2