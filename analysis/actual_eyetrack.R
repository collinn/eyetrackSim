



saccades[, sn := saccadenum]
saccades[sn >= 6, sn := 6]

saccades[, maxsn := max(saccadenum), by = .(trialID, subjectID)]


## From actual eyetrack data, should move this elsewhere
library(eyetrack)
fit <- lm(durr ~ saccadenum + I(object == "target") + port, data = saccades[maxsn <= 6, ])
fit2 <- lm(durr ~ sn + I(object == "target") + port, data = saccades)

histogram(~durr|as.character(sn), data = saccades, grid = TRUE)
