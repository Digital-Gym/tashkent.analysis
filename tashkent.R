#  ======= SETUP =========
# Run those 3 lines first
library("rstudioapi")

current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
data_dir <- file.path(current_dir, "data")
setwd(data_dir)

getwd() # check if everything is correct

# =========== air ========
air <- read.csv("air.csv")

sliced_air <- air[ -1, ]


colnames(sliced_air) <- as.character(unlist(sliced_air[1, ]))

sliced_air <- sliced_air[-1, ]

air_data <- sliced_air[, -c(1,2,3, ncol(sliced_air))]

vec <- unlist(air_data[15, -1])


# =========== population ========

pop <- read.csv('population.csv')
pop <- pop[-c(1,2), -c(1,2,3)]

vec2 <- unlist(pop[16, c(13:24)])

# =========== Coal consumption ========

coal <- read.csv('coal.csv')
coal <- coal[c(2,3,4), -c(1,2,3)]

vec3 <- unlist(coal[2, c(13:24)])
vec4 <- unlist(coal[3, c(13:24)])

# =========== Gas consumption ========

gas <- read.csv('gas.csv')
gas <- gas[c(2:4), -c(1:3)]

vec5 <- unlist(gas[2, c(13:24)])
vec6 <- unlist(gas[3, c(13:24)])

# ========= ANOVA , Regression analysis ========

tashkent_frame <- data.frame(
  Year = c(2011:2022),
  Air = vec,
  Population = vec2,
  CoalProd = vec3,
  CoalConsum = vec4,
  GasProd = vec5,
  GasConsum = vec6
)

anova <- aov(Air ~ Population + CoalConsum + GasConsum, data=tashkent_frame)

summary(anova)

summary(lm(CoalConsum ~ GasConsum, data=tashkent_frame)) # low impact
summary(lm(CoalConsum ~ Population, data=tashkent_frame)) # high impact
summary(lm(Air ~ CoalConsum + Population + GasConsum, data=tashkent_frame)) # low impact

# ========== Plots =============

# pollution plot
plot(x = tashkent_frame$Year,
     y = tashkent_frame$Air,
     main="Air pollution level in Tashkent",
     type='l',
     xlab='Years',
     ylab='Pollutans in 000th',
     col='red',
)

# population plot
plot(x = tashkent_frame$Year, y = tashkent_frame$Population, type="l", col="orange",
     main="Population of Tashkent",
     xlab='Year',
     ylab='in `000')

# =========================== COAL VS GAS ====================================
# ====== Coal PLOTS =====
plot(x = tashkent_frame$Year, y = tashkent_frame$CoalProd, type="o", col="darkgreen",
     main="Coal Consumption and Production in Tashkent",
     xlab='Year',
     ylab='Figures in `000 tonns',
     ylim=range(c(tashkent_frame$CoalProd,
                  tashkent_frame$CoalConsum
     ))
)


lines(x = tashkent_frame$Year, y = tashkent_frame$CoalConsum, type="o", col="darkred")


legend("topleft", legend=c("Production", "Consumption"), col=c("darkgreen", "darkred"), lty=1:1)


# ====== GAS PLOTS =====
plot(x = tashkent_frame$Year, y = tashkent_frame$GasProd, type="o", col="skyblue",
     main='Gas consumption and productions in Tashkent',
     xlab='Year',
     ylab='Numbers in `000 tonns',
     ylim=range(c(tashkent_frame$GasProd,
                  tashkent_frame$GasConsum
     ))
)


lines(x = tashkent_frame$Year, y = tashkent_frame$GasConsum, type="o", col="pink")


legend("topright", legend=c("Production", "Consumption"), col=c("skyblue", "pink"), lty=1:1)

# ====== Comparison: Gas vs Coal =====
percent_change <- function(your_column){
  return(c(0, diff(your_column) / your_column[-length(your_column)] * 100))
}

a <- percent_change(tashkent_frame$CoalConsum)
b <- percent_change(tashkent_frame$GasConsum)

plot(x = tashkent_frame$Year, y = a, type="o", col="black",
     main="Comparion of changes in consumption of coal and gas",
     xlab='Year',
     ylab='Changes in %',
     ylim=range(c(a,b))
)


lines(x = tashkent_frame$Year, y = b, type="o", col="skyblue")


legend("topright", legend=c("Coal", "Gas"), col=c("black", "skyblue"), lty=1:1)
# ==========================================================================
