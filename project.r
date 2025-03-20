library( dagitty )
library( bayesianNetworks )
# library(bnlearn)
library(lavaan)
# library(Rgraphviz)
library(semTools)
library(semPlot)
# library(lavaanPlot)
# Read csv file
WHRData = read.csv('/home/marie/pCloudDrive/BayesianNetworks/DataForTable2.1WHR2023.csv', sep=',')
colnames(WHRData) <- c('Country','Year','LifeLadder', 'GDP', 'SocialSupport', 'Health', 'Freedom', 'Generosity', 'Corruption', 'PositiveAffect', 'NegativeAffect')

drop <- c('Country', 'Year')
WHRData = WHRData[,!(names(WHRData) %in% drop)]
WHRData <- na.omit(WHRData)

#data = read.csv('DataForTable2.1WHR2023.csv',sep=',')
#print(data)


#####################ASSSIGNMENT 1########################
# model <- lm(LifeLadder ~ SocialSupport + Freedom + Corruption + NegativeAffect + GDP + Health + Generosity + PositiveAffect, data = WHRData)
# print(summary(model))


# #This was the model we started with from our assumptions
# dagStart <- dagitty('
# dag {
# Corruption [pos="-0.974,-1.117"]
# Freedom [pos="-0.461,-1.086"]
# GDP [pos="-1.655,-0.678"]
# Generosity [pos="-0.557,-0.689"]
# Health [pos="-1.444,-1.518"]
# LifeLadder [pos="-1.386,0.661"]
# NegativeAffect [pos="-0.073,-1.560"]
# PositiveAffect [pos="-0.421,-0.125"]
# SocialSupport [pos="-0.246,0.583"]
# Corruption -> GDP
# Corruption -> LifeLadder
# Corruption -> NegativeAffect
# Corruption -> Freedom
# Corruption -> Generosity
# Corruption -> Health
# Corruption -> SocialSupport
# Freedom -> LifeLadder
# Freedom -> PositiveAffect
# Freedom -> NegativeAffect
# Freedom -> Generosity
# GDP -> SocialSupport
# GDP -> LifeLadder
# GDP -> Health
# Generosity -> PositiveAffect
# Health -> LifeLadder
# Health -> NegativeAffect
# Health -> Freedom
# PositiveAffect -> LifeLadder
# SocialSupport -> Freedom 
# SocialSupport -> Health
# SocialSupport -> LifeLadder
# SocialSupport -> PositiveAffect
# SocialSupport -> NegativeAffect
# }
# ')

# #and then this is the model we ended up with
# dagFinal <- dagitty('
# dag {
# Corruption [pos="-0.974,-1.117"]
# Freedom [pos="-0.461,-1.086"]
# GDP [pos="-1.655,-0.678"]
# Generosity [pos="-0.557,-0.689"]
# Health [pos="-1.444,-1.518"]
# LifeLadder [pos="-1.386,0.661"]
# NegativeAffect [pos="-0.073,-1.560"]
# PositiveAffect [pos="-0.421,-0.125"]
# SocialSupport [pos="-0.246,0.583"]
# Corruption -> GDP
# Corruption -> LifeLadder
# Corruption -> NegativeAffect
# Corruption -> Freedom
# Corruption -> Generosity
# Corruption -> Health
# Corruption -> SocialSupport
# Freedom -> LifeLadder
# Freedom -> PositiveAffect
# Freedom -> NegativeAffect
# Freedom -> Generosity
# GDP -> SocialSupport
# GDP -> LifeLadder
# GDP -> Health
# GDP -> Generosity
# Generosity -> PositiveAffect
# Health -> LifeLadder
# NegativeAffect -> Health
# Health -> Freedom
# PositiveAffect -> LifeLadder
# NegativeAffect -> PositiveAffect
# SocialSupport -> Freedom 
# SocialSupport -> Health
# SocialSupport -> LifeLadder
# SocialSupport -> PositiveAffect
# SocialSupport -> NegativeAffect
# }
# ')

# #This is how we ran the tests
# res <- localTests( x=dagStart, data=na.omit(WHRData), type="cis")
# plotLocalTestResults(res)


# #HERE WE START FINDING THE COEFFICIENTS IN OUR GRAPH, SO THE STRENGTH OF CAUSAL RELATIONSHIPS
# fit <- sem( toString(dagFinal,"lavaan"),data=scale(WHRData))
# # fg <- lavaanToGraph(fit, digits=2)
# # cg <- coordinates(dagStart)
# # coordinates(fg) <- cg
# # plot(fg, show.coefficients=TRUE)
# semPaths(fit, what="est", layout="circle", fade=FALSE, levels=c(4, 4))


############################ASSIGNNMENT 2########################
dagOutcome <- dagitty('
dag {
Corruption [adjusted, pos="-0.974,-1.117"]
Freedom [pos="-0.461,-1.086"]
GDP [exposure,pos="-1.655,-0.678"]
Generosity [pos="-0.557,-0.689"]
Health [pos="-1.444,-1.518"]
LifeLadder [outcome,pos="-1.386,0.661"]
NegativeAffect [pos="-0.073,-1.560"]
PositiveAffect [pos="-0.421,-0.125"]
SocialSupport [pos="-0.246,0.583"]
Corruption -> Freedom
Corruption -> GDP
Corruption -> Generosity
Corruption -> Health
Corruption -> LifeLadder
Corruption -> NegativeAffect
Corruption -> SocialSupport
Freedom -> Generosity
Freedom -> LifeLadder
Freedom -> NegativeAffect
Freedom -> PositiveAffect
GDP -> Generosity
GDP -> Health
GDP -> LifeLadder
GDP -> SocialSupport
Generosity -> PositiveAffect
Health -> LifeLadder
NegativeAffect -> Health
NegativeAffect -> PositiveAffect
PositiveAffect -> LifeLadder
SocialSupport -> Freedom
SocialSupport -> Health
SocialSupport -> LifeLadder
SocialSupport -> NegativeAffect
SocialSupport -> PositiveAffect
}
')


# TO = c("LifeLadder", "Generosity", "Health")
# FROM = c("GDP", "Freedom", "GDP")
# TO = c("LifeLadder")
# FROM = c("GDP")
# white <- data.frame(FROM, TO)
# cpdag <- pc.stable(WHRData, undirected=FALSE, whitelist=white)
cpdag <- pc.stable(as.data.frame(scale(WHRData)), undirected=FALSE)
# plot(fit)
# dag <- set.arc(cpdag, "Freedom", "Generosity")
# dag <- set.arc(dag, "GDP", "Health")
plot(cpdag)
# print(vstructs(cpdag))

# learn the network structure.
# cpdag = pc.stable(WHRData)
# set the direction of the only undirected arc, A - B.
# dag = set.arc(cpdag, "Freedom", "Generosity")
# dag = set.arc(dag, "Health", "GDP")

# strength <- arc.strength(fit, WHRData, criterion="x2")
# strength.plot(fit, strength)

# plot the network learned by hc().
# dag = hc(learning.test)
# strength = arc.strength(dag, learning.test, criterion = "x2")
# strength.plot(dag, strength)

# model <- bn.boot(WHRData, algorithm="pc.stable", statistic=localTests)
# model <- boot.strength(WHRData, algorithm="pc.stable")
# graphviz.plot(fit, shape = "ellipse", highlight = list(arcs = fit$arcs))

dagLearned <- dagitty('
dag {
Corruption [pos="-0.974,-1.117"]
Freedom [pos="-0.461,-1.086"]
GDP [exposure,pos="-1.655,-0.678"]
Generosity [pos="-0.557,-0.689"]
Health [pos="-1.444,-1.518"]
LifeLadder [outcome,pos="-1.386,0.661"]
NegativeAffect [pos="-0.073,-1.560"]
PositiveAffect [pos="-0.421,-0.125"]
SocialSupport [pos="-0.246,0.583"]         
GDP -> LifeLadder
GDP -> SocialSupport
SocialSupport -> LifeLadder
SocialSupport -> PositiveAffect
Health -> LifeLadder
Health <-> GDP
Freedom -> LifeLadder
Freedom <-> Generosity
Freedom -> Corruption
Freedom -> PositiveAffect
Generosity -> Corruption
Generosity -> PositiveAffect
Corruption -> LifeLadder
PositiveAffect -> LifeLadder
NegativeAffect -> SocialSupport
NegativeAffect -> Corruption
NegativeAffect -> PositiveAffect
}')

dagLearned1 <- dagitty('
dag {
Corruption [pos="-0.974,-1.117"]
Freedom [pos="-0.461,-1.086"]
GDP [exposure,pos="-1.655,-0.678"]
Generosity [pos="-0.557,-0.689"]
Health [pos="-1.444,-1.518"]
LifeLadder [outcome,pos="-1.386,0.661"]
NegativeAffect [pos="-0.073,-1.560"]
PositiveAffect [pos="-0.421,-0.125"]
SocialSupport [pos="-0.246,0.583"]         
GDP -> LifeLadder
GDP -> SocialSupport
SocialSupport -> LifeLadder
SocialSupport -> PositiveAffect
Health -> LifeLadder
Health -> GDP
Freedom -> LifeLadder
Freedom -> Generosity
Freedom -> Corruption
Freedom -> PositiveAffect
Generosity -> Corruption
Generosity -> PositiveAffect
Corruption -> LifeLadder
PositiveAffect -> LifeLadder
NegativeAffect -> SocialSupport
NegativeAffect -> Corruption
NegativeAffect -> PositiveAffect
}')

dagLearned2 <- dagitty('
dag {
Corruption [pos="-0.974,-1.117"]
Freedom [pos="-0.461,-1.086"]
GDP [exposure,pos="-1.655,-0.678"]
Generosity [pos="-0.557,-0.689"]
Health [pos="-1.444,-1.518"]
LifeLadder [outcome,pos="-1.386,0.661"]
NegativeAffect [pos="-0.073,-1.560"]
PositiveAffect [pos="-0.421,-0.125"]
SocialSupport [pos="-0.246,0.583"]         
GDP -> LifeLadder
GDP -> SocialSupport
SocialSupport -> LifeLadder
SocialSupport -> PositiveAffect
Health -> LifeLadder
Health -> GDP
Freedom -> LifeLadder

Freedom -> Corruption
Freedom -> PositiveAffect
Generosity -> Corruption
Generosity -> PositiveAffect
Generosity -> Freedom
Corruption -> LifeLadder
PositiveAffect -> LifeLadder
NegativeAffect -> SocialSupport
NegativeAffect -> Corruption
NegativeAffect -> PositiveAffect
}')


dagLearned3 <- dagitty('
dag {
Corruption [pos="-0.974,-1.117"]
Freedom [pos="-0.461,-1.086"]
GDP [exposure,pos="-1.655,-0.678"]
Generosity [pos="-0.557,-0.689"]
Health [pos="-1.444,-1.518"]
LifeLadder [outcome,pos="-1.386,0.661"]
NegativeAffect [pos="-0.073,-1.560"]
PositiveAffect [pos="-0.421,-0.125"]
SocialSupport [pos="-0.246,0.583"]         
GDP -> LifeLadder
GDP -> SocialSupport
GDP -> Health
SocialSupport -> LifeLadder
SocialSupport -> PositiveAffect
Health -> LifeLadder

Freedom -> LifeLadder

Freedom -> Corruption
Freedom -> PositiveAffect
Generosity -> Corruption
Generosity -> PositiveAffect
Generosity -> Freedom
Corruption -> LifeLadder
PositiveAffect -> LifeLadder
NegativeAffect -> SocialSupport
NegativeAffect -> Corruption
NegativeAffect -> PositiveAffect
}')


dagLearned4 <- dagitty('
dag {
Corruption [pos="-0.974,-1.117"]
Freedom [pos="-0.461,-1.086"]
GDP [exposure,pos="-1.655,-0.678"]
Generosity [pos="-0.557,-0.689"]
Health [pos="-1.444,-1.518"]
LifeLadder [outcome,pos="-1.386,0.661"]
NegativeAffect [pos="-0.073,-1.560"]
PositiveAffect [pos="-0.421,-0.125"]
SocialSupport [pos="-0.246,0.583"]         
GDP -> LifeLadder
GDP -> SocialSupport
GDP -> Health
SocialSupport -> LifeLadder
SocialSupport -> PositiveAffect
Health -> LifeLadder

Freedom -> LifeLadder
Freedom -> Generosity
Freedom -> Corruption
Freedom -> PositiveAffect
Generosity -> Corruption
Generosity -> PositiveAffect
Corruption -> LifeLadder
PositiveAffect -> LifeLadder
NegativeAffect -> SocialSupport
NegativeAffect -> Corruption
NegativeAffect -> PositiveAffect
}')

# semSyntax(sem( toString(dagLearned,"lavaan")), syntax = "lavaan")
# fit <- sem( toString(dagLearned,"lavaan"),data=scale(WHRData), fixed.x=FALSE)
# fit <- sem( toString(dagLearned3,"lavaan"),data=scale(WHRData), fixed.x=FALSE)
# semPaths(fit, what="est", layout="circle", fade=FALSE, levels=c(4, 4))
# semPaths(fit, what="stand", layout="circle", fade=FALSE, levels=(c(4, 4)), exoCov = FALSE)
d <- as.data.frame(scale(WHRData), col.names=c('Country','Year','LifeLadder', 'GDP', 'SocialSupport', 'Health', 'Freedom', 'Generosity', 'Corruption', 'PositiveAffect', 'NegativeAffect'))
# m <- lm(LifeLadder ~ SocialSupport + Freedom + Corruption + NegativeAffect + GDP + Health + Generosity + PositiveAffect, data = WHRData)

#adjusted for Learned4
mGDP <- lm(LifeLadder ~ GDP, d)
print(mGDP)
#adjusted for Outcome
mGDPadj <- lm(LifeLadder ~ GDP + Corruption, d)
print(mGDPadj)
#adjusted for Learned
mGDPadjLearn <- lm(LifeLadder ~ GDP + Health, d)
print(mGDPadjLearn)

#not adjusted for anything
mPos <- lm(LifeLadder ~ PositiveAffect, d)
print(mPos)
#adjusted for Outcome
mPosadjMan <- lm(LifeLadder ~ PositiveAffect + Corruption + Freedom + GDP + Health + SocialSupport, d)
print(mPosadjMan)
#adjsuted for Learned and Learned4
mPosadjLearn <- lm(LifeLadder ~ PositiveAffect + Corruption + Freedom + GDP + SocialSupport , d)
print(mPosadjLearn)

# > coef(mGDP)
# (Intercept)         GDP 
#  -1.7960229   0.7763974 
# > coef(mGDPadj)
# (Intercept)         GDP  Corruption 
#  -0.2325218   0.7075835  -1.2307063 
# > coef(mPos)
#    (Intercept) PositiveAffect 
#       1.914117       5.423038 
# > coef(mPosadjMan)
#    (Intercept) PositiveAffect     Corruption        Freedom            GDP 
#    -2.72414697     2.42428070    -0.76309281     0.44620101     0.38701260 
#         Health  SocialSupport 
#     0.02768828     1.82084774 
# > coef(mPosadjLearn)
#    (Intercept) PositiveAffect     Corruption        Freedom            GDP 
#     -2.3417136      2.3730491     -0.7443356      0.5858571      0.5203080 
#  SocialSupport 
#      1.8655027 


# fg <- lavaanToGraph(fit, digits=2)
# cg <- coordinates(dagLearned)
# coordinates(fg) <- cg
# plot(fg, show.coefficients=TRUE)