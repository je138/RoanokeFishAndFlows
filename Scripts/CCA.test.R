getwd()
library(vegan)
library(grid)

# abund_table<-read.csv("./Data/RawData/SPE_pitlatrine.csv",row.names=1,check.names=FALSE)
# #Transpose the data to have sample names on rows
# abund_table<-t(abund_table)

# ### Example 2: Constrained ordination (RDA)
# ## The example uses formula interface to define the model
# data(dune)
# data(dune.env)
# ## No constraints: PCA
# mod0 <- rda(dune ~ 1, dune.env)
# mod0
# plot(mod0)
# ## All environmental variables: Full model
# mod1 <- rda(dune ~ ., dune.env)
# mod1
# plot(mod1)
# ## Automatic selection of variables by permutation P-values
# mod <- ordistep(mod0, scope=formula(mod1))
# mod
# plot(mod)
# ## Permutation test for all variables
# anova(mod)
# ## Permutation test of "type III" effects, or significance when a term
# ## is added to the model after all other terms
# anova(mod, by = "margin")
# ## Plot only sample plots, use different symbols and draw SD ellipses
# ## for Managemenet classes
# plot(mod, display = "sites", type = "n")
# with(dune.env, points(mod, disp = "si", pch = as.numeric(Management)))
# with(dune.env, legend("topleft", levels(Management), pch = 1:4,
#                       title = "Management"))
# with(dune.env, ordiellipse(mod, Management, label = TRUE))
# ## add fitted surface of diversity to the model
# ordisurf(mod, diversity(dune), add = TRUE)


data(varespec, varechem)
mod <- cca(varespec ~ Al + P + K, varechem)
## leverage
hatvalues(mod)
plot(hatvalues(mod), type = "h")
## ordination plot with leverages: points with high leverage have
## similar LC and WA scores
plot(mod, type = "n")
ordispider(mod)       # segment from LC to WA scores
points(mod, dis="si", cex=5*hatvalues(mod), pch=21, bg=2) # WA scores
text(mod, dis="bp", col=4)
## deviation and influence
head(rstandard(mod))
head(cooks.distance(mod))
## Influence measures from lm
y <- decostand(varespec, "chi.square") # needed in cca
y1 <- with(y, Cladstel)         # take one species for lm
lmod1 <- lm(y1 ~ Al + P + K, varechem, weights = rowSums(varespec))
## numerically identical within 2e-15
range(cooks.distance(lmod1) - cooks.distance(mod)[, "Cladstel"])
## t-values of regression coefficients based on type = "canoco"
## residuals
coef(mod)
coef(mod)/sqrt(diag(vcov(mod, type = "canoco")))
