###
### Very simple PCA Demo!
###

n = 7
p = 3
set.seed(1)
x = matrix(rnorm(n*p), n, p)
x

###
### Data has correlation:
###

cor(x)

###
### Scale: Center/SD
###

x = scale(x)
x
cor(x)

###
### Let's check the definition of
###  sample correlation:
###

cor(x)
sample_corr = t(x) %*% x / (n - 1)
sample_corr

###
### Let's do an eigen value decomposition and check
###  some stuff:
###

e_c = eigen(sample_corr)
e_c

###
### e_c$vectors has a1, a2, ..., ap
###

rotation_matrix = e_c$vectors
rotation_matrix

a1 = rotation_matrix[,1]
a2 = rotation_matrix[,2]
a3 = rotation_matrix[,3]

###
### This is a dot product of a1 and a3.
### Can you guess what these will be?
###

t(a1) %*% a1
t(a1) %*% a2

###
### These are my new Z's
###

z = x %*% rotation_matrix 
z

###
### Can you guess what this will be?
###

round( cor(z), 10)

###
### Can you guess these?
###

e_c

sd(z[,1])
sd(z[,2])
sd(z[,3])


###
### Official PCA:
###

pca_x = prcomp(x)
pca_x

e_c

predict(pca_x)
z

###
### Aside V matrices should be the same
### and the eigen vector but could be times -1
###

svd(x)
svd(t(sample_corr) %*% sample_corr)


