###Excercises 1, 2, and 3
#1 Store sells two types of toys, A & B.... etc

#1. Use two different ways to solve Exercise 1 from lecture slides. First write the problem formulation 
#and then solve it

#A = x; B = y

# Maximize 2x + 3y

#R1 8x + 14y <=20000
#no more than $20k

#R2 x + y <=2000
#no more than 2k toys

#install package to solve in linear programming

install.packages("lpSolve") 

library("lpSolve")

f.obj <- c(2, 3)
f.con <- matrix (c(8,14,1,1), nrow=2, byrow=TRUE)
f.dir <- c("<=", "<=")
f.rhs <- c(20000,2000)

lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec = TRUE)

lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec = TRUE)$solution

library("lpSolveAPI") 
lprec1 <- make.lp(2, 2)

set.column(lprec1, 1, c(8,1))
set.column(lprec1, 2, c(14,1))

set.constr.type(lprec1, c("<=","<="))
set.rhs(lprec1, c(20000,2000))

lp.control(lprec1, sense="max")
set.objfn(lprec1, c(2,3))
set.type(lprec1, c(1,2), "integer")

solve.lpExtPtr(lprec1)
get.objective(lprec1)
get.variables(lprec1)


#Result

lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec = TRUE)
Success: the objective function is 4666 
> 
  > lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec = TRUE)$solution
[1] 1334  666

#2. Exercise 2 
#Minimize the total transportation cost whilte satisfying all the demand requirement subject to the supply capacity

cost2 = matrix(c(464,513,654,867,352,216,690,791,995,682,388,685),3,4,byrow = TRUE)
row.signs <- rep ("<=", 3)

row.rhs <- c(75, 125, 100) #supply on Cannery #1, 2, and 3 
col.signs <- rep (">=", 4)
col.rhs <- c(80, 65, 70, 85)#demand on Cannery #1, 2, and 3

lp.transport (cost2, "min", row.signs, row.rhs, col.signs, col.rhs)
lp.transport (cost2, "min", row.signs, row.rhs, col.signs, col.rhs)$solution

#Result
#lp.transport (cost2, "min", row.signs, row.rhs, col.signs, col.rhs)
Success: the objective function is 139835 
> lp.transport (cost2, "min", row.signs, row.rhs, col.signs, col.rhs)$solution
[,1] [,2] [,3] [,4]
[1,]   20    0    0   55
[2,]   60   65    0    0
[3,]    0    0   70   30

#3.
#A company has 4 machines available for assignment to 4 tasks... etc

assign.costs3 <- matrix (c(13,4,7,6,1,11,5,4,6,7,2,8,1,3,5,9), 4, 4, byrow = TRUE)
#Matrix is given on exercise #3

assign.costs3
lp.assign (assign.costs3)
lp.assign (assign.costs3)$solution

#Result
assign.costs3
[,1] [,2] [,3] [,4]
[1,]   13    4    7    6
[2,]    1   11    5    4
[3,]    6    7    2    8
[4,]    1    3    5    9
> lp.assign (assign.costs3)
Success: the objective function is 11 

> lp.assign (assign.costs3)$solution
[,1] [,2] [,3] [,4]
[1,]    0    1    0    0
[2,]    0    0    0    1
[3,]    0    0    1    0
[4,]    1    0    0    0