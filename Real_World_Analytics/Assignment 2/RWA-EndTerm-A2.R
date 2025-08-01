install.packages("lpSolveAPI")
library(lpSolveAPI)

#Question 1
garmentCompanyModel = make.lp(0, 2)# create lp model
lp.control(garmentCompanyModel, sense = "max")
set.objfn(garmentCompanyModel, c(10,8))# set the objective function
add.constraint(garmentCompanyModel, c(40,20), "<=", 9600)# cutting constraint
add.constraint(garmentCompanyModel, c(40,100), "<=", 24000)#sewing constraint
add.constraint(garmentCompanyModel, c(20,20), "<=", 6720)#packaging constraint
garmentCompanyModel

constraintNames = c("Constraint for cutting", "Constraint for sewing", "Constraint for packaging")
variableNames = c("Shirts", "pants")
dimnames(garmentCompanyModel) = list(constraintNames, variableNames)


solve(garmentCompanyModel)
garmentCompanyModel

message("\nmaximum value:",round(get.objective(garmentCompanyModel), 2))
# optimal solution for garment 
optimal_solution <- round(get.variables(garmentCompanyModel),2)
optimal_solution
#getting constraints
round(get.constraints(garmentCompanyModel) , 2)

#range for profit per shirt without affecting optimal solution
get.sensitivity.obj(garmentCompanyModel)


#Question 2

factoryproblem<-make.lp(10,9) #creating linear program with 10 constraints and 9 variables 

lp.control(factoryproblem,sense ="maximize")# the lp controls as "maximize" objective

#Setting the objective function
set.objfn(factoryproblem, c(15,11,15,10,6,10,25,21,25), indices = (1:9))# objective function variables

#Constraints
set.row(factoryproblem, 1, c(0.5,-0.5,-0.5), indices = c(1,4,7))  #Cotton in bloom proportion
set.row(factoryproblem, 2, c(0.4,-0.6,-0.6), indices = c(2,5,8))  #Cotton in amber proportion
set.row(factoryproblem, 3, c(0.5,-0.5,-0.5), indices = c(3,6,9))  #Cotton in leaf proportion
set.row(factoryproblem, 4, c(-0.4,0.6,-0.4), indices =c(1,4,7))   #Wool in bloom proportion
set.row(factoryproblem, 5, c(-0.4,0.6,-0.4), indices =c(2,5,8))   #Wool in amber proportion
set.row(factoryproblem, 6, c(-0.3,0.7,-0.3), indices = c(3,6,9))  #Wool in leaf proportion
set.row(factoryproblem, 7, c(1,1,1), indices = c(1,4,7))          #Demand for Bloom
set.row(factoryproblem, 8, c(1,1,1), indices = c(2,5,8))          #Demand for Amber
set.row(factoryproblem, 9, c(1,1,1), indices = c(3,6,9))          #Demand for Leaf
set.row(factoryproblem, 10, c(1,1,1,1,1,1,1,1,1), indices = c(1,2,3,4,5,6,7,8,9)) #Non- Negative Constraints

factoryproblem

set.rhs(factoryproblem, c(0,0,0,0,0,0,4200,3200,3500,0))# setting the right hand side values 

set.constr.type(factoryproblem, c(">=",">=",">=",">=",">=",">=","<=","<=","<=",">="))# setting the type of constraints 

set.type(factoryproblem, c(1:9),"real")# setting the type of factory problem as real

set.bounds(factoryproblem, lower = rep(0,9), upper = rep(Inf,9))# setting the boundary values with lower and upper

solve(factoryproblem)#solving the problem

factoryproblem
#Getting the objective function value 
objvalue<-get.objective(factoryproblem)
objvalue

#Getting the Solution variables for the factory problem
solution<-get.variables(factoryproblem)
solution


#Question 3
BiddingProblem <- make.lp(6, 6) #creating the linear program with 6 variables and 6 constraints

lp.control(BiddingProblem, sense= "minimize") #the lp controls as "minimize" objective

#setting the Objective Function with y1,y2,y3,y4,y5,v
set.objfn(BiddingProblem, c(0,0,0,0,0,1)) 

# Payoff matrix 
#Giant is chosen as Player1 and sky chosen as Player 2
set.row(BiddingProblem, 1, c(1,-1,-1,-1,-1,1), indices = c(1,2,3,4,5,6)) #when Player 1 choose Strategy 1
set.row(BiddingProblem, 2, c(1,1,-1,-1,-1,1), indices = c(1,2,3,4,5,6))  #when Player 1 choose Strategy 2 
set.row(BiddingProblem, 3, c(1,1,1,-1,-1,1), indices = c(1,2,3,4,5,6))   #when Player 1 choose Strategy 3
set.row(BiddingProblem, 4, c(1,1,1,1,-1,1), indices =c(1,2,3,4,5,6))     #when Player 1 choose Strategy 4
set.row(BiddingProblem, 5, c(-1,-1,-1,-1,1,1), indices =c(1,2,3,4,5,6))  #when Player 1 choose Strategy 5

set.row(BiddingProblem, 6, c(1,1,1,1,1,0), indices =c(1,2,3,4,5,6))      #Sum of total probability
BiddingProblem

set.rhs(BiddingProblem, c(0,0,0,0,0,1)) #Setting the Right hand side Values

set.constr.type(BiddingProblem, c(">=",">=",">=",">=",">=","=")) #Setting the constraint types
BiddingProblem

solve(BiddingProblem)#Solving Bidding problem

#Getting the objective value of Bidding Problem
objvalue1<-get.objective(BiddingProblem)
objvalue1

#Getting the Solution variables for  player 1 bidding 
solution1<-get.variables(BiddingProblem)
solution1

