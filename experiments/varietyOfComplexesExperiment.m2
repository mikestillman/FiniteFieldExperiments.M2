-- Consider the variety of complexes
--
-- K^1 --A--> K^2 --B--> K^2
--
-- It is the variety defined by the equation A*B = 0 
-- in the space of pairs of matrices (A,B)

restart
needsPackage"FiniteFieldExperiments"
setRandomSeed 0
K = ZZ/5
--R = K[a_1,a_2]**K[b_1..b_4]
R = K[a_1,a_2,b_1..b_4] -- bihomogeneous is a problem with interpolation
A = matrix{{a_1,a_2}}
B = matrix{{b_1,b_2},{b_3,b_4}}

-- make a blackBox
bb = blackBoxIdeal ideal(A*B)

-- make an experiment from this black box
e = new Experiment from bb

-- what is observed?
e.watchedProperties()
-- the codim of the tangent space is allways observed

-- evaluate at 100 random points
e.run(100)
-- most pairs of matrices do not lie on the
-- variety of complexes

-- look at more points
e.run(3000)

-- estimate the decomposition
e.estimateDecomposition()

-- there seem to be no components of codimension 0 or 1.
-- the points with codim tangentspace < 2 seem to be 
-- singular points

-- lets check this:
e.tryProperty("isCertainlySingularAt")
-- indeed our variety is singular at points with codim Tangentspace 0 or 1

-- for a better estimate, we have to include the singular points
-- in the count:
numAllPoints = sum apply({1,2},i->(e.counts())#{i})
(char K)^2*numAllPoints/e.trials()*1.0
-- 1.96

-- we now want to find point Properties separating these
-- 2 components

-- this most obvious invariant of A and B are their ranks

-- the matrix A at a point
Aat = point -> sub(A,point) 
-- its rank
rankAat = point -> rank Aat(point)
bb.rpp("rankAat",rankAat)

-- the matrix B at a point
Bat = point -> sub(B,point)
-- its rank
rankBat = point -> rank Bat(point)
bb.rpp("rankBat",rankBat)

-- observe the ranks
e.watchProperties{"rankAat","rankBat"}
-- the statistics have to be reset if the obervation changes
e.clear()
e.watchProperties{"rankAat","rankBat"}

-- run the experiment
e.run(1000)
time e.run(10000)

-- estimate the decomposition
e.estimateDecomposition()

-- the ranks (0,0), (0,1) and (1,0) do not seem to correpond to compoenents
-- the ranks (0,2) and (1,1) seem to correspond to distinct components
e.tryProperty("isProbablySmoothAt")

-- the ranks (0,0) and (0,1) seem to occur only in the singular locus
-- the ranks (1,0) seem to correpond to smooth points. Possibly 
-- on the same compoenent as (1,1)

-- we now use interpolation to study the components
-- (this still seems to be buggy)
viewHelp FiniteFieldExperiments

e.interpolateComponents(1,3)
code e.interpolateComponents
e.smoothPoints(10,10)
-- these do not seem to work
bb.resetInterpolation()
smoothPoints = select(e.points(),point->bb.isProbablySmoothAt(point))
#smoothPoints

bb.resetInterpolation()
time bb.interpolateComponentsAt(smoothPoints,2)
-- there seems to be a problem with deg=1 and empty interpolated ideals
e.tryProperty("interpolatedComponentNamesAt")
viewHelp BlackBoxIdeals


e = new Experiment from bb
e.clear()
e.watchProperty("interpolatedComponentNamesAt")
e.run(10000)
e.tryProperty("isProbablySmoothAt")
bb.onComponentPrecision()
