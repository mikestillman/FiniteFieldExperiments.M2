
restart
K = ZZ/7

needsPackage "FiniteFieldExperiments"

-- the parameter space of 4x4 matrices
bb = blackBoxParameterSpace(16,K);

-- a random point in the parameter space
testPoint = random(K^1,K^16)

-- the matrix 4x4 matrix M ("M at")
Mat = point -> point_{0..3}||point_{4..7}||point_{8..11}||point_{12..15}
bb.rpp("Mat",Mat);
Mat(testPoint)

-- the inverse matrix M^-1 ("M inverse at")
MinvAt = point -> (
    (Mat(point))^-1
    )
bb.rpp("MinvAt",MinvAt);
MinvAt(testPoint)

-- the rank of the inverse matrix
rankInvAt = point -> rank (MinvAt(point))
bb.rpp("rankInvAt",rankInvAt);
rankInvAt(testPoint)

-- the experiment
e = new Experiment from bb

-- observe the rank of the inverse matrix
e.watchProperty("rankInvAt")

e.run(100);
e.collectedCount()

-- select a point causing an error
errorPoint = ((e.pointLists())#{Error})#0

Mat(errorPoint)
-- this works fine

MinvAt(errorPoint)
-- this is the problem

-- the problem is that the deteriminant is nonzero
det Mat(errorPoint)


-- compute the inverse matrix only if the determinant is nonzero
MinvAt = point -> (
    if det Mat(point) == 0 then throw "Det = 0";
    (Mat(point))^-1
    )
-- update to the new version of MinvAt
bb.upp("MinvAt",MinvAt);
catch MinvAt(testPoint)
catch MinvAt(errorPoint)

-- run the experiment again
e.clear()
e.run(100);
e.collectedCount()

-- notice that we do not have to put any error handling code
-- into "rankInvAt" even though it uses "MinvAt". 
