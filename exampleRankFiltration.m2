restart
needsPackage "FiniteFieldExperiments"

-- example: in P^2 x P^2, a polynomial of degree (3,3)


kk = ZZ/5
bb = blackBoxParameterSpace(100, kk)
R = kk[x_0..x_2, y_0..y_2, Degrees => {3:{1,0}, 3:{0,1}}]
degrees R

mons = basis({3,3}, R)
calabiYauAt = (point) -> (mons * (transpose point))_(0,0)
testPt = random(kk^1, kk^100)
calabiYauAt testPt

-- smoothness is hard to check, so we assume true for now!
isSmoothCYAt = point -> (
    true
    )
bb.rpp("isSmoothCYAt", isSmoothCYAt)

-- for an example, take 10x10 matrix of all coefficients
-- this might be a poor choice, but it will get us going...!
Mat = point -> (
    kk := ring point;
    reshape(kk^10, kk^10, point)
    )
Mat testPt
rankMat = point -> rank Mat point
bb.rpp("rankMat", rankMat)

bb.pointProperties()

e = new Experiment from bb
e.watchProperties{"isSmoothCYAt", "rankMat"}
e.run(10000)

e.estimateStratification()
e.pointsByPropertyValues({true,8})
  -- same as: (e.pointLists())#{true,8}
e.tryProperty("rankMat")
