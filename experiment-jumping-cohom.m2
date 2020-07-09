-- Run this in directory FiniteFieldExperiments.M2/
--test
restart
needsPackage "MinimalPrimes"
installMinprimes()
needsPackage "FiniteFieldExperiments"

kk = ZZ/5
R = kk[c_44, c_49, c_71, c_75, c_77, c_82, c_83, c_88, c_90, c_94, c_96, c_99]
S = kk[x_0,x_1,x_2,y_0,y_1,y_2, Degrees => {3:{1,0},3:{0,1}}]
B1 = matrix {{x_0^3, x_0^2*x_1, x_0^2*x_2, x_0*x_1^2, x_0*x_1*x_2, x_0*x_2^2, x_1^3, x_1^2*x_2, x_1*x_2^2, x_2^3}}
B2 = matrix {{y_0^3, y_0^2*y_1, y_0^2*y_2, y_0*y_1^2, y_0*y_1*y_2, y_0*y_2^2, y_1^3, y_1^2*y_2, y_1*y_2^2, y_2^3}}
-- F = B1 * M * transpose B2 -- eventually want to consider rank of M5 at smooth F in P^2 x P^2.

M5 = matrix {
    {c_99, 0, 0, 0, c_94, 0, c_90, 0, 0, c_96}, 
    {0, c_77, 0, 0, 0, c_71, 0, c_75, 0, 0}, 
    {0, 0, c_88, c_82, 0, 0, 0, 0, c_83, 0}, 
    {0, 0, c_83, c_88, 0, 0, 0, 0, c_82, 0}, 
    {c_49, 0, 0, 0, c_44, 0, c_49, 0, 0, c_49}, 
    {0, c_75, 0, 0, 0, c_77, 0, c_71, 0, 0}, 
    {c_96, 0, 0, 0, c_94, 0, c_99, 0, 0, c_90}, 
    {0, c_71, 0, 0, 0, c_75, 0, c_77, 0, 0}, 
    {0, 0, c_82, c_83, 0, 0, 0, 0, c_88, 0}, 
    {c_90, 0, 0, 0, c_94, 0, c_96, 0, 0, c_99}}

-- survey the rank filtration of M5.
bb = blackBoxParameterSpace(12, kk)
testPt = random(kk^1, kk^12)
-- the matrix:
Mat = (pt) -> sub(M5, pt)
Fat = (pt) -> (B1 * Mat pt * transpose B2)_(0,0)
Fat testPt
rankAt = (pt) -> rank Mat pt
bb.rpp("rankAt", rankAt)

affineFat = (pt) -> sub(Fat pt, {x_2 => 1, y_2 => 1})
almostSingularLocusAt = (pt) -> (
    F0 := affineFat pt;
    ideal F0 + ideal jacobian ideal F0
    )
almostSmoothAt = (pt) -> codim almostSingularLocusAt pt > 4

bb.rpp("almostSmoothAt", almostSmoothAt)
-- bb.upp("almostSmoothAt", almostSmoothAt)


E = new Experiment from bb
E.watchProperties{"rankAt", "almostSmoothAt"}
E.run(1)
elapsedTime E.run(10)
elapsedTime E.run(100)
E.counts()
almostSmoothAt testPt
rk9pt = first E.pointsByKey({9})
F = Fat rk9pt

