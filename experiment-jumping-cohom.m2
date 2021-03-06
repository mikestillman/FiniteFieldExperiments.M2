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
-- the matrix at a point:
Mat = (pt) -> sub(M5, pt)
-- equation of the CY (in P^2 x P^2) at the point:
Fat = (pt) -> (B1 * Mat pt * transpose B2)_(0,0)
Fat testPt
-- rank of the matrix at the point. We register this too.
rankAt = (pt) -> rank Mat pt
bb.rpp("rankAt", rankAt)

-- The equation of one of the 9 affine open sets in the chart
affineFat = (pt) -> sub(Fat pt, {x_2 => 1, y_2 => 1})

-- The singular locus in this chart.
almostSingularLocusAt = (pt) -> (
    F0 := affineFat pt;
    ideal F0 + ideal jacobian ideal F0
    )
-- Whether the CY is smooth in this chart.  We register this.
almostSmoothAt = (pt) -> codim almostSingularLocusAt pt > 4

smoothAt = (pt) -> (
    -- returns a list of 9 booleans at the moment.
    F := Fat pt;
    issmooth := flatten for i from 0 to 2 list for j from 0 to 2 list (
      F0 := sub(F, {x_i => 1, y_j => 1});
      singF0 := ideal F0 + ideal jacobian ideal F0;
      codim singF0 > 4);
    all(issmooth, identity)
    )

smoothAt = (pt) -> (
    -- returns a list of 9 booleans at the moment.
    F := Fat pt;
    for ij in (0,0)..(2,2) do (
        (i,j) := ij;
        F0 := sub(F, {x_i => 1, y_j => 1});
        singF0 := ideal F0 + ideal jacobian ideal F0;
        if codim singF0 <= 4 then return "singular";
        );
    "smooth"
    )
bb.rpp("smoothAt", smoothAt)
--bb.upp("smoothAt", smoothAt)

-- bb.rpp("almostSmoothAt", almostSmoothAt)
-- bb.upp("almostSmoothAt", almostSmoothAt) -- this updates the fcn, if we have modified the function.

smallRankSmoothAt = (pt) -> (
    r := rankAt pt;
    if r < 7 then {smoothAt pt, r} else r
    )
bb.rpp("smallRankSmoothAt", smallRankSmoothAt)

-- The experiment is to determine the ranks of these matrices
-- at points where the CY is smooth on this chart.
E = new Experiment from bb
-- E.watchProperties{"almostSmoothAt", "rankAt"}
E.watchProperty "smallRankSmoothAt"
E.run(1)
elapsedTime E.run(10);
elapsedTime E.run(100); E.counts()
elapsedTime E.run(1000); E.counts()
E.estimateStratification()
elapsedTime E.run(10000); E.counts()
E.estimateStratification()

almostSmoothAt testPt
rk9pt = first E.pointsByKey({9})
F = Fat rk9pt

E.clear()
E.watchedProperties()

elapsedTime E.tryProperty("smoothAt")
smoothAt testPt
smoothAt random(kk^1, kk^12)

-- composed property

