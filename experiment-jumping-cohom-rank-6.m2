-- Study: the locus of the set of CY's of rank 6.
-- Run this in directory FiniteFieldExperiments.M2/
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

I6 = trim minors(7, M5);
Jac6 = jacobian I6;

-- survey the rank 6 locus
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

tangentSpaceCodimAt = (pt) -> (
    if rankAt pt > 6 then "not on rank6 locus"
    else rank substitute(Jac6, pt)
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

smallRankSmoothAt = (pt) -> (
    r := rankAt pt;
    if r < 7 then {smoothAt pt, r, tangentSpaceCodimAt pt} else r
    )
bb.rpp("smallRankSmoothAt", smallRankSmoothAt)

E = new Experiment from bb
E.setPointsPerComponent(1000)
E.watchProperty "smallRankSmoothAt"
E.run(1)
elapsedTime E.run(1000); E.counts()
elapsedTime E.run(10000); E.counts()
E.estimateStratification()
E.trials() / (5^4 * 1.0) -- 22 points is expected for a codim 4 component
  -- the number for (smooth,6,4) is 52 points (on one such run).
E.trials() / (5^6 * 1.0) -- 

-- o45 = Counts{{{singular, 3, 0}} => 2  }
--              {{singular, 4, 0}} => 10
--              {{singular, 5, 0}} => 45
--              {{singular, 6, 4}} => 244
--              {{singular, 6, 6}} => 5
--              {{smooth, 4, 0}} => 7
--              {{smooth, 5, 0}} => 19
--              {{smooth, 6, 4}} => 104
--              {{smooth, 6, 6}} => 2
--              {7} => 1234
--              {8} => 3552
--              {9} => 8348
--              {10} => 10429
104.0 * 5^4 / E.trials() -- 2.7 components
244.0 * 5^4 / E.trials() -- 6.3 components
2.0 * 5^6 / E.trials() -- 1.3 components
5.0 * 5^6 / E.trials() -- 3.2 components
E.counts()
somepts = E.pointsByKey({{"smooth",6,4}})
#somepts
C = minprimes(I6, Verbosity=>2); -- 81 components of codimensions:
-- o61 = Tally{3 => 2 }
--             4 => 63
--             6 => 16

for p in somepts list 
pt = somepts_0

matrix for p in somepts list 
  for c in C list if 0 == sub(gens c, p) then 1 else 0

for c in C list # positions(somepts, p -> sub(gens c, p) == 0)

somesingpts = E.pointsByKey({{"singular",6,4}})
#oo
for c in C list # positions(somesingpts, p -> sub(gens c, p) == 0)

C_0
codim C_0
trim ideal ((gens (minors(6, M5 % C_0))) % C_0)
see C_0

for a in (0,0)..(4,4) list ((b,c) := a; (b^2 + b*c + c^2)%5)
for a in (0,0)..(6,6) list ((b,c) := a; (b^2 + b*c + c^2)%7)
tally oo 
