-- From a project with Lara Anderson and James Gray.  Probably still of interest!
-- We will use this here to work with this package to stress it and test it.

-- Situation:
--  S = Cox ring of P^2 x P^2

invariants = method()

invariants(List, List) := (bidegree, group) -> (
     R := source group#0;
     c := symbol c;
     M := basis(bidegree, R);
     nvars := numColumns M;
     if nvars == 0 then return {};
     T := (coefficientRing R)[c_0..c_(nvars-1), generators R];
     oldvars := first entries sub(vars R, T);
     newvars := matrix{{T_0..T_(nvars-1)}};
     P := newvars * transpose sub(M, T);
     G := for g in group list map(T,T,newvars | sub(g.matrix,T));
     I := trim sum for g in G list (
	  trim ideal last coefficients(g P - P, Variables => oldvars)
	  );
     toGinvariant := map(T,T,(vars T) % I);
     Phat := toGinvariant P;
     ans := ideal last coefficients(Phat, Variables => first entries newvars);
     (sub(ans, R))_*
     )

-- The following routine is currently specialized to P^2 x P^2,
-- and the group action is specialzied to (a specific) Z/3 x Z/3.
-- Result is an ideal in a polynomial ring kk[b's, c's].
-- ASSUMPTIONS: srcdegree_0 is <= 3
--              srcdegree_1 >= 0
jumpingLocus = method()
jumpingLocus(List,List,Ring) := (srcdegree, formdegree,kk) -> (
     A := toField(kk[a]/(a^2+a+1));
     S := A[x_0..x_2, y_0..y_2, Degrees=>{3:{1,0}, 3:{0,1}}];
     sigma := map(S,S,{x_1,x_2,x_0,y_1,y_2,y_0});
     tau := map(S,S,{x_0,a*x_1,a^2*x_2,y_0,a^2*y_1,a*y_2});
     tau' := map(S,S,{x_0,a^2*x_1,a*x_2,y_0,a^2*y_1,a*y_2});
     M0 := invariants(formdegree, {sigma, tau});
     Msource := invariants({-srcdegree_0, srcdegree_1}, {sigma, tau'});
     Mtarget = invariants({-srcdegree_0 - formdegree_0, srcdegree_1 + formdegree_1},
	  {sigma, tau'});
     T := kk[b_1..b_(#Msource), c_1..c_(#M0), 
	  generators S, t_0, t_1, t_2]/(x_0*t_0-1, x_1*t_1-1, x_2*t_2-1);
     inv := map(T, S, {t_0, t_1, t_2, y_0, y_1, y_2});
     m0 := sum for i from 1 to #Msource list b_i * (inv Msource#(i-1));
     P := sum for i from 1 to #M0 list c_i * sub(M0#(i-1), T);
     Pm0 := m0*P;
     Tambient := ambient T;
     Pm0 = lift(Pm0, Tambient);
     Pm0 = sub(Pm0, {x_0 => 0, x_1 => 0, x_2 => 0});
     I := ideal for m in Mtarget list (
	  1/((size m)_kk) * contract(sub(inv m, Tambient), Pm0));
     U := kk[b_1..b_(#Msource), c_1..c_(#M0)];
     S' := kk (monoid S);
     Pparts := apply(M0, f -> sub(f, S'));
     (Pparts, sub(I,U))
     )

catalecticanty = method()
catalecticanty(List,List,Ring) := (srcdegree, formdegree,kk) -> (
     S := kk[x_0..x_2, y_0..y_2, Degrees=>{3:{1,0}, 3:{0,1}}];
     M0 := flatten entries basis(formdegree, S);
     Msource := flatten entries basis(srcdegree, S);
     Mtarget := flatten entries basis({srcdegree_0 - formdegree_0, srcdegree_1 + formdegree_1}, S);
     T := kk[b_1..b_(#Msource), c_1..c_(#M0), 
	  generators S, t_0, t_1, t_2]/(x_0*t_0-1, x_1*t_1-1, x_2*t_2-1);
     inv := map(T, S, {t_0, t_1, t_2, y_0, y_1, y_2});
     m0 := sum for i from 1 to #Msource list b_i * (inv Msource#(i-1));
     P := sum for i from 1 to #M0 list c_i * sub(M0#(i-1), T);
     Pm0 := m0*P;
     Tambient := ambient T;
     Pm0 = lift(Pm0, Tambient);
     Pm0 = sub(Pm0, {x_0 => 0, x_1 => 0, x_2 => 0});
     I := ideal for m in Mtarget list (
	  1/((size m)_kk) * contract(sub(inv m, Tambient), Pm0));
     U := kk[b_1..b_(#Msource), c_1..c_(#M0)];
     S' := kk (monoid S);
     Pparts := apply(M0, f -> sub(f, S'));
     (Pparts, sub(I,U))
     )

end--

use S1
phi = map(S1,S, {x_0^-1, x_1^-1, x_2^-1, y_0, y_1, y_2})
phi basis({3,1}, S)

use S1
sigma = map(S1,S1,{x_1,x_2,x_0,y_1,y_2,y_0})
tau = map(S1,S1,{x_0,a*x_1,a^2*x_2,y_0,a^2*y_1,a*y_2})

use U
sigma = map(U,U,{x_1,x_2,x_0,y_1,y_2,y_0})
tau = map(U,U,{x_0,a*x_1,a^2*x_2,y_0,a^2*y_1,a*y_2})

P = sub(basis({3,3},S), U) * transpose sub(vars T, U)
(sigma P - P)_(0,0)
I1 = trim ideal last coefficients((sigma P - P)_(0,0))
I2 = trim ideal last coefficients((tau P - P)_(0,0))
I = trim(I1 + I2)
numgens I

--- Making polynomial P Z/3 x Z/3 invariant
kk = QQ
A = toField(kk[a]/(a^2+a+1))
S = A[x_0..x_2, y_0..y_2, Degrees=>{3:{1,0}, 3:{0,1}}]
T = A[c_0..c_99]
U = T[x_0..x_2, y_0..y_2, Degrees=>{3:{1,0}, 3:{0,1}}, Join=>false, DegreeMap=>i->0]
P = ((vars T) * sub(transpose basis({3,3},S), U))_(0,0)
sigma = map(U,U,{x_1,x_2,x_0,y_1,y_2,y_0})
tau = map(U,U,{x_0,a*x_1,a^2*x_2,y_0,a^2*y_1,a*y_2})
I1 = trim ideal last coefficients((sigma P - P))
I2 = trim ideal last coefficients((tau P - P))
I = trim(I1 + I2)
toGinvariant = map(U,U,vars U | (sub(vars T, U) % I))
Phat = toGinvariant P

-- get the matrix corresponding to H^2(OO(-6,0)) --> H^2(OO(-3,3)) for Phat
B1 = sub(basis({3,0}, S), U)
B2 = sub(basis({0,3}, S), U)
M = contract(B2, transpose contract(B1, Phat))
R = ZZ/5[rsort support M]
M5 = sub(M, R)
factor det M5
I9 = minors(9, M5);
gens gb I9;
see ideal oo

print toString Phat
M

restart
kk = ZZ/32003
A = toField(kk[a]/(a^2+a+1))
S = A[x_0..x_2, y_0..y_2, Degrees=>{3:{1,0}, 3:{0,1}}]
T = A[c_0..c_99, x_0..x_2, y_0..y_2]
P = (matrix{{c_0..c_99}} * sub(transpose basis({3,3},S), T))_(0,0)

sigma = map(T,T,{c_0..c_99,x_1,x_2,x_0,y_1,y_2,y_0})
tau = map(T,T,{c_0..c_99,x_0,a*x_1,a^2*x_2,y_0,a^2*y_1,a*y_2})
I1 = trim ideal last coefficients(sigma P - P, Variables => {x_0,x_1,x_2,y_0,y_1,y_2})
I2 = trim ideal last coefficients(tau P - P, Variables => {x_0,x_1,x_2,y_0,y_1,y_2})
I = trim(I1 + I2)
toGinvariant = map(T,T,(vars T) % I)
Phat = toGinvariant P
ideal last coefficients(Phat, Variables => {c_0..c_99})
see oo

restart
load "jumping-cohom.m2"
kk = ZZ/32003
A = toField(kk[a]/(a^2+a+1))
S = A[x_0..x_2, y_0..y_2, Degrees=>{3:{1,0}, 3:{0,1}}]
sigma = map(S,S,{x_1,x_2,x_0,y_1,y_2,y_0})
tau = map(S,S,{x_0,a*x_1,a^2*x_2,y_0,a^2*y_1,a*y_2})
tau' = map(S,S,{x_0,a^2*x_1,a*x_2,y_0,a^2*y_1,a*y_2})
invariants({3,3}, {sigma, tau})
invariants({1,2}, {sigma, tau})
invariants({-2,2}, {sigma, tau})
invariants({0,0}, {sigma, tau})
invariants({0,-1}, {sigma, tau})

M0 = invariants({3,3}, {sigma, tau})
Msource = invariants({3,0}, {sigma, tau'})
Mtarget = invariants({0,3}, {sigma, tau})

-- steps:
-- form P, in a new ring (with the b and c variables, and the x,y, and inverses of x variables)
-- map M1 to inverses of x's.
-- mult general elem of M1 by P to get soemthing which hopefully we can make a matrix out of.
T = kk[b_1..b_(#Msource), c_1..c_(#M0), generators S, t_0, t_1, t_2]/(x_0*t_0-1, x_1*t_1-1, x_2*t_2-1)
inv = map(T, S, {t_0, t_1, t_2, y_0, y_1, y_2})
m0 = sum for i from 1 to #Msource list b_i * (inv Msource#(i-1))
P = sum for i from 1 to #M0 list c_i * sub(M0#(i-1), T)
Pm0 = m0*P
Tambient = ambient T
Pm0 = lift(Pm0, Tambient)
Pm0 = sub(Pm0, {t_0 => 0, t_1 => 0, t_2 => 0})
trim ideal for m in Mtarget list contract(sub(m, Tambient), Pm0)


restart
load "jumping-cohom.m2"
kk = ZZ/32003
kk = QQ
jumpingLocus({-3,0},{3,3},kk) -- finishes in singular
jumpingLocus({-3,3},{3,3},kk) -- finishes in singular
jumpingLocus({-6,0},{3,3},kk) -- finishes in singular
     
jumpingLocus({-5,1},{3,3},kk)

(P,I) = jumpingLocus({-6,3},{3,3},kk) -- key one of interest BIG ENOUGH
jumpingLocus({-4,5},{3,3},kk)  -- doesn't finish in singular BIG ENOUGH
jumpingLocus({-4,2},{3,3},kk)  -- doesn't finish in singular NOT BIG ENOUGH

I = jumpingLocus({-5,1},{2,2},kk)
primaryDecomposition I
     
describe ring I     
use ring I
M = contract(transpose matrix{{b_1..b_32}}, gens I);
A = ring M
M
--gens gb(M, Algorithm=>Homogeneous2);
Mb = contract(transpose matrix{{c_1..c_12}}, gens I);
Mb
decompose I
I1 = ideal select(I_*, f -> size f <= 4)
I12 = I1 : c_12;
I11 = I1 : I12
I12 = trim I12

I12 : c_2 == I12  -- true
I12a = eliminate(b_12, I12)
I12b = eliminate({b_9, b_12, b_11}, I12) -- irred, therefore I12 is prime, rational

J7 = ideal select(I_*, f -> size f == 7)
I12' = trim(I12 + J7)
I11' = trim(I11 + J7)
codim I12;
codim I11' -- 9, 4 linear forms
codim I12'
numgens I11'
(ideal gens gb I11')_*/factorize//netList

I0 = trim sub(I, {c_2=>0, c_9=>0, c_11=>0, c_12=>0})
I1 = ideal select(I0_*, f -> size f <= 3)

I0 = trim sub(I, {c_2=>0, c_9=>0, c_11=>0, c_12=>0, c_3=>0, c_4=>0, c_5=>0, c_6=>0, c_7=>0, c_8=>0})
I0sat = saturate(I0, ideal(c_1, c_10))
codim I0sat  -- 12
see I0
contract(transpose matrix{{c_1,c_10}}, gens I0)


gbTrace=3
J = ideal gens gb(I, DegreeLimit=>4);
J_*/factorize//netList
netList apply(select(J_*, f -> size f < 200), factorize)

I0 = ideal(I_0, I_1, I_2, I_3, I_4, I_5)  -- codim 6.
I0 = ideal(I_0, I_1, I_2, I_3, I_4)
I1 = ideal(I_6, I_7, I_8, I_9, I_10)  -- codim 5
I2 = ideal(I_11, I_12, I_13, I_14, I_15)  -- codim 5
codim I2

(ideal gens gb I2)/factorize//netList
F1 = (ideal gens gb I0)/factorize;
F1a = select(F1, f -> #f > 1 or f#0#0 > 1);
#F1
#F1a
F1a/last

--------------------
-- catalecticanty --

restart
load "jumping-cohom.m2"
kk = ZZ/32003
(P,I) = catalecticanty({3,0},{2,2},kk)
see I

--------------------
restart
load "jumping-cohom.m2"
kk = ZZ/32003
--kk = QQ
(P,I) = jumpingLocus({-6,0},{3,3},kk)
gbTrace=3
C = decompose I;
Isat = trim saturate(I, ideal(b_1..b_4));
decompose Isat
I = ideal reverse I_*
decompose I
contract(transpose matrix{{b_1..b_4}}, gens I)

use ring I
I1 = I : ideal(c_9..c_12)
I1 : ideal(c_9..c_12) == I1 -- true
I2 = I1 : ideal(b_1..b_4)
I2 : ideal(b_1..b_4) == I2 -- true
I2 = trim I2

I21 = I2 : (c_11-c_12)
I21' = I21 : (c_11-c_12)
  I21 == I21'
I22 = I2 : I21  
I22 = trim I22  

I221 = I22 : (c_7-c_8)
I22 : I221
I2 = I : I1
I21 = I2 : c_1
I22 = I2 : I21
I : (I : ideal(b_1-b_4, b_2-b_4, b_3-b_4))
oo : b_4
  --time irreducibleCharacteristicSeries I;
debug Core
gbTrace=3
I = ideal reverse I_*
m = gens I
S = ring I
re = rawIdealReorder raw m -- we need to substitute S_(re#i) => T_i beforehand and let the user undo it afterward, if desired
re' = inversePermutation re -- so the two substitutions are S_j => T_(re'_j) and T_i => S_(re_i)
k = coefficientRing ring I
T = k ( monoid [ Variables => S.generatorSymbols_re, Degrees => (degrees S)_re, MonomialOrder => Lex, Heft => heft S ] );
StoT = map(T,S,(generators T)_re')
TtoS = map(S,T,(generators S)_re )
m1 = StoT m
result = rawCharSeries raw m1  -- takes too long, why?  (i.e. in singular, it is fast)
apply(result, rawmat -> map(T,rawmat))

----------------------------------
-- Playing around with one of the cases
-- Trying to find the primary decomp (or min primes)
restart
load "jumping-cohom.m2"
kk = ZZ/32003
(P,I) = jumpingLocus({-5,1},{3,3},kk)
S = ring I
I
G4 = ideal gens gb(I, DegreeLimit=>4);
select(G4_*, f -> # factorize f >= 2)
M = b_1^2 | gens I
gbTrace=3
syz(M, SyzygyRows=>1, SyzygyLimit=>1000);

J = ideal select(I_*, f -> size f <= 7)
J = ideal drop(J_*, {6,6})
gens gb(J, Algorithm=>LinearAlgebra);
gens gb(J, Algorithm=>Homogeneous2, Strategy=>LongPolynomial);
codim J
Ja = saturate(J, b_1);
J : b_1 == J
J : c_1 == J

J1 = ideal(I_0, I_4, I_6, I_9) -- J1 is prime and rational
codim J1
for x in gens ring J1 list (J1 : x == J1) -- all true
J1a = eliminate(J1, {b_1,b_2,b_4})
codim J1a -- 2
for f in J1a_* list factorize f
see oo
J1 : b_1 == J1
J1 : c_1 == J1
----------------------------------
