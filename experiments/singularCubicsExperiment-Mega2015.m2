-- look at the hilbert scheme of singular
-- cubics

restart
needsPackage"BlackBoxParameterSpaces"
needsPackage"FiniteFieldExperiments"
setRandomSeed "8943758347"

-- we work in char 7
K = ZZ/7
-- the Coordinate Ring of IP^3
R = K[x,y,z,w]

-- make a blackbox describing the parameter space of cubic polynomials
bbC = blackBoxParameterSpace(20,K);
-- this Black Box is still empty:
bbC.pointProperties()

-- there are 20 monomials in degree 3
mons3 = super basis(3,R)

-- make a cubic from 20 coefficients 
cubicAt = (point) -> matrix entries (point*transpose mons3)

-- Example
fermatPoint = matrix {{1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1_K}}
cubicAt(fermatPoint)

-- register this function in the BlackBox
bbC = bbC.registerPointProperty("cubicAt",cubicAt);
bbC.pointProperties()
-- {cubicAt}

-- calculate singular locus of cubic
singularLocusAt = (point) -> ideal jacobian cubicAt(point)

-- Example
codim singularLocusAt(fermatPoint)
-- the fermat cubic is smooth

-- Example: coefficients of the Cayley Cubic 
--          4*(x^3+y^3+z^3+w^3)-(x+y+z+w)^3
cayleyPoint = matrix {{3,-3,-3,-3,-3,1,1,-3,1,-3,3,-3,-3,-3,1,-3,3,-3,-3,3_K}}
codim singularLocusAt(cayleyPoint)
degree singularLocusAt(cayleyPoint)
-- the cayley cubic has 4 singular points

-- register this function in the BlackBox
bbC = bbC.registerPointProperty("singularLocusAt",singularLocusAt);
bbC.pointProperties()

-- calculate number of singular points
-- special treatment for cubics with 0 or infinitely many singular points
degreeSingularLocusAt = (point) -> (
     s := singularLocusAt(point);
     if dim s == 0 then return 0;
     if dim s == 1 then return degree s;
     if dim s >= 2 then return infinity
     -- these are affine dimensions
     )

-- Example
degreeSingularLocusAt(fermatPoint)
degreeSingularLocusAt(cayleyPoint)

-- register this function
bbC = bbC.registerPointProperty("degreeSingularLocusAt",degreeSingularLocusAt);
bbC.pointProperties()

-- now we can study the distribution of singular cubics in the parameter
-- space of all cubics:

e = new Experiment from bbC;
-- so far nothing is observed:
e.watchedProperties()
-- {}

e.watchProperty("degreeSingularLocusAt")
-- now we watch for the degree of the singular locus
e.watchedProperties()
-- {degreeSingularLocusAt}


-- lets look at 100 cubics
e.run(100)

-- lets look at some more
e.run(3000)

-- what does this mean? Lets try to estimate the stratification of 
-- the parameter space of cubics by this property:
e.estimateStratification()

-- interpretation:
--
-- * the locus of smooth cubics has codim 0 
-- * the locus of singular cubics has codim 1
-- * the locus of cubics with a singular locus of degree 2
--   looks strange, it has estimated codimension between 1 and 2. 
--   the reason might be, that there several components. In this case
--   probability of finding such a cubic would be
--
--   p = r*1/(7^c) with c the codimension and r the number of components
--   Taking logarithms give
--   -log_7(p) = c-log_7(r)

-- so the estimated codimension of the degree 2 locus would be off
-- by

log(2)/log(7)
-- 0.35 (for two components)
log(3)/log(7) 
-- 0.56 (for three components)
log(4)/log(7)
-- 0.71 (for three componebts)

-- so at the moment one would guess that there are
-- * 2 codim 2 components for degree 2
-- * 3 codim 3 components for degree 3


-- we will now try to find a point property that
-- distinguishes the components with a singular
-- locus of degree 2

-- for our convenience the experiment has
-- collected some interesting points
e.collectedCount()
e.pointLists()

-- take the first point with a degree 2 singular locus
testPoint = (e.pointLists())#{2}#0
decompose singularLocusAt(testPoint)

-- possibly the cubics with a double point as singular locus
-- form a component separate from those with 2 reduced points
-- (without the data from the experiment, one could be 
--  tempted to think, that the ones with double points
--  are specializations of those with two reduced points)


-- lets look at the multiplicity structure of the singular locus
multiplicitiesSingularLocusAt = (point) -> (
     sing := singularLocusAt(point);
     d := degreeSingularLocusAt(point);
     if d == 0 then return {};
     if d == infinity then return {infinity};
     pSing := primaryDecomposition(sing);
     sort flatten apply(pSing,i->(
	       if dim i == 0 then return {};
	       r := degree radical i;
	       d := degree i;
	       apply(r,j->d//r)
	       ))
     )

-- Example
multiplicitiesSingularLocusAt(fermatPoint)
multiplicitiesSingularLocusAt(cayleyPoint)
multiplicitiesSingularLocusAt(testPoint)

-- register this property
bbC = bbC.registerPointProperty("multiplicitiesSingularLocusAt",multiplicitiesSingularLocusAt);
bbC.pointProperties()

-- lets try our new property on the collected points
-- to see whether it distinguishes components
e.collectedCount()
e.tryProperty("multiplicitiesSingularLocusAt")

-- in degree 2 this property divides the points in 2 
-- sets of equal size. 

-- lets run the experiment again and watch this property
e.clear() 
-- this erases statistics
e.collectedCount()
e.trials()

-- now refine the observation
e.watchedProperties()
e.watchProperty("multiplicitiesSingularLocusAt")
e.watchedProperties()

time e.run(1000)
-- used 9.04075 seconds

e.estimateStratification()
-- the package only estimates the codimensions of strata 
-- with enough points

-- lets do more points
time e.run(10000)    
-- used 94.8239 seconds
e.estimateStratification()     

-- indeed we find that the multiplicity structure
-- seems to seperate the components in codim 2 and 3

-- if we would continue looking at points we would find
-- that there seem to be 2 components with multiplicity structure {4}. 
-- These belong to singularities of type A_4 and D_4

-- the complete story can be found in Dolgachevs book (find reference)


-- ToDo: 
-- * remove empty lines in Sortable Counts
-- * reverse sort in Sortable Counts
-- * swap sides in estimated codimension    