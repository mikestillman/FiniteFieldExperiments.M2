-- finite field experiments

newPackage(
     "FiniteFieldExperiments",
     Version => "0.3", 
     Date => "15.04.2021",
     Authors => {{
             Name => "Hans-Christian Graf v. Bothmer", 
             Email => "hans.christian.v.bothmer@math.uni-hamburg.de", 
             HomePage => "http://www.crcg.de/wiki/Bothmer"},
         { Name => "Jakob Kroeker", 
             Email => "jakobkroeker.academic@spaceship-earth.net", 
             HomePage => "http://www.crcg.de/wiki/User:Kroeker"}
         },
     PackageExports => {
         "BlackBoxParameterSpaces", 
         "IntervalPkg" 
         },
     Headline => "finite field experiments for explicit and implicitly given ideals and parameter spaces",
     DebuggingMode => true,
     --CacheExampleOutput => true,
     AuxiliaryFiles=>true
)

-- Map eigentlich Morphism
export { 
  "Counts",
  "counts",
  "sortedCounts",
  "richPoints",
  "pointsWithProperties",
  "stats",  
  "pointPropertyTupleValues",
  "interpolatedIdeals",
  "bareIdeals",
  "estimateDecomposition", 
  "estimateStratification",
  "estimateCodim", 
  "createIterator",
  "createRandomPointIterator",
  "Map",
  -- "isOnComponent", böhse, symbol bereits in BlackBoxParameterSpaces. KONFLIKT
  "poissonEstimate",
  "Experiment",
  "InterpolationImage",
  "InterpolatedImage",
  "FFELogger",
  "createInterpolatedImage",
  "PointIterator",
  "RandomPointIterator",

  -- previously mutable exports
  "observedPropertyTuples",
  "observedPropertyTuple",
  "interpolateComponents",
  "getPoint",
  "reset",
  "setPosition",
  "setPoint",
  "pointData",
  "allInterpolatedIdeals",
  "setDecompositionConfidenceInterval",
  "smoothPoints",
  "experiment",
  "pointKey",
  "setPointIterator",
  "setPointGenerator",
 
  "membershipPrecision",
  "setMembershipPrecision",

  "clearWatchedProperties",
  "testDebug",
  "next",
  "begin",
  "count",
  "countinfo",    
  "collectedCount",
  "pointKeys",
  "points",
  "trials",

  "coefficientRingCardinality",
  "pointLists",
  "pointsByPropertyValues",
  "pointsByKey",

  "countData",

  "setIsInteresting",
  "isInteresting",

  "getExperimentData",
  "ignoreProperty",
  "propertyIsWatched",
  "ignoreProperties",

  "updateExperiment",
  "saveData",
  "loadData",

  "clear",
  "rankJacobianAtKey",
  "watchProperties",
  "watchProperty",
  "propertyName",
  "propertyAt",

  "tryProperty",
    
  "watchedProperties",
  "setWatchedProperties",

  "useRankJacobianAt",
  "usedRankJacobianAt",

  "pointsPerComponent",
  "setPointsPerComponent",
  "stratificationIntervalView",

  "countsByCount",

  "estimateStratification2",
  "experimentData",
  "isRandom"
  --"compatible"
}

-- The following are meant to be keys for internal use only
protect propertyList
protect propertyName
protect countData
protect experiment         --internal
protect experimentData     --internal
protect isInteresting      --internal
protect interpolatedIdeals --internal
protect isRandom           --internal
protect update              --intern

undocumented {
  --createIterator,            --document in random point iterator, later.
  createRandomPointIterator, --document in random point iterator, later.
  begin,                     --document in random point iterator, later.
  next,                      --document in random point iterator, later.
  points,                        --a list of all points not sorted into list. 
                               --not used anymore since tryProperty has
                               --been implemented
  reset,                         --iterator
  --compatible,           --internal method
  estimateCodim,      --deprecated
  getExperimentData,
  rankJacobianAtKey,      --document later, redesign
  loadData,           --IO; in development
  saveData,           --IO; in development
  savedExperimentData412398472398473923847, --IO; in development
  propertyAt,         --unnecessary/deprecated
  testDebug,
  updateExperiment,    -- newFeature not ready.
  FFELogger,            -- internal for debug.
  --watchedProperties,   -- replace with watchedProperties
  stratificationIntervalView,
  InterpolatedImage
}

exportMutable("savedExperimentData412398472398473923847");

if FiniteFieldExperiments#Options#DebuggingMode then
    errorDepth=0;

FFELogger = Logger("FiniteFieldExperiments");
FFELogger.setLogLevel(2);

ffelog = FFELogger;

Experiment = new Type of MutableHashTable;

--
-- ExperimentData is the data part of an Experiment. 
--  Experiment data (findings) and Experiment methods are separated to make storing more simple (or possible at all)
--
ExperimentData = new Type of MutableHashTable;

PointData = new Type of MutableHashTable;

new PointData from HashTable := (ancestorType, pointData)->( 
    return pointData;
);

load "FiniteFieldExperiments/Statistics.m2"
load "FiniteFieldExperiments/EstimatedStratification.m2"
load "FiniteFieldExperiments/EstimatedDecomposition.m2"
load "FiniteFieldExperiments/ExperimentData.m2"
load "FiniteFieldExperiments/PointIterator.m2"
load "FiniteFieldExperiments/Counts.m2"
load "FiniteFieldExperiments/Experiment.m2"

--load "./FiniteFieldExperiments/Interpolation.m2";

beginDocumentation()

load "FiniteFieldExperiments/DocExperiment.m2"
load "FiniteFieldExperiments/DocEstimatedDecomposition.m2"
load "FiniteFieldExperiments/DocEstimatedStratification.m2"
load "FiniteFieldExperiments/DocPointIterator.m2"

end------------------------------------------------------------------

restart
uninstallPackage "FiniteFieldExperiments"
uninstallPackage "BlackBoxParameterSpaces"
uninstallPackage "IntervalPkg"
uninstallPackage "M2Logging"
restart
installPackage "M2Logging"
installPackage "IntervalPkg"
installPackage "BlackBoxParameterSpaces"
installPackage "FiniteFieldExperiments"
check BlackBoxParameterSpaces
check FiniteFieldExperiments

loadPackage "FiniteFieldExperiments"

quit -- F11 F11 F12

viewHelp FiniteFieldExperiments
viewHelp BlackBoxParameterSpaces

restart
needsPackage "FiniteFieldExperiments"

R = (ZZ/7)[x_0..x_3]
M = matrix{
     {x_0,x_1,0},
     {x_1,x_2,x_3}
     }
I = minors(2,M)
B = blackBoxIdeal I
 
e = new Experiment from B

e.run(1, "numPointsPerComponentToCollect"=>20 ) -- gives error
e.run(1) 

e.run( 3000,  "numPointsPerComponentToCollect"=>20 ) -- gives error

pointData = e.getPointLists() -- gives error

apply(keys pointData,i->#(pointData#i))

assert (#(pointData#{2}) >= 40)

