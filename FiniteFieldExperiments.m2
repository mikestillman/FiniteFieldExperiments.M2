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
     Configuration => {},
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
  "estimateNumberOfComponents",              
  "createIterator",
  "createRandomPointIterator",
  "Map",
  -- "isOnComponent", böhse, symbol bereits in BlackBoxParameterSpaces. KONFLIKT
  "poissonEstimate",
  "Experiment",
  "InterpolationImage",
  "InterpolatedImage",
  "FFELogger",
  "ringCardinality",
  "createInterpolatedImage",
  "PointIterator",
  "RandomPointIterator"
}

FiniteFieldExperimentsProtect = ()->
(
    protect setPosition;
    protect setPoint;
    protect pointData;
    protect setDecompositionConfidenceInterval;
    protect allInterpolatedIdeals;
    protect experiment;
    protect pointKey;
    protect setPointIterator;
    protect setPointGenerator;
 
    protect clearWatchedProperties;
    protect testDebug;
    protect next;
    protect begin;
    protect count;
    protect countinfo;
    --protect point;
    --protect collectedCount;
    protect pointKeys; 
    --protect points;
    protect trials;
    protect smoothPoints;
  
 

    protect coefficientRingCardinality;
    protect pointLists;
    protect pointsByPropertyValues;
    protect pointsByKey;
    protect countData;
    protect setIsInteresting;
    protect isInteresting;
     

    protect getExperimentData;
    protect ignoreProperty;
    protect propertyIsWatched;
    protect ignoreProperties;

    protect update;
    protect updateExperiment;

    protect saveData;
    protect loadData;

    protect propertyList;
    protect clear;
    protect rankJacobianAtKey;
    protect watchProperty;
    protect watchProperties;
    protect setWatchedProperties;

    protect propertyName;
    protect propertyAt;

    protect tryProperty;

   
    --protect watchedProperties;

    protect useRankJacobianAt;
    protect usedRankJacobianAt;


    protect pointsPerComponent;
    protect setPointsPerComponent;
    protect stratificationIntervalView;
    protect countsByCount;  --protect countsByCount;

    protect experimentData; 
    protect isRandom;
    --protect compatible;
    protect membershipPrecision;
    protect setMembershipPrecision;
    --protect createMapHelper;
);

 

FiniteFieldExperimentsExport  = ()->
(
    exportMutable("observedPropertyTuples");
    exportMutable("observedPropertyTuple");
    exportMutable("interpolateComponents");
    exportMutable("getPoint");
    exportMutable("reset");
    exportMutable("setPosition");
    exportMutable("setPoint");
    exportMutable("pointData");
    exportMutable("allInterpolatedIdeals");
    exportMutable ("setDecompositionConfidenceInterval");
    exportMutable("smoothPoints");
    exportMutable("experiment");
    exportMutable("pointKey");
    exportMutable("setPointIterator");
    exportMutable("setPointGenerator");
 
    exportMutable("membershipPrecision");
    exportMutable("setMembershipPrecision");

    exportMutable("clearWatchedProperties");
    exportMutable("testDebug");
    exportMutable("next");
    exportMutable("begin");
    exportMutable("count");
    exportMutable("countinfo");    
    exportMutable("collectedCount");
    exportMutable("pointKeys");
    exportMutable("points");
    exportMutable("trials");
 
   
 

    exportMutable("coefficientRingCardinality");
    exportMutable("pointLists");
    exportMutable("pointsByPropertyValues");
    exportMutable("pointsByKey");

    exportMutable("countData");

    exportMutable("setIsInteresting");
    exportMutable("isInteresting");

    exportMutable("getExperimentData");
    exportMutable("ignoreProperty");
    exportMutable("propertyIsWatched");
    exportMutable("ignoreProperties");


    exportMutable("update");
    exportMutable("updateExperiment");
    exportMutable("saveData");
    exportMutable("loadData");

    exportMutable("propertyList");
    exportMutable("clear");
    exportMutable("rankJacobianAtKey");
    exportMutable("watchProperties");
    exportMutable("watchProperty");
    exportMutable("propertyName");
    exportMutable("propertyAt");

    exportMutable("tryProperty");

    
    exportMutable("watchedProperties");
    exportMutable("setWatchedProperties");

    exportMutable("useRankJacobianAt");
    exportMutable("usedRankJacobianAt");


    exportMutable("pointsPerComponent");
    exportMutable("setPointsPerComponent");
    exportMutable("stratificationIntervalView");


    exportMutable("countsByCount");

    exportMutable("estimateStratification2");
    exportMutable("experimentData");
    exportMutable("isRandom");
    --exportMutable("compatible");

    exportMutable("createExperimentData");
);


undocumented {
propertyList,              --internal variable
propertyName,              --internal variable
countData,                 --internal variable
createExperimentData,      --internal, only used for IO
--createIterator,            --document in random point iterator, later.
createRandomPointIterator, --document in random point iterator, later.
begin,                     --document in random point iterator, later.
next,                      --document in random point iterator, later.
points,                        --a list of all points not sorted into list. 
                               --not used anymore since tryProperty has
                               --been implemented
reset,                         --iterator
--compatible,           --internal method

estimateNumberOfComponents,    --internal
estimateCodim,      --deprecated
ringCardinality,    --internal
experiment,         --internal
experimentData,     --internal
getExperimentData,
isInteresting,      --internal
interpolatedIdeals, --internal
isRandom,           --internal
rankJacobianAtKey,      --document later, redesign
loadData,           --IO; in development
saveData,           --IO; in development
savedExperimentData412398472398473923847, --IO; in development
propertyAt,         --unnecessary/deprecated
testDebug,
update,              --intern
updateExperiment,    -- newFeature not ready.
FFELogger,            -- internal for debug.
--watchedProperties,   -- replace with watchedProperties
stratificationIntervalView,
InterpolatedImage
}

exportMutable("savedExperimentData412398472398473923847");

FiniteFieldExperimentsExport();

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

