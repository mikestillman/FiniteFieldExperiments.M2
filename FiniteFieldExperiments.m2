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
  "createInterpolatedImage"
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

ffelog := FFELogger;



Experiment = new Type of MutableHashTable;


load "FiniteFieldExperiments/Statistics.m2"

--load "./FiniteFieldExperiments/Interpolation.m2";

--
-- ExperimentData is the data part of an Experiment. 
--  Experiment data (findings) and Experiment methods are separated to make storing more simple (or possible at all)
--
ExperimentData = new Type of MutableHashTable;

PointData = new Type of MutableHashTable;

new PointData from HashTable := (ancestorType, pointData)->( 
    return pointData;
);




--------------------------------------------------------------------------------------------------------------
---- observedValues, realizedValues realizedProperties and pointPropertyTupleValues do the same. Choose the preferred naming!
--------------------------------------------------------------------------------------------------------------

observedPropertyTuples = method();
observedPropertyTuples (Experiment) := Thing => (experiment)->
(
    return experiment.observedPropertyTuples();
);

--------------------------------------------------------------------------------------------------------------
---- realizedValue, realizedProperty realizedPointProperty , observedPropertyTuple do the same. Choose the preferred naming 
--------------------------------------------------------------------------------------------------------------
observedPropertyTuple = method();
observedPropertyTuple (Experiment,ZZ) := Thing => (experiment,pos)->
(
    return experiment.observedPropertyTuple(pos);
);


-- return the statistics for the watched properties : as Counts (derived from Tally)
-- where the key is the realized value set of properties and the value is the number of observed occurances
counts = method(); 
counts (Experiment) := Counts => (experiment)->
(
    return experiment.counts();
);

-- counts the number of collected points. Return type is Counts (derived from Tally and Tally from HashTable)
-- where the key is the realized value set of properties and the value is the number of collected points
collectedCount = method(); 
collectedCount (Experiment) := Counts => (experiment)->
(
    return experiment.collectedCount();
);

--------------------------------------------------------------------------------------------------------------
---- realizedValue, realizedProperty realizedPointProperty do the same. Choose the preferred naming 
--------------------------------------------------------------------------------------------------------------



points = method();

points(Experiment) := Thing => (experiment)->
(
    return experiment.points();
)


-- should return a points with their properties. (a HashTable where points are the keys)
richPoints = method();
 
richPoints (Experiment) := Thing => (experiment)->
(
    return experiment.pointsWithProperties();
);

-- same as richPoints 
pointsWithProperties = richPoints;


-------------------------------------------
-- unused/deprecated:
--createPointData = (pBlackBox, point)->
--(
--    blackBox := pBlackBox;
--    p := new MutableHashTable;
--    p.point = point;
    
--    localIsCertainlySingularAt := null;
--    p.isCertainlySingularAt = ()->
--    (
--        if (localIsCertainlySingularAt=!=null) then return localIsCertainlySingularAt;
--        localIsCertainlySingularAt = blackBox.isCertainlySingularAt(point);
--        return localIsCertainlySingularAt;
--    );
--);


new ExperimentData from Ring := (E, coeffRing) -> 
(
     ffelog.debug (toString E);
     e := new MutableHashTable;
     e.coefficientRing = coeffRing;
     e.pointData = new MutableHashTable; -- (JK) unfortunate naming
    -- format: key=>{ideal, maxDegree, name} --later: data type for interpolated ideal (RichInterpolatedIdeal?)
     e.countData = new Counts;
     e.trials = 0;
     e.propertyList = {};
     e.isRandom = null;
     return e;
);
 

createExperimentData = (coeffRing,pointData,countData, trials, propertyList,isRandom) -> (
    e := new ExperimentData;
    e.coefficientRing = coeffRing;
    e.pointData = new MutableHashTable from pointData;
    e.countData = countData;
    e.trials = trials;
    e.propertyList = propertyList;
    e.isRandom = isRandom;
    return e;
)

new ExperimentData from ExperimentData := (E,ed) -> 
(
    -- print (toString E);
    e := new MutableHashTable;
    e.coefficientRing = ed.coefficientRing;
    e.pointData = copy ed.pointData;
    e.countData = copy ed.countData;
    e.trials = ed.trials;
    e.propertyList = copy ed.propertyList;
    e.isRandom = ed.isRandom;
    return e;
);

ExperimentData == ExperimentData := Boolean=>(ed1,ed2)->
(

    if ( ed1.coefficientRing === ed2.coefficientRing and
            ed1.propertyList    == ed2.propertyList and
            keys ed1.pointData      == keys ed2.pointData and    
            keys ed1.countData      == keys ed2.countData and  
            ed1.isRandom        == ed2.isRandom  and
            ed1.trials        == ed2.trials  ) then 
    (
        for key in keys ed2.pointData do
        ( 
            if not ed1.pointData#key == ed2.pointData#key then 
            return false;
        );

        -- TODO HC+M: should the following be countData instead?
        for key in keys ed2.pointData do
        ( 
            if not ed1.countData#key == ed2.countData#key then 
            return false;
        );
        return true;
    )
    else return false; 
);

--coeffRing,pointData,countData,trials,propertyList,isRandom

toExternalString(ExperimentData) := String=> (ed)->
( 
   return "createExperimentData " | "(" 
   | toExternalString ed.coefficientRing | ", \n "
   | toExternalString (new HashTable from ed.pointData) | ", \n" -- HC:introduced External
   | toExternalString ed.countData | ",\n"
   | toExternalString ed.trials | ",\n"
   | toExternalString ed.propertyList | ",\n"
   | toExternalString ed.isRandom 
   | ")" ;
)

ExperimentData + ExperimentData := ExperimentData=>(ed1,ed2)->
(

    if ( ed1.coefficientRing === ed2.coefficientRing and
         ed1.propertyList    == ed2.propertyList and
         ed1.isRandom        == ed2.isRandom  ) then 
    (
        edNew := new ExperimentData from ed1;
        
        for key in keys ed2.pointData do
        ( 
        if not edNew.pointData#?key then 
            edNew.pointData#key = copy ed2.pointData#key
        else
            edNew.pointData#key = edNew.pointData#key | copy ed2.pointData#key;
        );
        edNew.countData = ed1.countData + ed2.countData;
        edNew.trials = ed1.trials + ed2.trials;
        return edNew;
    )
    else error ("+: experiment data not compatible");                   
);


PointIterator = new Type of HashTable;
new PointIterator from Thing := (E, thing) -> 
(
    error("not impelemted");
);

RandomPointIterator = new Type of PointIterator;
new RandomPointIterator from Thing := (E, thing) -> 
(
    error("not impelemted");
);


createRandomPointIterator = method();

createRandomPointIterator (Function) := RandomPointIterator =>( weakRandomPointGenerator )->
(
    -- todo improvement: use own seek and remember initial seek to get reproducible    
    rpi := new MutableHashTable;
    randomPoint := null;

    currTrial := 0;

    rpi.next=()->
    (
        while(true) do 
        (
                randomPoint = weakRandomPointGenerator();
                currTrial = currTrial + 1;
                if randomPoint =!= null then break;
        );
        
        return true;
    );
    rpi.position = ()->
    (
        return currTrial;
    );
        
    rpi.setPosition =(trials)->
    (
        currTrial= trials;
    );
    
    -- usually only for loading from file.    
    rpi.setPoint = (point)->
    (
        randomPoint = point;
    );
    
    rpi.reset = () ->
    (
            randomPoint = null;
            currTrial = 0;
    );

    rpi.begin = ()->
    (
        -- jk why do we this???
        ri := createRandomPointIterator(weakRandomPointGenerator);
        ri.next();
        return ri;
    );

    rpi.getPoint = ()-> randomPoint;
    rpi = new HashTable from rpi;
    rpi = newClass(RandomPointIterator,rpi);
    return rpi;
)

createRandomPointIterator (Ring,ZZ) := HashTable =>( coeffRing,numVariables )->
(
    rpi := new MutableHashTable;
    randomPoint := null;

    currTrial := 0;

    rpi.next=()->
    (
        randomPoint = random( coeffRing^1, coeffRing^numVariables );         
        currTrial = currTrial + 1;

        return true;
    );

    rpi.reset = () ->
    (
        randomPoint = null;
        currTrial = 0;
    );

    rpi.position = ()->
    (
        return currTrial;
    );

    rpi.begin=()->
    (
        ri := createRandomPointIterator(coeffRing, numVariables );
        ri.next();
        return ri;
    );

    rpi.getPoint=()-> randomPoint;
    
    rpi = new HashTable from rpi;
    rpi = newClass(RandomPointIterator,rpi);
    return rpi;
);

createIterator = method();
createIterator (List) := PointIterator =>( pPoints )->
(
    pIterator := new MutableHashTable; 

    points := pPoints  ;

    point := null;

    pointCount := #points;

    currPosition := 0;

    pIterator.position = ()->
    (
        return currPosition;
    );

    pIterator.reset = () ->
    (
            point = null;
            currPosition = 0;
    );

    pIterator.next = ()->
    (
        if (currPosition+1 >= pointCount) then return false;
        point = points#currPosition;
        currPosition = currPosition + 1;
        return (true) ;
    );

    pIterator.begin = ()->
    (
        localPointIterator := createIterator( pPoints);
        localPointIterator.next();
        return localPointIterator;
    );

    pIterator.getPoint = ()-> point;
    pIterator = new HashTable from pIterator;
    pIterator = newClass(PointIterator,pIterator);
    return pIterator;
);




   --#??? fuer den Fall dass dich die Schlüssel nicht sortieren lassen.

   
-- todo: naming 'countsByCount' is still unfortunate

   -- countsByCount() sorts the statistic by Number
   
   
sortedCounts = method();
sortedCounts ( Experiment ) := (experiment)->
(
    return experiment.sortedCounts();
);


 
ringCardinality = (rng)->
(
    cardinality := null;
    if char rng ==0 then   cardinality  = infinity;
    try { cardinality = (rng).order } then () else();
    return cardinality;
);
 

new Experiment from Thing := (E, thing) -> 
(
    error("not impelemted");
)


 
 


Experiment + Experiment := Experiment => (re1, re2)->
(
    bb1 := re1.blackBoxIdeal();
    bb2 := re2.blackBoxIdeal();

    if  re1#"compatible"(re2)    then
    (
        re := copy re1;
        re.merge(re2);
        return re;
    )
    else error ("experiments not compatible");
);


watchedProperties = method();

watchedProperties (Experiment) := List => (experiment)->
(
    return experiment.watchedProperties();
);




--purpose of new Type Counts: to install sort as a method for Counts, but not for all Tally types.
--
Counts = new Type of Tally; 


--net (Counts) := Net =>(cs)->
--(
--    strcountinfo := "-- count() structure: \n-- values of watched properties" => count ";   
--    sss = stac (strcountinfo, net cs);
--    return net sss;
--)


SortableCounts = new Type of List;

new SortableCounts from Counts := (E, thing) -> 
(
    sortableCounts:= apply (keys thing, key->(key,thing#key));
    return sortableCounts;
)


net (SortableCounts) := Net =>(cs)->
(
    strcountinfo := "-- count structure: (values of watched properties) =>  count ";
    L := apply(cs, entry->  stack ( horizontalJoin(net entry#0 ," => " , net entry#1), " "));    
    L2 := stack L;
    L3 := horizontalJoin(toString class cs,"{",L2, "}");
    L4 := stack (strcountinfo, L3);
    return net L4;
)

-- sortfkt is a either sort or rsort.
-- internal method
--
sortCounts := method();
sortCounts (SortableCounts, MethodFunctionWithOptions) := SortableCounts => (countDataList, sortfkt)->
(    
    counts := new Counts from countDataList;
    
    --print (toString counts);
    prerearrangedData := new MutableHashTable;
    
    -- 1. use the count number as key and the values of corresponding watched properties as values.
    for key in keys counts do
    (
        if not  prerearrangedData#?(counts#key) then 
            prerearrangedData#(counts#key)= { key }
        else
        (
            prerearrangedData#(counts#key)=  prerearrangedData#(counts#key) | { key };
        );
    );
    -- 2. now create a list of the point count and sort it
    ---- bug fixed (unique missing) todo: test for this bug!
    toSort := unique apply( keys counts, key -> (counts#key));
    sorted := sortfkt (toSort); 
    rearrangedData := {};

    -- 3.  asseble the result
    for count in sorted do
    (
        for entry in prerearrangedData#count do
        --rearrangedData = rearrangedData | {(count,entry)};
        rearrangedData = rearrangedData | {(entry,count)};
    );
    --print (toString rearrangedData);
    --return new List from rearrangedData;
    return new SortableCounts from rearrangedData;
);

sort (SortableCounts) := SortableCounts =>opts-> (countData)->
(
    return sortCounts(countData, sort);
);

rsort (SortableCounts) := SortableCounts =>opts-> (countData)->
(
    return sortCounts(countData, rsort);
);

-- sorts experiment statistics by count ascending
sort (Counts) := Counts =>opts-> (countData)->
(
    --  a HashTable is not sortable; => converting counts to a list of pairs (realizedpropertyValueTuple, count) 
    countDataList := new SortableCounts from countData;
    return sortCounts(countDataList, sort);
);

-- sorts experiment statistics by count descending

rsort (Counts) := Counts =>opts-> (countData)->
( 
    --  a HashTable is not sortable; => converting counts to a list of pairs (realizedpropertyValueTuple, count) 
    countDataList := new SortableCounts from countData;
    return sortCounts(countDataList, rsort);
);

new Experiment from BlackBoxParameterSpace := (E, pBlackBox) -> 
(
    blackBoxIdeal := pBlackBox;    --black box ideal or black box parameter space so far
    experimentData := new ExperimentData from blackBoxIdeal.coefficientRing;

    coefficientRingCardinality := ringCardinality( blackBoxIdeal.coefficientRing );

    experiment := new MutableHashTable;
    experiment = newClass( Experiment, experiment );

    -- todo: issue about naming here. blackBox (what?)
    --experiment.bbi = blackBoxIdeal; -- is this safe?
    --experiment.blackBoxIdeal = blackBoxIdeal;


    -- todo: maype return a (deep) copy 
    experiment.getExperimentData = ()->
    (
        ffelog.warning("-- warning : you got a reference to experiment data, do not modify! ");
        return experimentData;
    );

    -- some of the  following values initialized at end 
    -- ( e.g. propertiesAt initialization depends presence of some functions defined later)
    pointsPerComponent := 10;

    rankJacobianAtKey := null;
    rankJacobianAt := null;

    propertiesAt := (point)->{};
    isInteresting := (point)->true;

    pointIterator := createRandomPointIterator ( experimentData.coefficientRing, blackBoxIdeal.numVariables );

    experiment.experimentData=()->
    (
        return new ExperimentData  from experimentData;
    );

    experiment.blackBoxIdeal=()->
    ( 
        return blackBoxIdeal;
    );
    
    experiment.interpolateComponents = method();
    experiment.interpolateComponents (ZZ ) := List=>(maxDeg)->
    (
        return blackBoxIdeal.interpolateComponentsAt(experiment.points(), maxDeg);
    );
    
    
    decompositionConfidenceInterval := 1.96; -- value times sigma (measured in sigma)
    
    -- later the decompositionEstimator should be public and modifiable 
    -- (provide a method to modify the confidence parameter)    
    --decompositionEstimator := createDecompositionEstimator(experiment,"confidence"=>1.96);

    experiment.estimateDecomposition = () -> 
    (
        estimateDecomposition(experiment, "confidence"=> decompositionConfidenceInterval)
    );
    
    --experiment.estimateDecomposition = () -> (decompositionEstimator.estimateDecomposition(experiment));

    -- (jk) later setDecompositionConfidenceInterval must be dropped, because we never may know the config parameters for   
    -- a custom decomposition parameter (maybe provided by the user => learn the plugin interface!)
    -- the confidence interval is measured in sigma (not in percent)
    --
    experiment.setDecompositionConfidenceInterval = (confidenceInterval)->
    (
        decompositionConfidenceInterval = confidenceInterval;
    );

    experiment.estimateStratification = () -> (estimateStratification(experiment));

--   experiment.estimateStratification2 = () -> ( estimateStratification2(experiment) );

    experiment#"compatible" = method();
    experiment#"compatible" (Experiment) := Boolean =>(re2)->
    (
        bb1 := experiment.blackBoxIdeal();
        bb2 := re2.blackBoxIdeal();

     return (  experiment.pointsPerComponent() ==  re2.pointsPerComponent() and
               experiment.coefficientRing()       === re2.coefficientRing() and
               experiment.watchedProperties()     ==  re2.watchedProperties() and
               experiment.rankJacobianAtKey()         ==  re2.rankJacobianAtKey() and
               --experiment.isInteresting           ===  re2.isInteresting -- um sicher zu gehen, kann man  alle punkte in re2 durch isInteresting von e jagen und umgekehrt.
               experiment.pointKeys()             ==  re2.pointKeys() and

              bb1.numVariables            ==   bb2.numVariables and
              bb1.numGenerators           ==   bb2.numGenerators and
              bb1.coefficientRing         ===  bb2.coefficientRing 
             );
    );


    experiment.merge = method();

    -- todo: how to prevent from self-merging?
    experiment.merge (Experiment)  := Experiment => (re)->
    (
        if (experimentData==re.experimentData() ) then 
            error ("attempt to merge with itself");

        if   experiment#"compatible"(re) then 
        experimentData = experimentData + re.experimentData()
        else
        error ("experiments not compatible!");
    );
    
    experiment.stratificationIntervalView = ()->
    (
            stratificationData := experiment.estimateStratification();
            return stratificationIntervalView(stratificationData);
    );

    experiment.coefficientRing = ()->
    (
        
        return experimentData.coefficientRing;
    );
    
    -- todo: rename to sortedCounts or similar
    experiment.countsByCount = ()->
    (   
        --print ("--warning countsByCount deprecated. Use 'sort counts experiment' or 'sort experiment.counts()' or  experiment.sortedCounts()");
        return sort new SortableCounts from experimentData.countData ; 
    );
    
    -- same as countsByCount
    experiment.sortedCounts = ()->
    (           
        return sort  experimentData.countData ; 
    );



    experiment.coefficientRingCardinality=()->
    (
        -- return    ringCardinality( blackBoxIdeal.coefficientRing );
        return    coefficientRingCardinality;
    );

    
    experiment.testDebug=()->
    (
        a:=5;
        1/0;
        return a;
    );

        
    experiment.isInteresting=(point)->
    ( 
        return isInteresting(point);
    );

    experiment.points = ()->
    (
        return flatten  apply (keys experimentData.pointData, propertiesAsKey -> experimentData.pointData#propertiesAsKey);
    );
    
  

    experiment.smoothPoints = (precision, trials)->
    (
        plist :=  experiment.points();
        smothPoints :=  {};
        jet := null;
        for point in plist do
        (
                jetOrException := catch jetAt(blackBoxIdeal,point,precision,trials);
                if isDerivedFrom(jetOrException, Jet) then
                (
                    smothPoints = smothPoints | {point};
                );
                
        );
        return smothPoints;
    );



    runExperimentOnce := method();


    experiment.setPointsPerComponent = (numPointsPerComponent)->
    ( 
        pointsPerComponent = numPointsPerComponent;
    );


    experiment.pointsPerComponent = ()->
    ( 
        return pointsPerComponent ;
    );
    

    


    experiment.position =(property)->
    (
        return position( experiment.watchedProperties(), watchedProperty->watchedProperty == property );
    );




    experiment.clear = ()->
    ( 
        pointIterator.reset();
        --  later  call experimentDataClear() instead.
        experimentData.trials = 0;
        experimentData.pointData = new MutableHashTable;
        experimentData.countData = new Counts;
    );
    
    experiment.rankJacobianAtKey = ()->
    (
        return rankJacobianAtKey;
    );

    experiment.usedRankJacobianAt = ()->
    (
        return rankJacobianAtKey;
    );

    -- question: why the hell we pass the number of wanted points per component and do not acces the 
    -- experiment => to see the dependency
    --
    runExperimentOnce(ExperimentData, Matrix,ZZ ) := Tally => (experimentData, point, wantedPointsPerComponent) -> 
    (  
        K := experimentData.coefficientRing;
        --prime = char K
        numVariables := blackBoxIdeal.numVariables;
        wantedPoints := wantedPointsPerComponent;

        -- if ideal vanishes on the random point do something
        if experiment.isInteresting(point) then   
        (
            valuesTuple := propertiesAt(point); -- todo: valuesTuple: better naming?
 
            -- countData number of found points for each rank and property
            experimentData.countData = experimentData.countData + new Counts from tally {valuesTuple};
          
            if  rankJacobianAt =!= null then 
            (
                FFELogger.debug( "update wanted points" );
                rankJacobian := rankJacobianAt(point);  
                upperEstimate := (estimateNumberOfComponents(experiment,valuesTuple)).max;
                ffelog.debug ("upper estimate number of components:  " | toString upperEstimate );
                --upperEstimate := 1; -- test

                wantedPoints = max(1,upperEstimate)*wantedPointsPerComponent;
            );            

            -- remember some points
            if experimentData.pointData#?(valuesTuple) then 
            (
                -- collect a fixed number of points per estimated component
                -- use upper limit of estimation for this             
                if #(experimentData.pointData#valuesTuple) < wantedPoints then 
                (
                    FFELogger.debug( "attaching point" );
                    experimentData.pointData#valuesTuple = experimentData.pointData#valuesTuple | {point};
                );
            )
            else (
                FFELogger.debug( "attaching first point for some valuesTuple key");
                experimentData.pointData#valuesTuple = {point};
            );
        );
        -- this trial is counted in runExperiments and not here to allow update() without changing trial num.
    );

   
    experiment.setPointIterator  = (pRpi)->
    (
        if experiment.trials()=!=0 then 
            error ("cannot change point iterator - experiment was already run! You could call clear() and retry.");
        pointIterator = pRpi;
    );

    experiment.setPointGenerator  = (pGen) ->
    (
        wrip := createRandomPointIterator(pGen);
        experiment.setPointIterator(wrip);
    );

    experiment.trials = ()-> pointIterator.position();

    runExperiment := method();
    runExperiment(ExperimentData, Thing, ZZ) := Tally => (experimentData, pPointIterator, newTrials) -> 
    ( 
        for i in 1..newTrials do
        (
            assert( pPointIterator.next() );
            runExperimentOnce( experimentData, pPointIterator.getPoint(), pointsPerComponent );
            experimentData.trials =  experiment.trials();
        );
    );


    positionsOfProperties := (newWatchedList)->
    ( 
        return  apply( newWatchedList, 
                        targetProperty -> position( experiment.watchedProperties() , (prop)->prop==targetProperty)
            );
    );

    updateWatchedListIsProjection := (newWatchedList)->
    (
        propertyPositions :=  positionsOfProperties(newWatchedList);

        return( 0 == #(select(propertyPositions, (pos)->pos===null)) ); 
    );

    projectionUpdate :=  ( newWatchedList) -> 
    (     
        -- assume: new watched property list is projection of old one.

        -- 1. get the tuple of positions of new watchedProperties in propertyList.
        propertyPositions :=  positionsOfProperties(newWatchedList);


        assert( 0 == #(select(propertyPositions, (pos)->pos===null)) );        

        --    experimentData.propertyList 

        newCountData := new MutableHashTable;

        newKey := null;

        -- sum up counts
        for key in keys experimentData.countData do
        (
            newKey = apply( propertyPositions, pos-> key#pos);
            if (not newCountData#?newKey) then  (    newCountData#newKey =   experimentData.countData#key     )
                                            else (    newCountData#newKey =   newCountData#newKey + experimentData.countData#key     );

        );
        -- reclassify collected points

        newPoints := new MutableHashTable;

        for key in keys experimentData.pointData do
        (
            newKey = apply( propertyPositions, pos-> key#pos);
            if (not newPoints#?newKey)    then  (    newPoints#newKey =   experimentData.pointData#key     )
                                            else (    newPoints#newKey =   newCountData#newKey |  experimentData.pointData#key     );

        );

        experimentData.countData = new Counts from new Tally from newCountData;
        experimentData.pointData    = newPoints;
        
        --experimentData.propertyList = newWatchedList;
    );

    
    catchedPointProperty := (propertyName, point)->
    (
        try
        (
            resultOrException := catch (blackBoxIdeal.pointProperty(propertyName))(point);
            if isDerivedFrom(resultOrException, Exception) then
            (
                return class resultOrException;
            )
            else
            (
                return resultOrException;
            );
        )
        then
        (
        )
        else
        (
            return Error;
        );        
    );

    setWatchedPropertiesInternal := (propListToObserve)->
    ( 
        for propertyName in propListToObserve do
        (
            if not blackBoxIdeal.hasPointProperty(propertyName) then 
                error ("blackBoxIdeal seems not to have property" | propertyName );
        );
        experimentData.propertyList=propListToObserve;
        propertiesAt = (point)->
        ( 
            --apply( experimentData.propertyList, propertyName->( (blackBoxIdeal.pointProperty(propertyName))(point) ) )
            apply( experimentData.propertyList, propertyName->( catchedPointProperty(propertyName,point) ) )  
        );   
    );

    experiment.useRankJacobianAt = (rankJacobianAtName)->
    ( 
        if experiment.trials()=!=0 then 
            error ("cannot change rankJacobianAt  - experiment was already run! You could clear() the statistics and retry. ");

        if rankJacobianAtName===null then
        (
            rankJacobianAtKey = null;
            rankJacobianAt = null ;
            return;
        );

        if (not blackBoxIdeal.hasPointProperty(rankJacobianAtName)) then 
            error ("blackBoxIdeal seems not to have property " | rankJacobianAtName );
    
        if (rankJacobianAtKey=!=null) then
        (
            if (experiment.propertyIsWatched(rankJacobianAtKey)) then
            (
                experimentData.propertyList = delete(rankJacobianAtKey, experimentData.propertyList ) ;
                setWatchedPropertiesInternal( experimentData.propertyList );   
            );
        );

        rankJacobianAtKey = rankJacobianAtName;
        rankJacobianAt = blackBoxIdeal.pointProperty(rankJacobianAtName) ;
        experiment.watchProperty(rankJacobianAtKey);
    
    );



    experiment.clearWatchedProperties = (   )->
    (
        experiment.clear(); 
        setWatchedPropertiesInternal({});
    );

    experiment.reset = (   )->
    (
        --redundant: experiment.clear(); 
        experiment.clear(); 
        clearWatchedProperties();
    );

    --newPropertyListIsProjection = (newPL)->
    --(
    --     
    --);

    experiment.setWatchedProperties = ( propertyStringList )->
    (  

        if ( updateWatchedListIsProjection(propertyStringList) ) then 
        (
            projectionUpdate(propertyStringList);
            setWatchedPropertiesInternal(propertyStringList);
        )
        else
        (

            if experiment.trials()=!=0 then error ("cannot change watched properties - experiment was already run! Clear statistics and retry.");

            -- improvement: allow in case the new properties is a projection
            setWatchedPropertiesInternal(propertyStringList);
        );
    );

    UpdateWatchedPropertiesError := "cannot change watched properties - experiment was already run! You could clear() the statistics and retry.";
  

    experiment.propertyIsWatched = method();
    experiment.propertyIsWatched (String) := Boolean => (propertyName)->
    (
        if ( #(select( experiment.watchedProperties(), (prop)->propertyName==prop)) >0 ) then
        ( 
            return true;
        );    
        return false;
    );

    watchProperty := method();
    watchProperty ( String ) := List => (propertyName)->
    (
        if not blackBoxIdeal.hasPointProperty(propertyName) then 
            error ("blackBoxIdeal seems not to have property" | propertyName );

        if (not experiment.propertyIsWatched(propertyName)) then
        (
            if (experiment.trials()=!=0 ) then error (UpdateWatchedPropertiesError);
        );

        experimentData.propertyList = unique (experimentData.propertyList | { propertyName }) ;
        setWatchedPropertiesInternal( experimentData.propertyList );   
      
    );
    
    watchProperty ( Function ) := List => (propertyMethod)->
    (
         watchProperty(toString propertyMethod);
    );
    
    experiment.watchProperty = method();
    experiment.watchProperty ( String ) := List => (propertyName)->
    (
        watchProperty(propertyName);
        return experiment.watchedProperties();
    );
    
    experiment.watchProperty ( Function ) := List => (propertyFunction)->
    (
        return experiment.watchProperty(toString propertyFunction);
    );

    experiment.watchProperties = (propertyNameList)->
    (
        for propertyName in propertyNameList do
        (
            watchProperty(propertyName);
        );
        return experiment.watchedProperties();
    );

    

    assertPropertyIsWatched := (propertyName) ->
    (
        if (not experiment.propertyIsWatched(propertyName)) then 
            error ("given property '" |propertyName| "' is not watched !");
    );

    assertIgnorePropertyAllowed := (propertyName) ->
    (
        if (propertyName == "rankJacobianAt") then
        (
            if (class blackBoxIdeal===BlackBoxIdeal) then
            (
                error (" removing 'rankJacobianAt' from watched properties for a " | toString class blackBoxIdeal | " not allowed !")
            );
        );
    );


    experiment.ignoreProperty = (propertyName)->
    (
        
        assertPropertyIsWatched(propertyName);
        assertIgnorePropertyAllowed(propertyName);

    
        newPropertyList := experimentData.propertyList ;
    
        newPropertyList = delete(propertyName, newPropertyList ) ;
    
        projectionUpdate( newPropertyList );

        setWatchedPropertiesInternal( newPropertyList );   
    );


    experiment.ignoreProperties = (ignorePropertyStringList)->
    (
        for propertyName in ignorePropertyStringList do
        (
            assertPropertyIsWatched(propertyName);
            assertIgnorePropertyAllowed(propertyName);
        );

        newPropertyList := experimentData.propertyList ;
        apply( ignorePropertyStringList, propToIgnore-> ( newPropertyList= delete(propToIgnore, newPropertyList ); ));
        projectionUpdate( newPropertyList );
        setWatchedPropertiesInternal(newPropertyList);    
    );

 
    experiment.interpolateComponents = (maxDeg, onComponentPrecision)->
    (
            blackBoxIdeal.interpolateComponents( experiment.smoothPoints(10,10), maxDeg, onComponentPrecision);
    );


    experiment.watchedProperties =  ()->
    (
        return experimentData.propertyList;
    );

    
    experiment.setIsInteresting = (pIsInteresting)->
    (  
       if experiment.trials()=!=0 then 
            error ("cannot change isInteresting - experiment was already run! You could call clear() and retry.");
        if ( pIsInteresting=!=isInteresting ) then
        (
            isInteresting = pIsInteresting;
        );
    );

    ---- syntax for the moment too hard (method without parameters)
    --experiment.runExperimentOnce = method(Options => (options runExperimentOnce));
    -- experiment.runExperimentOnce() := Thing => opts->()->
    --(
    --   return runExperimentOnce(experimentData);
    --);  
  

    experiment.run = method();
    experiment.run(ZZ) := Thing => (newTrials)->
    (
        runExperiment( experimentData, pointIterator, newTrials );
        return sort experiment.counts();
    );
    
    --
    -- returns a hashtable with 
    -- 
    experiment.pointLists = ()->
    (
        return new HashTable from experimentData.pointData;
    );

    experiment.pointsByKey = (key)->
    (
        return  experiment.pointsByPropertyValues(key);
    );
    
    experiment.pointsByPropertyValues = (key)->
    (
        if not (experimentData.pointData)#?key then 
            error "invalid key";
        return  (experimentData.pointData)#key;
    );
    
      -- returns a HashTable with watched BlackBoxIdeal properties as keys and corresponding occured count as values.
    -- naming is unfortunate
    experiment.counts = ()->
    (
        return new Counts from experimentData.countData;
    );    
    
    -- count the number of collected points!
    experiment.collectedCount = ()->
    (
        return new Counts from apply( experiment.pointKeys(), key->( key=> #(experimentData.pointData)#key ) );
    );
    
    -- or observedPointPropertySetValues, observedPropertyTupleValues
    experiment.observedPropertyTuples = ()->
    (
        return keys experimentData.pointData;
    );


    -- deprecated
    experiment.pointKeys = ()->
    (
        print("--warning: .pointKeys() is deprecated. use .realizedProperties()");
        return experiment.observedPropertyTuples();
    );
    
    -- (JK) we do not need this one.
    experiment.observedPropertyTuple = method();
    experiment.observedPropertyTuple (ZZ) := Thing =>(index)->
    (
        propertyvalues := experiment.observedPropertyTuples();
        if ( (index<0) or (index>= #propertyvalues) ) then error "invalid point position ";
        return propertyvalues#index;
    );
    
    -- (JK) we do not need this one.
    experiment.pointKey = method();
    experiment.pointKey(ZZ) := Thing => (index)->
    (
       print("--warning: .pointKey() is deprecated. use .realizedProperty()");
       return experiment.observedPropertyTuple(index); 
    );
    
    

   --init part:

        -- todo: test if changing blackBoxIdeal.rankJacobianAt is transparent
        -- (means after blackBoxIdeal.updatePointProperty("rankJacobianAt") the new one is called)
 
        experiment.watchProperties( {} );

        if  blackBoxIdeal.hasPointProperty("isZeroAt") then
        (
            --print("isInteresting"| toString  (blackBoxIdeal.isZeroAt ));
            isInteresting = blackBoxIdeal.pointProperty("isZeroAt");

            --     experiment.watchProperties( {"isZeroAt"} );
        );

        if blackBoxIdeal.hasPointProperty("jacobianAt") then 
        (
            assert ( blackBoxIdeal.hasPointProperty("rankJacobianAt") );
        );

        if blackBoxIdeal.hasPointProperty("rankJacobianAt") then 
        (
            assert ( blackBoxIdeal.hasPointProperty("rankJacobianAt") );
            rankJacobianAtKey = "rankJacobianAt";
            rankJacobianAt =  blackBoxIdeal.pointProperty("rankJacobianAt");


            experiment.watchProperties( {"rankJacobianAt"} );

        );



    --end init:

    experiment.tryProperty = (tProp) -> 
    (
        ffelog.info("-- ( " | toString experiment.watchedProperties() |" | " | tProp | " ) => count " );
        pointListsCopy := experiment.pointLists();
        tally flatten apply(keys pointListsCopy, --keys are the value tuples of the watched properties,
                       valueTuple->apply(pointListsCopy#valueTuple, -- for a value tuple set we have a list of stored points.
                                     point->( valueTuple, (blackBoxIdeal.pointProperty(tProp))(point))
                                 ) 
                 )
    );
   
    experiment.saveData  = (filename) ->
    (
        f := openOut(filename);        
        --maybe not that smart...
        externalString := toExternalString experiment.experimentData(); 
        --also not that smart...
        f << "savedExperimentData412398472398473923847 = " << externalString;
        f << flush;
        f << close;
    );

    experiment.loadData = (filename) ->
    (
        load filename;
        experimentData = savedExperimentData412398472398473923847;
        apply( keys experimentData.pointData, key-> 
                                        ( (experimentData.pointData)#key = apply( (experimentData.pointData)#key , point->
                                                                                sub(point, experimentData.coefficientRing )
                                                                            )
                                        )
                );
    );
    
 
 

 
    ffelog.debug ("type of internal experiment variable is : " | toString class experiment );
    return experiment; 
    -- return new HashTable from experiment; 
);



-- tryProperty = (experiment, property) ->(
--     pointLists := experiment.pointLists();
--     apply(
--           apply((keys pointLists),key -> (key,tally apply(pointLists#key,property))),
--           i->print (net i#0|" => "|net i#1)
--           )
--     )

beginDocumentation()

load "FiniteFieldExperiments/ExperimentsDoc.m2"


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
---

--update := method();
--update(ExperimentData) := Tally => opts -> (experimentData) -> 
--( 
--   pointIterator := createIterator (  experiment.points() );
--   experimentData.pointData = new MutableHashTable;
--   experimentData.countData = new Tally;

--   while ( pointIterator.next() ) do
--   (
--       runExperimentOnce( experimentData, pointIterator.getPoint(), pointsPerComponent );
--   );
--);


-- deprecated
estimateStratification2 = (e) -> 
(
    --count := e.countData();
    trials := e.trials();
    orderK := (e.coefficientRing()).order; -- this must be read from the experimentdata
    -- print "--";
    -- apply(e.countsByCount(),i->(
    --       --print (net((log(trials)-log(i#0))/log(charK))|" <= "|net(i#1)));
    --       print (net(round(1,(log(trials)-log(i#0))/log(orderK)))|" <= "|net(i#1)))
    --       )
    --  ;
    estimate := flatten apply(e.countsByCount(),k->( round1withZeros((log(trials)-log(k#0))/log(orderK)), (k#1)) );
    return  createEstimatedStratification(e.blackBoxIdeal(), estimate, e.watchedProperties() );
)



estimateDecompositionOld := (experiment) -> 
(
    countData := experiment.counts();
    posRankJacobianAt := experiment.position(  experiment.usedRankJacobianAt() );
    if posRankJacobianAt === null then error("To estimate the decomposition, \"rankJacobianAt\" must be watched");

    cardinality := experiment.coefficientRingCardinality();

    print( "(estimated codim, estimated number of components [confidence interval] <= {watched Properties})");
    print "";
    apply(sort apply(keys countData,key->
        (net(
            key#posRankJacobianAt,
            estimateNumberOfComponents(
            experiment.trials(),
            key#posRankJacobianAt,
            countData#key,
            cardinality ) )
        ) |" <= " |net key ),
    print);
);
-- todo: updateExperiment?

    -- maybe think about writing 'connectProperty' ... (propName, bb.propName); default is 1:1.
    -- then, what should happen if a user requests to watch property xy ?
    

quit -- F11 F11 F12

path = append(path,"/Users/bothmer/Desktop/projekte/strudel/Jakob2010/GitHub/padicLiftM2/")

uninstallPackage"FiniteFieldExperiments"
time installPackage"FiniteFieldExperiments"

viewHelp FiniteFieldExperiments
viewHelp BlackBoxParameterSpaces

restart

loadPackage"BlackBoxParameterSpaces"
load "FiniteFieldExperiments.m2"
needsPackage"FiniteFieldExperiments"

R = (ZZ/7)[x_0..x_3]
M = matrix{
     {x_0,x_1,0},
     {x_1,x_2,x_3}
     }
I = minors(2,M)
B = blackBoxIdeal I
 
e = new Experiment from B

e.run(1, "numPointsPerComponentToCollect"=>20 ) 
e.run(1) 

e.run( 3000,  "numPointsPerComponentToCollect"=>20 )

pointData = e.getPointLists()

apply(keys pointData,i->#(pointData#i))

assert (#(pointData#{2}) >= 40)

