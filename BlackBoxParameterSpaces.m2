-- XXXX TODO for 11 Feb 2021
-- reorder the export list to match location of functions in each file (at least for methods).
-- reorder the doc to match this order
-- if possible, reorder the tests too.
-- what about the debugging statements?
newPackage(
     "BlackBoxParameterSpaces",
     Version => "1.1", 
     Date => "04.02.2021",
     Authors => {{
           Name => "Jakob Kroeker", 
           Email => "jakobkroeker.academic@spaceship-earth.net", 
           HomePage => "http://www.crcg.de/wiki/User:Kroeker"},{
           Name => "Hans-Christian Graf v. Bothmer", 
           Email => "hans.christian.v.bothmer@math.uni-hamburg.de", 
           HomePage => "http://www.crcg.de/wiki/Bothmer"}    
      },
     PackageExports => {"M2Logging"},
     Headline => "black boxes for explicit and implicitly given ideals",
     DebuggingMode => true,
     CacheExampleOutput => false,
     AuxiliaryFiles => true
)

export { 
    "Error",
    "Exception",
    "SmoothnessTester",
    "JetAtCalculator",
    "setJetAtCalculator",
    "setSmoothnessTester",
    "assertEx",
    "OnComponentAnswerStrategy",
    "PlainTextSmoothnessInfoWithAnswerPair",
    "NullIfNotSmooth",
    "ExceptionIfNotSmooth",
    "SmoothnessInfoWithAnswerPair",
    "plainTextSmoothnessInfoWithAnswerPair",
    "nullIfNotSmoothStrategy",
    "exceptionIfNotSmooth",
    "smoothnessInfoWithAnswerPair",
    "refineInterpolation",
     
    "monomialBasisSize",
    "basicJetLengthHeuristic",
    "constantJetLengthHeuristic",
    "setBlackBoxLogLevel",
    --"compatible",
    "continueJet",
    "JetLengthHeuristic",
    "ConstantJetLengthHeuristic",
    "BasicJetLengthHeuristic",
    "InterpolationMonomialDegreeHeuristic",
    "BasicInterpolationMonomialDegreeHeuristic",
    "ConstantInterpolationMonomialDegreeHeuristic",
    "sameComponentAt",     
    "componentNamesAt",
    "jetStatsAt",
    --"locus",
    --"joinJetSets",
    "JetSet",
    "addElement",
    "isDerivedFrom",
    "Jet",
    "pointProperties",
    "memberMethods",
    "attributes",
    "clearCoeffDenominators",
    "BlackBoxIdeal",
    "BlackBoxParameterSpace",
    "blackBoxParameterSpace",
    "blackBoxIdeal",
    "blackBoxIdealFromEvaluation",
    "BlackBoxLogger",
    "getEpsRing",
    --"bestJetAt",
    "jetAt",
    "jetAtWithInfo",
    "isCertainlySingularAt",
    "isProbablySmoothAt",
    "keysWithoutSymbols",    
    "guessAcceptedParameterNumber",
    "dropDegreeInfo",
    "createInterpolatedComponent",
    "interpolateAt",
    "interpolate",
    "MapHelper",
    "InterpolatedComponent",
    "SingularPointException",
    "PointNotOnBlackBox",
    "createMapHelper",
    "deduceNumGenerators",
    
    "continueJetWithInfo",
    "transformedAnswer",
    "onComponentAnswerStrategies",
    "setOnComponentAnswerStrategy",
    "onComponentAnswerStrategy",
    "onComponentPrecision",
    "interpolateComponentAt",
    "interpolateComponentsAt",
    "interpolatedComponentsAt",
    "interpolatedComponentNames",
    "interpolatedComponentNamesAt",
    "interpolatedComponentByName",
    "interpolatedComponents",        
    --"reset",
    "setOnComponentPrecision",
    "resetInterpolation",
    "setInterpolator",
    "setMonomialDegreeHeristic",
    "componentsAt",
    "interpolationTargetJetLength",
    "setJetLength",
    "setAdditionalJetLength",
    "setJetLengthHeuristic",
    "setMonomialDegreeHeuristic",
    "targetMonomialDegree",
    "componentNames",
    "clearCache",
    "minComponentDegree",
    "maxInterpolationDegree",
    "maxComponentDegree",
    "blackBox",
    "dropComponent",
    "increaseInterpolationJetLength",
    "decreaseInterpolationJetLength",
    "setSameComponentPrecision",
    "setComponentNamePrefix",
    "sameComponentTargetJetLength",
    "additionalJetLength",
    "interpolator",
    "componentByName",
    "renameInterpolatedComponent",
    "renameComponent",
    "componentNameInUse",
    "componentNamesInUse",
    "jetSet",
    "setName",
    --"name",
    --"name",
    "isOnComponent",
    "isOnInterpolatedComponent",
    "enableChecks",
    "disableChecks",
    "withChecks",  
 
    "jacobianAt",
    "rankJacobianAt",
        
    "valuesAt",
        
    "unknownIsValid",                 
    "numVariables",  
    "numGenerators",
    "isZeroAt",      
    "registerPointProperty", 
    "rpp",
    "upp",
    "setPointProperty",
    "setValuesAt",    
    "checkInputPoint",
    
    "setIsZeroAt",
    
    "pointProperty",
    "updatePointProperty",
    "setJacobianAt",

    "hasPointProperty",
    "pointPropertiesAsSymbols",
    --"memberMethods",
    --"attributes",
    "NumTrials",
    "setSingularityTestOptions",
    "updateSingularityTest",
    "singularityTestOptions",
    "updateBlackBox"
}

exportMutable{
    "eps"
    }

----------------------------------------------------------------------
-- Main types in the package -----------------------------------------
----------------------------------------------------------------------
Jet = new Type of HashTable;
JetSet = new Type of MutableHashTable;
BlackBoxParameterSpace = new Type of MutableHashTable; -- a type representing a parameter space.
BlackBoxIdeal = new Type of  BlackBoxParameterSpace;
----------------------------------------------------------------------

--todo: fix duplicate code,  -  padicLiftProtect and padicLiftExport




--load "./BlackBoxParameterSpaces/Exceptions.m2";

BlackBoxLogger = Logger("BlackBoxParameterSpaces");
bblog = BlackBoxLogger; --shortcut

-- todo: how to switch this on and off by the user? using closures again? 
--if BlackBoxParameterSpaces#Options#DebuggingMode then 
--    BlackBoxLogger.setLogLevel(LogLevel.DEBUG);

if BlackBoxParameterSpaces#Options#DebuggingMode then
    errorDepth=0;

--if BlackBoxParameterSpaces#Options#DebuggingMode then bblog.setLogLevel(LogLevel.DEBUG);

setBlackBoxLogLevel = (level)->
(
    BlackBoxLogger.setLogLevel(level);
);

load "BlackBoxParameterSpaces/Utils.m2"
load "BlackBoxParameterSpaces/Jets.m2"
load "BlackBoxParameterSpaces/ParameterSpaces.m2"
load "BlackBoxParameterSpaces/Interpolation.m2";
load "BlackBoxParameterSpaces/Ideals.m2";

-- development remark: we need jetAt at least per blackbox individually.
-- todo: we have to differ between interpolator and interpolation.

load "BlackBoxParameterSpaces/Tests.m2"
    
beginDocumentation()

load "BlackBoxParameterSpaces/UtilsDoc.m2"
load "BlackBoxParameterSpaces/IdealsDoc.m2"
load "BlackBoxParameterSpaces/ParameterSpacesDoc.m2"
load "BlackBoxParameterSpaces/JetsDoc.m2"
load "BlackBoxParameterSpaces/InterpolationDoc.m2"


-- JK we have to put the undocumented statement at the end, because undocumented checks, if the 
-- functions or symbols are already defined!
--
-- the following symbols which are marked as undocumented are in fact documented 
-- inside the BlackBoxParameterSpace and BlackBoxIdeal
-- please do only mark documented symbols as undocumented, 
-- at least there should be a comment note inside this package.
-- 

-*
undocumented { 
    --(isCertainlySingularAt, BlackBoxParameterSpace,Matrix,HashTable),
    --(isCertainlySingularAt, BlackBoxParameterSpace, Matrix, ZZ, ZZ),
    setJetLengthHeuristic,
    setMonomialDegreeHeuristic,
    setName,   
    enableChecks,
    disableChecks,
    withChecks,
    (net,Jet),
    (net,JetSet),
    (net, InterpolatedComponent),
    (net, BlackBoxParameterSpace),  
    setIsZeroAt,      -- internal   
    setJacobianAt,    -- internal   
    setValuesAt,      -- internal   
    setPointProperty, -- internal   
    --(deduceNumGenerators), -- internal   
    dropDegreeInfo,      -- internal   
    updateBlackBox,  --internal    
    keysWithoutSymbols,
    checkInputPoint,
    pointPropertiesAsSymbols,
    unknownIsValid,
    NumTrials,
    pointProperty, -- internal function
    guessAcceptedParameterNumber, -- internal function
    updateSingularityTest, --internal function
    createMapHelper,
    JetLengthHeuristic,
    ConstantJetLengthHeuristic,
    constantJetLengthHeuristic,
    BasicJetLengthHeuristic,
    basicJetLengthHeuristic,
    InterpolationMonomialDegreeHeuristic,
    BasicInterpolationMonomialDegreeHeuristic,
    ConstantInterpolationMonomialDegreeHeuristic,
    setMonomialDegreeHeristic,
    interpolationTargetJetLength,
    setJetLength,
    setAdditionalJetLength,   
    targetMonomialDegree,
    clearCache,
    minComponentDegree,
    maxInterpolationDegree,
    maxComponentDegree,
    blackBox,
    increaseInterpolationJetLength,
    decreaseInterpolationJetLength,
    setSameComponentPrecision,
    setComponentNamePrefix,
    sameComponentTargetJetLength,
    additionalJetLength,
    interpolator,
    componentNameInUse,
    componentNamesInUse,
    jetSet,    
    "InterpolatedComponent ? InterpolatedComponent", -- implemented comparison by name for sorting purposes. probably not a good idea, since the order holds for the names but not for the ideals !!
    -- purpose of overriding new from Thing is to disallow arbitrary HashTables as objects
    (NewFromMethod, JetSet, Thing),
    (NewFromMethod, BlackBoxIdeal, Thing),
    (NewFromMethod, BlackBoxParameterSpace, Thing),
    (NewFromMethod, JetLengthHeuristic, Thing),
    (NewFromMethod, BasicInterpolationMonomialDegreeHeuristic, Thing),
    (NewFromMethod, BasicJetLengthHeuristic, Thing),
    (NewFromMethod, ConstantInterpolationMonomialDegreeHeuristic, Thing),
    (NewFromMethod, ConstantJetLengthHeuristic, Thing),
    (NewFromMethod, InterpolatedComponent, Thing),
    (NewFromMethod, InterpolationMonomialDegreeHeuristic, Thing),
    (NewFromMethod, MapHelper, Matrix),
    (NewFromMethod, JetAtCalculator, Thing),
    (NewFromMethod, SmoothnessTester, Thing),
    transformedAnswer,       --internal
    (createInterpolatedComponent,Ideal,ZZ,Thing,BlackBoxParameterSpace),
    (deduceNumGenerators,BlackBoxParameterSpace),      --internal
    (deduceJacobianAt, BlackBoxParameterSpace, Matrix),
    renameComponent,
    isOnComponent,
    deduceNumGenerators,
    sameComponentAt,
    assertEx,
    NullIfNotSmooth,
    ExceptionIfNotSmooth,
    SmoothnessInfoWithAnswerPair,
    PlainTextSmoothnessInfoWithAnswerPair,
    nullIfNotSmoothStrategy,
    exceptionIfNotSmooth,
    smoothnessInfoWithAnswerPair,
    plainTextSmoothnessInfoWithAnswerPair,
    (monomialBasisSize ,ZZ, ZZ , Ring),
    componentByName,
    componentNames,
    componentNamesAt,
    componentsAt,
    JetAtCalculator,
    MapHelper,
    SmoothnessTester,
    createInterpolatedComponent,
    isDerivedFrom,
    monomialBasisSize,
    setBlackBoxLogLevel,
    continueJetWithInfo,
    jetAtWithInfo,
    onComponentAnswerStrategies,
    setOnComponentAnswerStrategy,
    onComponentAnswerStrategy,
    setInterpolator,
    setJetAtCalculator,
    setSmoothnessTester
} 
*-
-- undocumented: "compatible"

end--------------------------------------------------------------

restart
uninstallPackage"BlackBoxParameterSpaces"
uninstallPackage "IntervalPkg"
uninstallPackage "M2Logging"
restart
loadPackage "BlackBoxParameterSpaces"
restart
installPackage "M2Logging"
installPackage "IntervalPkg"
installPackage"BlackBoxParameterSpaces"
check "BlackBoxParameterSpaces"
viewHelp "BlackBoxParameterSpaces"

