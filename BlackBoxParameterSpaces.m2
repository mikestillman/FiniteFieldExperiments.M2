-- Start here
-- 2 tests are failing: they try to write jetSet, componentNames as variables.
-- fix this.
newPackage(
     "BlackBoxParameterSpaces",
     Version => "1.0", 
     Date => "24.03.2017",
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

--needsPackage "SimpleDoc";
--needsPackage "Text";

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
    "deduceNumGenerators"
}

----------------------------------------------------------------------
-- Main types in the package -----------------------------------------
----------------------------------------------------------------------
Jet = new Type of HashTable;
JetSet = new Type of MutableHashTable;
BlackBoxParameterSpace = new Type of MutableHashTable; -- a type representing a parameter space.
BlackBoxIdeal = new Type of  BlackBoxParameterSpace;
----------------------------------------------------------------------

idealBlackBoxesProtect = ()->
(
    --protect eps;
    protect withChecks;
    protect enableChecks;
    protect disableChecks;
    protect jacobianAt;
    protect rankJacobianAt;
    protect valuesAt;
    protect unknownIsValid;
    protect numVariables;
    protect numGenerators;
    protect isZeroAt;
    protect registerPointProperty;
    protect rpp;
----    protect numTrials;
    protect setSingularityTestOptions;
    protect singularityTestOptions;
    protect updateSingularityTest;
    protect setPointProperty;
    protect setValuesAt;
    protect checkInputPoint;
    --protect deduceNumGenerators;
    protect setIsZeroAt;
  
    protect pointProperty;
    protect updatePointProperty;
    protect setJacobianAt;

    protect hasPointProperty;
    protect pointPropertiesAsSymbols;
    --protect memberMethods;
    --protect attributes;
    --protect knownProperties;
    --protect createMapHelper;
    protect updateBlackBox;
    protect setInterpolator;
);

--todo: fix dublicate code,  -  padicLiftProtect and padicLiftExport

idealBlackBoxesExportImmutable = ()->
(
    export("continueJetWithInfo");
    export("transformedAnswer");
    export("onComponentAnswerStrategies");
    export("setOnComponentAnswerStrategy");
    export("onComponentAnswerStrategy");
    export("onComponentPrecision");
    export("interpolateComponentAt");
    export("interpolateComponentsAt");
    export("interpolatedComponentsAt");
    export("interpolatedComponentNames");
    export("interpolatedComponentNamesAt");
    export("interpolatedComponentByName");
    export("interpolatedComponents");        
    --export("reset");
    export("setOnComponentPrecision");
    export("resetInterpolation");
    export("setInterpolator");
    export("setMonomialDegreeHeristic");
    export("componentsAt");
    export("interpolationTargetJetLength");
    export("setJetLength");
    export("setAdditionalJetLength");
    export("setJetLengthHeuristic");
    export("setMonomialDegreeHeuristic");
    export("targetMonomialDegree");
    export("componentNames");
    export("clearCache");
    export("minComponentDegree");
    export("maxInterpolationDegree");
    export("maxComponentDegree");
    export("blackBox");
    export("dropComponent");
    export("increaseInterpolationJetLength");
    export("decreaseInterpolationJetLength");
    export("setSameComponentPrecision");
    export("setComponentNamePrefix");
    export("sameComponentTargetJetLength");
    export("additionalJetLength");
    export("interpolator");
    export("componentByName"),
    export("renameInterpolatedComponent"),
    export("renameComponent"),
    export("componentNameInUse"),
    export("componentNamesInUse"),
    export("jetSet");
    export("setName");
    --export("name");
    --export("name");
    export("isOnComponent");
    export("isOnInterpolatedComponent");
    export("enableChecks");
    export("disableChecks");
    export("withChecks");  
 
    exportMutable("eps");
    export("jacobianAt");
    export("rankJacobianAt");
        
    export("valuesAt");
        
    export("unknownIsValid");                 
    export("numVariables");  
    export("numGenerators");
    export("isZeroAt");      
    export("registerPointProperty"); 
    export("rpp");
    export("upp");
    export("setPointProperty");
    export("setValuesAt");    
    export("checkInputPoint");
    
    export("setIsZeroAt");
    
    export("pointProperty");
    export("updatePointProperty");
    export("setJacobianAt");

    export("hasPointProperty");
    export("pointPropertiesAsSymbols");
    --export("memberMethods");
    --export("attributes");
    export("NumTrials");
    export("setSingularityTestOptions");
    export("updateSingularityTest");
    export("singularityTestOptions");
    export("updateBlackBox");
    
)

idealBlackBoxesExportMutable = ()->
(
    exportMutable("continueJetWithInfo");
    exportMutable("transformedAnswer");
    exportMutable("onComponentAnswerStrategies");
    exportMutable("setOnComponentAnswerStrategy");
    exportMutable("onComponentAnswerStrategy");
    exportMutable("onComponentPrecision");
    exportMutable("interpolateComponentAt");
    exportMutable("interpolateComponentsAt");
    exportMutable("interpolatedComponentsAt");
    exportMutable("interpolatedComponentNames");
    exportMutable("interpolatedComponentNamesAt");
    exportMutable("interpolatedComponentByName");
    exportMutable("interpolatedComponents");        
    --exportMutable("reset");
    exportMutable("setOnComponentPrecision");
    exportMutable("resetInterpolation");
    exportMutable("setInterpolator");
    exportMutable("setMonomialDegreeHeristic");
    exportMutable("componentsAt");
    exportMutable("interpolationTargetJetLength");
    exportMutable("setJetLength");
    exportMutable("setAdditionalJetLength");
    exportMutable("setJetLengthHeuristic");
    exportMutable("setMonomialDegreeHeuristic");
    exportMutable("targetMonomialDegree");
    exportMutable("componentNames");
    exportMutable("clearCache");
    exportMutable("minComponentDegree");
    exportMutable("maxInterpolationDegree");
    exportMutable("maxComponentDegree");
    exportMutable("blackBox");
    exportMutable("dropComponent");
    exportMutable("increaseInterpolationJetLength");
    exportMutable("decreaseInterpolationJetLength");
    exportMutable("setSameComponentPrecision");
    exportMutable("setComponentNamePrefix");
    exportMutable("sameComponentTargetJetLength");
    exportMutable("additionalJetLength");
    exportMutable("interpolator");
    exportMutable("componentByName"),
    exportMutable("renameInterpolatedComponent"),
    exportMutable("renameComponent"),
    exportMutable("componentNameInUse"),
    exportMutable("componentNamesInUse"),
    exportMutable("jetSet");
    exportMutable("setName");
    --exportMutable("name");
    --exportMutable("name");
    exportMutable("isOnComponent");
    exportMutable("isOnInterpolatedComponent");
    exportMutable("enableChecks");
    exportMutable("disableChecks");
    exportMutable("withChecks");  
 
    exportMutable("eps");
    exportMutable("jacobianAt");
    exportMutable("rankJacobianAt");
        
    exportMutable("valuesAt");
        
    exportMutable("unknownIsValid");                 
    exportMutable("numVariables");  
    exportMutable("numGenerators");
    exportMutable("isZeroAt");      
    exportMutable("registerPointProperty"); 
    exportMutable("rpp");
    exportMutable("upp");
    exportMutable("setPointProperty");
    exportMutable("setValuesAt");    
    exportMutable("checkInputPoint");
    
    exportMutable("setIsZeroAt");
    
    exportMutable("pointProperty");
    exportMutable("updatePointProperty");
    exportMutable("setJacobianAt");

    exportMutable("hasPointProperty");
    exportMutable("pointPropertiesAsSymbols");
    --exportMutable("memberMethods");
    --exportMutable("attributes");
    exportMutable("numTrials");
    exportMutable("setSingularityTestOptions");
    exportMutable("updateSingularityTest");
    exportMutable("singularityTestOptions");
    exportMutable("updateBlackBox");
    
)

load "BlackBoxParameterSpaces/Utils.m2"

--load "./BlackBoxParameterSpaces/Exceptions.m2";


-- TODO: what does this comment mean?  -- swith between protect and export - both are not possible!

--idealBlackBoxesProtect() -- protect the symbols for checking package correctness: no symbol variables should be overwritten by accident!
idealBlackBoxesExportImmutable(); -- export the symbols to make the package work 


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






-- getPropertySymbols() 
-- 
-- Returns all relevant symbols (propertySymbols) 
-- from different M2 scopes corresponding to a given method name (propertyName).
-- Background: suppose the user registered a point property with name 'PP' 
-- in a black box 'bb' and want to access it using the  '.' operator.
-- To achieve that, internally we have to add in the HashTable 'bb' an entry with 'symbol(PP)' as a key 
-- and the point property function as a value.
-- Now in Macaulay2  symbols are bound to scopes and thus in different scopes symbols with same name may coexist.
-- So it may happen that the registered key symbol 'PP' differes from the to the user visible symbol 'PP' at the top level
-- and the access 'bb.PP' leads to 'error: key not found in hash table'
-- The workaround is to register the same property at least twice using the symbol  getGlobalSymbol( 'PP' ) as a key.
-- I'm not sure, but for some situations it was also necessary to use the symbol from 'User#"private dictionary"'
-- via getGlobalSymbol( User#"private dictionary",'PP')
-- and the symbol from the package dictionary "BlackBoxParameterSpaces.Dictionary"
-- see also https://github.com/jakobkroeker/FiniteFieldExperiments.M2/issues/116
-- 
-- TODO question: what happens in case the user loads a package which defines the same symbol?

-- artificial example:  
-- restart
-- loadPackage "BlackBoxParameterSpaces"
-- fragileHT = new HashTable from {
--   getGlobalSymbol(User#"private dictionary", "valuesAt") => 5
-- }
-- fragileHT.valuesAt -- error: key not found in hash table
-- 

getPropertySymbols = method ();

getPropertySymbols(String) := List => (propertyName)->
(
    propertySymbols := {} ;
    try (  propertySymbols = propertySymbols | { getGlobalSymbol(BlackBoxParameterSpaces.Dictionary, propertyName)} );

    -- todo question: should the symbol in the users private dictionary always be created?
    try  (  propertySymbols = propertySymbols | { getGlobalSymbol propertyName} ) else 
    ( 
        propertySymbols =  propertySymbols | { getGlobalSymbol( User#"private dictionary", propertyName); }
    );
    return propertySymbols;
);


isDerivedFrom = method();

isDerivedFrom (Thing, Type) := Boolean => (testee, expectedType)->
(
    if (class testee===expectedType) then return true;
    
    parentClass := parent class testee;
    
    while (parentClass =!= Thing) do
    (
        if (parentClass===expectedType) then return true;
        parentClass = parent parentClass;
    );
    if (parentClass===expectedType) then return true;    
    return false;
)

load "BlackBoxParameterSpaces/Jets.m2"
load "BlackBoxParameterSpaces/ParameterSpaces.m2"
load "BlackBoxParameterSpaces/Interpolation.m2";
load "BlackBoxParameterSpaces/Ideals.m2";

-- XXXX
-- development remark: we need jetAt at least per blackbox individually.

-- todo: we have to differ between interpolator and interpolation.

load "BlackBoxParameterSpaces/Tests.m2"
    
beginDocumentation()

load "BlackBoxParameterSpaces/IdealsDoc.m2"
load "BlackBoxParameterSpaces/ParameterSpacesDoc.m2"
load "BlackBoxParameterSpaces/JetsDoc.m2"
load "BlackBoxParameterSpaces/InterpolationDoc.m2"

doc ///
    Key
        BlackBoxParameterSpaces
    Headline
          black boxes for implicitly given ideals
    Description
        Text
            The BlackBoxes of this Package come in two flavors:
            
            1) @TO BlackBoxParameterSpace@
            
            This represents a family of algebraic objects over
            an affine space K^n in a pointwise fashion. Together with 
            the package FiniteFieldExperiments this can be
            used to study the stratification of the  parameter
            space K^n with respect to various properties of the algebraic
            objects parametrized. For a quick start look at the
            @TO "Singularities of cubic surfaces" @-tutorial.
            
            2) @TO BlackBoxIdeal@
             
            A BlackBoxIdeal is a special BlackBoxParameterSpace.
            Here equations for a specific stratum of the
            parameter space are known at least implicitly.
            
            Together with 
            the package FiniteFieldExperiments this stratum can
            then be studied much more precisely. For example
            heuristic estimates on the number and codimension
            of its reduced components can be obtained. If one
            is lucky, even equations for the different components
            can be found. For a quick start look at the 
            @TO "Variety of Complexes" @-tutorial.

    Caveat
            The package is probably not threadsafe.
         
         
///

doc ///
    Key
        clearCoeffDenominators
        (clearCoeffDenominators, Ideal )   
    Headline
        convert an ideal with rational coefficients to an ideal with integer coefficients
    Usage   
        clearCoeffDenominators(IdealInQQ)
    Inputs  
        IdealInQQ:Ideal
             ideal with rational coefficients
    Outputs
        : Ideal
             ideal with integer coefficients with the same zero set over QQ as the input ideal
    Description
        Text
           \break  Example:  convert an ideal with coefficients in QQ to an ideal with   coefficients in ZZ
        Example          
            RQ = QQ[x];
            FQ = {1/3*x+1,1/5*x+2};        
            IFQ = ideal FQ
            IFZ = clearCoeffDenominators(IFQ)
    Caveat
        Conversion implemented only for cases where the ideal coefficient ring is QQ( or ZZ).
///

-- JK we have to put the undocumented statement at the end, because undocumented checks, if the 
-- functions or symbols are already defined!
--
-- the following symbols which are marked as undocumented are in fact documented 
-- inside the BlackBoxParameterSpace and BlackBoxIdeal
-- please do only mark documented symbols as undocumented, 
-- at least there should be a comment note inside this package.
-- 
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



end--------------------------------------------------------------

-- undocumented: "compatible"

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
check BlackBoxParameterSpaces

