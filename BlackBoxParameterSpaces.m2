-- start at XXXX
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

userDictHasKey = (key)->
(
    keyStr := toString key;
    userKeyList := apply(keys User#"private dictionary", b->toString b);
    pos := position(userKeyList, (v)->v==keyStr);
    return (pos =!= null);
);
 
userDictKey = (key)->
(
    keyStr := toString key;
    userKeyList := apply(keys User#"private dictionary", b->toString b);
    pos := position(userKeyList, (v)->v==keyStr);
    return (keys User#"private dictionary")#pos;
);


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
    protect numTrials;
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

idealBlackBoxesExport = ()->
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

assertEx = method();
assertEx (Boolean, String) := Nothing=>(condition)->
(
    assert(condition);
    return null;
)
assertEx (Boolean, String) := Nothing => (condition, errorMessage)->
(
    if (not condition) then error (errorMessage);
    return null;
)
TEST ///
    -- test assertEx
  
    assertEx(true, "");
  
    try( assertEx(false,"errorMsg") ) then 
    (
        assert(false) 
    )    else
    (
        print "test assertEx: error triggered as expected."
    )


///



--load "./BlackBoxParameterSpaces/Exceptions.m2";
------------------------------------------------
-- EXCEPTIONS
------------------------------------------------
Error = new Type of HashTable;
Exception = new Type of HashTable;
SingularPointException = new Type of Exception;


singularPointException = ()->
(
    return new SingularPointException from {}
)

PointNotOnBlackBox = new Type of Exception;

------------------------------------------------
-- END EXCEPTIONS
------------------------------------------------
--load "./BlackBoxParameterSpaces/Utils.m2";

------------------------------------------------
-- UTILS
------------------------------------------------
--
-- guessAcceptedParameterNumber(): 
--
-- find out for a function, how many parameters it does accept.
-- if a function accepts variable number of parameters, returns null
-- if it did not find 'numparms:' in the disasseble string, returns null.
--

guessAcceptedParameterNumber = method();

guessAcceptedParameterNumber( Function ) := ZZ => (foo)->
(
    lst := disassemble foo;

    --bblog.debug  ("disassemble result: " | lst );
    lst = separate( " ", lst );

    restargsPos := position( lst, (str)-> str=="restargs:" );
    numparmsPos := position( lst, (str)-> str=="numparms:" );

    if restargsPos=!=null then 
    (
        newlst := drop (lst, restargsPos);
        restargsPos = position( newlst, (str)-> str=="restargs:" ); 
        
        if restargsPos=!=null then 
        (
            --bblog.info ("do not know how to handle methods with a chain of several '->' ");
            --return null; 
        );
    );

    if numparmsPos===null then 
    (
        --bblog.warning (" warning: did not find the position of 'numparms:' in dissasseble string ");
        return null; 
    )
    else
        return  value lst#(numparmsPos+1);
);


-- guessAcceptedParameterNumber():
--
--   find out for a method, how many parameters it does accept.
--   Works only if a single function is installed for that method;
--   if multiple functions are installed for the same method, returns null.
--   status: beta.
--
guessAcceptedParameterNumber( MethodFunction ) := ZZ=>(foo)->
(
    func := apply( methods foo , m-> lookup m);
    if #func==1 then 
    (
        return  (guessAcceptedParameterNumber func#0);
    );
    if #func>1 then 
    (
        --bblog.info ("did not expect a method with multiple installed functions. ");
        return null;
    )
    else
    (
        error ("guessAcceptedParameterNumber: no functions installed for that method; something is screwed up ");
    );
);



testGuessAcceptedParameterNumber = ()->
(
    a:=null;   b:=null;   c:=null;

    foo := (a)->(5);
    assert(1==guessAcceptedParameterNumber foo);

    bar := (a,b)->(5);
    assert(2==guessAcceptedParameterNumber bar);

    foobar := method();
    foobar(Boolean,Boolean,String) := ZZ => (a, b, c)->5;
    assert( 3==guessAcceptedParameterNumber foobar );

    -- do not know how to check and what to do for this case:
    -- foo = a->(a,b)->(5);
    -- (foo(isPrime))(4,3)

    foo = a->(a,b);

    assert( 1==guessAcceptedParameterNumber foo );

)


TEST ///
    debug BlackBoxParameterSpaces
    idealBlackBoxesProtect()
    testGuessAcceptedParameterNumber()
///


-- polynomialLCMDenominator()
--
-- computes the least common multiple of the denominators of the polynomial (rational) cofficients;
-- that means if we have a polynomial = sum { (a_i/b_i)*monomial_i }, a_i and b_i integers,
-- then the function returns LCM( (b_i) )
--
polynomialLCMDenominator = (polynomial)->
(
    coeffRng := null;
    LCMDenominator := 1;
    --
    summands := { polynomial };
    while (coeffRng=!=ZZ and coeffRng=!=QQ) do
    (
        try ( coeffRng = coefficientRing ring  summands#0 ) then
        (        
             summands = flatten apply( summands, summand-> apply(flatten entries  last coefficients summand, j->sub(j,coeffRng) ) );    
        )
        else
        ( 
            error("expected rationals as coefficient ring!"); 
        );
    );
    if (coeffRng===QQ) then   LCMDenominator =  lcm apply(summands ,j-> denominator j ) ;
    return LCMDenominator;
)


-- clearCoeffDenominators() 
--
-- converts an ideal with rational coefficients 
-- to an ideal with integer coefficients while preserving the vanishing set.
-- e.g. if sub(IdealWithRationalCoeffs,point)==0, then  
-- sub( clearCoeffDenominators(IdealWithRationalCoeffs),point)==0 and vice versa
--
clearCoeffDenominators  = method();

clearCoeffDenominators (Ideal)  :=  Ideal =>  (IdealWithRationalCoeffs)->
(
    if (coefficientRing ring IdealWithRationalCoeffs=!=ZZ and coefficientRing ring IdealWithRationalCoeffs=!=QQ) then
    error("expected rationals as coefficient ring!");
    dstrng := ZZ[gens ring IdealWithRationalCoeffs];
    modgens := apply(flatten entries gens IdealWithRationalCoeffs, i->polynomialLCMDenominator(i)*i );
    return sub(ideal modgens,dstrng );
)


-- testClearCoeffDenominators()        
--
-- test conversion of an ideal with rational coefficients to an ideal 
-- with integer coefficients while preserving the vanishing set. 
--
testClearCoeffDenominators =()->
(
    x := null;  x=symbol x;
    y := null;  y=symbol y;
    RQ := QQ[x,y];
    x = (gens(RQ))#0;
    FQ := { (1/3*x+1) ,  (y+1/2)}; 
    IFQ := ideal FQ;
    IFZ := clearCoeffDenominators(IFQ);  
    FZ := (entries (gens IFZ)_0)#0;

    assert(   FZ == sub(x+3,ring FZ)   ) ; 

    point := matrix {{-3,-1/2}};
    assert( sub(IFQ,point)==0 );
    assert( sub(IFZ,point)==0 );

    point = random(QQ^1,QQ^2);
    assert( sub(IFQ,point)==sub(IFZ,point) );    
)

-- testNestedRingCoeffsLCMDenominator()
-- 
-- test polynomialLCMDenominator ( computing the least common multiple 
-- of the denominators of polynomials with rational coefficients)
-- in case that the rationals(QQ) are not the coefficient ring
--
testNestedRingCoeffsLCMDenominator =()->
(
    x:=null; x=symbol x;
    y:=null;  y=symbol y;
    z:=null;  z=symbol z;
    RQ := QQ[x,y];
    x = (gens(RQ))#0;

    RQQ := RQ[z];
    polFQ :=  (1/3*x+1)*z; 

    lcmDenom := polynomialLCMDenominator( polFQ );
    assert(lcmDenom==3);   
)

-- testTensoredClearCoeffDenominators()
-- 
-- test conversion of an ideal with rational coefficients to an ideal 
-- with integer coefficients while preserving the vanishing set. 
-- (clearCoeffDenominators)
-- the special case, that the ring the equations belong to is a tensor product of other rings. 
--
testTensoredClearCoeffDenominators =()->
(
    x:=null; x=symbol x;
    y:=null;  y=symbol y;
    z:=null;  z=symbol z;
    R1Q := QQ[x,y];
    x = (gens(R1Q))#0;

    R2Q := QQ[z];

    RTQ := R1Q**R2Q**QQ;

    x = (gens(RTQ))#0;
    z = (gens(RTQ))#2;
    polFTQ :=  (1/3*x+1)*z; 

    lcmDenom := polynomialLCMDenominator( polFTQ );
    assert(lcmDenom==3);
    IFQ := ideal polFTQ;
    IFZ := clearCoeffDenominators(IFQ);  
    FZ := (entries (gens IFZ)_0)#0;
    assert(   FZ == sub(x*z+3*z,ring FZ)   ) ;       
)

-- keysWithoutSymbols(): 
-- 
-- returns hashtable keys without symbol keys.
--
keysWithoutSymbols = method();
keysWithoutSymbols(HashTable) := List => (bb)->
(
    bbh:= new HashTable from bb;
    keylist := keys bbh;
    keylistResult := keylist;
    for key in keylist do 
    (  
        if (class key)===Symbol then
        (
            keyAsString := toString key;
            if bbh#?keyAsString then
                keylistResult=delete(key,keylistResult);
        );
    );
    return keylistResult;
)

------------------------------------------------
-- END UTILS
------------------------------------------------


-- swith between protect and export - both are not possible!

--idealBlackBoxesProtect() -- protect the symbols for checking package correctness: no symbol variables should be overwritten by accident!
idealBlackBoxesExport(); -- export the symbols to make the package work 


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

getPropertySymbols := method ();

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


-- XXXXX This is where we start moving code from ---
-- Currently: we can't load, because of 
--   error: mutable unexported unset symbol(s) in package BlackBoxParameterSpaces: 'getPropertySymbols'
--   BlackBoxParameterSpaces/ParameterSpaces.m2:538:25-538:43: here is the first use of 'getPropertySymbols'


-- development remark: we need jetAt at least per blackbox individually.

-- todo: we have to differ between interpolator and interpolation.


blackBoxIdeal = method();

 

-- todo: how to check, if 'ring equationsIdeal' is not a quotient ring?

--
-- this function is final, that means nobody should use this method for creating a derived object
--
new BlackBoxIdeal from Ideal := (E, equationsIdeal)->
(
     blackBox :=  blackBoxParameterSpaceInternal( BlackBoxIdeal, ring equationsIdeal );
    
    
    -- maybe blackBox.addProperty( ideal, equationsIdeal)
    blackBox#"ideal" = equationsIdeal;     
    blackBox.ideal = equationsIdeal;      
    
   
    
    
    -- registering valuesAt generates 'isZeroAt' and 'jacobianAt', too !  
    blackBox.registerPointProperty("valuesAt",( bb, point)->  
        (
            result :=  gens sub( equationsIdeal , point);
            return  result;
        )
    );


    -- maybe blackBox.addProperty( jacobian, jacobian gens  equationsIdeal )
    blackBox.jacobian = jacobian gens  equationsIdeal;


    -- we have to call updatePointProperty for "jacobianAt", because "jacobianAt" is present. 

    blackBox.updatePointProperty( "jacobianAt",
        ( bb, point )->
            (  
                -- attention, duplicate code!!!
                if (blackBox.withChecks) then
                (
                    if (not ( blackBox.valuesAt( point )==0))  then  
                    (
                        --error("point does not belong to the ideal ! ");
                        throw new PointNotOnBlackBox from {"errorMessage" => " Point " |toString point|" not on Black box"};
                    );
                );
                -- attention, duplicate code!!!
                jacobianM2MatrixAt := sub( blackBox.jacobian , point);
                return jacobianM2MatrixAt;
            )
    );   
    
    return blackBox; 
)

--
-- this function is final, that means nobody should use this method for creating a derived object
--
blackBoxIdeal (Ideal) := BlackBoxIdeal =>(equationsIdeal)->
(
   return new BlackBoxIdeal from equationsIdeal;
)

blackBoxIdeal (Ideal, Boolean) := BlackBoxIdeal =>(equationsIdeal, withChecks)->
(
    bb := new BlackBoxIdeal from equationsIdeal;
    bb.disableChecks();
    return bb;
)



testBlackBoxIdeal=()->
(
    x  := null;
    x  = symbol x;
    rng := ZZ/7[x];
    coeffRng := coefficientRing rng;
    x = (gens rng)#0;

    RP := ZZ/7[x];
    IFP := ideal { 3*x^2+1, 5*x-1 };        
    IFPBlackBox := blackBoxIdeal( IFP );
    point := matrix {{3}};
    rng13 := ZZ/13;
 
    assert( IFPBlackBox.jacobian== jacobian IFP);

    jac := null;
    point = matrix {{3}};
    try (   jac= IFPBlackBox.jacobianAt(point); ) then 
    (
        error("testblackBoxIdeal: jacobianAt should fail due the coefficient ring of the point matrix does not match the ideal coefficient ring and also the ideal coefficients are not integers ");
    )  
    else ();
    point = sub( point, coeffRng ) ;
    try {    jac= IFPBlackBox.jacobianAt(point); } 
    else
    (
        error("testblackBoxIdeal: jacobianAt should succeed  due the coefficient ring of the point matrix matches the ideal coefficient ring ");
    );  
    IFPBlackBox.ring;
    IFPBlackBox.valuesAt(point) ;
    assert(   IFPBlackBox.isZeroAt( point ) );
    assert( IFPBlackBox.jacobianAt(point)==sub( jacobian IFP,point) );
    assert( IFPBlackBox.valuesAt(point)== gens sub(  IFP, point ) );
)



--
--  creates a BlackBoxIdeal from a given evaluation method ('valuesAt') which takes a point (a row matrix)
--
--  parameters: numVariables in the parameter space,
--              coefficientRing , the 
--              valuesAt: a method for evaluating the object at one parameter point.
--
--
-- this function is final, that means nobody should use this method for creating a derived object
--

blackBoxIdealFromEvaluation = method();


blackBoxIdealFromEvaluation( Ring, Function ) := HashTable => ( pRing, pValuesAt ) ->
(

   blackBox := blackBoxParameterSpaceInternal(BlackBoxIdeal, pRing );
   blackBox.registerPointProperty ("valuesAt", (bb,point)->pValuesAt(point) ); --sets isZeroAt, jacobianAt, rankJacobianAt and numGenerators

   check := ()->
   (
        numVariables :=  blackBox.numVariables;
        
        point := matrix { apply(numVariables, i-> 0_(blackBox.coefficientRing) ) };
        blackBox.valuesAt( point );
        blackBox.isZeroAt( point );
   );

   check(); 
   return blackBox;
)

ideal (BlackBoxIdeal) := Ideal =>(bbI)->
(
    if (bbI#?"ideal") then
        return bbI#"ideal";
    if (bbI#?"ring") then
    (
        -- optional todo : we could cache the ideal here, once it is computed,
        -- but then it gets complicated to assure consistency
        return  ideal bbI.valuesAt(gens ideal gens bbI#"ring");        
    );
    error ("internally no ring is stored");    
);


TEST ///
   -- test extracting ideal from evaluation
    x  = symbol x;
    y  = symbol y;

    RP := ZZ/7[x,y];
    IFP := ideal ( 3*x^2+1, 5*x-1*y );      

    rank source gens IFP;
  
    evaluation := (point)->
    (
        return gens sub(IFP, point);
    );  
    evalBlackBox := blackBoxIdealFromEvaluation ( RP, evaluation );
    
    I = ideal evalBlackBox;
    
    assert((gens I)% IFP==0 )
    assert((gens IFP)% I==0 )
    
///


testBlackBoxIdealFromEvaluation = ()->
(
    x  := null;
    x  = symbol x;
    rng := ZZ/7[x];
    coeffRng := coefficientRing rng;
    x = (gens rng)#0;


    RP := ZZ/7[x];
    IFP := ideal ( 3*x^2+1, 5*x-1 );      

    rank source gens IFP;
  
    evaluation := blackBoxIdeal( IFP );
    --evaluation := basicBlackBox();
  
    -- deprecated:
    -- evalBlackBox := blackBoxIdealFromEvaluation ( # (gens evaluation.ring), coefficientRing evaluation.ring, evaluation.valuesAt );
    evalBlackBox := blackBoxIdealFromEvaluation ( RP, evaluation.valuesAt );

    point := matrix {{3_(ZZ/7)}} ;
    assert( evaluation.isZeroAt( point ) );
    assert( evaluation.valuesAt( point ) == evalBlackBox.valuesAt( point ) );
    assert( evaluation.jacobianAt( point ) == evalBlackBox.jacobianAt( point ) );

    assert( evaluation.numVariables ==evalBlackBox.numVariables );

    outerPoint := matrix {{2_(ZZ/7)}} ;

    assert( evaluation.valuesAt( outerPoint ) == evalBlackBox.valuesAt( outerPoint ) );

    assert( not evaluation.isZeroAt( outerPoint ) );

    assert( evaluation.coefficientRing === coeffRng);

    assert( evaluation.unknownIsValid ( ( gens ring IFP )#0 ));

    y  := null;    y  = symbol y;
    rngy := ZZ/7[y];
    y = (gens rng)#0;

    assert( not evaluation.unknownIsValid (  y ) );

);

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
    numTrials,
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

