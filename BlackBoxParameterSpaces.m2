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
     AuxiliaryFiles=>true
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
    "deduceNumGenerators"
}


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

------------------------------------------------
-- END UTILS
------------------------------------------------


Jet = new Type of HashTable;

jetObject = method();

-- jetObject.parent is currently black box
-- issue: probably parent will change in future, but 
-- we are stuck to 4 parameters to a method in M2, so how to pass the black box (to a BlackBoxJet)?

jetObject( Thing, Thing, Thing, ZZ) := Jet=>(parent, point, value, jetLength)->
(
      jetObj := new HashTable from { ("value",value),
                                     ("jetLength",jetLength),                                   
                                     ("parent",parent),
                                     ("blackBox",parent),
                                     ("point",point),
                                   };
    jetObj = newClass (Jet, jetObj);
    return jetObj;
);

 

net (Jet) := Net =>(jet)->
(
    sss :=  { --( net( "parent")  | " <= " | net ( class jet#"parent") ),
              --( net( "point")  | " <= " | net (jet#"point") ), 
              ( "(" | net (jet#"jetLength") |") " | net (jet#"value") )
              --( net( "length")  | " = " | net (jet#"jetLength") )              
            } ;    
    return net stack sss;
)

-- TODO naming: length, or precision?
length (Jet) := ZZ =>(jet)->
(
    return jet#"jetLength";
);

TEST /// 
    -- test 'length(Jet)'
    Fp = ZZ/101
    R = Fp[x,y]
    I = ideal(x^2-y^2+x^3)
    bbI = blackBoxIdeal I;
    point = matrix{{3,6_Fp}}
    bbI.isZeroAt(point)
    j = bbI.jetAt(point,3)
    assert(length j == 3)
///


sub (Ideal, Jet ) := Thing =>(I,jet)->
(
    return (sub(I,jet#"value"));
)



-- swith between protect and export - both are not possible!

--idealBlackBoxesProtect() -- protect the symbols for checking package correctness: no symbol variables should be overwritten by accident!
idealBlackBoxesExport(); -- export the symbols to make the package work 


needsPackage "SimpleDoc";
needsPackage "Text";


BlackBoxLogger = Logger("BlackBoxParameterSpaces");

-- todo: how to switch this on and off by the user? using closures again? 
--if BlackBoxParameterSpaces#Options#DebuggingMode then 
--    BlackBoxLogger.setLogLevel(LogLevel.DEBUG);

if BlackBoxParameterSpaces#Options#DebuggingMode then
    errorDepth=0;



bblog := BlackBoxLogger; --shortcut

--if BlackBoxParameterSpaces#Options#DebuggingMode then bblog.setLogLevel(LogLevel.DEBUG);

setBlackBoxLogLevel = (level)->
(
    BlackBoxLogger.setLogLevel(level);
);




savedEpsRings := new MutableHashTable;


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


--
-- package-global symbol for "eps"
--
--geps := getSymbol "eps";

geps := symbol eps;

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

getEpsRingFunction := (coeffring, epsDim)->
(
    assert (isDerivedFrom(coeffring,Ring));
    assert (isDerivedFrom(epsDim,ZZ) or isDerivedFrom(epsDim,InfiniteNumber));
    
    if isDerivedFrom(epsDim,InfiniteNumber) then
    (
       if epsDim=!=infinity then error ("epsDim " | toString epsDim |"not supported");
    );
    if isDerivedFrom(epsDim,ZZ) then
    (
        if (epsDim<0) then error("expected epsDim>0 ");
    );
    
    leps := geps;   
    epsRng := null;
    eps := null;
    
    if not (savedEpsRings#?(coeffring,epsDim) ) then 
    (
        polRing := coeffring[leps];
        leps = first gens(polRing);
        if epsDim===infinity then 
        (
            savedEpsRings#(coeffring,epsDim) = polRing            
        )
        else
        (
            savedEpsRings#(coeffring,epsDim) = polRing/(leps^(epsDim+1));            
        );
        epsRng = savedEpsRings#(coeffring, epsDim);
        epsRng#"epsPrecision" = epsDim;
        eps = first gens epsRng;
        (savedEpsRings#(coeffring,epsDim))#"eps" = eps;
        (savedEpsRings#(coeffring,epsDim)).eps = eps;        
        --for symb in getPropertySymbols("eps") do 
        -- (
        --   assert(symb=!=null);
        --   (savedEpsRings#(coeffring,epsDim))#symb  = eps;
        --)
    ); 
    epsRng = savedEpsRings#(coeffring, epsDim);
    eps = (gens epsRng)#0;
    return epsRng
)



getEpsRing = method();

-- todo: get rid of duplicate code in getEpsRing()...

getEpsRing(Ring, InfiniteNumber) := Ring => (coeffring, epsDim)->
(
    return getEpsRingFunction(coeffring, epsDim);   
)


getEpsRing(Ring, ZZ) := Ring => (coeffring, epsDim)->
(
    return getEpsRingFunction(coeffring, epsDim);
)


testEpsRing = ()->
(
    rng := getEpsRing(ZZ,1);
    eps:= (gens rng)#0;
    assert (1_rng+eps_rng + eps_rng*eps_rng == 1_rng+eps_rng );

    rng1 := getEpsRing(ZZ,1);
 
    rng2 := getEpsRing(ZZ,1);

    assert(rng1===rng2 );
);


TEST ///
    debug BlackBoxParameterSpaces
    testEpsRing();
///




-- introduce a new type representing a parameter space.

BlackBoxParameterSpace = new Type of  MutableHashTable;






-- deduceNumGenerators():
--
--   for an (ideal) blackbox, determine the number of   generators (or equations). This is possible in case 
--   the blackbox provides the 'valuesAt'-property.
--
--   todo: this is a generic version. if the blackbox is given by an ideal I,
--        the generators can be determined easily by #(gens ideal I)
--   todo: what should the user do, if deduceNumGenerators() fails? 
--

deduceNumGenerators  = method();

deduceNumGenerators (BlackBoxParameterSpace) := ZZ => (blackBox)->
(
    bblog.debug(" enter deduceNumGenerators ") ;

    if not blackBox.hasPointProperty("valuesAt") then 
    ( 
        error ("cannot determine the number of generators/equations, since valuesAt is missing");
    );
    try ( numVar:=blackBox.numVariables; ) else (  error (" blackBox.numVariables is missing"); );
    
    computed  := false; 
    maxTrials := 100;
    currTrial := 0;
    rng := blackBox.coefficientRing;
    numGenerators := null;
    
    while numGenerators===null and currTrial<maxTrials do
    (
        try
        (
            bblog.debug(" deduceNumGenerators: enter try block ") ;
            
            tmppoint := matrix random(rng^1,rng^(blackBox.numVariables) );
            
            bblog.debug(" deduceNumGenerators:computed tmppoint ") ;
            
            valuesMatrix := blackBox.valuesAt( tmppoint );
            
            bblog.debug(" valuesMatrix computed ") ;
            
            --print valuesMatrix;
            
            assert (numRows valuesMatrix==1);
            numGenerators = numColumns valuesMatrix;            
        )
        then ( computed=true ) 
        else ();
        
        
        currTrial = currTrial+1;
    );
    if not computed then error " failed to deduce number of generators/equations";
    return numGenerators;
);


testDeduceNumGenerators = ()->
(
    x := symbol x;
    R := ZZ[x];
    blackBoxDummy := blackBoxParameterSpace(R);
    blackBoxDummy.hasPointProperty = (propertyName)->
    (
        if propertyName==="valuesAt" then return true;
        return false;
    );
    
    blackBoxDummy.valuesAt = (point)-> ( return matrix {{1,2,3,4,5}} );
    
    blackBoxDummy.coefficientRing = ZZ;
    blackBoxDummy.numVariables = 5;
    numGenerators := deduceNumGenerators(blackBoxDummy);
    assert( numGenerators==5 );
);


TEST ///
    debug BlackBoxParameterSpaces
    idealBlackBoxesProtect()
    testDeduceNumGenerators()
///


-- dropDegreeInfo():
-- 
--   for some matrix operations (which ones?), degree information needs to be dropped,
--   which is done by this method. Used in '.jacobianAt' which in turn is  used in the 'padicLift' package.
--
dropDegreeInfo = method();

dropDegreeInfo (Matrix) := Matrix=> (mat)->
(
    return map( (ring mat)^(numRows mat) ,(ring mat)^(numColumns mat), mat );
);


deduceJacobianAt = method();

-- deduceJacobianAt()
--
-- Constructs a jacobian at a point supposing that the blackBox implements evaluation at a point.
-- Currently expects that blackBox implements 'valuesAt' and knows number of generators/equations(numGenerators).
-- The second dependency could be removed, as the column number  of the returned 
-- 'valuesAt' evaluation ( a row vector) should be the same as number of generators/equations.
-- 
-- Remark: this stuff is very hard to debug because of the try clauses in the black box.
--
deduceJacobianAt (BlackBoxParameterSpace, Matrix) := ZZ=> ( blackBox, point )->
(
  
    --valuesAt := null; numGenerators := null;
    --valuesAt = global valuesAt;

    --numGenerators = global numGenerators;

    if not  blackBox.hasPointProperty("valuesAt") 
        then error("deduceJacobianAt: to construct jacobian at a point the black box need at least the property 'valuesAt' ");
    -- assert( blackBox#?(global valuesAt) ); -- this should be a 
    try {  blackBox.numGenerators() } else (
        error("deduceJacobianAt: to construct jacobian at a point the black box requested number of generators ('numGenerators'), which was not present.");
    );

    -- assert( blackBox#?(global numGenerators) ); -- this should be a test.

    rngPoint := ring point;
    numVariables := blackBox.numVariables ;      

    -- attention, duplicate code!!!
    if (blackBox.withChecks) then
    (
        if (not ( blackBox.valuesAt( point )==0))  then  
        (
            --error("point does not belong to the ideal ! ");
            throw new PointNotOnBlackBox from {"errorMessage" => "deduceJacobianAt: point " | toString point | "does not belong to the object! "}
        )
    );
    -- attention, duplicate code!!!
    
    epsRng := getEpsRing(rngPoint, 1);
    eps := (gens epsRng)#0;


    jacobianMatrixAt := mutableMatrix( rngPoint ,  numVariables, blackBox.numGenerators() );
    for unknownIdx  in 0..(numVariables-1) do
    (
        newpoint := new MutableMatrix from sub( point, epsRng );

        newpoint_(0,unknownIdx) = newpoint_(0,unknownIdx)+eps;
        valueVec := blackBox.valuesAt( matrix newpoint );  
        for equationIdx in 0..numColumns valueVec-1 do
        (
            coordinateValue := last coefficients (sub(valueVec_(0,equationIdx), epsRng ), Monomials=>{1  , eps } );
            if ( not (coordinateValue)_(0,0) ==0) then error("error in jacobianAt. please contact the developers");
            jacobianMatrixAt_(unknownIdx,equationIdx) = sub( (coordinateValue)_(1,0) , rngPoint  )  ;
        );
    );
    return matrix jacobianMatrixAt;
);


testDeduceJacobianAt = ()->
(
    K := ZZ/5;
    x := getSymbol "x";  y := getSymbol "y";  z := getSymbol "z";

    R := K[x,y,z];

    x = (gens R)#0;  y = (gens R)#1;  z = (gens R)#2;

    -- ideal of a plane and a line that intersect at the origin
    I := ideal (x*z,y*z);
     
    blackBoxDummy := blackBoxParameterSpace(R);

    point := matrix{{1_R,1_R,0_R}};

    blackBoxDummy.numGenerators= ()-> return (numColumns (sub (gens I, point ) ) ) ;

    blackBoxDummy.hasPointProperty = (propertyName)->
    (
        if propertyName==="valuesAt" or propertyName==="jacobianAt"  then return true;
        return false;
    );

    blackBoxDummy.valuesAt= (point)-> ( return sub(generators I, point); );

    blackBoxDummy.numVariables = #(gens R);
    
    blackBoxDummy.withChecks = true;

    computedJac := deduceJacobianAt( blackBoxDummy, point);

    targetJac := dropDegreeInfo sub(jacobian generators I, point); 
    assert( computedJac == targetJac );

    point = matrix{{ 0_R, 0_R, 5_R}};
    computedJac = deduceJacobianAt( blackBoxDummy, point);
    targetJac = dropDegreeInfo sub(jacobian generators I, point); 
    assert( computedJac == targetJac );
);


TEST ///
    debug BlackBoxParameterSpaces
    idealBlackBoxesProtect()
    testDeduceJacobianAt()
///



-- question: what is a (succeeded) jet of length 1 at a singular point??


-- jetAtWithInfoResultFunction
-- 
-- constructs a jetAtWithInfoResult  Hashtable from (bestJet and failedJetLength) 
-- with entries "jet", "bestJet", "failedJetLength", "succeeded" 
--
-- purpose: return multiple variables at once in jetAtWithInfo. 
-- Returning a sequence is not an option since then the implementation is not extensible
-- (how to add an additional returned variable without breaking existing code?)
--
-- "bestJet" is always set to bestJet and "failedJetLength" always to failedJetLength.
-- If failedJetLength is null, then "jet" is also set to bestJet and "succeed" to true.
-- otherwise "jet" is set to null and "succeed" to false. 
--
jetAtWithInfoResultFunction := (bestJet, failedJetLength)->
(    
    assert(class bestJet === Jet);
    jet := null;
    if (failedJetLength === null) then 
    (
        jet = bestJet;
    )
    else
    (
        assert(class failedJetLength===ZZ);
        assert(  failedJetLength>=0);
    );
    
    return new HashTable from {    "jet"            => jet, 
                                   "bestJet"        => bestJet, 
                                   "failedJetLength"=> failedJetLength,
                                   "succeeded"      => failedJetLength === null
                              };
);

--
-- see jetAtWithInfoResultFunction()
--
jetAtWithInfoResult := method();
jetAtWithInfoResult ( Jet, ZZ ) := HashTable => (bestJet, failedJetLength)->
(    
    return jetAtWithInfoResultFunction(bestJet, failedJetLength);
);

-- special case for null...
--
-- see jetAtWithInfoResultFunction()
--
jetAtWithInfoResult ( Jet, Nothing ) := HashTable => (bestJet, failedJetLength)->
(
     return jetAtWithInfoResultFunction(bestJet, failedJetLength);
);






JetAtCalculator = new Type of  HashTable;

new JetAtCalculator from Thing := ( E, thing) -> 
(
    error "creating JetAtCalculator from  type " | toString E | " not implemented ";
);


basicJetAtCalculator = ()->
(
    jetAtCalculator := new MutableHashTable;

    -- continueJetWithInfo()
    --
    -- Ccontinues a given jet up to a requested jetLength (if possible)
    -- Returns a hashtable with a bunch of information, see jetAtWithInfoResult()
    --
    -- todo: eventually cache jacobian and jacobian kernel
    --
    jetAtCalculator#"continueJetWithInfo" = method();

    jetAtCalculator#"continueJetWithInfo"( Jet, ZZ ) := HashTable => (   jet, jetLength )  ->
    (  
        blackBox := jet#"blackBox";
        
        assert ( 0 == blackBox.valuesAt(jet#"value") );
        assert ( jetLength >= 0 );
        assert ( jetLength >= length jet );  
        
        failedJetLength := null;    

        if (jetLength==0) then return   jetAtWithInfoResult(jet, failedJetLength);
        
        point := jet#"point";
        
        epsPrecision := length jet;  
            
        coeffRng := (blackBox.coefficientRing); -- we need the braces here !!!
        
        jetValue := jet#"value";
        
        liftingFailed := false;
        
        jetObj := null;
        prejet := null;

        succeededJetLength := length jet;    
        jacobianM2Transposed := transpose blackBox.jacobianAt(point) ;
        
        
        jacobianKernel := generators kernel jacobianM2Transposed ; -- syz would also work
        
        if (length jet==0) then 
        (
            epsPrecision = 1;
            epsRng := getEpsRing( blackBox.coefficientRing,  epsPrecision );
            eps := (gens epsRng)#0;

            
            rnd := random( coeffRng^(numColumns(jacobianKernel)), coeffRng^epsPrecision );
            if (numColumns(jacobianKernel)>0) then 
            (   
                while  zero(rnd) do
                (
                    rnd = random( coeffRng^(numColumns(jacobianKernel)), coeffRng^epsPrecision );
                );
            );

            lengthOneLift := sub(point,epsRng) + transpose(sub( jacobianKernel*rnd, epsRng) *eps);
            
            -- first lift will always succeed!
            if ( blackBox.valuesAt(lengthOneLift)!=0 ) then 
            (      
                liftingFailed = true;
                failedJetLength = epsPrecision;
            )
            else
            (
                succeededJetLength = 1;
                jetValue = lengthOneLift;
            );
        );
    
        if (not liftingFailed) then 
        (
            for  epsPrecision in (1 + succeededJetLength)..jetLength do 
            (
                epsRng := getEpsRing( coeffRng, epsPrecision);
                eps := (gens epsRng)#0;
        
                prejet =  sub(jetValue,epsRng);

                valuesAtJet := blackBox.valuesAt(prejet );

                rightHandSide := matrix mutableMatrix( coeffRng, numColumns valuesAtJet ,1 );
                
                if not zero(valuesAtJet) then 
                (           
                    rightHandSide = transpose last coefficients (valuesAtJet, Monomials=>{ eps^epsPrecision });
                    -- one could also use contract since eps^epsPrec is the highest possible degree
                );
        
                rightHandSide = sub(rightHandSide,coeffRng);
            
                if not (0==rightHandSide % jacobianM2Transposed ) then 
                (
                    failedJetLength = epsPrecision;
                    liftingFailed = true;
                    break; 
                );
                succeededJetLength = epsPrecision;
                x := rightHandSide // jacobianM2Transposed ;
                x = x + jacobianKernel* random(coeffRng^(numColumns(jacobianKernel)), coeffRng^1 );
                x = transpose x;
        
                nextJetValue := sub (prejet, epsRng ) - sub( x, epsRng ) * eps^epsPrecision;
                assert ( 0 == blackBox.valuesAt(nextJetValue) ); -- debug
                jetValue = nextJetValue;
            );
        );

        bestJetObject := jetObject (blackBox,  point, jetValue, succeededJetLength);
        
        return  jetAtWithInfoResult(bestJetObject, failedJetLength);
    );

    -- jetAtWithInfo(): 
    --
    --   tries once to compute a jet , ( see http://en.wikipedia.org/wiki/Jet_%28mathematics%29 for jet definition;) 
    --   for the used computation algorithm see the bacherlor thesis at 'http://www.centerfocus.de/theses/js.pdf' .
    --
    --   preconditions: black box provides evaluation at a point ('valuesAt') and valuesAt(point) evaluates to zero.
    -- 
    --   returns a hashtable with entries
    -- 
    --  - "succeeded" a boolean, 
    --  - "failedJetLength"  contains the jet length at which the computation failed, otherwise null
    --  - "jet"  contains the jet, if succeeded, otherwise null. The jet of the length n has the form
    --           j = point + eps*y_1 + . . . + eps^n * y_n 
    --           such that F(j) = 0, 
    --           where F: E_(n+1)^m -> E_(n+1)^k 
    --           with E_(n+1) = K[eps]/( eps^(n+1) ) 
    --           whereby K is the coefficient ring (blackBox.coefficientRing), 
    --           m is the number of variables (blackBox.numVariables) of the parameter space (same as entries in the point vector)
    --           and k is the number of the generators/equation of the (implicitly or explicitly) given ideal. 
    --

        
    jetAtCalculator#"jetAtWithInfo" = method();

    -- jetAtWithInfo():
    --
    -- here we improve precision by 1 in each step
    -- using Newtons-Algorithm one could double precision in each step, but
    -- for this we also need high precision jacobi-matrices.
    -- For black-Box-Jacobi-Matrices we might not have high precision Jacobi-Matrices
    -- todo question: what do we mean by high precision Jacobi-Matrices?
    -- todo: remove duplicate code (see continueJetWithInfo)
    --
    jetAtCalculator#"jetAtWithInfo"( BlackBoxParameterSpace, Matrix, ZZ ) := HashTable => ( blackBox,  point, jetLength )  ->
    (
        assert ( jetLength >= 0 );
        
        if not (blackBox.isZeroAt(point)) then 
        (
            --error(" point is not on BlackBox ");
            throw new PointNotOnBlackBox from {"errorMessage" => "jetAtWithInfo: point " | toString point | "does not belong to the object! "}
        );

        liftingFailed := false;
        failedJetLength := null;
        epsPrecision := 0;
        epsRng := getEpsRing( blackBox.coefficientRing,  epsPrecision );
            
        jet := sub(point, epsRng);

        succeededJetLength := 0;    
        
        
        localJetObject := jetObject (blackBox,  point, jet, succeededJetLength);
        
        if (jetLength==0) then 
        (
            return  jetAtWithInfoResult(localJetObject, failedJetLength);
        );
        
        return jetAtCalculator#"continueJetWithInfo"(localJetObject, jetLength);
    );





    -- jetAt()
    --
    -- Computes a jet with given jetLength once using jetAtWithInfo()
    -- Returns the jet if succeeded, otherwise the point is singular and an SingularPointException is thrown
    -- 
    jetAtCalculator#"jetAt" = method();

    jetAtCalculator#"jetAt"( BlackBoxParameterSpace, Matrix, ZZ) := Jet => ( blackBox,  point, jetLength )  ->
    (
        jetResult  := jetAtCalculator#"jetAtWithInfo" ( blackBox,  point, jetLength);      
        
        if (jetResult#"jet"=== null) then 
        (
        --error ("point is not smooth"); -- is better 
        throw new SingularPointException from {"errorMessage"=>"Point is not smooth",
                                                "failedJetLength" => jetResult#"failedJetLength",
                                                "failedJet" => jetResult#"bestJet",                                            
                                                    };
        );     
        return jetResult#"jet";
    );



    -- continueJetOrException()
    --
    -- Continues a given jet up to a requested jetLength (if possible) using continueJetWithInfo()
    -- returns the computed jet if succeeded, otherwise the point is singular and a SingularPointException is thrown.
    --
    --
    jetAtCalculator#"continueJet" = method();

    jetAtCalculator#"continueJet"( Jet, ZZ) := Jet => ( jet, jetLength )  ->
    (
        jetResult  := jetAtCalculator#"continueJetWithInfo" ( jet, jetLength);      
        
        if (jetResult#"jet"=== null) then 
        (
        throw new SingularPointException from {"errorMessage"=>"Point is not smooth",
                                                "failedJetLength" => jetResult#"failedJetLength",
                                                "failedJet" => jetResult#"bestJet",                                            
                                                    };
        );     
        return jetResult#"jet";
    );
 


    -- jetStatsAt()
    --
    -- computes jet statistics at a point, namely the counts of first failed jet lenght for several trials
    -- This may be of interest at singular points.
    --
    -- Parameters:  a black box, a point, the maximal jet length and number of trials to compute a jet.
    --
    -- Returns a hashtable with 
    --
    -- "targetJetLength"
    -- "numTrials" 
    -- "jetSets" -- a hashtable of jet list at point with their length as HashTable key
    -- "failedLengthCount" -- a (Tally) where the key is the jetLength l,
    --                      and the value is the count of trials where the computation failed at length l.
    --
    jetAtCalculator#"jetStatsAt" = method();
    jetAtCalculator#"jetStatsAt"( BlackBoxParameterSpace, Matrix, ZZ, ZZ) := HashTable => ( blackBox,  point, jetLength, numTrials )  ->
    (
    if ( numTrials<1 ) then error "jetAtStats: expected numTrials >=1 ";
        
        jetStats := new MutableHashTable ;
            
        jetStats#"jetSets" = new MutableHashTable ;       
        jetStats#"targetJetLength" = jetLength;
        jetStats#"numTrials" = numTrials;            
        jetStats#"failedLengthCount" = new Tally;
        
        jetStats#"succeededCount" = 0;
            
        jetResult  := null;           
        
        for i in 1..numTrials do
        (
            jetResult = jetAtCalculator#"jetAtWithInfo" ( blackBox,  point, jetLength);
            if (jetResult#"jet"=== null) then   
            (
                jetStats#"failedLengthCount" = jetStats#"failedLengthCount" + tally {jetResult#"failedJetLength"};
            )
            else
            (
                jetStats#"succeededCount" = jetStats#"succeededCount"+1;
                    
            );
            if (jetResult#"bestJet"=!= null) then   
            (
                currentJet := jetResult#"bestJet";
                currentJetLength  := length currentJet;           
                
                if (not (jetStats#"jetSets")#?currentJetLength) then 
                (
                    (jetStats#"jetSets")#currentJetLength = new JetSet from currentJet;
                )
                else
                (
                    addElement(jetStats#"jetSets"#currentJetLength, currentJet);
                );                                    
            );
        );
        jetStats#"jetSets" = new HashTable from    jetStats#"jetSets";
        return new HashTable from jetStats;
    );
    
    jetAtCalculator.jetAt = jetAtCalculator#"jetAt";
    jetAtCalculator.continueJet = jetAtCalculator#"continueJet";
 
    jetAtCalculator.jetAtWithInfo = jetAtCalculator#"jetAtWithInfo";
    jetAtCalculator.continueJetWithInfo = jetAtCalculator#"continueJetWithInfo";
    
    jetAtCalculator.jetStatsAt = jetAtCalculator#"jetStatsAt";
    
    jetAtCalculator = newClass(JetAtCalculator,jetAtCalculator);
    
    return jetAtCalculator;    
)


TEST ///
  -- test for bug that  valuesAt(jet) is non zero (jet was incorrectly in a ring with higher precision than required)

    kk = QQ
    R = QQ[x,y]
    I = ideal (x*y)

    origin = matrix{{0,0_kk}}
    p1     = matrix{{1,0_kk}}

    myValuesAt = (p) -> (  return gens sub(I,p);  );


    bb = blackBoxIdealFromEvaluation(R, myValuesAt)

    jetStats = bb.jetStatsAt(origin,2,10)

    jetsL1 = jetStats#"jetSets"#1
    jetL1 = first jetsL1#"jets"
    L1values =  bb.valuesAt(jetL1#"value")
    assert (0 == L1values)
    epsRng = ring L1values;
    assert (epsRng#"epsPrecision"==1)
    -- check that we are in R[eps]/eps^2 for precision 1 jet
    assert (ideal epsRng == ideal (first gens last epsRng.baseRings)^2)

    jetL2 = bb.jetAt(p1,2)
    valuesL2 =  bb.valuesAt(jetL2#"value")
    assert (0 == valuesL2)
    epsRng = ring valuesL2;
    assert (epsRng#"epsPrecision"==2)
    -- check that we are in R[eps]/eps^3 for precision 2 jet
    assert (ideal epsRng == ideal (first gens last epsRng.baseRings)^3)

    jetL0 = bb.jetAt(p1,0)
    assert (jetL0#"value" == sub(p1,ring jetL0#"value") )

///





SmoothnessTester = new Type of  HashTable;

new SmoothnessTester from Thing := ( E, thing) -> 
(
    error "creating SmoothnessTester from  type " | toString E | " not implemented ";
);

basicSmoothnessTester = method();

basicSmoothnessTester (JetAtCalculator) := SmoothnessTester => ( jetAtCalculator) -> 
(
 
    localSmoothnessTester := new MutableHashTable;
    --
    --
    --
    localSmoothnessTester#"isCertainlySingularAt" = method();
    localSmoothnessTester#"isCertainlySingularAt"( BlackBoxParameterSpace, Matrix, ZZ, ZZ) := MutableHashTable => ( blackBox,  point, jetLength, numTrials ) ->
    (
        if ( numTrials<1 ) then error "isCertainlySingularAt: expected numTrials >=1 ";
        
        for i in 1..numTrials do
        (
            jetOrError := catch jetAtCalculator.jetAt( blackBox,  point, jetLength);
            if isDerivedFrom(jetOrError,SingularPointException) then 
            (
                return true;
            );
            if not isDerivedFrom(jetOrError, Jet) then 
            (
                throw jetOrError; -- it is a different error 
            );
        );
        return false;
    );
    
    localSmoothnessTester#"isCertainlySingularAt"( BlackBoxParameterSpace, Matrix, HashTable) := MutableHashTable => ( blackBox,  point, options ) ->
    (
        return localSmoothnessTester.isCertainlySingularAt(blackBox, point, options.precision, options.numTrials);
    );    
    
    localSmoothnessTester.isCertainlySingularAt = localSmoothnessTester#"isCertainlySingularAt";
   
   
   
    localSmoothnessTester#"isProbablySmoothAt"  = method();


    localSmoothnessTester#"isProbablySmoothAt" ( BlackBoxParameterSpace, Matrix, ZZ, ZZ) := MutableHashTable  =>
        ( blackBox,  point, jetLength, numTrials ) ->
    (
            return not localSmoothnessTester.isCertainlySingularAt( blackBox,  point,  jetLength, numTrials );
    );
    
    
    localSmoothnessTester#"isProbablySmoothAt"( BlackBoxParameterSpace, Matrix, HashTable) := MutableHashTable => ( blackBox,  point, options ) ->
    (
        return not localSmoothnessTester.isCertainlySingularAt(blackBox, point, options.precision, options.numTrials);
    );
    
    localSmoothnessTester.isProbablySmoothAt =  localSmoothnessTester#"isProbablySmoothAt";
    
    
    localSmoothnessTester#"setJetAtCalculator" = (jetAtCalculatorP)->
    (
        jetAtCalculator = jetAtCalculatorP;
    );
    
    localSmoothnessTester.setJetAtCalculator = localSmoothnessTester#"setJetAtCalculator";    
    
    localSmoothnessTester = newClass(SmoothnessTester, localSmoothnessTester);
    
    
  
    return localSmoothnessTester;
);


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





--
-- disable BlackBoxParameterSpace construction using new for an arbitrary parameter:
--
new BlackBoxParameterSpace from Thing := ( E, thing) -> 
(
    error "creating blackbox from  type " | toString E | " not implemented ";
);

--
-- disable BlackBoxParameterSpace construction using new without parameters:
--
new BlackBoxParameterSpace   := (E) -> 
(
    error "creating empty blackbox not possible. You have at least to provide the number of variables and their ring";
);


BlackBoxIdeal = new Type of  BlackBoxParameterSpace;


-- disable BlackBoxIdeal construction using new without parameters ( bb = new BlackBoxIdeal ):
new BlackBoxIdeal   := (E) -> 
(
    error "creating empty blackbox not possible. You have at least to provide the number of variables and their ring";
);

-- disable BlackBoxIdeal construction using new for an arbitrary parameter. ( bb=new BlackBoxIdeal from ... ):
new BlackBoxIdeal from Thing := ( E, thing) -> 
(
    error "creating blackbox from  type " | toString E | " not implemented ";
);




-- Type MapHelper contains a matrix and a function
-- to evaluate this matrix on a point
-- This is for example used when projecting onto a
-- subspace (i.e elimination of variables)
--
MapHelper = new Type of HashTable;

--
--
--
createMapHelper = (mapMatrix, imageRing) -> 
(
    mapData := new MutableHashTable;
    
    mapData#"imageRing" = imageRing;
    mapData#"matrix" = mapMatrix;
    
    mapData#"valueAt" =  method();    
    mapData#"valueAt" (Matrix) := Matrix => (point)->
    (
        return sub(mapMatrix,point);
    );
   
    mapData#"valueAtJet" = method();
    mapData#"valueAtJet" (Jet) := Jet => (jet) -> 
    (
        return jetObject(jet#"parent", jet#"point", (mapData#"valueAt")(jet#"value"), jet#"jetLength");                  
    );
   
    return new MapHelper from mapData
);

new MapHelper from Matrix := (XXX, mapMatrix) -> 
(
    -- das hier ist irgendwie alles Quatsch...
    --sourceRing := ring mapMatrix;
    --K := coefficientRing sourceRing;
    --m := rank source mapMatrix;
    --xxx := symbol xxx;    -- todo: get symbol in user space?
    --imageRing := K[xxx_1..xxx_m];
    imageRing := null; 
    return createMapHelper(mapMatrix, imageRing);
);



JetSet = new Type of MutableHashTable;
 

new JetSet from Thing := ( E, thing) -> 
(
    error "creating JetSet from  type " | toString E | " not implemented or not possible ";
);


new JetSet from Jet := ( E, thing) -> 
(
   mht := new MutableHashTable;
   
   mht#"jets" = new List from {thing};
   mht#"point" = thing#"point";
   return mht;
);

net (JetSet) := Net => (jetSet)->
(
    result := net "JetSet{ point: " |net jetSet#"point" | net ", jets{.."| net size jetSet | net "..}}";
    return result;
);

 

-- how to hide compatible?
compatible = method();


compatible (Jet,Jet) := Boolean => (jet1, jet2 )->
(    
    if  ( jet1#"parent"===jet2#"parent" and 
          jet1#"point" ===jet2#"point"      ) then
         (
            return true;
         );
         return false;        
);

compatible (JetSet,JetSet) := Boolean => (jetSet1, jetSet2 )->
(    
    if (0== size jetSet1 or 0== size jetSet2 ) then   return true;   
    return compatible(  jetSet1#"jets"#0,   jetSet2#"jets"#0);
);

size (JetSet) := ZZ =>(jetset)->
(
    return #(jetset#"jets");
)


-- probablySameComponent; certainlyDifferentComponent

addElement = method();

addElement (JetSet,Jet) := JetSet => (jetSet, jet)->
(
    if ( size jetSet===0 or
         compatible(jetSet#"jets"#0, jet) 
       ) then
        (
            jetSet#"jets" = append(jetSet#"jets",jet);            
            return jetSet;
        );
    error ("JetSet and Jet are probably incompatible (either they do not start at the same point or do not belong to the same black box)");
)

joinJetSets = method();

joinJetSets (JetSet,JetSet) := JetSet => (jetSet1, jetSet2 )->
(
    if (compatible (jetSet1, jetSet2)) then 
    (
        result := new JetSet;
        result#"jets" = unique join (jetSet1#"jets",jetSet2#"jets");
        return result;
    )
    else
    (
        error ("jet sets probably not compatible");
    );
);




TEST ///
    -- restart 
    -- loadPackage "BlackBoxParameterSpaces"
    debug BlackBoxParameterSpaces --otherwise we have no 'compatible()'
    K = QQ;
    R = K[x,y];
    bbI = new BlackBoxIdeal from ideal x*y;
    
    
    point = matrix{{0_QQ, 0_QQ}};
    point2 = matrix{{0, 1_K}};
    
    jet =  bbI.jetAt(point,1)
    
    --locus jet
    jet2 =  bbI.jetAt(point,1)   
    
    compatible(jet,jet2) 
    
    jetSet = new JetSet from jet
    
    --locus jetSet      
    compatible(jetSet,jetSet)    
    
    addElement(jetSet,jet2)
    
    joinJetSets(jetSet,jetSet);    
    
    jet3 := bbI.jetAt(point2,1)
    
    try (addElement(jetSet,jet3)) then (error ("adding incompatible jet should fail"))  else()
    
    -- unique: not im    
///


load "./BlackBoxParameterSpaces/Interpolation.m2";


-- development remark: we need jetAt at least per blackbox individually.

pointProperties = method();

pointProperties (BlackBoxParameterSpace) := List => (bb)->
{
    return bb.pointProperties();
}


attributes = method();

attributes (BlackBoxParameterSpace) := List => (bb)->
{
    return bb.attributes();
}


memberMethods = method();

memberMethods (BlackBoxParameterSpace) := List => (bb)->
{
    return bb.memberMethods();
}

-- todo: we have to differ between interpolator and interpolation.


-- internal method to create a basic black box ( a black box for a parameter space )
--
-- The reason for using an internal (mutable black box) and a public protected blackbox 
-- is to prevent the user from accidental object changing.

-- The object write protection has to be used as  follows: 
--  as a final object for the user always a nonmutable object copy has to be returned;
--  while for the internal 'class' inheritance the non-copied original mutable object 
--  (as created by the internal methods) is needed.
--
-- Since the user has access only to the copy and may modify the object e.g. by registering properties,
-- all (potential) mutable black box properties needs to be stored as local variables in the internal methods, 
-- like 'bbPointProperties' in 'blackBoxParameterSpaceInternal'.
--
-- Access to potential mutable  black box properties by a user can only be modelled  through 'get'-methods, 
-- like blackBox.numGenerators()
-- and may never accessed directly, because then other (shallow) copies of the same black box would run out of sync.
-- Because blackBox.numGenerators() is defined in the same context with the 
-- local variable 'localNumGenerators', the variable 'localNumGenerators'
-- is visible (and modifiyable) inside 'blackBoxParameterSpaceInternal', but not outside!


blackBoxParameterSpaceInternal = method();

blackBoxParameterSpaceInternal( Type, ZZ, Ring  ) := HashTable => ( resultType, numVariables, coeffRing ) ->
(
    
    blackBox := new MutableHashTable;
    
    blackBox = newClass(resultType, blackBox);
    
    jetAtCalculator := basicJetAtCalculator();
    
    blackBox.withChecks = true;
    
    blackBox.disableChecks = ()->
    (
         blackBox.withChecks = false;
    );
    
    blackBox.enableChecks =  ()->
    (
         blackBox.withChecks = true;
    );
    
    -- public: 
    blackBox.coefficientRing = coeffRing;  
    blackBox.numVariables = numVariables;  -- stores the number of the variables in the parameter space.
    
    -- private: 
    bbPointProperties := new HashTable  ;     -- stores the point properties
    localNumGenerators := null;                  -- stores the number of the generators/equations. Since this value may be updated,
                                           -- the value cannot be stored in the blackBox directly as a key-value pair, 
                                           -- because, otherwise different black box references referring to the same object
                                           -- could get out of sync. The variable is accessed by a getter(numGenerators())
    
    
    
    
    singularTestOptions := new MutableHashTable;
    singularTestOptions.precision = 10;
    singularTestOptions.numTrials = 2;

   
    -- checks the consistency of the point with the black box 
    -- 2. the point should be given as a column matrix having same number of columns as blackBox.numVariables
    -- 1. if blackBox.coefficientRing is ZZ, then every ring for point entries is allowed (a) why, , (b) is this always correct?
    --    if blackBox.coefficientRing is NOT ZZ, then either the ring of point has to coincide with the 'blackBox.coefficientRing', or
    --                                             the coefficientRing( ring point) has to coincide with the 'blackBox.coefficientRing'. 
    -- 
    checkInputPoint := (point)->
    (
        errorMsg := "  ideal is defined over "| toString  blackBox.coefficientRing | "and not over " | toString ring point |"!" ;
        if blackBox.coefficientRing =!= ZZ  
           and ring point =!= blackBox.coefficientRing
        then 
        (
            try ( if coefficientRing ring point=!= blackBox.coefficientRing then error errorMsg )  
            then () 
            else ( error (errorMsg) );
        );
        if (numColumns point=!=blackBox.numVariables) then 
        ( 
            error (" illegal point : expected " | toString blackBox.numVariables | " coordinates");
        );
    );


    -- pointProperty():
    --
    --    return a point property (by symbol or by name) stored in 'bbPointProperties'. 
    --    If a property does not exist, throws an error.
    --
    blackBox.pointProperty = method();

    --
    -- get a point property by name
    --
    blackBox.pointProperty (String) := Function =>( prop ) ->
    (
        if not bbPointProperties#?prop then        
            error (" blackbox does not have requested point property " | toString prop  );

        return bbPointProperties#prop;
    );     
     
    blackBox.pointProperty (Symbol) := Function =>( prop ) ->
    (
        return blackBox.pointProperty(toString prop );
    );


    --  hasPointProperty:
    --    checks, if the blackbox has a specified property (by name or by symbol)
    --
    blackBox.hasPointProperty = method()  ;
    blackBox.hasPointProperty (String) := Boolean =>(propertyName)->
    (
        return bbPointProperties#?propertyName;
    );
    
    blackBox.hasPointProperty ( Symbol ) := Boolean => (propertySymbol)->
    (
        return blackBox.hasPointProperty(toString propertySymbol)
    );

    
    -- 'pointProperties':   
    --     returns a list of all registered point properties (as Strings)
    --
    blackBox.pointProperties = ()->
    (   
        return sort unique apply (keys bbPointProperties, key->toString key);
    );


    -- 'pointPropertiesAsSymbols' : 
    --  returns a list of all registered point properties (as Symbols)
    --

    getPropertySymbol := method ();
    getPropertySymbol(String) := Symbol=> (propertyName)->
    (
     
        propertySymbol := null;
        try  (  propertySymbol = getGlobalSymbol propertyName; ) 
        else 
        ( 
            propertySymbol = getGlobalSymbol( User#"private dictionary", propertyName); 
        );
        return propertySymbol;
    );

    blackBox.pointPropertiesAsSymbols = ()->
    (   
        return apply( blackBox.pointProperties(), propertyName-> getPropertySymbol(propertyName) );
    );


    -- setPointProperty(): 
    --  
    --  internal method to set a point property. 
    --                     Is called by 'outerSetPointProperty', 'setIsZeroAt', 'setValuesAt', 'setJacobianAt'
    -- 
    -- the method works as follows: 
    -- 1. the current (internal) variable 'bbPointProperties' is copied and transformed to a mutable HashTable
    -- 3. the propertyMethod (see 2) is added to the internal variable  'bbPointProperties'
    -- 4. the 'bbPointProperties' are changed to immutable.
    -- 5. the (internal) blackBox HashTable is extended by methods which accept one parameter and
    --     call the corresponding propertyMethod in 'pointProperty' This level of indirection is done,
    --     to keep access to the correct bbPointProperties even if the internal BlackBox object is replaced or updated: 
    --     all (updated) blackboxes will refer to the same 'bbPointProperties' variable. 
    --  

    --  after a call of 'getUpdatedBlackBox', the property is accessible by 
    --  its symbol name and eventually by the symbol, if there is no symbol clash
    -- 
    -- Remark. the first parameter is a symbol and not a name, because it is imaginable, that a user / a package author 
    --   could want to pass the concrete symbol he wants.  
    --
    setPointProperty := method();
    setPointProperty ( Symbol, Function ) := Thing => ( propertySymbol, propertyMethod )->
    (

        bblog.debug(" called setPointProperty for " | toString propertySymbol ) ;

        propertyName := toString propertySymbol;

        assert(propertyName=!=null); 

        packageSymbol := null;
        if BlackBoxParameterSpaces.Dictionary#?propertyName then 
        packageSymbol  = BlackBoxParameterSpaces.Dictionary#propertyName;

        -- step 1
        bbPointProperties = new MutableHashTable from bbPointProperties;

        bblog.debug(" set point Property " | propertyName) ;

 
        -- step 2,3
        -- remember: cannot return in a try statement!
        bbPointProperties#propertyName = ( point )-> 
        ( 
            checkInputPoint(point);
            result := (propertyMethod)(  point ); 
            return result;
        );

        -- inconsistent and unnecessary ?    
        -- bbPointProperties#propertySymbol = bbPointProperties#propertyName;
    
        if packageSymbol=!=null then 
        (
            bbPointProperties#packageSymbol = bbPointProperties#propertyName;
        );
  
        -- step 5
        if mutable blackBox then 
        (
            bblog.debug(" mutable blackBox ") ;
            blackBox#propertySymbol        = (point)->( (blackBox.pointProperty(propertyName))(point) );
            blackBox#propertyName          = (point)->( (blackBox.pointProperty(propertyName))(point) );

            --if packageSymbol=!=null then 
            --  blackBox#packageSymbol        = (point)->( (blackBox.pointProperty(propertyName))(point) );

            for symb in getPropertySymbols(propertyName) do 
            (
                assertEx(symb=!=null, " symbol " | toString symb | "is null for property "  | propertyName);
                blackBox#symb  = (point)->( (blackBox.pointProperty(propertyName))(point) );
            )
        );
        -- step 4
        bbPointProperties = new HashTable from bbPointProperties;
    );

    setPointProperty ( String, Function) := Thing => ( propertyName, propertyMethod )->
    (
        propertySymbol := getPropertySymbol(propertyName);
        setPointProperty( propertySymbol, propertyMethod );
    );


    --  valuesAtWrapper: post check the result of the 'valuesAt' method:
    --                  the result is expected to be an 1-row matrix. 
    --
    valuesAtWrapper := ( pValuesAt, point) ->
    (
        result := pValuesAt(  point); 
        try  ( assert(numRows result==1); ) else { error ( "valuesAt did not return an one-row matrix "); };
        return result;
    );

    -- setIsZeroAt
    -- 
    --   sets the check, if a point belongs to the object (isZeroAt(point)==true ) or not.
    --
    --   called by   'outerSetPointProperty'<-{'registerPointProperty', 'updatePointProperty'}, 'setValuesAt'
    -- 
    setIsZeroAt := (pIsZeroAt) ->
    (   
        -- parameter is called differently to the symbol 'isZeroAt', otherwise it seems we could get the wrong value...
        setPointProperty("isZeroAt" , pIsZeroAt );
    );
    
    
   
    
    
    smoothnessTester := basicSmoothnessTester( jetAtCalculator);
    
    
    connectSmoothnessTester := ()->
    (
        localIsCertainlySingularAtWrapper  := ( point )  ->
        (
            return smoothnessTester.isCertainlySingularAt( blackBox,  point, singularTestOptions );
        );
        setPointProperty("isCertainlySingularAt" , localIsCertainlySingularAtWrapper );

        
        localIsProbablySmoothAtWrapper  := ( point )  ->
        (
            return not smoothnessTester.isCertainlySingularAt( blackBox,  point, singularTestOptions );
        );
        setPointProperty("isProbablySmoothAt" , localIsProbablySmoothAtWrapper );
    );
    
    
    blackBox#"setSmoothnessTester" = (smoothnessTesterP)->
    (
        smoothnessTester = smoothnessTesterP;
        connectSmoothnessTester();
    );   
    blackBox.setSmoothnessTester =  blackBox#"setSmoothnessTester";
    
    
    connectJetAtCalculator := ()->
    (
        blackBox#"jetAt" = (point, jetLength)->
        (
            return jetAtCalculator.jetAt(blackBox, point, jetLength);
        );
        blackBox.jetAt = blackBox#"jetAt";                     
        
        blackBox#"jetAtWithInfo" = (point, jetLength)->
        (
            return jetAtCalculator.jetAtWithInfo(blackBox, point, jetLength);
        );        
        blackBox.jetAtWithInfo = blackBox#"jetAtWithInfo";   
        
        
        blackBox#"continueJet" = jetAtCalculator.continueJet;
        blackBox.continueJet = blackBox#"continueJet";   
        
        blackBox#"continueJetWithInfo" =jetAtCalculator.continueJetWithInfo;
        blackBox.continueJetWithInfo = blackBox#"continueJetWithInfo";    
        
        
        blackBox#"jetStatsAt" = (point, jetLength, numTrials)->
        (
            return jetAtCalculator.jetStatsAt(blackBox, point, jetLength,numTrials);
        );
        blackBox.jetStatsAt = blackBox#"jetStatsAt";    
    );
    
    blackBox#"setJetAtCalculator" = (jetAtCalculatorP)->
    (
        jetAtCalculator = jetAtCalculatorP;
        connectJetAtCalculator();
        smoothnessTester.setJetAtCalculator(jetAtCalculator); 
        -- alternatively to 'smoothnessTester.setJetAtCalculator' we pass a 'getJetAtCalculator()'
        -- to SmoothnessTester constructor (dependency injection?)
    );     
    blackBox.setJetAtCalculator = blackBox#"setJetAtCalculator";
    
    
    -- setJacobianAt():
    -- 
    --    set a method to compute the jacobian at a point.
    --
    --   called by 'outerSetPointProperty'<-{'registerPointProperty', 'updatePointProperty'}, 'setValuesAt'
    --  
    --   triggers updates for  isProbablySmoothAt, 'rankJacobianAt'.
    --  
    setJacobianAt := (pJacobianAt) ->
    ( 
        bblog.info( "setJacobianAt: updates also (   rankJacobianAt)" );      

        localJacobianAt := ( point)->
        (
            return dropDegreeInfo( pJacobianAt(point) );
        );

        setPointProperty( "jacobianAt" , localJacobianAt );

        localRankJacobianAt := (  point)->
        (
            return  rank blackBox.jacobianAt(point) ;
        );     

        setPointProperty( "rankJacobianAt" , localRankJacobianAt );
        
        connectJetAtCalculator();
        connectSmoothnessTester();                   
    );

    
    
    updateSingularityTest := ()->
    (
        if blackBox.hasPointProperty("valuesAt") then
        (
            localIsCertainlySingularWrapper  := (  point )  ->
            (
                return smoothnessTester.isCertainlySingularAt( blackBox,  point, singularTestOptions );
            );

            
            if blackBox.hasPointProperty("isCertainlySingularAt") then 
            (
                blackBox.updatePointProperty("isCertainlySingularAt", localIsCertainlySingularWrapper)
            )
            else
            (
                blackBox.registerPointProperty("isCertainlySingularAt", localIsCertainlySingularWrapper);
            );
            
            
            localIsProbablySmoothWrapper  := (  point )  ->
            (
                return smoothnessTester.isProbablySmoothAt( blackBox,  point, singularTestOptions );
            );

            
            if blackBox.hasPointProperty("isProbablySmoothAt") then 
            (
                blackBox.updatePointProperty("isProbablySmoothAt", localIsProbablySmoothWrapper)
            )
            else
            (
                blackBox.registerPointProperty("isProbablySmoothAt", localIsProbablySmoothWrapper);
            );


        );
        return;
    );
    



    -- setValuesAt:
    --    set a method to compute the values of the generators/equations at a given point.
    --
    --   called by 'outerSetPointProperty'<-{'registerPointProperty', 'updatePointProperty'}, 'setValuesAt'
    --  
    --   triggers updates for 'isZeroAt', 'numGenerators', jacobianAt', 'rankJacobianAt'.
    --  
    setValuesAt := (pValuesAt) ->
    (      
        bblog.info( "setValuesAt: updates (isZeroAt, numGenerators, jacobianAt, rankJacobianAt)" );      
        localValuesAt := (point)->return valuesAtWrapper(pValuesAt, point ) ;
        -- when using valuesAt instead of localValuesAt we get the wrong  (symbol valuesAt) (local valuesAt)
        setPointProperty( "valuesAt"  ,  localValuesAt );
        localNumGenerators =  deduceNumGenerators(blackBox)  ; --depends on valuesAt.

        blackBox.numGenerators = ()->
        (
            -- if (localNumGenerators===null) then ( error "failed to deduce number of generating equations" );
            return localNumGenerators;
        );

        bblog.info( "updated blackBox.numGenerators to " | toString blackBox.numGenerators() );   

        setIsZeroAt(
            (point)->( return blackBox.valuesAt(point)==0 ;) 
        );

        ----- jacobian at:
        localMethod :=  (point)->deduceJacobianAt( blackBox, point );
        setJacobianAt ( localMethod );  
        updateSingularityTest();
        if (not blackBox#?"interpolator") then 
        (
            blackBox.setInterpolator( createSimpleInterpolator(blackBox) );    
        );
    );
 

    -- outerSetPointProperty:
    --    this method is the common between 'updatePointProperty' and 'registerPointProperty' and was therefore outsorced
    --   setting properties isZeroAt, valuesAt, jacobianAt are handled especially.

    --    since it is allowed for the provided propertyMethod call to accept one parameter (point ) 
    --      or two parameters (blackBox, point) the two possible calls are wrapped in a function, which accepts a single parameter (point)

    outerSetPointProperty := ( propertySymbol, propertyMethod)->
    (
        propertyName := toString  propertySymbol;


        acceptedNumParameters := guessAcceptedParameterNumber propertyMethod;

        if not (acceptedNumParameters==2 or  acceptedNumParameters==1 ) then 
            error (" provided method " | propertyName | " expected to accept 1 or 2 parameters:  ( blackbox, point ),  or (point) , but the passed one seems to accept " | toString acceptedNumParameters);



        --if acceptedNumParameters=!=2 then 
        --    error (" provided method " | propertyName | " expected to accept 2 parameters ( blackbox, point ),  
        -- but the passed one seems to accept " | toString acceptedNumParameters);
        
        -- now wrap the provided method if neccesary in a way that it accepts only a point: 

        localPropertyMethod := propertyMethod;

        if acceptedNumParameters==2 then 
        (
            localPropertyMethod = ( point )-> 
            ( 
                return propertyMethod( blackBox,   point ); 
            );
        );
        
        if propertyName==="isZeroAt" then 
            return setIsZeroAt(localPropertyMethod); --probably not necessary

        if propertyName==="valuesAt" then 
            return setValuesAt(localPropertyMethod);  -- triggers initialization of 'isZeroAt' , 'numGenerators' and 'jacobianAt'
    
        if propertyName==="jacobianAt" then 
            return setJacobianAt(localPropertyMethod); -- triggers initialization of  'rankJacobianAt'

        setPointProperty( propertySymbol, localPropertyMethod );
    );

    -- todo : test ; three scenarios should work: 
    --          a user registers a point property
    --          a point property is registered in this package
    --          a point property is registered in a different package
    -- 

    blackBox.setPointProperty = method();

    blackBox.setPointProperty(String, Function) := (BlackBoxParameterSpace) => ( propertyName, propertyMethod )->
    (
        propertySymbol := getPropertySymbol(propertyName);
        assert(propertySymbol=!=null);
        outerSetPointProperty( propertySymbol, propertyMethod );
        blackBox.updateBlackBox();
        return blackBox;
    );

    --  updatePointProperty()
    --
    -- if a property is already set, updates it.
    --
    -- todo: test if updating 'valuesAt' will  trigger updating isZeroAt and jacobianAt 
    --     
    blackBox.updatePointProperty = method();

    blackBox.updatePointProperty(String, Function) := (BlackBoxParameterSpace) => ( propertyName, propertyMethod )->
    (
        if  (  bbPointProperties#?propertyName ) then
        (  
            return  blackBox.setPointProperty( propertyName, propertyMethod );
        ) 
        else 
        (
            error ("point property "| toString propertyName | " does not exist.") 
        );
    );
   

    blackBox.updatePointProperty(Function) := (BlackBoxParameterSpace) => (  propertyMethod )->
    (
        propertyName := toString propertyMethod;
        return updatePointProperty(propertyName, propertyMethod);
    );
    
    -- updateBlackBox()
    --
    -- update keys of the blackBox Hashtable in case there are known point properties but no corresponding 
    -- keys. Initial purpose (that changed): blackbox variable was by intention not writeable and modification needed
    -- copying. 
    -- Current purpose: access point property by property name string via '#' operator. 
    -- But, question, does this also add the symbolic stuff??? Something seems still weird here...(jk, 07.02.2017)
    -- 
    blackBox.updateBlackBox = () ->
    (
        return;
        -- not necessary anymore(?)
        for  property in blackBox.pointProperties() do
        (
            propkeys := unique sort {( property )} | {toString property};
           
            for key in propkeys  do
            (
                if not blackBox#?key then 
                (
                    -- hier springen wir jetzt nie(?) rein.
                    --print("here3");
                    blackBox.pointProperty(property);
                    --blackBox#key = (point)->( (blackBox.pointProperty(toString property))(point) );
                    blackBox#key =  (blackBox.pointProperty(toString property)) ;                 
                )
                else 
                (      
                );     
            );    

        );
    );


    -- registerPointProperty()
    --
    -- a method to register a point property, while providing a propertySymbol,  
    -- expecting that after registering (and getUpdatedBlackBox() ) the property will be accessible via  blackBox#propertySymbol .
    -- Usually providing the corresponding symbol is not necessary, but it could be, since each package has its own symbol scope.
    --
  
    --
    
    blackBox.registerPointProperty = method();    
    
    -- todo: this method with this interface should not be publicly visible or accessible (is an internal one)
    --
    blackBox.registerPointProperty(String, Symbol, Function) := BlackBoxParameterSpace => 
      ( propertyName, propertySymbol, propertyMethod )->
    (
        assert( (toString propertySymbol)==propertyName);

        if  ( not  bbPointProperties#?propertyName 
        and not  blackBox#?propertySymbol          and   not  blackBox#?propertyName  ) then 
        (
            outerSetPointProperty( propertySymbol, propertyMethod );
        )
        else error(" key  "| propertyName |"  exists already. If it is a point property, please use 'updatePointProperty' for updating.");
        blackBox.updateBlackBox();
        return blackBox;
    );

    -- public..
    --
    blackBox.registerPointProperty(String, Function) := Thing => ( propertyName, propertyMethod )->
    (
        propertySymbol :=  getPropertySymbol(propertyName);
        return blackBox.registerPointProperty(  propertyName, propertySymbol, propertyMethod )
    );
    
    blackBox.registerPointProperty( Function) := Thing => (  propertyMethod )->
    (
        propertyName := toString propertyMethod;
        propertySymbol :=  getPropertySymbol(propertyName);
        return blackBox.registerPointProperty(  propertyName, propertySymbol, propertyMethod )
    );

    blackBox.rpp =  blackBox.registerPointProperty;

    blackBox.upp =  blackBox.updatePointProperty;

    --
    -- memberMethods()
    --
    -- return a list of known methods. Manually updated.
    --
    blackBox.memberMethods = ()->
    (   
        methods:= { getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "memberMethods" ) ,
                    getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "attributes" ) ,
                    getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "pointProperties" ),
                    getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "pointPropertiesAsSymbols" ),
                    getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "hasPointProperty" ),
                    --getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "pointProperty" ),
                    getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "registerPointProperty" ),
                    getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "setSingularityTestOptions" ),
                    getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "rpp" ), 
                    getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "upp" ), 
                    getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "updatePointProperty" )
                    --getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "unknownIsValid" )
            };
    --  methods:= {   memberMethods,
    --                attributes,
    --                pointProperties,
    --                pointPropertiesAsSymbols,
    --                hasPointProperty,
    --                pointProperty,
    --                registerPointProperty, 
    --                updatePointProperty,
    --                unknownIsValid
    --              };

        sortedMethods := sort apply(methods, i-> ( toString i, i ));
        sortedMethods = apply(sortedMethods, i->(i_1));
        return sortedMethods;  
        --return methods;
    );
  
  
    -- attributes()
    --
    -- returns a list of known attributes. 
    -- (computed as 'keys blackBox which are not Functions'  {pointPropertiesAsSymbols() }
    --
    blackBox.attributes = ()->
    (
        all :=  keysWithoutSymbols blackBox;
        all = select (all, (foo)->(not instance(blackBox#foo,Function)));   
        kM := kPP := kPS := {};
        -- kM =  blackBox.memberMethods();
        -- kPP =  blackBox.pointProperties();
        kPS  =  blackBox.pointPropertiesAsSymbols();
        toRemove := kM | kPP |kPS;
        for symb in toRemove do
        all = delete(symb,all);
        if ( blackBox#?"valuesAt" ) then
        (
            all = all | { getGlobalSymbol( BlackBoxParameterSpaces.Dictionary, "numGenerators" ) };
        );
        sortedAttributes := sort apply(all, i-> ( toString i, i ));
        sortedAttributes = apply(sortedAttributes, i->(i_1));
        return sortedAttributes;  
    );


    -- singularityTestOptions()
    --
    -- returns currently used configuration for singularity test (at a point)
    --
    blackBox.singularityTestOptions = ()->
    (
        if not blackBox.hasPointProperty("valuesAt") then
        ( 
                error "no singularity test options: valuesAt-property not available";
        );
        return new HashTable from singularTestOptions;
    );


    -- setSingularityTestOptions()
    --
    -- sets currently used configuration for singularity test (at a point)
    --
    blackBox.setSingularityTestOptions = (prec, numTrials)->
    (
        if not blackBox.hasPointProperty("valuesAt") then
        ( 
                error "cannot set singularity test options: valuesAt-property not available";
        );

        if (prec <0) then error "setSingularityTestOptions: expected prec >= 0";
        if (numTrials <=0) then error "setSingularityTestOptions: expected numTrials > 0";

        singularTestOptions.precision = prec;
        singularTestOptions.numTrials = numTrials;
        updateSingularityTest();

    );

    
    -- todo: choose later a different component calculator depending on strategy option
    
    -- better: check component calculator type /interface.
    blackBox.setInterpolator = (interpolatorParam)->
    (
        blackBox.onComponentAnswerStrategies = interpolatorParam.onComponentAnswerStrategies;
        blackBox.setOnComponentAnswerStrategy   =  interpolatorParam.setOnComponentAnswerStrategy;
        blackBox.onComponentAnswerStrategy   =  interpolatorParam.onComponentAnswerStrategy;
        
        -- TODO well, setOnComponentPrecision() should be more generic as different component calculators may 
        -- have different configuration settings ( so may be another component calculator has no 'onComponentPrecision' property)
        blackBox.setOnComponentPrecision   =  interpolatorParam.setOnComponentPrecision;
        blackBox.onComponentPrecision   =  interpolatorParam.onComponentPrecision;
        blackBox#"interpolator"   = interpolatorParam;
        blackBox.interpolator   = interpolatorParam;
        blackBox.resetInterpolation   = interpolatorParam.resetInterpolation;
        blackBox.isOnInterpolatedComponent  = interpolatorParam.isOnComponent;
        blackBox.interpolatedComponents     =  interpolatorParam.components;
        blackBox.interpolatedComponentNames  =  interpolatorParam.componentNames;
        blackBox.componentNameInUse  =  interpolatorParam.componentNameInUse;
        blackBox.interpolatedComponentByName = interpolatorParam.componentByName;
        blackBox.renameInterpolatedComponent =  interpolatorParam.renameComponent;
         -- todo: when we change the interpolator, this will stop to work:
        blackBox.interpolateComponentsAt  = interpolatorParam.interpolateComponentsAt;    
        blackBox.refineInterpolation  = interpolatorParam.refineInterpolation;    
        blackBox.interpolateComponentAt      =  interpolatorParam.interpolateComponentAt;
        blackBox.interpolatedComponents         =   interpolatorParam.components;  
        
        localinterpolatedComponentsAt := (point) -> interpolatorParam.componentsAt(point);
        
        blackBox.setPointProperty("interpolatedComponentsAt", localinterpolatedComponentsAt );    
        
        localinterpolatedComponentNamesAt := (point) -> interpolatorParam.componentNamesAt(point);
        
        blackBox.setPointProperty("interpolatedComponentNamesAt", localinterpolatedComponentNamesAt);
                
        -- TODO well , setSameComponentPrecision should be more generic as different component calculators may 
        -- have different configuration settings ( so may be another component calculator has no 'onComponentPrecision' property)
        --blackBox.setSameComponentPrecision =  interpolatorParam.setSameComponentPrecision;
    );
    

    
    -- a user should not call this method...

    return blackBox;
)

TEST ///
  -- test interpolateComponents
  kk := ZZ
  R := ZZ[x,y]
  I:= ideal (x*y*(x^2-y^2))
  bb = new BlackBoxIdeal from I
  point1 = matrix {{1,1_ZZ}}
  point2 = matrix {{1,0_ZZ}}
  pointList = {point1,point2}
  maxDegree := 4
  bb.interpolateComponentsAt(pointList, maxDegree);
///
 


-- blackBoxParameterSpaceInternal()
-- 
-- this function may be used to create a derived object, which inherits properties of an black box ideal
-- since the blackbox object is not copied 
--
blackBoxParameterSpaceInternal(Type, Ring) := HashTable => ( resultType, pRing ) ->
(
    blackBox := blackBoxParameterSpaceInternal(resultType, #(gens pRing), coefficientRing pRing);
    
    blackBox#"ring" = pRing;
    blackBox.ring = pRing;

    assert( blackBox.numVariables == #( gens blackBox#"ring") );
    
    blackBox.unknownIsValid = (unknown)->
    (
        if not ( blackBox#"ring" === ring unknown) then 
        ( 
            bblog.error( "the unknown is not element of the equations ideal ring" );
            return false;
        );
        return true;
    );
    
    return blackBox;
)

listToStack := (L)->
(
    return ( apply(L, i -> ("--   " | toString i)));
)


net (BlackBoxParameterSpace) := Net =>(bb)->
(
    L := {"--" | toString class bb};
    L = L | {"--"};
    L = L | {"-- attributes:"};
    L = L | {"-- "} | listToStack bb.attributes();
    L = L | {"--"};
    L = L | {"-- methods:"};
    L = L | {"-- "} | listToStack  bb.memberMethods();
    L = L | {"--"};
    L = L | {"-- point properties:"};
    L = L | {"-- "} | listToStack  bb.pointProperties();

    return stack L;
);


blackBoxIdeal = method();

 
blackBoxParameterSpace = method();

--
-- this function is final, that means nobody should use this method for creating a derived object
--
new BlackBoxParameterSpace from Ring := (E, pRing )->
(
    blackBox := blackBoxParameterSpaceInternal(BlackBoxParameterSpace, pRing);
   
    return blackBox;
)

--
-- this function is final, that means nobody should use this method for creating a derived object
--
blackBoxParameterSpace(Ring) := HashTable => ( pRing ) ->
( 
    return new BlackBoxParameterSpace from pRing;
);


--blackBoxParameterSpaceInternal(ZZ,Ring) := HashTable => ( numVariables, coeffRing ) ->
--(
--    assert ( numVariables>0 );
--    a := null;
--    a = symbol a;
--    rng := coeffRing[a_1..a_numVariables];
--    return blackBoxParameterSpaceInternal( rng );
--)

-- this function is final, that means nobody should use this method for creating a derived object

blackBoxParameterSpace(ZZ, Ring) := BlackBoxParameterSpace => ( numVariables, coeffRing )  ->
(
    blackBox := blackBoxParameterSpaceInternal(BlackBoxParameterSpace, numVariables, coeffRing );
    return blackBox;
)


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
    
beginDocumentation()

load "BlackBoxParameterSpaces/IdealsDoc.m2"
load "BlackBoxParameterSpaces/ParameterSpacesDoc.m2"
load "BlackBoxParameterSpaces/JetsDoc.m2"
load "BlackBoxParameterSpaces/InterpolationDoc.m2"

load "BlackBoxParameterSpaces/Tests.m2"

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

