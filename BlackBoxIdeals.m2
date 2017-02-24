newPackage(
     "BlackBoxIdeals",
     Version => "0.2", 
     Date => "23.02.2017",
     Authors => {{
           Name => "Jakob Kroeker", 
           Email => "jakobkroeker.academic@spaceship-earth.net", 
           HomePage => "http://www.crcg.de/wiki/User:Kroeker"},{
           Name => "Hans-Christian Graf v. Bothmer", 
           Email => "bothmer@math.uni-hannover.de", 
           HomePage => "http://www.crcg.de/wiki/Bothmer"}    
      },
     Configuration => {},
     PackageExports => {"M2Logging"},
     Headline => "black boxes for explicit and implicitly given ideals",
     DebuggingMode => true,
     CacheExampleOutput => true,
     AuxiliaryFiles=>true
)


needsPackage "M2Logging";


export { 
    "bare",
    "jetStatsAt",
    "locus",
    "joinJetSets",
    "firstElem",
    "JetSet",
    "AddElement",
    "compatible",
    "isDerivedFrom",
    "Point",
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
    "bestJetAt",
    "jetAt",
    "jetAtOrException",
    "jetAtWithInfo",
    "jetAtOrNull",
    "isCertainlySingularAt",
    "isProbablySmoothAt",
    "keysWithoutSymbols",    
    "guessAcceptedParameterNumber",
    "dropDegreeInfo",
    "createInterpolatedIdeal",
    "interpolateBB",
    "interpolate",
    "MapHelper",
    "InterpolatedIdeal",
    "SingularPointException",
    "PointNotOnBlackBox",
    "createMapHelper"
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
    protect setComponentCandidates;
    protect componentCandidates;
    protect setOnComponentPrecision;
    protect interpolateComponents;
    protect componentCandidatesAt;

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
    protect deduceNumGenerators;
    protect setIsZeroAt;
  
    protect type;
    protect unknowns;
    protect pointProperty;
    protect updatePointProperty;
    protect setJacobianAt;
    protect equations;

    protect hasPointProperty;
    protect knownPointPropertiesAsSymbols;
    protect knownMethods;
    protect knownAttributes;
    --protect knownProperties;
    --protect createMapHelper;
    protect updateBlackBox;
    protect setComponentCalculator;
);

--todo: fix dublicate code,  -  padicLiftProtect and padicLiftExport

idealBlackBoxesExport = ()->
(
    exportMutable("isOnComponent");
    exportMutable("enableChecks");
    exportMutable("disableChecks");
    exportMutable("withChecks");
    exportMutable("setComponentCandidates");
    exportMutable("componentCandidates");
    exportMutable("setOnComponentPrecision");
    exportMutable("interpolateComponents");

    exportMutable("componentCandidatesAt");
 
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
    exportMutable("deduceNumGenerators");
    exportMutable("setIsZeroAt");
    exportMutable("type");
    exportMutable("unknowns");
    exportMutable("pointProperty");
    exportMutable("updatePointProperty");
    exportMutable("setJacobianAt");
    exportMutable("equations");

    exportMutable("hasPointProperty");
    exportMutable("knownPointPropertiesAsSymbols");
    exportMutable("knownMethods");
    exportMutable("knownAttributes");
    exportMutable("numTrials");
    exportMutable("setSingularityTestOptions");
    exportMutable("updateSingularityTest");
    exportMutable("singularityTestOptions");
    exportMutable("updateBlackBox");
    exportMutable("setComponentCalculator");
)

-- the following symbols which are marked as undocumented are in fact documented 
-- inside the BlackBoxParameterSpace and BlackBoxIdeal
-- please do only mark documented symbols as undocumented, 
-- at least there should be a comment note inside this package.
-- 
undocumented { 
    setIsZeroAt,      -- internal   
    setJacobianAt,    -- internal   
    setValuesAt,      -- internal   
    setPointProperty, -- internal   
    deduceNumGenerators, -- internal   
    dropDegreeInfo,      -- internal   
    equations,
    keysWithoutSymbols,
    checkInputPoint,
    knownPointPropertiesAsSymbols,
    type,
    unknownIsValid,
    unknowns,
    numTrials,
    pointProperty, -- internal function
    guessAcceptedParameterNumber, -- internal function
    updateSingularityTest, --internal function
    createMapHelper
} 


-- swith between protect and export - both are not possible!

--idealBlackBoxesProtect() -- protect the symbols for checking package correctness: no symbol variables should be overwritten by accident!
idealBlackBoxesExport(); -- export the symbols to make the package work 


needsPackage "SimpleDoc";
needsPackage "Text";


BlackBoxLogger = Logger("BlackBoxIdeals");

-- todo: how to switch this on and off by the user? using closures again? 
--if BlackBoxIdeals#Options#DebuggingMode then 
--    BlackBoxLogger.setLogLevel(LogLevel.DEBUG);

if BlackBoxIdeals#Options#DebuggingMode then
    errorDepth=0;



bblog := BlackBoxLogger; --shortcut

-- bblog.setLogLevel(LogLevel.DEBUG);


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

    bblog.debug  ("disassemble result: " | lst );
    lst = separate( " ", lst );

    restargsPos := position( lst, (str)-> str=="restargs:" );
    numparmsPos := position( lst, (str)-> str=="numparms:" );

    if restargsPos=!=null then 
    (
        newlst := drop (lst, restargsPos);
        restargsPos = position( newlst, (str)-> str=="restargs:" ); 
        
        if restargsPos=!=null then 
        (
            bblog.info ("do not know how to handle methods with a chain of several '->' ");
            --return null; 
        );
    );

    if numparmsPos===null then 
    (
        bblog.warning (" warning: did not find the position of 'numparms:' in dissasseble string ");
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
        bblog.info ("did not expect a method with multiple installed functions. ");
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

    foo = a->(a,b);

    assert( 1==guessAcceptedParameterNumber foo );

)


TEST ///
    debug BlackBoxIdeals
    idealBlackBoxesProtect()
    testGuessAcceptedParameterNumber()
///


SingularPointException = new Type of HashTable;


PointNotOnBlackBox = new Type of HashTable;



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
-- and the symbol from the package dictionary "BlackBoxIdeals.Dictionary"
-- see also https://github.com/jakobkroeker/FiniteFieldExperiments.M2/issues/116
-- 
-- TODO question: what happens in case the user loads a package which defines the same symbol?

-- artificial example:  
-- restart
-- loadPackage "BlackBoxIdeals"
-- fragileHT = new HashTable from {
--   getGlobalSymbol(User#"private dictionary", "valuesAt") => 5
-- }
-- fragileHT.valuesAt -- error: key not found in hash table
-- 

getPropertySymbols := method ();

getPropertySymbols(String) := List => (propertyName)->
(
    propertySymbols := {} ;
    try (  propertySymbols = propertySymbols | { getGlobalSymbol(BlackBoxIdeals.Dictionary, propertyName)} );

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
    debug BlackBoxIdeals
    testEpsRing();
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

-- deduceNumGenerators():
--
--   for an (ideal) blackbox, determine the number of   generators (or equations). This is possible in case 
--   the blackbox provides the 'valuesAt'-property.
--
--   todo: this is a generic version. if the blackbox is given by an ideal I,
--        the generators can be determined easily by #(gens ideal I)
--   todo: what should the user do, if deduceNumGenerators() fails? 
--
deduceNumGenerators := (blackBox)->
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


testDeduceNumGenerators=()->
(
    blackBoxDummy := new MutableHashTable;
    blackBoxDummy.hasPointProperty = (propertyName)->
    (
        if propertyName==="valuesAt" then return true;
        return false;
    );
    blackBoxDummy.valuesAt= (point)-> ( return matrix {{1,2,3,4,5}} );
    
    blackBoxDummy.coefficientRing = ZZ;
    blackBoxDummy.numVariables = 5;
    numGenerators := deduceNumGenerators(blackBoxDummy);
    assert( numGenerators==5 );
);


TEST ///
    debug BlackBoxIdeals
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


-- deduceJacobianAt()
--
-- Constructs a jacobian at a point supposing that the blackBox implements evaluation at a point.
-- Currently expects that blackBox implements 'valuesAt' and knows number of generators/equations(numGenerators).
-- The second dependency could be removed, as the column number  of the returned 
-- 'valuesAt' evaluation ( a row vector) should be the same as number of generators/equations.
-- 
-- Remark: this stuff is very hard to debug because of the try clauses in the black box.
--
deduceJacobianAt = ( blackBox, point )->
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

    blackBoxDummy := new MutableHashTable;

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
    debug BlackBoxIdeals
    idealBlackBoxesProtect()
    testDeduceJacobianAt()
///


-- introduce a new type representing a parameter space.

BlackBoxParameterSpace = new Type of  MutableHashTable;


Point = new Type of HashTable;

pointObject = method();
pointObject (Thing, Matrix) := Point =>(parent, point)->
(
    resultPoint := new HashTable from {("parent", parent),
                                       ("point", point)};
    resultPoint 
)


-- parent is needed for coercion


Jet = new Type of HashTable;

jetObject := method();

jetObject( Thing, Thing, Thing, ZZ) := Jet=>(parent, point, value, jetLength)->
(
      jetObj := new HashTable from { ("value",value),
                                     ("jetLength",jetLength),                                   
                                     ("parent",parent),
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

locus = method();

locus (Jet) := ZZ =>(jet)->
(
    return jet#"point";
);

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
    j = jetAt(bbI,point,3,1)
    assert(length j == 3)
///


sub (Ideal, Jet ) := Thing =>(I,jet)->
(
    return (sub(I,jet#"value"));
)

-- question: what is a (succeeded) jet of length 1 at a singular point??







jetAtResultFunction := (bestJet, failedJetLength)->
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

jetAtResult := method();
jetAtResult ( Jet, ZZ ) := HashTable => (bestJet, failedJetLength)->
(    
    return jetAtResultFunction(bestJet, failedJetLength);
);

-- special case for null...
jetAtResult ( Jet, Nothing ) := HashTable => (bestJet, failedJetLength)->
(
     return jetAtResultFunction(bestJet, failedJetLength);
);


-- jetAtSingleTrial(): 
--
--   tries once to compute a jet , ( see http://en.wikipedia.org/wiki/Jet_%28mathematics%29 for jet definition;) 
--   for the used computation algorithm see the bacherlor thesis at 'http://www.centerfocus.de/theses/js.pdf' .
--
--   preconditions: black box provides evaluation at a point. ('valuesAt') 
-- 
--  returns a hashtable with entries
-- 
--  - "succeeded" a boolean, 
--  - "failedJetLength"  contains the jet length at which the computation failed, otherwise null
--  - "jet"  contains the jet . The jet of the length n has the form
--           j = point + eps*y_1 + . . . + eps^n * y_n 
--           such that F(j) = 0, 
--           where F: E_(n+1)^m -> E_(n+1)^k 
--           with E_(n+1) = K[eps]/( eps^(n+1) ) 
--           whereby K is the coefficient ring (blackBox.coefficientRing), 
--           m is the number of variables (blackBox.numVariables) of the parameter space (same as entries in the point vector)
--           and k is the number of the generators/equation of the (implicitly or explicitly) given ideal. 
--

 
jetAtSingleTrial = method();

-- jetAtSingleTrial():
--
-- here we improve precision by 1 in each step
-- using Newtons-Algorithm one could double precision in each step, but
-- for this we also need high precision jacobi-matrices.
-- For black-Box-Jacobi-Matrices we might not have high precision Jacobi-Matrices
-- todo question: what do we mean by high precision Jacobi-Matrices?
--
jetAtSingleTrial( BlackBoxParameterSpace, Matrix, ZZ ) := HashTable => ( blackBox,  point, jetLength )  ->
--jetAtSingleTrial( HashTable, Matrix, ZZ ) := HashTable => ( blackBox,  point, jetLength )  ->
(

    assert ( jetLength >= 0 );
    
    if not (blackBox.isZeroAt(point)) then 
    (
        --error(" point is not on BlackBox ");
        throw new PointNotOnBlackBox from {"errorMessage" => "jetAtSingleTrial: point " | toString point | "does not belong to the object! "}
    );
    

    liftingFailed := false;
    failedJetLength := null;
    jet := point;
    
    jetObj := null;

    succeededJetLength := 0;    
    jacobianM2Transposed := transpose blackBox.jacobianAt(point) ;
    
    if (jetLength==0) then 
    (
        jetObj = jetObject (blackBox,  point, jet, succeededJetLength);      
        return  jetAtResult(jetObj, failedJetLength);
    );
     

    jacobianKernel := generators kernel jacobianM2Transposed ; -- syz would also work

    epsPrecision := 1;  
      
    epsRng := getEpsRing( blackBox.coefficientRing,  epsPrecision );
    eps := (gens epsRng)#0;

    coeffRng := (blackBox.coefficientRing); -- we need the braces here !!!

     
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
    if ( blackBox.valuesAt(jet)!=0 ) then 
    (      
        liftingFailed = true;
        failedJetLength = epsPrecision;
    )
    else
    (
        succeededJetLength = 1;
        jet = lengthOneLift;
    );
    
 
    if (not liftingFailed) then 
    (
        for  epsPrecision in 2..jetLength do 
        (
            epsRng = getEpsRing( coeffRng, epsPrecision);
            eps = (gens epsRng)#0;
    
            jet =  sub(jet,epsRng);

            valuesAtJet := blackBox.valuesAt(jet );

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
    
            jet2 := sub (jet, epsRng ) - sub( x, epsRng ) * eps^epsPrecision;
            assert ( 0 == blackBox.valuesAt(jet2) ); -- debug
            jet = jet2;
        );
    );

    bestJetObject := jetObject (blackBox,  point, jet, succeededJetLength);
    
    return  jetAtResult(bestJetObject, failedJetLength);
);



jetAtSingleTrial( BlackBoxParameterSpace, Jet, ZZ ) := HashTable => ( blackBox,  richJet, jetLength )  ->
(  
    assert ( 0 == blackBox.valuesAt(richJet#"value") );
    assert ( jetLength >= 0 );
    assert ( jetLength > length richJet );       

    point := locus richJet;
    -- einfacher
    if (length richJet<=1) then return jetAtSingleTrial ( blackBox,  point, jetLength );
    
    liftingFailed := false;
    failedJetLength := null;    
    
    jetObj := null;

    succeededJetLength := length richJet;    
    jacobianM2Transposed := transpose blackBox.jacobianAt(point) ;
    
    
    jacobianKernel := generators kernel jacobianM2Transposed ; -- syz would also work

    epsPrecision := length richJet;  
          
    coeffRng := (blackBox.coefficientRing); -- we need the braces here !!!
    
    jet := richJet#"value";

    if (not liftingFailed) then 
    (
        for  epsPrecision in (1 + succeededJetLength)..jetLength do 
        (
            epsRng := getEpsRing( coeffRng, epsPrecision);
            eps := (gens epsRng)#0;
    
            jet =  sub(jet,epsRng);

            valuesAtJet := blackBox.valuesAt(jet );

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
    
            jet2 := sub (jet, epsRng ) - sub( x, epsRng ) * eps^epsPrecision;
            assert ( 0 == blackBox.valuesAt(jet2) ); -- debug
            jet = jet2;
        );
    );

    bestJetObject := jetObject (blackBox,  point, jet, succeededJetLength);
    
    return  jetAtResult(bestJetObject, failedJetLength);
);




-- 'jetAt()'
--
--   tries to compute a jet at a given point P several times (= numTrials). 
--   The given point P belongs to the zero set of the black box.
-- 
--   for the returned result and input restrictions see 'jetAtSingleTrial'
--



-- computes a jet with given jetLength several times. 
-- If one computation fails, throws an exception.
-- returns the last one (very stupid..., all previous jets lost)
-- 
jetAtOrException = method();

-- throws an exception if one trial fails or returns the last computed jet otherwise.
jetAtOrException( BlackBoxParameterSpace, Matrix, ZZ, ZZ) := Jet => ( blackBox,  point, jetLength, numTrials )  ->
(
    if ( numTrials<1 ) then error "jetAt: expected numTrials >=1 ";
    
    jetResult  := jetAtSingleTrial ( blackBox,  point, jetLength);      
    
    if (jetResult#"jet"=== null) then 
    (
       throw new SingularPointException from {"errorMessage"=>"Point is not smooth",
                                              "failedJetLength" => jetResult#"failedJetLength",
                                              "failedJet" => jetResult#"bestJet",                                            
                                                };
    );        
    
    for i in 2..numTrials do
    (
        jetResult = jetAtSingleTrial ( blackBox,  point, jetLength);
         if (jetResult#"jet"=== null) then 
         (
                throw new SingularPointException from {"errorMessage"=>"Point is not smooth",
                                                   "failedJetLength" => jetResult#"failedJetLength",
                                                    "failedJet" => jetResult#"bestJet"                                              
                                              }
         );
    );
    return jetResult#"jet";
);

jetAtOrException( BlackBoxParameterSpace, Matrix, ZZ) := Jet => ( blackBox,  point, jetLength )  ->
(
    return jetAtOrException( blackBox,  point, jetLength, 1 );
);

-- throws an exception if one trial fails or returns the last computed jet otherwise.
jetAtOrException( BlackBoxParameterSpace, Jet, ZZ, ZZ) := Jet => ( blackBox,  richJet, jetLength, numTrials )  ->
(
    if ( numTrials<1 ) then error "jetAt: expected numTrials >=1 ";
    
    jetResult  := jetAtSingleTrial ( blackBox,  richJet, jetLength);      
    
    if (jetResult#"jet"=== null) then 
    (
       throw new SingularPointException from {"errorMessage"=>"Point is not smooth",
                                              "failedJetLength" => jetResult#"failedJetLength",
                                              "failedJet" => jetResult#"bestJet",                                            
                                                };
    );        
    
    for i in 2..numTrials do
    (
        jetResult = jetAtSingleTrial ( blackBox,  richJet, jetLength);
         if (jetResult#"jet"=== null) then 
         (
                throw new SingularPointException from {"errorMessage"=>"Point is not smooth",
                                                   "failedJetLength" => jetResult#"failedJetLength",
                                                    "failedJet" => jetResult#"bestJet"                                              
                                              }
         );
    );
    return jetResult#"jet";
);



jetAtOrException( BlackBoxParameterSpace, Jet, ZZ) := Jet => ( blackBox,  richJet, jetLength )  ->
(
    return jetAtOrException( blackBox,  richJet, jetLength, 1 );
);



-- returns null if one of the numTrials failed or the jet from the last trial (very stupid..., all previous jets lost)

jetAtOrNull = method();
jetAtOrNull( BlackBoxParameterSpace, Matrix, ZZ, ZZ) := Jet => ( blackBox,  point, jetLength, numTrials )  ->
(
   if ( numTrials<1 ) then error "jetAtOrNull: expected numTrials >=1 ";
    
    jetResult  := jetAtSingleTrial ( blackBox,  point, jetLength);      
    
    if (jetResult#"jet"=== null) then   return jetResult#"jet";
    
    for i in 2..numTrials do
    (
        jetResult = jetAtSingleTrial ( blackBox,  point, jetLength);
        if (jetResult#"jet"=== null) then break;
    );
    return jetResult#"jet";
);


jetAtOrNull( BlackBoxParameterSpace, Matrix, ZZ) := Jet => ( blackBox,  point, jetLength )  ->
(
    return jetAtOrNull( blackBox,  point, jetLength, 1 );
);


-- returns the first failed jet or the last jet in case no trial failed.
jetAtWithInfo = method();
jetAtWithInfo( BlackBoxParameterSpace, Matrix, ZZ, ZZ) := Jet => ( blackBox,  point, jetLength, numTrials )  ->
(
   if ( numTrials<1 ) then error "jetAtWithInfo: expected numTrials >=1 ";
    
    jetResult  := jetAtSingleTrial ( blackBox,  point, jetLength);      
    
    if (jetResult#"jet"=== null) then   return jetResult;
    
    for i in 2..numTrials do
    (
        jetResult = jetAtSingleTrial ( blackBox,  point, jetLength);
        if (jetResult#"jet"=== null) then break;
    );
    return jetResult;
);


jetAtWithInfo( BlackBoxParameterSpace, Matrix, ZZ) := Jet => ( blackBox,  point, jetLength )  ->
(
    return jetAtWithInfo( blackBox,  point, jetLength, 1 );
);


-- use the exception variant as default for jetAt, because computation at a singular point should hurt.
-- change: do we need numTrials for jetAt? If numTrials >1 we still keep only one jet. That is very inefficient.
jetAt = jetAtOrException;  



-- returns the first failed jet or the last jet in case no trial failed.
jetStatsAt = method();
jetStatsAt( BlackBoxParameterSpace, Matrix, ZZ, ZZ) := HashTable => ( blackBox,  point, jetLength, numTrials )  ->
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
        jetResult = jetAtSingleTrial ( blackBox,  point, jetLength);
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
                AddElement(jetStats#"jetSets"#currentJetLength, currentJet);
            );                                    
        );
    );
    jetStats#"jetSets" = new HashTable from    jetStats#"jetSets";
    return new HashTable from jetStats;
);


jetStatsAt( BlackBoxParameterSpace, Matrix, ZZ) := Jet => ( blackBox,  point, jetLength )  ->
(
    return jetStatsAt( blackBox,  point, jetLength, 1 );
);





isCertainlySingularAt = method();
isCertainlySingularAt( BlackBoxParameterSpace, Matrix, ZZ, ZZ) := MutableHashTable => ( blackBox,  point, jetLength, numTrials ) ->
--isCertainlySingularAt( HashTable, Matrix, ZZ, ZZ) := MutableHashTable => ( blackBox,  point, jetLength, numTrials ) ->
(
    if ( numTrials<1 ) then error "isCertainlySingularAt: expected numTrials >=1 ";
    
    for i in 1..numTrials do
    (
        jetOrError := catch jetAt( blackBox,  point, jetLength);
        if (class jetOrError === SingularPointException) then 
        (
            return true;
        );
        if (class jetOrError =!= Jet) then 
        (
            return jetOrError; -- it is a different error 
        );
    );
    return false;
);




isCertainlySingularAt( BlackBoxParameterSpace, Matrix, HashTable) := MutableHashTable => ( blackBox,  point, options ) ->
--isCertainlySingularAt( HashTable, Matrix, HashTable) := MutableHashTable => ( blackBox,  point,  options ) ->
(
    return isCertainlySingularAt(blackBox, point, options.precision, options.numTrials);
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




-- disable BlackBoxParameterSpace construction using new for an arbitrary parameter:
new BlackBoxParameterSpace from Thing := ( E, thing) -> 
(
    error "creating blackbox from  type " | toString E | " not implemented ";
);

-- disable BlackBoxParameterSpace construction using new without parameters:
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




-- InterpolatedIdeal = new Type of MutableHashTable;

InterpolatedIdeal = new Type of HashTable;

new InterpolatedIdeal from Thing :=  (InterpolatedIdealAncestor,l)->
(  
    ---type check?
    error("InterpolatedIdeal from Thing not supported or not implemented");
);


--new InterpolatedIdeal from MutableHashTable :=  (InterpolatedIdealAncestor,l)->
--(  
    ----type check?
    --return l;
--);

--new InterpolatedIdeal from List :=  (InterpolatedIdealAncestor,l)->
--(  
--    return new InterpolatedIdeal from new MutableHashTable from l;
--);



-- find polynomials containing a component
-- via interpolation

-- interpolate()
--
-- find linear combinations of monomials containing a given jet
-- it seems that currently jetList have only one entry.
--
interpolate = (mons, jetList) -> 
(
    bblog.debug("--interpolate call;");
    bblog.debug("--interpolate call; \n --mons : " | toString mons );
    bblog.debug("--interpolate call;--jetList :" | toString jetList);
    bblog.debug("--interpolate call ok");    
    R := ring mons;
    K := coefficientRing R;
    failedJets := select(jetList, jetP->jetP===null);    
    if #failedJets >0 then 
    ( 
        error throw new SingularPointException from {"errorMessage"=>"jets not succeed. Point is not smooth?";}
    );
    --
    -- substitute jets into the monomials and take coefficients
    -- die jets werden in jedes monom eingesetzt. (matrix : #monome x 1 für einen jet.)
    -- substitutedList := apply(jetList, jetP ->   sub(mons, jetP#"value"));
    -- nun haben wir für je einen jet J anstatt n monome n polynome in eps. 
    -- 'last coefficients' gruppiert die Koeffizienten nach Grad von 'eps', d.h. für jeden Grad von eps 
    -- haben wir eine Matrix-Zeile mit #monome Einträgen.
    -- BlackBoxLogger.debug("interpolate, substituded:" | toString substitutedList);
    -- coeffs := apply(substitutedList, sp-> coefficients sp);    
    -- BlackBoxLogger.debug("interpolate, coeffs:" | toString coeffs);
    --
    coeffList := apply(jetList,jetP ->  sub(last coefficients sub(mons, jetP#"value"),K));
    BlackBoxLogger.debug("interpolate, coeffList:" | toString coeffList);    
    --
    -- haben wir einen weiteren Jet, so muss die gleiche Linearkombination der Spalten für den ersten
    -- auch für den zweiten Jet gelten
    --  => wir hängen die Koeffizientenmatrix für den zweiten eingesetzten jet einfach an die erste Matrix unten dran
    --    (weitere Zeilen kommen hinzu, Kommando 'fold'; mit || als Konkatenation)
    --
    folded := fold((a,b)->(a||b),coeffList);
    BlackBoxLogger.debug("interpolate, folded coefflist" | toString folded);
    -- nun suchen wir für die Spalten eine Linearkombination, so dass jede Zeile zu 0 wird. (jet auf der Komponente).
    -- find interpolation solution    
    s := syz folded; -- todo question: is the syz command necessary here? 
    BlackBoxLogger.debug("interpolate, syzygies" | toString s);
    -- make polynomials from the solution
    -- (jk) todo: is mingens fast enough?
    I := ideal mingens ideal(mons*s);
    return I;
)


-- interpolateBB()
--
-- find all polynomials of degree smallerEqual than maxDegree containing the component of BB containing the point
-- 
-- this is the most basic simple minded implementation where only one very long jet is considered.
--


createInterpolatedIdeal = method();
createInterpolatedIdeal( Ideal, ZZ, Thing, String ) := InterpolatedIdeal => 
                       (I,maxDegree, witness, description)->
(
    mutableDescription := description;
    
    getDescription := ()->
    (
        return mutableDescription;
    );
    setDescription := (newDescription)->
    (
        mutableDescription = newDescription
    );
    
     result := new HashTable from {    
        "ideal" => I,        
        "maxDegree" => maxDegree,
        "witness" => witness,
        "description" => description,
        --"getDescription" => getDescription,
        --"setDescription" => setDescription
    };
    result = newClass(InterpolatedIdeal, result);
    return result;
);

createInterpolatedIdeal( Ideal, ZZ, String ) := InterpolatedIdeal => 
                       (I,maxDegree,  description)->
(
    return createInterpolatedIdeal (I,maxDegree, null, description);
);

ideal ( InterpolatedIdeal) := Ideal => (ii)->
(
    return ii#"ideal";
)

bare = method();
bare ( InterpolatedIdeal) := Ideal => (ii)->
(
    return ii#"ideal";
)

TEST ///
    -- loadPackage "BlackBoxIdeals"
    R = ZZ[x,y]
    I = ideal (x*y)
    II = createInterpolatedIdeal(I, 1, 1, "as")
    ideal II
    bare II     
///


InterpolateBBOptions = new Type of HashTable;

interpolateBBoptions = method();
interpolateBBoptions(ZZ, ZZ) := InterpolateBBOptions=>(maxDegree, jetLength)->
(
    return new HashTable from {("maxDegree",maxDegree),("jetLength",jetLength) };
)



interpolateBB = method();

-- another interpolateBB variant: pass jet as parameter.

interpolateBB(BlackBoxParameterSpace, Matrix, ZZ, MapHelper) := InterpolatedIdeal =>
             (BB,point,maxDegree,mmap) -> 
(
    R := mmap#"imageRing";
    mons := matrix {flatten apply(maxDegree+1, currentDegree->flatten entries basis(currentDegree,R))};
    -- find one jet with precision 10 more then number of monomials
    jetP := jetAtOrException(BB,point,rank source mons+10,2);
    -- !!!this heuristic must be tested!!!
    -- Test: see if interpolated polynomials are in at least one
    -- irreducible component of the BlackBoxIdeal.
    jetPimage :=  (mmap#"valueAtJet")(jetP);
    bareIdeal:= interpolate(mons,{jetPimage});
   
    idealWitness := new JetSet from jetP;
    interpolatedIdeal := createInterpolatedIdeal(bareIdeal, maxDegree, idealWitness, "");
    return interpolatedIdeal;
)

interpolateBB(BlackBoxParameterSpace,Matrix,ZZ,Matrix) := Ideal =>
             (BB,point,maxDegree,mmap) -> 
(
    interpolateBB(BB,point,maxDegree,new MapHelper from mmap)
)


interpolateBB(BlackBoxParameterSpace,Matrix,ZZ) := Ideal =>
             (BB,point,maxDegree) -> 
(
    interpolateBB(BB,point,maxDegree,createMapHelper(vars BB.ring,BB.ring))
)


-- createSimpleComponentCalculator should be private, since each blackbox needs an own component calculator.

-- or, the jets should be stored in the blackBox


-- development remark: we need jets at least per blackbox individually.

pointProperties = method();

pointProperties (BlackBoxParameterSpace) := List => (bb)->
{
    return bb.pointProperties();
}


attributes = method();

attributes (BlackBoxParameterSpace) := List => (bb)->
{
    return bb.knownAttributes();
}


memberMethods = method();

memberMethods (BlackBoxParameterSpace) := List => (bb)->
{
    return bb.knownMethods();
}


createSimpleComponentCalculator = (blackBoxParameter) ->
(

    blackBox := blackBoxParameter;

    simpleComponentCalculator := MutableHashTable;
    
    -- cached component candidates
    
    componentCandidates := new MutableHashTable; -- should not be a HashTable but a type

    -- cached jets

    jets := new MutableHashTable;

    simpleComponentCalculator.setComponentCandidates = (cc)->
    ( 
        -- jets = new MutableHashTable; -- clear old cache for point jet        
        componentCandidates = cc;
    );

    simpleComponentCalculator.componentCandidates = ()->
    (
        return new HashTable from componentCandidates;
    );

    onComponentPrecision := 5;

    simpleComponentCalculator.setOnComponentPrecision = (precision)->
    (
        onComponentPrecision=precision;
    );

    -- todo: rename to isProbablyOnComponent
    --
    simpleComponentCalculator.isOnComponent = method();

    
    --
    -- uses a single long jet to test if a point is on a component.
    --
    simpleComponentCalculator.isOnComponent ( Ideal, Matrix, ZZ) := Boolean => (componentIdeal, point, onComponentPrecisionParam)->
    (
        if (sub( componentIdeal,point)!=0) then return false;
        
        bblog.debug ("enter simpleComponentCalculator.isOnComponent");
        if not (jets#?point) then
        (        
            -- we have no cached jets for the given point => compute jets
            jets#point = jetAt( blackBox ,point, onComponentPrecisionParam, 1);
            -- here we could also update statistic if the jet succeeded or not...
        );
        jetP := jets#point;  
        
        if (jetP === null) then
        (
            bblog.debug ("leave 1 simpleComponentCalculator.isOnComponent");
            throw new SingularPointException from {"errorMessage"=>"jets not succeeded. Point "|  toString point | "is not smooth?";};
        );

        if jetP#"jetLength" < onComponentPrecisionParam then 
        (
            -- we have cashed jets, but they are too short.. => compute jets of requested length
            -- TODO improvement/optimisation: start from existing jet and enlarge it.
            --jets#point = jetAt( blackBox ,point, onComponentPrecisionParam, 1);
            jets#point = jetAt( blackBox ,jetP, onComponentPrecisionParam, 1);
            jetP = jets#point;  
        );

        --if jetP#"succeeded" then 
        if (jetP =!= null) then
        (
            -- check if the point is on the component or not
            -- print "succeeded";            
            -- testing throw new SingularPointException from {"errorMessage"=>"jets not succeeded. Point is not smooth?";};
            bblog.debug ("leave 3 simpleComponentCalculator.isOnComponent");
            return (0 == sub( componentIdeal, jetP#"value" ));
        )
        else
        (
            bblog.debug ("leave 2 simpleComponentCalculator.isOnComponent");
            throw new SingularPointException from {"errorMessage"=>"jets not succeeded. Point is not smooth?";};
        );

    );

    simpleComponentCalculator.isOnComponent ( Ideal, Matrix) := Boolean => ( componentIdeal, point)->
    (
        return ( componentIdeal, point, onComponentPrecision);
    );

    simpleComponentCalculator.isOnComponent ( String , Matrix, ZZ) := Boolean => (componentId, point, onComponentPrecision)->
    (
        if (not componentCandidates#?componentId) then error "component does not exist";
        return simpleComponentCalculator.isOnComponent( componentCandidates#?componentId,  point, onComponentPrecision) ;
    );
    
    simpleComponentCalculator.isOnComponent ( String , Matrix, ZZ) := Boolean => (componentId, point)->
    (
        if (not componentCandidates#?componentId) then error "component does not exist";
        return simpleComponentCalculator.isOnComponent( componentCandidates#?componentId,  point, onComponentPrecision) ;
    );

    -- should return smooth point list and eventually singular point list.
    simpleComponentCalculator.interpolateComponents  = (pointList, interpolationMaxDegree, onComponentPrecision) -> 
    (
        bblog.debug("enter InterpolateComponents");
        idealCount := 0;
        localInterpolatedIdeals := {};

        -- T := timing 
        
        err := null;

        for point in pointList do
        (
            BlackBoxLogger.debug("interpolateComponents: point "| toString point );

            --time if bb.isCertainlySingularAt(point) then 
            --(
            --   continue;
            --);
            pointIsSingular := false;

            -- check if point is already on one of the known components
            bIsOnComponent := false;
            for interpolData in localInterpolatedIdeals do
            (
                -- at first do a cheap test with precision = 0;
                isOnComponentPrecision0Result := simpleComponentCalculator.isOnComponent (  interpolData#"ideal", point, 0 );
                if ((class isOnComponentPrecision0Result)=== SingularPointException) then
                (
                    pointIsSingular = true;
                    break;
                );
                if isOnComponentPrecision0Result then
                (
                    isOnComponentResult := simpleComponentCalculator.isOnComponent ( interpolData#"ideal", point, onComponentPrecision );
                    if ((class isOnComponentResult) === SingularPointException) then
                    (
                        pointIsSingular = true;
                        break;
                    );
                    if (isOnComponentResult) then
                    (
                        bIsOnComponent = true; 
                        --print "bIsOnComponent";
                        break;
                    );
                ); 
            );
            if (bIsOnComponent) then 
            (
                BlackBoxLogger.debug("interpolateComponents: point "| toString point| " is on component!");
                continue;
            );
            if ( pointIsSingular) then 
            (
                BlackBoxLogger.debug("interpolateComponents: point "| toString point| " was singular");
                continue;
            );

            -- separate compute from 
            localInterpolatedIdealOrError := catch interpolateBB (blackBox , point,interpolationMaxDegree);                     
            

            if (  isDerivedFrom(localInterpolatedIdealOrError,SingularPointException)) then 
            (
                BlackBoxLogger.debug("interpolateComponents: point "| toString point| " was singular");
            )
            else  
            (                                         
                  localInterpolatedIdeals = localInterpolatedIdeals | { localInterpolatedIdealOrError  };  
                  idealCount = idealCount +1 ;
            );
        );
        -- print "timing for loop", T#0;
        
        
        interpolatedIdeals := new MutableHashTable from 
            apply ( #localInterpolatedIdeals, idx-> (("ideal_" |toString idx ) => localInterpolatedIdeals#idx ) ) ;

        simpleComponentCalculator.setComponentCandidates(interpolatedIdeals);
        return simpleComponentCalculator.componentCandidates();
    );


    -- several components  because the point could be singular
    simpleComponentCalculator.componentCandidatesAt = ( point)->
    (
        candidates := {};
        for componentKey in keys componentCandidates do
        (
            if ( simpleComponentCalculator.isOnComponent( componentKey, point)) then
            (
                candidates = candidates | { componentKey };
            );
        );
        return candidates;
    );
    
    return simpleComponentCalculator;
);


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

    -- get a point property by name
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


    -- 'knownPointPropertiesAsSymbols' : 
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

    blackBox.knownPointPropertiesAsSymbols = ()->
    (   
        return apply( blackBox.pointProperties(), propertyName-> getPropertySymbol(propertyName) );
    );


    -- 'setPointProperty': 
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
        if BlackBoxIdeals.Dictionary#?propertyName then 
        packageSymbol  = BlackBoxIdeals.Dictionary#propertyName;

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
                assert(symb=!=null);
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


    -- setJacobianAt:
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

        localIsCertainlySingularAtWrapper  := ( point )  ->
        (
            return isCertainlySingularAt( blackBox,  point, singularTestOptions );
        );

        setPointProperty("isCertainlySingularAt" , localIsCertainlySingularAtWrapper );

        localIsProbablySmoothAtWrapper  := ( point )  ->
        (
            return not isCertainlySingularAt( blackBox,  point, singularTestOptions );
        );

        setPointProperty("isProbablySmoothAt" , localIsProbablySmoothAtWrapper );


        setPointProperty( "rankJacobianAt" , localRankJacobianAt );
    );

    updateSingularityTest := ()->
    (
        if blackBox.hasPointProperty("valuesAt") then
        (
            localIsCertainlySingularWrapper  := (  point )  ->
            (
                return isCertainlySingularAt( blackBox,  point, singularTestOptions );
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
                return not isCertainlySingularAt( blackBox,  point, singularTestOptions );
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
            propertySymbol := getPropertySymbol(propertyName);
            assert(propertySymbol=!=null);
            
            outerSetPointProperty( propertySymbol, propertyMethod );
        ) 
        else ( error ("point property "| toString propertyName | " does not exist.") );
        blackBox.updateBlackBox();
        return blackBox;
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
    -- knownMethods()
    --
    -- return a list of known methods. Manually updated.
    --
    blackBox.knownMethods = ()->
    (   
        methods:= { getGlobalSymbol( BlackBoxIdeals.Dictionary, "knownMethods" ) ,
                    getGlobalSymbol( BlackBoxIdeals.Dictionary, "knownAttributes" ) ,
                    getGlobalSymbol( BlackBoxIdeals.Dictionary, "pointProperties" ),
                    getGlobalSymbol( BlackBoxIdeals.Dictionary, "knownPointPropertiesAsSymbols" ),
                    getGlobalSymbol( BlackBoxIdeals.Dictionary, "hasPointProperty" ),
                    --getGlobalSymbol( BlackBoxIdeals.Dictionary, "pointProperty" ),
                    getGlobalSymbol( BlackBoxIdeals.Dictionary, "registerPointProperty" ),
                    getGlobalSymbol( BlackBoxIdeals.Dictionary, "setSingularityTestOptions" ),
                    getGlobalSymbol( BlackBoxIdeals.Dictionary, "rpp" ), 
                    getGlobalSymbol( BlackBoxIdeals.Dictionary, "upp" ), 
                    getGlobalSymbol( BlackBoxIdeals.Dictionary, "updatePointProperty" )
                    --getGlobalSymbol( BlackBoxIdeals.Dictionary, "unknownIsValid" )
            };
    --  methods:= {   knownMethods,
    --                knownAttributes,
    --                pointProperties,
    --                knownPointPropertiesAsSymbols,
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
  
  
    -- knownAttributes()
    --
    -- returns a list of known attributes. 
    -- (computed as 'keys blackBox which are not Functions' \ {knownPointPropertiesAsSymbols() }
    --
    blackBox.knownAttributes = ()->
    (
        all :=  keysWithoutSymbols blackBox;
        all = select (all, (foo)->(not instance(blackBox#foo,Function)));   
        kM := kPP := kPS := {};
        -- kM =  blackBox.knownMethods();
        -- kPP =  blackBox.pointProperties();
        kPS  =  blackBox.knownPointPropertiesAsSymbols();
        toRemove := kM | kPP |kPS;
        for symb in toRemove do
        all = delete(symb,all);
        if ( blackBox#?"valuesAt" ) then
        (
            all = all | { getGlobalSymbol( BlackBoxIdeals.Dictionary, "numGenerators" ) };
        );
        sortedAttributes := sort apply(all, i-> ( toString i, i ));
        sortedAttributes = apply(sortedAttributes, i->(i_1));
        return sortedAttributes;  
    );


    -- get singularityTestOptions
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


    -- set singularityTestOptions
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
    
    
    
    componentCalculator := createSimpleComponentCalculator(blackBox);
    
    -- better: check component calculator type /interface.
    blackBox.setComponentCalculator = (componentCalculatorObj)->
    (
        componentCalculator = componentCalculatorObj;
        blackBox.isOnComponent = componentCalculator.isOnComponent;
    );
    
    blackBox.isOnComponent = componentCalculator.isOnComponent;

    blackBox.setComponentCandidates = (cc)->
    ( 
        componentCalculator.setComponentCandidates(cc);        
    );

    blackBox.componentCandidates = ()->
    (
        return componentCalculator.componentCandidates();
    );

    -- TODO well, this should be more generic as different component calculators may 
    -- have different configuration settings ( so may be another component calculator has no 'onComponentPrecision' property)
    --
    blackBox.setOnComponentPrecision = (precision)->
    (
        componentCalculator.setOnComponentPrecision(precision);
    );

    blackBox.interpolateComponents  = (pointList, maxDegree, onComponentPrecision) -> 
    (
        return componentCalculator.interpolateComponents( pointList, maxDegree, onComponentPrecision);
    );

    --blackBox.componentCandidatesAt = (point)->
    --(
    --    return componentCalculator.componentCandidatesAt(point);
    --);

    blackBox.registerPointProperty("componentCandidatesAt", componentCalculator.componentCandidatesAt);
   
    -- store the current type of the black box
    blackBox.type = BlackBoxParameterSpace;

    -- a user should not call this method...

    return blackBox;
)

 


-- blackBoxParameterSpaceInternal()
-- 
-- this function may be used to create a derived object, which inherits properties of an black box ideal
-- since the blackbox object is not copied 
--
blackBoxParameterSpaceInternal(Type, Ring) := HashTable => ( resultType, pRing ) ->
(
    blackBox := blackBoxParameterSpaceInternal(resultType, #(gens pRing), coefficientRing pRing);
    
    blackBox.ring = pRing;
    blackBox.unknowns = gens blackBox.ring;
    assert( blackBox.numVariables == #blackBox.unknowns );
    
    blackBox.unknownIsValid = (unknown)->
    (
        if not ( blackBox.ring === ring unknown) then 
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
    L := {"--" | toString bb.type};
    L = L | {"--"};
    L = L | {"-- attributes:"};
    L = L | {"-- "} | listToStack bb.knownAttributes();
    L = L | {"--"};
    L = L | {"-- methods:"};
    L = L | {"-- "} | listToStack  bb.knownMethods();
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
   
    blackBox.type = BlackBoxParameterSpace;   
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
    blackBox.type = BlackBoxParameterSpace;
    --bb := newClass( BlackBoxParameterSpace, blackBox );
    return blackBox;
)


-- todo: how to check, if 'ring equationsIdeal' is not a quotient ring?

-- blackBoxIdealInternal()
-- 
-- this function may be used to create a derived object, which inherits properties of an black box ideal
-- since the blackbox object is not copied 
--
blackBoxIdealInternal := ( equationsIdeal)->
(   
    blackBox :=  blackBoxParameterSpaceInternal( BlackBoxIdeal, ring equationsIdeal );
    
    blackBox.type = BlackBoxIdeal;
    
    -- maybe blackBox.addProperty( ideal, equationsIdeal)
    blackBox.ideal = equationsIdeal;      
    
    -- maybe blackBox.addProperty( equations, gens equationsIdeal )
    blackBox.equations =  gens  equationsIdeal; 
    
    
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
new BlackBoxIdeal from Ideal := (E, equationsIdeal)->
(
    blackBox := blackBoxIdealInternal(equationsIdeal);
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
    assert( IFPBlackBox.unknowns=={x} );
    assert( IFPBlackBox.equations==gens IFP);
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




-- blackBoxIdealFromEvaluationInternal()
--
--   this function may be used to create a derived object, which inherits properties of an black box ideal
--   since the blackbox object is not copied (and not write-protected)
--

blackBoxIdealFromEvaluationInternal := method();

blackBoxIdealFromEvaluationInternal(Type, ZZ, Ring, Function) := HashTable => ( resultType, numVariables, coeffRing, pValuesAt )  ->
(
    blackBox := blackBoxParameterSpaceInternal( resultType, numVariables, coeffRing );

    blackBox.type = resultType;

    --sets isZeroAt, jacobianAt, rankJacobianAt and numGenerators
    blackBox.registerPointProperty ("valuesAt", (bb,point)->pValuesAt(point) ); 

    check := ()->
    (
         numVariables :=  blackBox.numVariables;

         point := matrix { apply(numVariables, i-> 0_(blackBox.coefficientRing) ) };
         blackBox.valuesAt( point );
         blackBox.isZeroAt( point );
    );

    check();  
    return blackBox ;
)

-- blackBoxIdealFromEvaluationInternal()
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

blackBoxIdealFromEvaluation(ZZ, Ring, Function) := HashTable => ( numVariables, coeffRing, valuesAt )  ->
(
    blackBox := blackBoxIdealFromEvaluationInternal(BlackBoxIdeal, numVariables, coeffRing, valuesAt ) ;
    --blackBox.type = BlackBoxIdeal;
    --blackBox = newClass( BlackBoxIdeal, blackBox ); 
    return blackBox ;
)


blackBoxIdealFromEvaluation( Ring, Function ) := HashTable => ( pRing, pValuesAt ) ->
(

   blackBox := blackBoxParameterSpaceInternal(BlackBoxIdeal, pRing );
   blackBox.type = BlackBoxIdeal;
   blackBox.registerPointProperty ("valuesAt", (bb,point)->pValuesAt(point) ); --sets isZeroAt, jacobianAt, rankJacobianAt and numGenerators

   check := ()->
   (
        numVariables :=  blackBox.numVariables;
        
        point := matrix { apply(numVariables, i-> 0_(blackBox.coefficientRing) ) };
        blackBox.valuesAt( point );
        blackBox.isZeroAt( point );
   );

   check(); 
   --blackBox = newClass( BlackBoxIdeal, blackBox ); 
   return blackBox;
)




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
  
    evalBlackBox := blackBoxIdealFromEvaluation ( # (gens evaluation.ring), coefficientRing evaluation.ring, evaluation.valuesAt );

    point := matrix {{3_(ZZ/7)}} ;
    assert( evaluation.isZeroAt( point ) );
    assert( evaluation.unknowns=={x} );
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
    

doc ///
   Key
        BlackBoxIdeal
        (NewFromMethod, BlackBoxIdeal, Ideal)
        (NewFromMethod, BlackBoxIdeal, Thing)
   Headline
        an unified interface to a black box ideal
   Usage   
        new BlackBoxIdeal from pIdeal
        blackBoxIdealFromEvaluation( rng, evaluationMethod )
        blackBoxIdealFromEvaluation( variableNumber, coeffRing, evaluationMethod)
   Inputs  
        pIdeal:Ideal
             with integer coefficients
   Outputs
        : BlackBoxIdeal
   Description
         Text
            {\bf Design} \break
            The {\tt  BlackBoxIdeal } objects implements the interface of @TO BlackBoxParameterSpace @ \break 
            and in addition following methods and attributes:

            \,\, \bullet \,{\tt numGenerators() }: number of  generators/equations \break \break             

            Point properties:\break
            \,\, \bullet \, @TO "isZeroAt" @ \break
            \,\, \bullet \, @TO "valuesAt" @ \break
            \,\, \bullet \, @TO "jacobianAt" @ \break
            \,\, \bullet \, @TO "rankJacobianAt" @ \break
                        
            If the black boxes was generated from an explicit ideal, the
            following properties are also defined: \break 
            \,\, \bullet \,{\tt unknowns}: a list of the parameter variables . \break
            \,\, \bullet \,{\tt ideal}: the original ideal  \break
            \,\, \bullet \,{\tt equations}: generators/equations of the original ideal/polynomial system \break
            
        Text
            \break  For an example see @TO blackBoxIdealFromEvaluation@, @TO blackBoxIdeal @.         
///

doc ///
   Key
        blackBoxIdealFromEvaluation        
   Headline
        create a  BlackBoxIdeal  describing an ideal  with integer coefficient ring
   Usage   
        blackBoxIdealFromEvaluation( rng, evaluationMethod )
        blackBoxIdealFromEvaluation( variableNumber, coeffRing, evaluationMethod)
   Inputs  
        rng:Ring
             with integer coefficients
        variableNumber:ZZ
             number of parameter variables
        coeffRing:Ring
             ring to whicht the parameter variables belong to.
        evaluationMethod:Function
             function which accepts a point given as a row matrix and returns the evaluation at this point as a row matrix.
   Outputs
        : BlackBoxIdeal
   Description   
        Text    
           Creates a blackbox describing an implicitly given ideal from an evaluation method \break
           \break  Example:  create an {\tt BlackBoxIdeal } object from an evaluation:
        Example          
            RQ := QQ[x];
            IFQ := ideal { 1/3*x^2-100/3, 1/5*x+2 };        
            evaluation := (point)-> (return sub( gens IFQ, point) );
            bbI := blackBoxIdealFromEvaluation( RQ, evaluation );
        Text
            \break Now access some ideal propeties via the black box interface:
        Example          
            -- keys bbI
            bbI.knownAttributes()
            bbI.unknowns
            point := matrix { {1_QQ } };
            bbI.pointProperties()
            bbI.valuesAt(point)

            point = matrix { {-10_QQ } };
            bbI.jacobianAt(point)            
   Caveat
        may have problems if the coefficient ring is something obscure? Rationals and integers as coefficient ring should be ok.
///

--        new BlackBoxIdeal from Ideal
doc ///
   Key
        blackBoxIdeal
   Headline
        create a {\tt BlackBoxIdeal } 
   Usage   
        blackBoxIdeal(anIdeal)
   Inputs  
        anIdeal:Ideal
   Outputs
        : BlackBoxIdeal
   Description      
        Text
           Creates a blackbox describing an ideal \break \break
           See also @TO blackBoxIdealFromEvaluation@  \break
           \break  Example:  create an {\tt BlackBoxIdeal } object from an ideal:
        Example          
            RQ := QQ[x];
            IFQ := ideal { 1/3*x^2-100/3, 1/5*x+2 };        
            IFZ := clearCoeffDenominators(IFQ)
            bbI := blackBoxIdeal(IFZ);
        Text
            \break Now access some ideal propeties via the black box interface:
        Example          
            -- keys bbI
            bbI.knownAttributes()
            bbI.ideal
            bbI.unknowns
            bbI.equations
            bbI.jacobian
            point := matrix { {1_QQ} };
            bbI.pointProperties()
            bbI.valuesAt(point)
            point = matrix { {-10_QQ} };
            bbI.valuesAt(point)
            bbI.jacobianAt(point)            
   Caveat
        does not check if the ideal ring is a quotient ring (not supported?)
///


TEST ///
    debug BlackBoxIdeals
    idealBlackBoxesProtect()
    testClearCoeffDenominators()
///



TEST ///
    debug BlackBoxIdeals
    idealBlackBoxesProtect()
    testNestedRingCoeffsLCMDenominator()
///
         

TEST ///
    debug BlackBoxIdeals
    idealBlackBoxesProtect()
    testTensoredClearCoeffDenominators()
///

TEST ///
    debug BlackBoxIdeals
    idealBlackBoxesProtect()
    testBlackBoxIdeal()
///

TEST ///
    debug BlackBoxIdeals
    idealBlackBoxesProtect()
    testBlackBoxIdealFromEvaluation()
///


TEST ///

    debug BlackBoxIdeals
    idealBlackBoxesProtect()

    R = ZZ[x_0..x_3]

    M = matrix{
        {x_0,x_1,0},
        {x_1,x_2,x_3}
        }

    I = minors(2,M)

    smoothPoint = matrix{{1,0,0,0}}
    singularPoint = matrix{{0,0,1,0}}
    offPoint = matrix{{1,11,0,0}}

    S = ZZ[s,t]

    line = matrix{{0,0,s,t}}

    B = blackBoxIdeal I

    assert (sub(jacobian I,smoothPoint) == B.jacobianAt smoothPoint)
    assert (rank(B.jacobianAt smoothPoint) == codim(I,Generic=>true))
    assert (rank(B.jacobianAt singularPoint) < codim(I,Generic=>true))
    assert( B.isZeroAt(smoothPoint) );
    assert( B.isZeroAt(singularPoint));
    assert(B.isZeroAt(line));

    B.valuesAt(offPoint)
    B.isZeroAt(offPoint)

    assert(not B.isZeroAt(offPoint))

    prime = 11;
    K = ZZ/prime;
    assert (B.isZeroAt(sub(smoothPoint,K)));
    assert(B.isZeroAt(sub(offPoint,K)));

    apply(100,i->(
        r = random(K^1,K^4);
        assert ((B.isZeroAt r) == ((B.valuesAt r) == 0));
        ));

        
    evalLinePlusConic = point -> (
        ePoint = flatten entries point;
        M = matrix{
        {ePoint#0,ePoint#1,0},
        {ePoint#1,ePoint#2,ePoint#3}
        };
        matrix{{det M_{0,1},det M_{0,2},det M_{1,2}}}
        )

    B2 = blackBoxIdealFromEvaluation( 4, ZZ, evalLinePlusConic)

    apply(100,i->(
        r = random(K^1,K^4);
        assert (B2.isZeroAt(r) == B.isZeroAt(r));
        assert (B2.valuesAt(r) == B.valuesAt(r));
                if B2.isZeroAt(r) then 
            (             
            assert (B2.jacobianAt(r) == B.jacobianAt(r));
            ) else ();
        ));

    assert  B2.isZeroAt(line)


    assert(sub(B.jacobian,line)== sub(jacobian I,line))
    assert (B.jacobianAt(line) == dropDegreeInfo( sub(jacobian I,line)) )
///


TEST ///
    --debug BlackBoxIdeals
    
    bbRankM = blackBoxParameterSpace( 5 ,ZZ )
    assert(bbRankM.numVariables==5);
    assert(bbRankM.coefficientRing===ZZ);

    -- assert( bbRankM.numGenerators() === null)

    rankMat := ( blackBox, point )->5 

    assert(not  bbRankM.hasPointProperty("rankMat") );

    bbRankM.registerPointProperty( "rankMat", rankMat )

    assert(  bbRankM.hasPointProperty("rankMat") );

    point  = matrix {{1,2,3,4,5}};

    assert( rankMat(bbRankM, point) == (bbRankM.pointProperty("rankMat"))(point) );

    rankMatNew := (blackBox, point)->3
    bbRankM.updatePointProperty("rankMat",rankMatNew)
    assert( rankMatNew(bbRankM,point) == (bbRankM.pointProperty("rankMat"))(point) );
    assert( rankMatNew(bbRankM,point) == (bbRankM.pointProperty(getGlobalSymbol "rankMat"))(point) );
 

    -- assert bbRankM#?(global rankMat); -- fails..
    assert bbRankM#?("rankMat");

    assert( rankMatNew(bbRankM,point) == bbRankM.rankMat(point) );

    assert(bbRankM.coefficientRing===ZZ);

    rankMatNew := (blackBox, point)->4 --also influences bbRankM; because rebuild does not copy; it just exports new registered properties.

    bbRankM.updatePointProperty("rankMat",rankMatNew)

    (bbRankM.pointProperty("rankMat"))(point);

    assert( rankMatNew(bbRankM,point) == bbRankM.rankMat(point) );
    assert( rankMatNew(bbRankM,point) == bbRankM#"rankMat"(point) );
    assert( rankMatNew(bbRankM,point) == (bbRankM.pointProperty("rankMat"))(point) );
    assert( rankMatNew(bbRankM,point) == (bbRankM.pointProperty(getGlobalSymbol "rankMat"))(point) );
    keys bbRankM
    
    valuesAt := ( blackBox, point )-> matrix {{1,2}};

    bbRankM.registerPointProperty( "valuesAt", valuesAt );
    -- that is unfortunate; registering a point property requires a rebuild.   
     
    assert(  bbRankM.hasPointProperty("isZeroAt") );

    assert(  bbRankM.hasPointProperty("jacobianAt") );
 
    assert( bbRankM.numGenerators() =!= null)

    assert( bbRankM.numGenerators() === 2 )
    -- bbRankM.numGenerators()
    
    illegalPoint := matrix {{1,2,3,4,5,6}}; 
   
    try ( bbRankM.rankMat(illegalPoint) ) then ( assert(false) ) else ();

    bbRankM = blackBoxParameterSpace( 5 ,ZZ/7 )

    valuesAt := ( blackBox, point )-> matrix {{1,2}};

    --bbRankM.registerPointProperty("valuesAt",valuesAt);
    bbRankM.rpp("valuesAt",valuesAt);

    point  = sub(point,ZZ/7); 

    bbRankM.valuesAt(point)

    illegalPoint := sub(point,ZZ/2); 

    try ( bbRankM.valuesAt(illegalPoint) ) then ( assert(false) ) else ();

   
///

--load "./BlackBoxIdeals/documentation/blackBoxParameterSpace.m2";


-- how to document 
-- blackBoxParameterSpace(ZZ,Ring) ?


doc ///
   Key
        blackBoxParameterSpace
   Headline
        create (3) a BlackBoxParameterSpace  describing an parameter space
   Usage   
        blackBoxParameterSpace(rng)
        blackBoxParameterSpace( variableNumber, coeffRing)
   Inputs  
        rng:Ring
             a polynomial ring 
        variableNumber:ZZ
             number of parameter variables
        coeffRing:Ring
             ring to which the parameter variables belong to; 

   Outputs
        : BlackBoxParameterSpace
   Description  
        Text    
           Creates (4) a blackbox describing an parameter space    \break            
           \break  Example:  create an {\tt BlackBoxParameterSpace } object :
        Example     
            coeffRing := ZZ/3;
            numVariables := 5;
            bbParamSpace = blackBoxParameterSpace( numVariables , coeffRing );
            rankMat := (blackBox, point)->55;
            bbParamSpace.knownMethods()
            bbParamSpace = bbParamSpace.registerPointProperty("rankJacobianAt", rankMat );
        Text
            \break Now access some propeties via the black box interface:
        Example          
            -- keys bbParamSpace
            bbParamSpace.knownAttributes()
            bbParamSpace.coefficientRing
            bbParamSpace.knownPointProperties()
            point := matrix { {1,2,3,4,5} };
            point = sub( point, coeffRing); 
            bbParamSpace.rankJacobianAt(point)
            rankMatNew := (blackBox, point)->6;
            bbParamSpace = bbParamSpace.updatePointProperty("rankJacobianAt", rankMatNew );
            bbParamSpace.rankJacobianAt(point)
///

doc ///
    Key
        BlackBoxParameterSpace
        (NewFromMethod,BlackBoxParameterSpace,Ring)
        (NewFromMethod,BlackBoxParameterSpace,Thing)
    Headline
          black boxes for parameter spaces
    Description
        Text
            A black box parameter space is used to implement parameter spaces
            with their universal families in a pointwise fashion.
            
          
      
            Implements an unified interface for some explicit and implicit given ideals. \break
            See also @TO BlackBoxIdeal @
            \break             \break

            { \bf QuickStart } \break \break 
            For a quick start see the @TO "Singularities of cubic surfaces" @-tutorial
            \break \break
            { \bf Design } \break \break 
            The simplest parameter space black box  implements  \break \break 
            Attributes:\break 
            \,\, \bullet \,{\tt numVariables }: number of variables in the parameter space. \break
            \,\, \bullet \,{\tt coefficientRing }: the ring the parameter variables belong to (or embeddable to) \break
            
            Methods:\break 
            \,\, \bullet \,  @TO pointProperties@() : returns a list of all known properties at a point for a blackbox \break
            \,\, \bullet \,  @TO  knownMethods@() : returns a list of all known methods of the blackbox \break 
            \,\, \bullet \,  @TO knownAttributes@() : returns a list of all known attributes  \break 
            \,\, \bullet  \, @TO registerPointProperty@(propertyName, propertyMethod) : \break 
            \,\, \, \, register a new point property for a BlackBox, e.g. evaluation at a point. \break
            \,\, \, \, expected Interface of {\tt propertyMethod} is: \break  
            \,\, \, \, \, (  {\tt blackBox }: @TO BlackBoxParameterSpace @,  {\tt point }: @TO Matrix@ )  \break 
            \break
            Special point property names:
            \,\, \, \, There are several special property names:  @TO valuesAt@, @TO jacobianAt@. \break 
            \,\, \, \,  If one of this properties is registered or updated, it triggers update of dependent properties \break
            \,\, \, \,  e.g. registering evaluation @TO valuesAt@  will implicitly \break 
            \,\, \, \, construct   @TO isZeroAt@, @TO numGenerators@,  @TO jacobianAt@, @TO rankJacobianAt@ \break 
            \,\, \, \,  registering evaluation @TO jacobianAt@  will implicitly \break 
            \,\, \, \, construct  @TO rankJacobianAt@ \break 
            \break
            \,\, \bullet  \, @TO updatePointProperty@(propertyName, propertyMethod) }: update an existing point property for a BlackBox. \break
            \break \break  

            In addition, if the black box was created from an ideal () or an evaluation (), it has additional methods and attributes,
            see @TO BlackBoxIdeal @ 

            The blackbox interface allows implementation of some algorithms e.g. padic lifting. 
        Text
           \break  For usage example see @TO blackBoxParameterSpace@
///
            
doc ///
    Key
        "Singularities of cubic surfaces"
    Headline
        use a blackBoxParameterSpace to study the space of cubic surfaces
    Description
        Text
            A black box parameter space is used to implement parameter spaces
            with their universal families in a pointwise fashion.
            
            Let us build the parameter space of cubic surfaces with a view of 
            studying its stratification with respect to singularity type.
            
            We work in charateristic 7.            
        Example    
            K = ZZ/7
        Text
            The coordinate ring of IP^3
        Example
            R = K[x,y,z,w]
        Text  
            Make an empty blackbox which will later contain our 
            describtion of the parameter space of cubic surfaces.
            It will depend on 20 parameters, since acubic polynomial 
            in 4 variables has 20 coefficients.
        Example
            bbC = blackBoxParameterSpace(20,K);
            bbC.pointProperties()
        Text
            We now build the cubics from the coefficents, i.e. we
            construct the member of the universal familiy over 
            a given parameter point:
        Example
            mons3 = matrix entries transpose super basis(3,R)
            cubicAt = (point) -> matrix entries (point*mons3)
        Text 
            register this function in the black box 
        Example
            bbC = bbC.registerPointProperty("cubicAt",cubicAt);
            bbC.pointProperties()
        Text
            Lets test this functionality with some special cubics.
            The first example is the cubic cone. It is singular
            at (0:0:0:1):                   
        Example    
            cubicCone = matrix{{x^3+y^3+z^3}}
            coeffCubicCone = contract(transpose mons3,cubicCone)
            bbC.cubicAt(coeffCubicCone)
        Text
            The second example is the Fermat cubic. It is smooth everywhere    
        Example
            cubicFermat = matrix{{x^3+y^3+z^3+w^3}}
            coeffCubicFermat = contract(transpose mons3,cubicFermat)
            bbC.cubicAt(coeffCubicFermat)
        Text
            Now we want to implement the stratification by singularity type.
            For this we first determine the singular locus of a cubic surface:
        Example
            singularLocusAt = (bb,point) -> ideal jacobian bb.cubicAt(point)
            bbC = bbC.rpp("singularLocusAt",singularLocusAt);
            bbC.pointProperties()
            bbC.singularLocusAt(coeffCubicCone)   
            bbC.singularLocusAt(coeffCubicFermat)
        Text
            As a first approximation of the singularity type we use
            the degree of the singular locus
        Example
            degreeSingularLocusAt = (bb,point) -> (
                 s := bb.singularLocusAt(point);
                 if dim s == 0 then return 0;                
                 if dim s == 1 then return degree s;                 
                 if dim s >= 2 then return infinity;
              )
            bbC = bbC.rpp("degreeSingularLocusAt",degreeSingularLocusAt);
            bbC.pointProperties()
        Text
            Calculate the degree of the singular locus for our examples
        Example
            bbC.degreeSingularLocusAt(coeffCubicCone)
            bbC.degreeSingularLocusAt(coeffCubicFermat)
        Text
            Now the BlackBoxParameterSpace hat a number of point properties
        Example
            bbC.pointProperties()
        Text
            These properties can now be used in a finite field experiment
            that studies the statification of our parameter space. Here is a
            simple minded version of such an experiment:
        Example
            tally apply(100,i->bbC.degreeSingularLocusAt(random(K^1,K^20))) 
        Text
            We see that there is an open stratum of smooth cubics. The
            largest closed stratum consists of those cubics with a A1 singularity.
            The package finiteFieldExperiments helps to do the bookkeeping 
            for such experiments and also provides more detailed interpretation
            of the results.

///

doc ///
    Key
        BlackBoxIdeals
    Headline
          black boxes for explicit and implicitly given ideals
    Description
        Text
            Implements an unified interface for some explicit and implicit given ideals \break  
            \break
            Purpose: use a unified interface for finite field experiments; see FiniteFieldExperiments and padicLift package
            \break
            Currently three BlackBox constructors are available:\break
            \,\,  \bullet \,   @TO blackBoxParameterSpace@  \break
            \,\,  \bullet \,   @TO blackBoxIdeal@  \break
            \,\,  \bullet \,  @TO blackBoxIdealFromEvaluation@  \break \break

            All black boxes implement the interface of @TO BlackBoxParameterSpace @ \break
            If the black box was created from an ideal or an evaluation, it has additional properties, see  @TO BlackBoxIdeal @ 
            \break  \break
            {\bf QuickStart } \break \break
            For a tutorial see @TO "Singularities of cubic surfaces" @
                        
      
    Caveat
         The black box properties are write-protected to prevent accidental modification  by the user; \break 
         however implementing write-protection leads to undesired code compexity and at the same time it is still possible to overcome the  protection. \break
         Currently adding properties to the blackBox with more than one parameter (point) is not implemented (e.g. jet computation ). \break
         Also not done yet is the implementation of the {\tt Observable } interface for the various black boxes ({\tt FiniteFieldExperiment } which will be  {\tt Observers }) \break
         Finally, the package is probably not threadsafe.
         
         
///

TEST  /// 
    debug BlackBoxIdeals
    idealBlackBoxesProtect()

    -- bblog is not defined... why ?

    BlackBoxLogger.debug("test update valuesAt property ")
    rng := ZZ/7[x]

    I = ideal(6*x)

    bb = blackBoxIdeal I

    result :=  matrix{{5}};

    point := matrix{{0_rng}};

    bb.updatePointProperty ("valuesAt", (blackBox,point)->result )  --fails

    assert (result==bb.valuesAt(point) );

    result =  matrix{{0}};

    bb.updatePointProperty ("valuesAt", (blackBox,point)->result )  --fails

    assert (result==bb.valuesAt(point) ); 
///

doc ///
    Key
        getEpsRing
        (getEpsRing, Ring, ZZ)
        eps
    Headline
        get a ring for jet calculations
    Usage   
        getEpsRing(R,d)
    Inputs  
        R: Ring
             the coefficient Ring
        d: ZZ 
         the precision of the Ring for jet calculations
    Outputs
        : Ring
             R[eps]/eps^{d+1}
    Description
        Text
           The advantage of using this function is that for each
           precision and coefficient ring R[eps]/eps^{d+1} is
           created only once.
        Example          
          E2 = getEpsRing(QQ,2)
          eps^2+eps^3
          E3 = getEpsRing(QQ,3)
          eps^2+eps^3
        Text
          Be aware that there are now several eps-es. This can
          lead to unexpected behavior:
        Example
          use E2; eps
        Text  
          Notice that this still gives the eps in E3.
          The underscore notation also does not help in this situation:
        Example
          try eps_E2 then "works" else "error"
        Text      
          The following works, but is not recomended since the 
          code does not check whether eps is a variable in a ring:
        Example
          sub(eps,E2)
          eps = 1
          sub(eps,E2)
        Text  
          We recommend the following:
        Example
          E2.eps
          E3.eps
          E2.eps^3
        Text
          This was implemented by hand for these rings. It does not work
          in general. 
///


doc ///
    Key
        singularityTestOptions
    Headline
        show current parameters for singularity test
    Usage   
        bb.singularityTestOptions()
    Inputs  
        bb: BlackBoxIdeal
             an black box ideal
    Outputs
        : HashTable
            with  keys { \tt precision } (the required length of the computed jets)
            and { \tt numTrials } ( the number of required jet computations )
    Description
        Text
            Show currently used parameters for @TO2{isCertainlySingularAt,"point singularity test"}@
            There are two parameters, the required length of the computed jets ( { \tt precision } )
            and  the number of required jet computations ({ \tt numTrials }). If one of the jet computations fails,
            the point is certainly not smooth.

            Let us construct a black box
        Example
            R = QQ[x,y]
            bbI = blackBoxIdeal ideal(x^2-y^3);
        Text
            Now we may look at the default singularity test parameters:
        Example
            bbI.singularityTestOptions()
        Text
            The test parameters may be modified by the user with @TO{setSingularityTestOptions}@
        Example
            jetLength = 3;
            numTrials = 5;
            bbI.setSingularityTestOptions(jetLength, numTrials);
            bbI.singularityTestOptions()
    SeeAlso
        setSingularityTestOptions
        isProbablySmoothAt
        isCertainlySingularAt
///

doc ///
    Key
        isProbablySmoothAt
        isCertainlySingularAt
    Headline
        heuristic test of smoothness
    Usage   
        bb.isProbablySmoothAt(point)
        bb.isCertainlySingularAt(point)
    Inputs  
        bb: BlackBoxIdeal
             an black box ideal
        point: Matrix 
         the coordinates of a point
    Outputs
        : Boolean
    Description
        Text
          Checks for smoothness of a point on the
          vanishing set of the black box ideal, by
          trying to find a @TO2{jetAt,"jet"}@ starting at the point
          using 
          If the point is smooth on the vanishing
          set of the black box ideal, arbitray jets
          can always be found. If one or more jets are found
          the point is probably smooth. If the search
          for a jet failes only once, the vanishing
          set is certainly singular at this point.
          
          Consinder for example the cuspidal cubic:
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal(x^2-y^3);
        Text
          The cuspidal cubic is singular at the origin:
        Example    
          origin = matrix{{0,0_QQ}}
          bbI.isCertainlySingularAt(origin)
        Text
          For the tests the length is taken by the precision of 
          @TO{singularityTestOptions}@ and the required number of successful trials  
          is given by 'numTrials'
        Example    
          bbI.singularityTestOptions()
        Text
          The default singularityTestOptions can be changed with
          @TO{setSingularityTestOptions}@
          Consider a point on the cuspidal cubic different from the origin:
        Example
          otherPoint = matrix{{8,4_QQ}}
        Text
          Check whether the other point lies on the cuspidal cubic:
        Example  
          bbI.isZeroAt(otherPoint)
        Text
          We now check for smoothness:
        Example
          bbI.isCertainlySingularAt(otherPoint)
          bbI.isProbablySmoothAt(otherPoint)
        Text
          If the point is not on the vanishing set defined by
          the black box ideal, an exception PointNotOnBlackBox is thrown
        Example
          pointNotOnCurve = matrix{{4,5_QQ}}
          bbI.isZeroAt(pointNotOnCurve)
          catch bbI.isCertainlySingularAt(pointNotOnCurve)
          catch jetAt(bbI,pointNotOnCurve,1,1)
    SeeAlso
       setSingularityTestOptions
       jetAt
      
///

doc ///
    Key
        jetAt
        (jetAt, BlackBoxParameterSpace, Matrix, ZZ, ZZ)
    Headline
        find jets on the varieties defined by a black box
    Usage   
        jetAt(bb,point,prec,n)
    Inputs  
        bb: BlackBoxIdeal
             an black box ideal
        point: Matrix 
             the coordinates of a point
        prec: ZZ
             the precision of the desired jet
        n: ZZ
             the number of trial used to find a jet    
    Outputs
        : MutableHashTable
    Description
        Text
          Tries to find a jet starting at a given point on 
          a variety given by a black box.
          
          Consinder for example a nodal cubic:
        Example
          Fp = ZZ/101
          R = Fp[x,y]
          I = ideal(x^2-y^2+x^3)
          bbI = blackBoxIdeal I;
        Text
          Consider a point on the nodal cubic different from the origin:
        Example
          point = matrix{{3,6_Fp}}
        Text
          Check whether the other point lies on the nodal cubic:
        Example  
          bbI.isZeroAt(point)
        Text
          We now look for a jet:
        Example
          j = jetAt(bbI,point,3,1)
        Text
          The defining equations of the ideal indeed vanish on the jet:
        Example
          sub(I,j)
        Text
          At the origin the nodal cubic is singular. Short jets can be found,
          but not long ones:
        Example
          origin = matrix{{0,0_Fp}}
          catch jetAt(bbI,origin,1,1)  
          catch jetAt(bbI,origin,2,1)  
          catch jetAt(bbI,origin,3,1)  
        Text
          Notice that the search for fails at length 2 most of the time,
          since the singularity has multiplicity 2. If one tries
          long enough a longer jet can be found (lying on one
          of the branches at the origin):
        Example        
          jetStatsAt(bbI,origin,3,200) 
    SeeAlso
        isProbablySmoothAt
        isCertainlySingularAt
///

 

doc ///
    Key
        "isZeroAt"
    Headline
         Check if a given point lies on the vanishing set defined by a black box ideal.
    Usage   
         bbI.isZeroAt(point)
    Inputs  
        bbI: BlackBoxIdeal
        point: Matrix
             coordinates of a point
    Outputs
        : Boolean
    Description
        Text
          This is a point property.
          Checks if the the point lies on the vanishing set defined by the
          black box ideal. 
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal(x^2-y^3);
          bbI.isZeroAt(matrix{{0,0_QQ}})
          bbI.isZeroAt(matrix{{8,4_QQ}})            
          bbI.isZeroAt(matrix{{8,5_QQ}})
///

doc ///
    Key
        "valuesAt"
    Headline
        evaluate the generators of black box ideal at a given point 
    Usage   
        bbI.valuesAt(point)
    Inputs  
        bbI: BlackBoxIdeal
        point: Matrix
             coordinates of a point
    Outputs
        : Boolean
    Description
        Text
          This is a point property. It evaluates the generators of the
          black box ideal at the point
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal(x^2,y^3);
          bbI.valuesAt(matrix{{2,3_QQ}})
        Text
          valuesAt is a special pointProperty
          in the sense that the package uses them to compute other
          point properties. Therefore
          updating valuesAt triggers updates for 
          isZeroAt, numGenerators, jacobianAt, rankJacobianAt. 
    Caveat
          This works only with black box ideals, since they contain an algorithm
          that can evaluate the generators of the black box ideal. A black box parameter spaces
          might contain only an algorithm that checks whether all generators vanish. 
          This happens for example if one considers the moduli space of singular cubics. 
          One can check whether a
          given cubic is singular without calculating the value of the 
          corresponding discriminant. 
///

doc ///
    Key
        "jacobianAt"
    Headline
        evaluate the jacobian matrix of a black box ideal at a given point 
    Usage   
        bbI.jacobianAt(point)
    Inputs  
        bbI: BlackBoxIdeal
        point: Matrix
             coordinates of a point
    Outputs
        : Boolean
    Description
        Text
          This is a point property. It evaluates the jacobian matrix of the
          black box ideal at the point
        Example
          R = QQ[x,y]
          I = ideal(x^2-y^3);
          bbI = blackBoxIdeal I;
          point = matrix{{8,4_QQ}}
          bbI.isZeroAt(point)
          bbI.jacobianAt(point)
          sub(jacobian I,point)
          bbI.isProbablySmoothAt(point)
        Text
          The cuspidal cubic considered above is singular at the
          origin. Therefore the jacobian matrix vanishes there:
        Example
          origin = matrix{{0,0_QQ}}
          bbI.isZeroAt(origin)
          bbI.jacobianAt(origin)
          bbI.isCertainlySingularAt(origin)
        Text
          jacobianAt is a special pointProperty
          in the sense that the package uses it to compute other
          pointProperties. Therefore an update of 
          jacobianAt triggers an update 
          for 'rankJacobianAt'.  
    Caveat
          This works only with black box ideals, since they contain an algorithm
          that can evaluate the generators of the black box ideal. A black box parameter spaces
          might contain only an algorithm that checks whether all generators vanish. 
          This happens for example if one considers the moduli space of singular cubics. 
          One can check whether a
          given cubic is singular without calculating the value of the 
          corresponding discriminant. 
///

doc ///
    Key
        "registerPointProperty"
        "rpp"
    Headline
        register a new point property in a black box.
    Usage   
        bbI = bbI.registerPointProperty(name,propertyAt)
        bbI = bbI.rpp(name,propertyAt)
    Inputs  
        bbI: BlackBoxParameterSpace
        name : String
          name of the new point property
        propertAt: Function 
          that takes coordinates of a point and returns anything.
    Outputs
        : BlackBoxParameterSpace
    Description
        Text
          rpp and 
          registerPointProperty are synonymous. rpp is provided
          to save typing time...
          
          This method is used to register new property in a
          blackBoxIdeal or a blackBoxParameterSpace. 
          
          Lets for example build a black box parameter space
          for cubic surfaces in IP^3 over the finite field with
          7 Elements.
        Example
          Fp = ZZ/7
          R = Fp[x,y,w,z]
        Text
          there are 20 monomials of degree 3 in 4 variables
        Example
          mons3 = basis(3,R)
        Text
          Therefore we need a 20 dimensional parameter space
        Example
          bbC = blackBoxParameterSpace(20,Fp);
        Text
          This has no known point properties 
        Example
           bbC.pointProperties()
        Text
          We now make a function that constructs a cubic
          polynomial from 20 parameters
        Example
          cubicAt = (point) -> point*transpose mons3
          cubicAt(matrix{{1_Fp,19:0}})
          fermatPoint = matrix {{1_Fp,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1}}
          cubicAt(fermatPoint)
        Text
          To have this available inside the blackbox we need to register
          it
        Example
          bbC = bbC.registerPointProperty("cubicAt",cubicAt);
          bbC.pointProperties()
        Text
          Now we can use the property from the black box
        Example
          bbC#"cubicAt"(fermatPoint)
          bbC.cubicAt(fermatPoint)
        Text
          Registering a point property is useful when the black box
          is used in a finite field experiment. Registered
          point properties are available to an experiment while it is
          running. Also it is useful to register for bookkeeping reasons,
          so that for example pointProperties will return
          the correct answer.
///

doc ///
    Key
        "updatePointProperty"
        "upp"
    Headline
        change a point property in a black box.
    Usage   
        bbI.updatePointProperty(name,propertyAt)
        bbI.upp(name,propertyAt)
    Inputs  
        bbI: BlackBoxParameterSpace
        name : String
          name of point property to be changed
        propertAt: Function 
          that takes coordinates of a point and returns anything.
    Outputs
        : BlackBoxParameterSpace
    Description
        Text
          upp and 
          updatePointProperty are synonymous.
          
          This method is used to change a property of 
          blackBoxIdeal or a blackBoxParameterSpace. The need
          for this arises usually while programming when one
          realizes that a programming mistake was made.  
          
          Lets look at a stupid, but illustrative example:       
        Example
          bbC = blackBoxParameterSpace(2,QQ);
          bbC = bbC.rpp("product",(point) -> sum flatten entries point);
          bbC.product(matrix{{5,6_QQ}})
          bbC.upp("product",(point) -> product flatten entries point);
          bbC.product(matrix{{5,6_QQ}})
        Text
          It is also possible to update pointProperties that are
          predefined by the package. Here
          valuesAt and jacobianAt are special pointProperties,
          in the sense that the package uses them to compute other
          pointProperties. Therefore
          updating valuesAt triggers updates for 
          isZeroAt, numGenerators, jacobianAt, rankJacobianAt.
          Similarily updating jacobianAt triggers an update 
          for 'rankJacobianAt'.  
    SeeAlso
         registerPointProperty
         rpp
         valuesAt
         jacobianAt
///

doc ///
    Key
        "hasPointProperty"
    Headline
        check whether a point property of a black box is defined
    Usage   
        bbI.hasPointProperty(name)
    Inputs  
        bbI: BlackBoxParameterSpace
        name : String
          name of point property to be checked
    Outputs
        : Boolean
    Description
        Text
          check whether a point property is defined.
          
          Every BlackBoxIdeal has the property "jacobianAt" since
          it has (at least implicitly) access to a representation of 
          the generators of the ideal:
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal (x^2-y^3);
          bbI.hasPointProperty("jacobianAt")
          bbI.pointProperties()
        Text
          A blackBoxParameterSpace usually does not have 
          the property "jacobianAt" since such a parameter space
          does not even have an implicit representation of 
          the equations.
        Example
          bbParam = blackBoxParameterSpace(2,QQ);     
          bbParam.hasPointProperty("jacobianAt") 
          bbParam.pointProperties()
        Text
          To illustrate why this can happen think for example of 
          the space of
          singular cubics. In a BlackBoxParameterSpace one
          would simply test the smoothness of a give cubic
          via Groebner basis calculation. This does not automatically
          give rise to a representation of the corresponding
          Diskriminant. 
    SeeAlso
         pointProperties
         registerPointProperty
///

doc ///
    Key
        "knownAttributes"
        "numVariables"
    Headline
        list the attributes of a black box 
    Usage   
        bbI.knownAttributes()
    Inputs  
        bbI: BlackBoxParameterSpace
    Outputs
        : List       
    Description
        Text
          Every BlackBoxIdeal has some attributes
          provided by the package 
          (no new attributes can be defined by the user).
          The difference between attributes and methods is,
          that an attribute is a constant while a property is 
          a function.
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal (x^2-y^3);
          bbI.knownAttributes()
        Text
          Lets look at these attributes in turn
        Example
          bbI.coefficientRing
          bbI.equations
          bbI.ideal
          bbI.jacobian
          bbI.numVariables
          bbI.ring
          bbI.type
        Text
          The type can also be "BlackBoxParameterSpace"  
        Example
          bbI.unknowns
        Text
          Lets now look at a blackBoxIdeal defined by an
          evaluation. The standart example is the determinant
          of a matrix:
        Example
          M = (point) -> (point_{0,1,2}||point_{3,4,5}||point_{6,7,8})
          phonePoint = matrix{{1,2,3,4,5,6,7,8,9_QQ}}
          M(phonePoint)
          detM = (point) -> matrix{{det(M(point))}}
          detM(phonePoint)   
          S = QQ[m_1..m_9]  
          bbE = blackBoxIdealFromEvaluation( S, detM );
          bbE.valuesAt(phonePoint)
          bbE.knownAttributes()
        Text
          Notice that "equations" and "jacobian" are missing, since
          no explicit equations of the blackBoxIdeal are provided.
        Example
          bbP = blackBoxParameterSpace(2,QQ);
          bbP.knownAttributes()
        Text
          For a blackPointParameterSpace "ring" and "unknowns" are 
          missing since there are no equations (not even implicit ones)  
    SeeAlso
         pointProperties
         knownMethods
///

doc ///
    Key
        "knownMethods"
    Headline
        list the methods of a black box 
    Usage   
        bbI.knownMethods()
    Inputs  
        bbI: BlackBoxParameterSpace
    Outputs
        : List       
    Description
        Text
          Every BlackBoxIdeal has some methods
          provided by the package 
          (no new attributes can be defined by the user).
          The difference between attributes and methods is,
          that an attribute is a constant while a property is 
          a function.
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal (x^2-y^3);
          bbI.knownMethods()
        Text
          Each of these methods has their own documentation node.
    SeeAlso
         hasPointProperty
         knownAttributes
         pointProperties
         knownPointPropertiesAsSymbols
         registerPointProperty
         rpp
         setSingularityTestOptions
         updatePointProperty
         upp
///

doc ///
    Key
        "pointProperties"
    Headline
        list the pointProperties of a black box 
    Usage   
        bbI.pointProperties()
        pointProperties bbI
    Inputs  
        bbI: BlackBoxParameterSpace
    Outputs
        : List       
    Description
        Text
          A pointProperty of a black box is a function that depends
          only on the coordinates of a point in the parameter space. 
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal (x^2-y^3);
          bbI.pointProperties()
          bbI.isZeroAt(matrix{{0,0_QQ}})
          bbI.jacobianAt(matrix{{1,1_QQ}})
          bbI.isProbablySmoothAt(matrix{{1,1_QQ}})
          bbI.isCertainlySingularAt(matrix{{0,0_QQ}})
        Text
          Each of these pointProperties has their own documentation node.
          New pointProperties can be defined by user using registerPointProperty.
          Exisiting pointProperties can be changed by using 
          updatePointProperty. 
          
          valuesAt and jacobianAt are special pointProperties,
          in the sense that the package uses them to compute other
          point properties. Therefore
          updating valuesAt triggers updates for 
          isZeroAt, numGenerators, jacobianAt, rankJacobianAt.
          Similarily updating jacobianAt triggers an update 
          for 'rankJacobianAt'.
    SeeAlso
      isCertainlySingularAt
      isProbablySmoothAt
      isZeroAt
      jacobianAt
      rankJacobianAt
      valuesAt
      registerPointProperty
      updatePointProperty
///



doc ///
    Key
        "rankJacobianAt"
    Headline
        determine the rank of the jacobian matrix of a black box ideal at a given point 
    Usage   
        bbI.rankJacobianAt(point)
    Inputs  
        bbI: BlackBoxIdeal
        point: Matrix
             coordinates of a point
    Outputs
        : Boolean
    Description
        Text
          This is a point property. It evaluates the jacobian matrix of the
          black box ideal at the point and determines its rank.
        Example
          R = QQ[x,y]
          I = ideal(x^2-y^3);
          bbI = blackBoxIdeal I;
          point = matrix{{8,4_QQ}}
          bbI.isZeroAt(point)
          bbI.jacobianAt(point)
          bbI.rankJacobianAt(point)
          bbI.isProbablySmoothAt(point)
        Text
          The cuspidal cubic considered above is singular at the
          origin. Therefore the jacobian matrix hat rank 0 there:
        Example
          origin = matrix{{0,0_QQ}}
          bbI.isZeroAt(origin)
          bbI.jacobianAt(origin)
          bbI.rankJacobianAt(origin)
          bbI.isCertainlySingularAt(origin)
        Text
          This point property is usefull when running experiments on
          black boxes that have serveral components of unknown 
          codimension. The rank of the jacobian matrix gives a upper bound on 
          the codimension of the components containing the point (with
          equality if the vanishing set is smooth at the point). Sorting 
          the number of points found in an experiment by the rank of their
          jacobian matrices
          helps to estimate the number of components of the vanishing set
          of the black box in each codimension.   
    Caveat
          This works only with black box ideals, since they contain an algorithm
          that can evaluate the generators of the black box ideal. A black box parameter spaces
          might contain only an algorithm that checks whether all generators vanish. 
          This happens for example if one considers the moduli space of singular cubics. 
          One can check whether a
          given cubic is singular without calculating the value of the 
          corresponding discriminant. 
///

doc ///
    Key
        "setSingularityTestOptions"
    Headline
        change how singularities are detected
    Usage   
        bbI.setSingularityTestOptions(precision,numTrials)
    Inputs  
        bbI: BlackBoxIdeal
        precision: ZZ
             length of jets used
        numTrials: ZZ
             number of such jets required     
    Outputs
        : Boolean
    Description
        Text
          To test wether a given point P on a variety given by
          a black box ideal is smooth, the package tries to construct
          several jets of certain length. The number of required trials and jetlengths
          is given by @TO{singularityTestOptions}@ and the default is to compute
          2 jets of length 10 starting at P. If the
          variety is smooth at P such jets always exists. If the variety is
          singular at P a generic jet can not be extended to arbitrary length.
          If the required number of jets can not be found the point property 
          isCertainlySingular has the value true. If the required number 
          of jets can be found the point property @TO{isProbablySmoothAt}@ has the
          value true.
        Example
          K = QQ
          R = QQ[x,y,z]      
          I = ideal (x*z,y*z)
          bbI = blackBoxIdeal I;
          pointOnLine = matrix{{0,1,0_K}}
          bbI.isProbablySmoothAt(pointOnLine)
          origin = matrix{{0,0, 0_K}}
          bbI.isProbablySmoothAt(origin)
          bbI.isCertainlySingularAt(origin)
        Text
          Since the singularity of the above curve
          at the origin is of small degree
          it is detected by looking at jets of length 10.
          If we change the test options to look only at jets of length 4,
          the singularity can not be detected.
        Example
          bbI.setSingularityTestOptions(4,1)
          bbI.singularityTestOptions()
          bbI.isCertainlySingularAt(origin)
        Text
          The construction of jets is time intensive. For many applications
          precision=2 and numTrials=1 is sufficient, even if this only detects
          the most simple singularities.  
    Caveat
          This works only with black box ideals, since they contain an algorithm
          that can calculate the derivative of the equations at a given point
          (jacobianAt). This is needed to construct jets iteratively.
    SeeAlso
          singularityTestOptions
          isCertainlySingularAt
          isProbablySmoothAt
          jetAt
///


doc ///
   Key
        interpolate
   Headline
        find polynomials containing a list of jets
   Usage   
        I = interpolate(mons,jetList)
   Inputs  
        mons:Matrix 
            of monomials
        jetList:List
            of jets    
   Description
        Text
            Finds those linear combinations of monomials that vanish
            on the given list of jets.
               
            Lets consider a black box that describes
            a line and a plane intersecting at the origin:    
        Example      
            K = ZZ/5
            R = K[x,y,z]
            I = ideal (x*z,y*z)
            bb = blackBoxIdeal I;       
        Text
            \break 
            Consider a point on the line:
        Example
            point = matrix{{0,0,1_K}}
        Text
            \break
            and a jet of lenght 3 starting at this point and
            lying on the variety described by the black box ideal
        Example
            j = jetAt(bb,point,4,1)     
        Text
            \break
            Now find linear polynomials containing this jet:
        Example
            interpolate(matrix{{x,y,z}},{j})   
        Text
            Notice that polynomials containig the line are found.
            The surface is invisible to the interpolation.   
   Caveat
       This function does not estimate the lenght of the jet needed to
       get a useful answer. (The jet should be at least as long as the
       number of monomials). This is done by @TO interpolateBB @. 
   SeeAlso
       interpolateBB
       createInterpolatedIdeal
///


doc ///
    Key
        interpolateBB
        (interpolateBB,  BlackBoxParameterSpace, Matrix, ZZ)
        (interpolateBB,  BlackBoxParameterSpace, Matrix, ZZ , MapHelper)
    Headline
        find polynomials containing a list of jets
    Usage   
        I = interpolateBB(BlackBox,point,maxDegree)
        I = interpolateBB(BlackBox,point,maxDegree,map)
    Inputs  
        maxDegree:ZZ 
            the maximal degree of polynomials considered
        BlackBox:BlackBoxIdeal
        point: Matrix
            a point where the Blackbox vanishes    
        map: MapHelper
            
    Description
        Text
           Finds all polynomials of degree at most maxDegree
           that contain the component on which the point lies.
           If the point is not smooth, an error will be produced.
       
           Lets consider a black box that describes
           a line and a plane intersecting at the origin:
        Example      
           K = ZZ/5
           R = K[x,y,z]
           I = ideal (x*z,y*z)
           bb = blackBoxIdeal I;       
        Text
           \break 
           Consider two points on the variety described 
           by the blackbox:
        Example
           pointOnLine = matrix{{0,0,1_K}}
           pointOnPlane = matrix{{0,1,0_K}}
        Text
           \break
           Now find linear equations containing the respective
           components on which the points lie:
        Example
           interpolateBB(bb,pointOnLine, 1)
           interpolateBB(bb,pointOnPlane, 1)
        Text
           \break
           Finding points on the different components can be done
           by running an  Experiment. Interpolating a component
           for all points found can be done by ....   
    Caveat
        This function does not work with multigraded rings.
        At the moment this has to be done by hand with @TO interpolate @. 
      
        At the moment the interpolation is done by producing one
        jet of the appropriate length. Often one could interpolate
        much faster if several shorter jets were used. (Most of the
        time is used when producing the jets)
    SeeAlso
        interpolate
        createInterpolatedIdeal    
///



TEST ///
    --test for issue #117
    coeffRing := ZZ/3;
    numVariables := 2;
    R = coeffRing[x,y];

    bb = blackBoxParameterSpace( numVariables , coeffRing );

    P = matrix{{0_coeffRing, 0_coeffRing}}
    valuesAt := (blackBox, point)-> matrix {{0_coeffRing }};
    bb = bb.rpp("valuesAt",valuesAt);

    bb.valuesAt(P)
    bb.isZeroAt(P)
    jetAt(bb,P,1,1)
///

TEST ///
    -- test for issue #111
    --
    coeffRing := ZZ/3;
    numVariables := 2;
    bb = blackBoxParameterSpace( numVariables , coeffRing );

    try (bb.setSingularityTestOptions(5,5);) then (error "should fail";) else();

    try (bb.singularityTestOptions();) then (error "should fail";) else();

    valuesAt := (blackBox, point)-> matrix {{5}};
    bb = bb.rpp("valuesAt",valuesAt);
    bb.singularityTestOptions()
    bb.setSingularityTestOptions(5,5);
    point = matrix{{0_coeffRing, 1_coeffRing}}
    bb.valuesAt (point)
    catchOrResult = bb.isCertainlySingularAt(point)
    assert (class catchOrResult === PointNotOnBlackBox)
///

TEST ///
    --test for issue #86
    R = QQ[x,y]
    I = ideal(x^5-y^7);
    bbI = blackBoxIdeal I;
    smoothPoint = matrix{{8,4_QQ}}
    bbI.valuesAt(smoothPoint)
        
    try  (bbI.isProbablySmoothAt(smoothPoint)) then (assert(false)) else ()
    origin = matrix{{0,0_QQ}}
    bbI.isProbablySmoothAt(origin)
    bbI.isCertainlySingularAt(origin)
    bbI.setSingularityTestOptions(4,1)
    bbI.isCertainlySingularAt(origin)
    bbI.isProbablySmoothAt(origin)
///


TEST ///
    -- test issue #21
    
    coeffRing := ZZ/3;
    numVariables := 2;
    bb = blackBoxParameterSpace( numVariables , coeffRing );

    R = coeffRing[x,y];
    valuesAt := (bb, point)-> matrix {{sub(  x^2, point )}};
    bb = bb.rpp("valuesAt",valuesAt);
    P = matrix{{0_coeffRing, 0_coeffRing}}
    bb.jacobianAt(P)
    P = matrix{{1_coeffRing, 1_coeffRing}}
    valuesAt(bb,P);
    
    --bb.jacobianAt(point) should throw an  PointNotOnBlackBox exception
    assert (PointNotOnBlackBox === class catch  bb.jacobianAt(P) )     

    valuesAt = ( point)-> matrix {{sub(  x^2, point )}};
    valuesAt(P);
    bbE = blackBoxIdealFromEvaluation (R , valuesAt );

    P = matrix{{0_coeffRing, 0_coeffRing}}
    bbE.jacobianAt(P)
    P = matrix{{1_coeffRing, 1_coeffRing}}
    --bbE.jacobianAt(point) should throw an  PointNotOnBlackBox exception
    assert (PointNotOnBlackBox === class catch  bbE.jacobianAt(P) ) 
    
    
    C = QQ;
    R = QQ[x];
    bbI = new BlackBoxIdeal from ideal x;
    point = matrix{{1_QQ}};
    
    -- bbI.jacobianAt(point) should gove an error , becaues the point is not on the variety
    assert(PointNotOnBlackBox === class catch  bbI.jacobianAt(point) )      
    

///


TEST ///
    -- test issue #35
    K = QQ;
    R = K[x,y];
    bbI = new BlackBoxIdeal from ideal x*y;
    point = matrix{{1_QQ, 1_QQ}};

    -- not on black box throws error
    jetOrError = catch jetAt(bbI,point,1,10)
    assert (class jetOrError === PointNotOnBlackBox)
    
    point = matrix{{0_QQ, 0_QQ}};
    jet =  jetAt(bbI,point,1,10)    
    assert(jet =!= null)
    assert(length jet == 1)
    jet =  jetAt(bbI,point,0,1)
    assert(length jet == 0)
///


JetSet = new Type of MutableHashTable;
 

new JetSet from Thing := ( E, thing) -> 
(
    error "creating JetSet from  type " | toString E | " not implemented or not possible ";
);


new JetSet from Jet := ( E, thing) -> 
(
   mht := new MutableHashTable;
   
   mht#"list" = new List from {thing};
   return mht;
);

firstElem = method();

firstElem (JetSet) := Jet => (jetset)->
(
    return first jetset#"list";
);

locus  (JetSet) := Thing => (jetSet )->
(
    return locus firstElem jetSet;
);

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
    return compatible(firstElem jetSet1, firstElem jetSet2);
);

size (JetSet) := ZZ =>(jetset)->
(
    return #(jetset#"list");
)


-- probablySameComponent; certainlyDifferentComponent

AddElement = method();

AddElement (JetSet,Jet) := JetSet => (jetSet, jet)->
(
    if ( size jetSet===0 or
         compatible(firstElem jetSet, jet) 
       ) then
        (
            jetSet#"list" = append(jetSet#"list",jet);            
            return jetSet;
        );
    error ("JetSet and Jet are probably incompatible");
)

joinJetSets = method();

joinJetSets (JetSet,JetSet) := JetSet => (jetSet1, jetSet2 )->
(
    if (compatible (jetSet1, jetSet2)) then 
    (
        result := new JetSet;
        result#"list" = unique join (jetSet1#"list",jetSet2#"list");
        return result;
    )
    else
    (
        error ("jet sets probably not compatible");
    );
);




TEST ///
    -- restart 
    -- loadPackage "BlackBoxIdeals"
    K = QQ;
    R = K[x,y];
    bbI = new BlackBoxIdeal from ideal x*y;
    
    
    point = matrix{{0_QQ, 0_QQ}};
    
    jet =  jetAt(bbI,point,1,10)
    
    locus jet
    jet2 =  jetAt(bbI,point,1,10)   
    
    compatible(jet,jet2) 
    
    jetSet = new JetSet from jet
    
    locus jetSet      
    compatible(jetSet,jetSet)    
    
    AddElement(jetSet,jet2)
    
    joinJetSets(jetSet,jetSet);    
    -- unique: not im    
///



end

-- Todo: introduce force mode option for registerPointProperty? (overwriting registered property?)
-- todo: introduce for all known properties a precomposition with checkInputPoint
    


-- Estring = new Type of String ; the goal was to distinguies string keys 
-- from symbol keys without changing string visuaulization (net())
-- but that turned out to be impossible(or too hard), because e.g. the operator '#' cannot be overloaded.


--net (String) := Net =>(str)->(   strl := "\""| str| "\"" ;   return stack separate strl; );


--(JK) why did I need comparison between a string and a symbol ? (jk) - if I recall correctly, it was related for intervals...

String ? Symbol := (str,symb)->
(
    1 ? 2
);

Symbol ? String := (str,symb)->
(
    2 ? 1
);
-- Q: how to get the parameter list of a function or method?
--
-- answer:  not possible yet, but it is possible to get the number of parameters 
-- using 'disassemble' for functions (where the number of variables is fixed and not variable)
-- and for methods it forks as follows: consider a method foo with several functions installed.
-- then   apply(methods foo, m-> (m, disassemble lookup m) )
   
-- just after exporting:
-- why is/was setting 'jacobianAt' necessary??? (jk)

-- jacobianAt := global jacobianAt;
-- jacobianAt = global jacobianAt;
--
--

-- jetAtWrapper currently deprecated !
-- options makes more sense for trials (=1)
jetAtWrapper = method( Options=>{"jetLength" =>4, "trials"=>5} );

jetAtWrapper( BlackBoxParameterSpace, Matrix )  := MutableHashTable => opts -> ( blackBox,  point )  ->
(
    return jetAt( blackBox,  point, opts#"jetLenth", opts#"trials");
);



-- need test for randomIterator (for fixed error : to less trials  )
--@TO2{jetAt,"jet"}@ computations at a point independently of the ideal representation. \break \break 


--        (clearCoeffDenominators, Ideal )    



Point := new Type of HashTable;


pointObject (Thing, Matrix) := (thing, point)->
(
    if not (thing.hasElement(point)) then 
        error(" point is not on parent ");
    pointObject = new HashTable from {("parent",thing),
                                      ("point",point)
                                     }
    return pointObject;
);


BlackBoxPoint := new Type of Point
{
    parent is of type BlackBoxParameterSpace or derived
    point is a Matrix?
}

blackBoxPointObject (BlackBoxParameterSpace, Matrix) := (blackBox, point)->
(
    if not (blackBox.isZeroAt(point)) then error(" point is not on BlackBox ");
    pointObject = new HashTable from {("parent",blackBox),
                                      ("point",point)
                                     }
    return pointObject;
);

-- single trial returns 

(jet, bestJet, failedJetLength)





JetInfoAt 
(
    bestJet
    worstJet
    Tally failedJetLength=> count
    targetLength
)

-- or even better, JetInfoAt contains pairs length -> list of jets with that length.

-- 
 
-- and now we can pass the black box and the point ! (we have free another two parameters)

-- we could do precision Jet and then check  first "jetLength", then 'precision ring jet'.


InterpolatedIdeal: parent is JetSet

Jet: parent is Point ? (or black box and point)




worstJetAt = method();

worstJetAt( BlackBoxParameterSpace, Matrix, ZZ, ZZ) := Jet => ( blackBox,  point, jetLength, numTrials )  ->
(
    if ( numTrials<1 ) then error "jetAt: expected numTrials >=1 ";
    
    jetResult  := jetAtSingleTrial ( blackBox,  point, jetLength);      
    
    worstJet := jetResult#"bestJet";
    
    for i in 2..numTrials do
    (
        newJetResult := jetAtSingleTrial ( blackBox,  point, jetLength);

        if ( length worstJet > length newJetResult#"bestJet" ) then
        ( 
            worstJet = newJetResult#"bestJet";
        );       
    );
    return worstJet;
);


-- returns the longest jet with length <=jetLength obtained after maximal numTrial trials
bestJetAt= method();

bestJetAt( BlackBoxParameterSpace, Matrix, ZZ, ZZ) := Jet  => ( blackBox,  point, jetLength, numTrials )  ->
(
    if ( numTrials<1 ) then error "jetAt: expected numTrials >=1 ";
    
    jetResult := jetAtSingleTrial ( blackBox,  point, jetLength);    

    if  jetResult#"succeeded"  then  return jetResult#"jet"; 
    
    bestJet := jetResult#"bestJet";

    for i in 2..numTrials do
    (
        newJetResult  := jetAtSingleTrial ( blackBox,  point, jetLength);

        if  newJetResult#"succeeded"  then return  newJetResult#"jet"; 

        if ( length bestJet  < length  newJetResult#"bestJet"  ) then
        ( 
            bestJet =  newJetResult#"bestJet";
        );       
    );
    return bestJet;
);
