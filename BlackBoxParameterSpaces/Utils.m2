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

