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


--
-- this function is final, that means nobody should use this method for creating a derived object
--
new BlackBoxParameterSpace from Ring := (E, pRing )->
(
    blackBox := blackBoxParameterSpaceInternal(BlackBoxParameterSpace, pRing);
   
    return blackBox;
)

blackBoxParameterSpace = method();

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

----------------------------------------------------
-- Basic accessor functions ------------------------
----------------------------------------------------

coefficientRing BlackBoxParameterSpace := bb -> bb.coefficientRing
numVariables BlackBoxParameterSpace := bb -> bb.numVariables

registerPointProperty = method()
registerPointProperty(BlackBoxParameterSpace, String, Function) := (bb, name, fcn) ->
    bb.registerPointProperty(name, fcn)

rpp = registerPointProperty

updatePointProperty = method()
updatePointProperty(BlackBoxParameterSpace, String, Function) := (bb, name, fcn) ->
    bb.updatePointProperty(name, fcn)

upp = updatePointProperty

    

----------------------------------------------------

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
    singularTestOptions.Precision = 10;
    singularTestOptions.NumTrials = 2;

   
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

        singularTestOptions.Precision = prec;
        singularTestOptions.NumTrials = numTrials;
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


