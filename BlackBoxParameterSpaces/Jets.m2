savedEpsRings := new MutableHashTable;

--
-- package-global symbol for "eps"
--
--geps := getSymbol "eps";

geps := symbol eps;


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


sub (Ideal, Jet ) := Thing =>(I,jet)->
(
    return (sub(I,jet#"value"));
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

--------------------------------------------------
-- Testing smoothness ----------------------------
--------------------------------------------------

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
        return localSmoothnessTester.isCertainlySingularAt(blackBox, point, options.Precision, options.NumTrials);
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
        return not localSmoothnessTester.isCertainlySingularAt(blackBox, point, options.Precision, options.NumTrials);
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

--------------------------------------------------
-- Projections? MapHelper ------------------------
--------------------------------------------------

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

--------------------------------------------------
-- JetSet ----------------------------------------
--------------------------------------------------
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

 
