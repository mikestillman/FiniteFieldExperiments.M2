---------------------------------------------------------------------


---------------------------------------------------------------------
TEST ///
    debug BlackBoxParameterSpaces
    idealBlackBoxesProtect()
    testClearCoeffDenominators()
///

TEST ///
    debug BlackBoxParameterSpaces
    idealBlackBoxesProtect()
    testNestedRingCoeffsLCMDenominator()
///

TEST ///
    debug BlackBoxParameterSpaces
    idealBlackBoxesProtect()
    testTensoredClearCoeffDenominators()
///

TEST ///
    debug BlackBoxParameterSpaces
    idealBlackBoxesProtect()
    testBlackBoxIdeal()
///

TEST ///
    debug BlackBoxParameterSpaces
    idealBlackBoxesProtect()
    testBlackBoxIdealFromEvaluation()
///

TEST ///

    debug BlackBoxParameterSpaces
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

    R = ZZ[x1,x2,x3,x4]
    B2 = blackBoxIdealFromEvaluation( R, evalLinePlusConic)

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
    --debug BlackBoxParameterSpaces
    
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

TEST  /// 
    debug BlackBoxParameterSpaces
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
    bb.jetAt(P,1)
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
    catchOrResult = catch bb.isCertainlySingularAt(point)
    assert (class catchOrResult === PointNotOnBlackBox)
///

TEST ///
    --test for issue #86
    kk = ZZ/2
    R = kk[x,y]
    I = ideal(x^5-y^7);
    bbI = blackBoxIdeal I;
    smoothPoint = matrix{{8,4_kk}}
    bbI.valuesAt(smoothPoint)
        
    bbI.isProbablySmoothAt(smoothPoint)
    origin = matrix{{0,0_kk}}
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
    jetOrError = catch bbI.jetAt(point,1)
    assert (class jetOrError === PointNotOnBlackBox)
    
    point = matrix{{0_QQ, 0_QQ}};
    jet =  bbI.jetAt(point,1)    
    assert(jet =!= null)
    assert(length jet == 1)
    jet =  bbI.jetAt(point,0)
    assert(length jet == 0)
///

