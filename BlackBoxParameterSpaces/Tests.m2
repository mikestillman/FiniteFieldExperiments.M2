TEST ///
-*
  restart
  needsPackage "BlackBoxParameterSpaces"
*-  
  kk = ZZ/5
  bb = blackBoxParameterSpace(10, kk);
  assert(coefficientRing bb === kk)
  assert(numVariables bb == 10)
  bb

  S = kk[x,y,z];
  I = ideal(x^5-y^2*z-z^2)
  bb = blackBoxIdeal I;
  numVariables bb
  coefficientRing bb
///


----------------------
-- Test of Utils -----
----------------------
TEST ///
    debug BlackBoxParameterSpaces
    testGuessAcceptedParameterNumber()
///

TEST ///
    debug BlackBoxParameterSpaces
    testDeduceNumGenerators()
///

TEST ///
    debug BlackBoxParameterSpaces
    testDeduceJacobianAt()
///

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

TEST ///
    debug BlackBoxParameterSpaces
    testClearCoeffDenominators()
///

TEST ///
    debug BlackBoxParameterSpaces
    testNestedRingCoeffsLCMDenominator()
///

TEST ///
    debug BlackBoxParameterSpaces
    testTensoredClearCoeffDenominators()
///

TEST ///
    debug BlackBoxParameterSpaces
    testBlackBoxIdeal()
///

TEST ///
    debug BlackBoxParameterSpaces
    testBlackBoxIdealFromEvaluation()
///

TEST ///

    debug BlackBoxParameterSpaces

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

----------------------------------------------------------------------
-- Test of Jets ------------------------------------------------------
----------------------------------------------------------------------

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
    
    js = new JetSet from jet
    
    --locus jetSet      
    compatible(js,js)
    
    addElement(js,jet2)
    
    joinJetSets(js,js);    
    
    jet3 := bbI.jetAt(point2,1)
    
    try (addElement(js,jet3)) then (error ("adding incompatible jet should fail"))  else()
    
    -- unique: not im    
///

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
 
---------------------------------
-- test of BlackBoxIdeal's ------
---------------------------------

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

--------------------------------
-- Tests: Interpolation --------
--------------------------------

TEST ///
    R = ZZ[x,y]
    monomialMaxDegree = 4;
    mons = matrix {flatten apply(monomialMaxDegree+1, currentDegree->flatten entries basis(currentDegree,R))};
    assert (rank source mons == monomialBasisSize(4,R));
   
///

TEST ///
    -- loadPackage "BlackBoxParameterSpaces"
    R = ZZ[x,y]
    I = ideal (x*y)
    bb = new BlackBoxIdeal from I
    p = matrix {{1,0_QQ}}
    jetset = new JetSet from bb.jetAt(p,1)
    II = createInterpolatedComponent(I, 1, jetset, bb)
    ideal II 
///

TEST /// 
    -- bug: a component name is used again after renaming.
    -- fix: always increase nextComponentId by one. 
    -- loadPackage "BlackBoxParameterSpaces"
    errorDepth=2
    K = ZZ/11;
    R = K[x,y]
    I = ideal (x-y^2)*x;
    bbI = new BlackBoxIdeal from I;
    p1 = matrix {{0,1_K}}
    p2 = matrix {{1,1_K}}
    c1 = bbI.interpolateComponentAt(p1)
    c1Name = c1#"name"()
    bbI.renameInterpolatedComponent(c1Name, "renamedComponent")
    c2 = bbI.interpolateComponentAt(p2)
    c2Name =  c2#"name"()
    assert(c1Name != c2Name)
    
///

TEST ///
    -- bug: jet length too short 
    -- loadPackage "BlackBoxParameterSpaces"
    kk = ZZ
    R = kk[x,y]
    I  = ideal (x*y*(x^2-y^2)*(y^4-3*x-7))
    bb = new BlackBoxIdeal from I; 
    
    p3 = matrix {{3,2_kk}}
    origin = matrix {{0, 0_kk}}
    singularPoint  = matrix{{0,0_kk}}
    bb.valuesAt p3
    jlh = basicJetLengthHeuristic(bb.interpolator);    
    maxDegree = 4;  
    mons = matrix {flatten apply(maxDegree+1, currentDegree->flatten entries basis(currentDegree,bb.ring))};
    
    jetLength = jlh.interpolationTargetJetLength(maxDegree)
    assert (jetLength >= rank source mons +10);
    
    pointList = {p1,singularPoint,p2,p3}
    pointList = {p1,p2,p3}
    pointList = {p3}
    maxDegree=1
    forcedInterpolationPrecision = 11
    iiList1 =  bb.interpolateComponentsAt(pointList,maxDegree)
    apply(iiList1, ic-> assert(1 == ic#"maxDegree"))
    maxDegree = 2
    iiList2 = bb.interpolateComponentsAt(pointList,maxDegree)   
    maxDegree=1
    iiList2b = bb.interpolateComponentsAt(pointList,1)
    apply(iiList2b, ic-> assert(2 == ic#"maxDegree"))
    
    bb.resetInterpolation()
    maxDegree = 2
    iiList2b = bb.interpolateComponentsAt(pointList,maxDegree)
    -- test zu lang !!
    --maxDegree = 5
    --bb.interpolateComponentsAt(pointList,maxDegree)
    --maxDegree = 6
    --bb.interpolateComponentsAt(pointList,maxDegree) 
    --bb.resetInterpolation()
    --maxDegree = 4
    --bb.interpolateComponentsAt(pointList,maxDegree) --ok
    --maxDegree = 5
///


TEST ///
   -- test for bug where onComponentPrecision is not respected (a longer jet was used instead of a short one)
    -- restart
    -- loadPackage "BlackBoxParameterSpaces"
 
    setRandomSeed(42); -- ensure reproducibility in test
    K = ZZ/7;
    R = K[x,y,z,w] ;     
    line = ideal (x,y);
    conic = ideal (w,x^2+y^2-z^2);
    bbI = blackBoxIdeal intersect(line,conic);
    assert (bbI.onComponentPrecision()==2); --default onComponent precision
    pointOnLine = matrix{{0,0,1,2_K}};
    pointOnConic = matrix{{3,4,5,0_K}};
    bbI.isZeroAt(pointOnLine);
    bbI.isZeroAt(pointOnConic);
    bbI.interpolateComponentAt(pointOnLine,1);
    bbI.renameInterpolatedComponent("c1","line");
    bbI.interpolateComponentAt(pointOnConic,1);
    bbI.renameInterpolatedComponent("c2","conic");
    pointOnLineAndPlane = matrix{{0,0,1,0_K}};
    assert(bbI.isZeroAt(pointOnLineAndPlane));
    compnames = bbI.interpolatedComponentNamesAt(pointOnLineAndPlane)    ;
    assert(1 == #compnames);
    bbI.setOnComponentPrecision(0);
    -- now we should get both components:
    compnames = bbI.interpolatedComponentNamesAt(pointOnLineAndPlane);
    assert(2 == #compnames);
    assert (null =!= position(compnames, name->name == "line"));
    assert (null =!= position(compnames, name->name == "conic"));
///


TEST ///
    -- bug: jet length too short (incorrect jet length heuristic)
    -- loadPackage "BlackBoxParameterSpaces"
    kk = QQ
    R = kk[x,y]
    I  = ideal (x*y*(x^2-y^2)*(y^4-3*x-7))
    bb = new BlackBoxIdeal from I;
    p1 = matrix {{1,1_kk}}
    p2 = matrix {{1,0_kk}}
    p3 = matrix {{3,2_kk}}
    origin = matrix {{0, 0_kk}}
    singularPoint  = matrix{{0,0_kk}}
    pointList = {p1,singularPoint,p2,p3}
    maxDegree = 1
    iiList1 = bb.interpolateComponentsAt(pointList,maxDegree)
    
    
    -- test zu lang!!!
    --maxDegree = 3
    --iiList3 = bb.interpolateComponentsAt(pointList,maxDegree) 
    --assert(3 == #iiList3);
    --bb.resetInterpolation()
    --maxDegree = 4
    --iiList4 = bb.interpolateComponentsAt(pointList,maxDegree)
    --assert(3 == #iiList4)
    
    --c3 = bb.componentsAt(p3)
    --assert (#c3 ==1)
    
    --c3 = first c3
    --assert (1==#(flatten entries gens ideal  c3))
    --assert (4==first flatten degrees ideal  c3)

    -- dies geht natuerlich nur wenn man alle Punkte angibt)
    --resultIdeal = product( apply( iiList4, ic->ideal ic))  
    -- assert (gens radical resultIdeal%  radical I
  
///



