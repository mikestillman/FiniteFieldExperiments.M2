TEST ///
    loadPackage ("FiniteFieldExperiments",Reload=>true)
    debug FiniteFieldExperiments
    FiniteFieldExperimentsProtect()
    assert ( (poissonEstimate(16,"confidence" => 2)).min == 8  );
    assert ( (poissonEstimate(16,"confidence" => 2)).max == 24 );
///

TEST ///
    loadPackage ("FiniteFieldExperiments",Reload=>true)
    debug FiniteFieldExperiments
    FiniteFieldExperimentsProtect()
    assert (poissonEstimate(16,"confidence"=>2) == new Interval from (8,24))
///

TEST ///
    debug FiniteFieldExperiments
    FiniteFieldExperimentsProtect()
    assert (estimateCodim(11^2*10,10,11) == new Interval from (1.8,2.4))
///


TEST ///
    debug FiniteFieldExperiments
    FiniteFieldExperimentsProtect()
    estimate =  estimateNumberOfComponents( 11^2*16, 2, 16, 11, "confidence"=>2 );
    estimate = estimate.round(1);
    assert ( estimate == new Interval from (0.5,1.5))
///

TEST ///

    rng = QQ[x];
    bbI = new BlackBoxIdeal from ideal(x)
    e = new Experiment from bbI

    weakPoint =()->
    (
        num := random(ZZ);
        if odd num then 
        return random(QQ^1,QQ^1)
        else
        return null;
    );
    wrpi = createRandomPointIterator(weakPoint);
    e.setPointIterator(wrpi);
    e.run(100)
    e.trials();
    assert(e.trials()>100);
    wrpi.reset();
    assert(wrpi.position()==0);
    assert(wrpi.getPoint()===null);
  
///

TEST ///
    debug FiniteFieldExperiments
    FiniteFieldExperimentsProtect()
    pointIterator = createRandomPointIterator(ZZ/7, 5)
    pointIterator.next()
    pointIterator.getPoint()
    apply(99, i-> pointIterator.next() )
    assert(pointIterator.position()==100);
    pointIterator.reset();
    assert(pointIterator.position()==0);
    assert(pointIterator.getPoint()===null);
///

TEST ///
   -- test that exceptions and Errors as point Properties are catched:
   
    K = ZZ/5;
    R = K[x,y,z];
    I = ideal (x*z,y*z);
    bb = blackBoxIdeal I;

    rankAllways5At = (point) -> 5;
    bb.rpp("exception",(p)->throw new Exception);

    e = new Experiment from bb;
    e.watchProperty("exception");
    bb.rpp("err",(p)->error("error during computation"));
    e.watchProperty("err");
    setRandomSeed (42);
    e.run(10)

    firstTuple= e.observedPropertyTuple(0);
    assert(firstTuple#1 === Exception);
    assert(firstTuple#2 === Error);
///

TEST ///

    rng = QQ[x];
    bbI = new BlackBoxIdeal from ideal(x)
    e = new Experiment from bbI
    
    weakPoint =()->
    (
        num := random(ZZ);
        if odd num then 
        return random(QQ^1,QQ^1)
        else
        return null;
    );
    wrpi = createRandomPointIterator(weakPoint);
    e.setPointGenerator(weakPoint);
///
