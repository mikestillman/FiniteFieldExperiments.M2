-- code for Experiment statistics

--95 % aller Messwerte haben eine Abweichung von höchstens 1,96 * sigma (Normalverteilung)

-- rename 'confidence' to 'confidenceInSigma' ?

poissonEstimate = method( Options => {"confidence" => 1.96} );

poissonEstimate(ZZ) := HashTable => opts -> (numPoints) -> 
( 
    -- we use a poisson approximation since there are only 
    -- very few solution points compared to the number of trials
    --
    -- we then use the normal approximatiom of the poissondistribution
    -- to calculate the confidence interval
    -- 
    -- (jk): to use the normal approximation of the poisson distribution numPoints should be >30 ?
    --
    --
    -- for a Poisson distribution the standard deviation is
    -- the square root of the expected value
    --
    err := opts#"confidence"*sqrt(numPoints); 
    estimate := new Interval from   (  max(round(1,numPoints-err), 0.0001),    round(1,numPoints+err)  );
    return estimate;
);


--
-- estimateCodim() is not used anywhere. deprecated?
--
estimateCodim = method( Options => (options poissonEstimate) );

estimateCodim( ZZ, ZZ, ZZ ) := Interval => opts->
    ( trials, numPoints, fieldCardinality ) -> 
(
    -- hints:
    -- estimatedInterval = fraction of fp-rational points (interval estimate) 
    --                   ≈ r * (1/cardinality)^codim + higher order terms in 1/cardinality
    -- rules: log_b(x) = - log_(1/b)(x). 
    -- with this we get. log_(1/cardinality) gamma_P = -log_(cardinality) gamma_P 
    -- = -log_natural(gamma_p)/log_natural(cardinality)
    --
    estimatedInterval := (1/trials)*poissonEstimate(numPoints,opts);     
    logEst := new Interval from (-log estimatedInterval.max,-log estimatedInterval.min);
    codimEst := (1/log fieldCardinality)*logEst;
    return new Interval from (round(1,codimEst.min),round(1,codimEst.max));
);


estimateNumberOfComponents = method( Options => (options poissonEstimate) );

estimateNumberOfComponents( ZZ, RR, ZZ, ZZ ) := HashTable => opts->
    ( trials, estimatedCodim, numPoints, fieldCardinality ) -> 
(
    return poissonEstimate( numPoints,opts )*( (fieldCardinality^estimatedCodim)/trials*1.0  );
)


estimateNumberOfComponents( ZZ, ZZ, ZZ, ZZ ) := HashTable => opts->    (trials, estimatedCodim, numPoints, fieldCardinality) -> 
(
    estimateNumberOfComponents(trials,estimatedCodim*1.0,numPoints,fieldCardinality,opts)
)

-- estimateNumberOfComponents()
--
-- estimates the interval for number of components for points with given set of properties 
-- uses jacobian rank as codim estimator.
-- the interval is estimated using poissonEstimate
--
-- (jk)  later :m'estimateNumberOfComponents'  could be encapsulated in a ComponentEstimator. 
--       Purpose: replace componentNumberEstimator if required (we could have different estimators)
--
-- interface: ComponentNumberEstimator.estimate(realizedPointPropertyTuple, estimatedCodimension) ;
--
estimateNumberOfComponents(Experiment,List) := HashTable => opts->    (experiment,realizedPointPropertyTuple) -> 
(
    countData := experiment.counts();
    posRankJacobianAt := experiment.position( experiment.usedRankJacobianAt() );
    if posRankJacobianAt === null then error("To estimate number of components, \"rankJacobianAt\" must be watched");
    
    cardinality := experiment.coefficientRingCardinality();
    
    jacobianRank := realizedPointPropertyTuple#posRankJacobianAt;
    estimatedCodimension:= jacobianRank;
    
    numPointsWithGivenPropertyTuple := countData#realizedPointPropertyTuple;
    
    return estimateNumberOfComponents(  experiment.trials(),
                                        estimatedCodimension,
                                        numPointsWithGivenPropertyTuple,
                                        cardinality,
                                        opts
                                     );
)

EstimatedDecomposition = new Type of HashTable;

-- todo 1. why do we not save the experiment?
-- todo 2: createEstimatedDecomposition should be an internal function.

createEstimatedDecomposition := (bbi, estimate, watchedProperties, usedConfidence)->
(
    ht :=   new HashTable from {  
        "blackBox" =>bbi,
        "estimate"=> estimate,
        "watchedProperties" =>watchedProperties,
        "usedConfidence" => usedConfidence
    
    };
    return new EstimatedDecomposition from ht;
)


net (EstimatedDecomposition) := Net =>(ed)->
(
   formatinfo :=  {
                    -- net "-- format: ",
                        net ("--(estimated codim, estimated number of components [confidence interval " | toString ed#"usedConfidence" |"*σ]) <= "
                            | toString ed#"watchedProperties" | " )")
                    };
    estimate := ed#"estimate";

    -- estimate#0 is (estimated codim, estimated number of components [confidence interval])
    -- and estimate#1 is the list of watched properties.
    sss := sort apply (estimate, entry ->  net( entry#0)  | " <= " | net (entry#1 ) );

    sss = formatinfo |sss;
    return net stack sss;
)


estimateDecomposition = method (Options =>  (options poissonEstimate));

  estimateDecomposition (Experiment) := EstimatedDecomposition  => opts -> (experiment) ->
    (
        posRankJacobianAt := experiment.position(  experiment.usedRankJacobianAt() );
        if posRankJacobianAt === null then error("To estimate the decomposition, \"rankJacobianAt\" must be watched");

        estimate := flatten apply(keys experiment.counts(), 
                                          valuesOfProperties-> (
                                                           (valuesOfProperties#posRankJacobianAt,
                                                            estimateNumberOfComponents(experiment,valuesOfProperties, opts)
                                                           ),
                                                           valuesOfProperties
                                                                )
                                    );
        
        return createEstimatedDecomposition(experiment.blackBoxIdeal(), estimate, experiment.watchedProperties(), opts#"confidence" );
    );


createDecompositionEstimator = method(Options => (options poissonEstimate));


createDecompositionEstimator (Experiment) := Thing => opts-> (experimentParameter) ->
(
    decompEstimator := new MutableHashTable;    
               
    decompEstimator.estimateDecomposition =  (experiment) -> 
    (
        posRankJacobianAt := experiment.position(  experiment.usedRankJacobianAt() );
        if posRankJacobianAt === null then error("To estimate the decomposition, \"rankJacobianAt\" must be watched");

        -- estimate data is  a list of pairs:
        -- first entry is (rank at points, interval for estimated number of components)
        -- and second entry is the value array of watched properties.
        estimate := flatten apply(keys experiment.counts(), valuesOfWatchedProperties-> (
                                                                    (valuesOfWatchedProperties#posRankJacobianAt,
                                                                    estimateNumberOfComponents(experiment,valuesOfWatchedProperties, opts)),
                                                                    valuesOfWatchedProperties
                                                                )
                                    );
        
        return createEstimatedDecomposition(experiment.blackBoxIdeal(), estimate, experiment.watchedProperties(), opts#"confidence" );
    );
    decompEstimator = new HashTable from decompEstimator;
    
    return decompEstimator;     
);

--needs to be documented
 
EstimatedStratification = new Type of HashTable;

createEstimatedStratification := (bbi, estimate, watchedProperties)->
(
    ht :=   new HashTable from {  
        "blackBox" =>bbi,
        "estimate"=> estimate,
        "watchedProperties" =>watchedProperties,
    
    };
    return new EstimatedStratification from ht;
)

netEstimatedStratification = (es)->
(
  
    formatinfo :=  {
                    -- net "-- format: ",
                        net ("-- estimated codim <= "
                            | toString es#"watchedProperties" | " )") 
                    };

    estimate := es#"estimate";

    sss := sort apply (estimate, entry ->  net( entry#0)  | " <= " | net (entry#1) );

    sss = formatinfo |sss;
    return stack sss;
)

net (EstimatedStratification) := Net =>(es)->
(
    return (net netEstimatedStratification(es));
)

possibleCodimComponents = (found, trials, orderK) -> 
(
    estimate := 1/trials*poissonEstimate(found);
    possibleAnswers := flatten apply(10,c->apply(4,d->(c,d+1,c-log(d+1)/log(orderK))));
    apply(
        select(possibleAnswers,a->((-log(estimate.min)/log(orderK)>a#2) and (-log(estimate.max)/log(orderK)<a#2))),
        a->(a#0,a#1)
        )
)

-- round a real number to one decimal place
-- and add a zero before and after the decimal point
-- if necessary (works only for positive numbers)
round1withZeros = (rr) -> 
(
    if rr<0 then error;
    if rr==0 then return "0.0";
    if rr<1 then return "0"|toString(round(1,rr));
    if round(1,rr) == round(0,rr) then return toString(round(1,rr))|".0";
    return toString(round(1,rr))
);


--deprecated?
roundCodim = (trials, found, orderK) -> 
(
    estimate := 1/trials*poissonEstimate(found);
    if (log(estimate.max)/log(orderK) - log(estimate.min)/log(orderK))<0.8 
    then return round1withZeros((log(trials)-log(found))/log(orderK))
    else return "..."
);
 
 estimateStratification = method();
 
estimateStratification (Experiment) := EstimatedStratification =>  (experiment) -> 
(
    trials := experiment.trials();
    orderK := experiment.coefficientRingCardinality(); -- this must be read from the experimentdata
    -- (jk): need more advice. Did we want to use a different ring for search that the ideal coefficient ring? If so, 

     -- if there are not enough points for a specific key, no reliable
     -- estimate can be made. More precisely:
     --
     -- Let n be the number of points found and t the number of trials.
     -- the error in the number of points is about 2\sqrt(n). So the maximum
     -- estimate within this error range is
     --
     -- log_p((n+2\sqrt(n))/t) 
     --  = log_p(n+2\sqrt(n)) - log_p(t)
     --  = log_p(n[1+2/\sqrt(n)) - log_p(t)
     --  = log_p(n) + log_p(1+2/\sqrt(n)) - log_p(t)
     --
     -- so the error in the estimated codimension is in the order of
     --      log_p(1+2/\sqrt(n))
     -- we want this error to be at most 0.2 so we get
     --
     --       log_p(1+2/\sqrt(n)) < 0.2
     --            (1+2/\sqrt(n)) < p^0.2
     --               2/\sqrt(n)  < p^0.2 - 1
     --               2/(p^0.2-1) < \sqrt(n)
     --               (2/(p^0.2-1))^2 < n
     --
     minPoints := (2/(orderK^0.2 - 1))^2;
     countData := experiment.counts();

     -- sort keys by number of occurence
     sortedKeysByOccurence := apply(reverse sort apply(keys countData,k->(countData#k,k)), i->i#1);
     -- remove keys that have not enough points
     sortedKeysByOccurence = select(sortedKeysByOccurence,k->countData#k>minPoints);
     

    --apply(sortedKeysByOccurence,k->(
    --      --print (net((log(trials)-log(i#0))/log(charK))|" <= "|net(i#1)));
    --      print (net(round(1,(log(trials)-log(countData#k))/log(orderK)))|" <= "|net(k)))
    --      );

    estimate := flatten apply(sortedKeysByOccurence,k->( (round1withZeros((log(trials)-log(countData#k))/log(orderK))), (k)) );
    --estimate := flatten apply(sortedKeysByOccurence,k->( (roundCodim(trials,countData#k,orderK))), (k)) );
    return  createEstimatedStratification(experiment.blackBoxIdeal(), estimate, experiment.watchedProperties() );
);




stratificationIntervalView := (stratificationData )->
(
    prerearrangedData := new HashTable from apply( keys stratificationData, key -> (stratificationData#key=>(stratificationData#key,key )));
    toSort := apply( keys stratificationData, key -> (stratificationData#key));
    sorted := sort (toSort); 
    rearrangedData := apply (sorted, key-> (prerearrangedData#key) );
    return new HashTable from rearrangedData;
);

