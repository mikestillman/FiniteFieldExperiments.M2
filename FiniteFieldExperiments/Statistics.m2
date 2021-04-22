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





