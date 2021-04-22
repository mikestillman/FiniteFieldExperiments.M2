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
