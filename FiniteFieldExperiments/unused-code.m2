-- note: this code is taken from FIniteFieldExperiments.m2
-- perhaps wee ant to include these functions, examples, perhaps not...

--#??? fuer den Fall dass dich die Schlüssel nicht sortieren lassen.

-- tryProperty = (experiment, property) ->(
--     pointLists := experiment.pointLists();
--     apply(
--           apply((keys pointLists),key -> (key,tally apply(pointLists#key,property))),
--           i->print (net i#0|" => "|net i#1)
--           )
--     )

---

--update := method();
--update(ExperimentData) := Tally => opts -> (experimentData) -> 
--( 
--   pointIterator := createIterator (  experiment.points() );
--   experimentData.pointData = new MutableHashTable;
--   experimentData.countData = new Tally;

--   while ( pointIterator.next() ) do
--   (
--       runExperimentOnce( experimentData, pointIterator.getPoint(), pointsPerComponent );
--   );
--);


-- deprecated
estimateStratification2 = (e) -> 
(
    --count := e.countData();
    trials := e.trials();
    orderK := (e.coefficientRing()).order; -- this must be read from the experimentdata
    -- print "--";
    -- apply(e.countsByCount(),i->(
    --       --print (net((log(trials)-log(i#0))/log(charK))|" <= "|net(i#1)));
    --       print (net(round(1,(log(trials)-log(i#0))/log(orderK)))|" <= "|net(i#1)))
    --       )
    --  ;
    estimate := flatten apply(e.countsByCount(),k->( round1withZeros((log(trials)-log(k#0))/log(orderK)), (k#1)) );
    return  createEstimatedStratification(e.blackBoxIdeal(), estimate, e.watchedProperties() );
)



estimateDecompositionOld := (experiment) -> 
(
    countData := experiment.counts();
    posRankJacobianAt := experiment.position(  experiment.usedRankJacobianAt() );
    if posRankJacobianAt === null then error("To estimate the decomposition, \"rankJacobianAt\" must be watched");

    cardinality := experiment.coefficientRingCardinality();

    print( "(estimated codim, estimated number of components [confidence interval] <= {watched Properties})");
    print "";
    apply(sort apply(keys countData,key->
        (net(
            key#posRankJacobianAt,
            estimateNumberOfComponents(
            experiment.trials(),
            key#posRankJacobianAt,
            countData#key,
            cardinality ) )
        ) |" <= " |net key ),
    print);
);
-- todo: updateExperiment?

    -- maybe think about writing 'connectProperty' ... (propName, bb.propName); default is 1:1.
    -- then, what should happen if a user requests to watch property xy ?
    
