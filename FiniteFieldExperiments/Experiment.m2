 --------------------------------------------------------------------------------------------------------------
---- observedValues, realizedValues realizedProperties and pointPropertyTupleValues do the same. Choose the preferred naming!
--------------------------------------------------------------------------------------------------------------

observedPropertyTuples = method();
observedPropertyTuples (Experiment) := Thing => (experiment)->
(
    return experiment.observedPropertyTuples();
);

--------------------------------------------------------------------------------------------------------------
---- realizedValue, realizedProperty realizedPointProperty , observedPropertyTuple do the same. Choose the preferred naming 
--------------------------------------------------------------------------------------------------------------
observedPropertyTuple = method();
observedPropertyTuple (Experiment,ZZ) := Thing => (experiment,pos)->
(
    return experiment.observedPropertyTuple(pos);
);


-- return the statistics for the watched properties : as Counts (derived from Tally)
-- where the key is the realized value set of properties and the value is the number of observed occurances
counts = method(); 
counts (Experiment) := Counts => (experiment)->
(
    return experiment.counts();
);

-- counts the number of collected points. Return type is Counts (derived from Tally and Tally from HashTable)
-- where the key is the realized value set of properties and the value is the number of collected points
collectedCount = method(); 
collectedCount (Experiment) := Counts => (experiment)->
(
    return experiment.collectedCount();
);

--------------------------------------------------------------------------------------------------------------
---- realizedValue, realizedProperty realizedPointProperty do the same. Choose the preferred naming 
--------------------------------------------------------------------------------------------------------------



points = method();

points(Experiment) := Thing => (experiment)->
(
    return experiment.points();
)


-- should return a points with their properties. (a HashTable where points are the keys)
richPoints = method();
 
richPoints (Experiment) := Thing => (experiment)->
(
    return experiment.pointsWithProperties();
);

-- same as richPoints 
pointsWithProperties = richPoints;


-------------------------------------------
-- unused/deprecated:
--createPointData = (pBlackBox, point)->
--(
--    blackBox := pBlackBox;
--    p := new MutableHashTable;
--    p.point = point;
    
--    localIsCertainlySingularAt := null;
--    p.isCertainlySingularAt = ()->
--    (
--        if (localIsCertainlySingularAt=!=null) then return localIsCertainlySingularAt;
--        localIsCertainlySingularAt = blackBox.isCertainlySingularAt(point);
--        return localIsCertainlySingularAt;
--    );
--);

-- todo: naming 'countsByCount' is still unfortunate

   -- countsByCount() sorts the statistic by Number
   
   
sortedCounts = method();
sortedCounts ( Experiment ) := (experiment)->
(
    return experiment.sortedCounts();
);


 
ringCardinality = (rng)->
(
    cardinality := null;
    if char rng ==0 then   cardinality  = infinity;
    try { cardinality = (rng).order } then () else();
    return cardinality;
);
 

new Experiment from Thing := (E, thing) -> 
(
    error("not impelemted");
)


Experiment + Experiment := Experiment => (re1, re2)->
(
    bb1 := re1.blackBoxIdeal();
    bb2 := re2.blackBoxIdeal();

    if  re1#"compatible"(re2)    then
    (
        re := copy re1;
        re.merge(re2);
        return re;
    )
    else error ("experiments not compatible");
);


watchedProperties = method();

watchedProperties (Experiment) := List => (experiment)->
(
    return experiment.watchedProperties();
);

new Experiment from BlackBoxParameterSpace := (E, pBlackBox) -> 
(
    blackBoxIdeal := pBlackBox;    --black box ideal or black box parameter space so far
    experimentData := new ExperimentData from blackBoxIdeal.coefficientRing;

    coefficientRingCardinality := ringCardinality( blackBoxIdeal.coefficientRing );

    experiment := new MutableHashTable;
    experiment = newClass( Experiment, experiment );

    -- todo: issue about naming here. blackBox (what?)
    --experiment.bbi = blackBoxIdeal; -- is this safe?
    --experiment.blackBoxIdeal = blackBoxIdeal;


    -- todo: maype return a (deep) copy 
    experiment.getExperimentData = ()->
    (
        ffelog.warning("-- warning : you got a reference to experiment data, do not modify! ");
        return experimentData;
    );

    -- some of the  following values initialized at end 
    -- ( e.g. propertiesAt initialization depends presence of some functions defined later)
    pointsPerComponent := 10;

    rankJacobianAtKey := null;
    rankJacobianAt := null;

    propertiesAt := (point)->{};
    isInteresting := (point)->true;

    pointIterator := createRandomPointIterator ( experimentData.coefficientRing, blackBoxIdeal.numVariables );

    experiment.experimentData=()->
    (
        return new ExperimentData  from experimentData;
    );

    experiment.blackBoxIdeal=()->
    ( 
        return blackBoxIdeal;
    );
    
    experiment.interpolateComponents = method();
    experiment.interpolateComponents (ZZ ) := List=>(maxDeg)->
    (
        return blackBoxIdeal.interpolateComponentsAt(experiment.points(), maxDeg);
    );
    
    
    decompositionConfidenceInterval := 1.96; -- value times sigma (measured in sigma)
    
    -- later the decompositionEstimator should be public and modifiable 
    -- (provide a method to modify the confidence parameter)    
    --decompositionEstimator := createDecompositionEstimator(experiment,"confidence"=>1.96);

    experiment.estimateDecomposition = () -> 
    (
        estimateDecomposition(experiment, "confidence"=> decompositionConfidenceInterval)
    );
    
    --experiment.estimateDecomposition = () -> (decompositionEstimator.estimateDecomposition(experiment));

    -- (jk) later setDecompositionConfidenceInterval must be dropped, because we never may know the config parameters for   
    -- a custom decomposition parameter (maybe provided by the user => learn the plugin interface!)
    -- the confidence interval is measured in sigma (not in percent)
    --
    experiment.setDecompositionConfidenceInterval = (confidenceInterval)->
    (
        decompositionConfidenceInterval = confidenceInterval;
    );

    experiment.estimateStratification = () -> (estimateStratification(experiment));

--   experiment.estimateStratification2 = () -> ( estimateStratification2(experiment) );

    experiment#"compatible" = method();
    experiment#"compatible" (Experiment) := Boolean =>(re2)->
    (
        bb1 := experiment.blackBoxIdeal();
        bb2 := re2.blackBoxIdeal();

     return (  experiment.pointsPerComponent() ==  re2.pointsPerComponent() and
               experiment.coefficientRing()       === re2.coefficientRing() and
               experiment.watchedProperties()     ==  re2.watchedProperties() and
               experiment.rankJacobianAtKey()         ==  re2.rankJacobianAtKey() and
               --experiment.isInteresting           ===  re2.isInteresting -- um sicher zu gehen, kann man  alle punkte in re2 durch isInteresting von e jagen und umgekehrt.
               experiment.pointKeys()             ==  re2.pointKeys() and

              bb1.numVariables            ==   bb2.numVariables and
              bb1.numGenerators           ==   bb2.numGenerators and
              bb1.coefficientRing         ===  bb2.coefficientRing 
             );
    );


    experiment.merge = method();

    -- todo: how to prevent from self-merging?
    experiment.merge (Experiment)  := Experiment => (re)->
    (
        if (experimentData==re.experimentData() ) then 
            error ("attempt to merge with itself");

        if   experiment#"compatible"(re) then 
        experimentData = experimentData + re.experimentData()
        else
        error ("experiments not compatible!");
    );
    
    experiment.stratificationIntervalView = ()->
    (
            stratificationData := experiment.estimateStratification();
            return stratificationIntervalView(stratificationData);
    );

    experiment.coefficientRing = ()->
    (
        
        return experimentData.coefficientRing;
    );
    
    -- todo: rename to sortedCounts or similar
    experiment.countsByCount = ()->
    (   
        --print ("--warning countsByCount deprecated. Use 'sort counts experiment' or 'sort experiment.counts()' or  experiment.sortedCounts()");
        return sort new SortableCounts from experimentData.countData ; 
    );
    
    -- same as countsByCount
    experiment.sortedCounts = ()->
    (           
        return sort  experimentData.countData ; 
    );



    experiment.coefficientRingCardinality=()->
    (
        -- return    ringCardinality( blackBoxIdeal.coefficientRing );
        return    coefficientRingCardinality;
    );

    
    experiment.testDebug=()->
    (
        a:=5;
        1/0;
        return a;
    );

        
    experiment.isInteresting=(point)->
    ( 
        return isInteresting(point);
    );

    experiment.points = ()->
    (
        return flatten  apply (keys experimentData.pointData, propertiesAsKey -> experimentData.pointData#propertiesAsKey);
    );
    
  

    experiment.smoothPoints = (precision, trials)->
    (
        plist :=  experiment.points();
        smothPoints :=  {};
        jet := null;
        for point in plist do
        (
                jetOrException := catch jetAt(blackBoxIdeal,point,precision,trials);
                if isDerivedFrom(jetOrException, Jet) then
                (
                    smothPoints = smothPoints | {point};
                );
                
        );
        return smothPoints;
    );



    runExperimentOnce := method();


    experiment.setPointsPerComponent = (numPointsPerComponent)->
    ( 
        pointsPerComponent = numPointsPerComponent;
    );


    experiment.pointsPerComponent = ()->
    ( 
        return pointsPerComponent ;
    );
    

    


    experiment.position =(property)->
    (
        return position( experiment.watchedProperties(), watchedProperty->watchedProperty == property );
    );




    experiment.clear = ()->
    ( 
        pointIterator.reset();
        --  later  call experimentDataClear() instead.
        experimentData.trials = 0;
        experimentData.pointData = new MutableHashTable;
        experimentData.countData = new Counts;
    );
    
    experiment.rankJacobianAtKey = ()->
    (
        return rankJacobianAtKey;
    );

    experiment.usedRankJacobianAt = ()->
    (
        return rankJacobianAtKey;
    );

    -- question: why the hell we pass the number of wanted points per component and do not acces the 
    -- experiment => to see the dependency
    --
    runExperimentOnce(ExperimentData, Matrix,ZZ ) := Tally => (experimentData, point, wantedPointsPerComponent) -> 
    (  
        K := experimentData.coefficientRing;
        --prime = char K
        numVariables := blackBoxIdeal.numVariables;
        wantedPoints := wantedPointsPerComponent;

        -- if ideal vanishes on the random point do something
        if experiment.isInteresting(point) then   
        (
            valuesTuple := propertiesAt(point); -- todo: valuesTuple: better naming?
 
            -- countData number of found points for each rank and property
            experimentData.countData = experimentData.countData + new Counts from tally {valuesTuple};
          
            if  rankJacobianAt =!= null then 
            (
                FFELogger.debug( "update wanted points" );
                rankJacobian := rankJacobianAt(point);  
                upperEstimate := (estimateNumberOfComponents(experiment,valuesTuple)).max;
                ffelog.debug ("upper estimate number of components:  " | toString upperEstimate );
                --upperEstimate := 1; -- test

                wantedPoints = max(1,upperEstimate)*wantedPointsPerComponent;
            );            

            -- remember some points
            if experimentData.pointData#?(valuesTuple) then 
            (
                -- collect a fixed number of points per estimated component
                -- use upper limit of estimation for this             
                if #(experimentData.pointData#valuesTuple) < wantedPoints then 
                (
                    FFELogger.debug( "attaching point" );
                    experimentData.pointData#valuesTuple = experimentData.pointData#valuesTuple | {point};
                );
            )
            else (
                FFELogger.debug( "attaching first point for some valuesTuple key");
                experimentData.pointData#valuesTuple = {point};
            );
        );
        -- this trial is counted in runExperiments and not here to allow update() without changing trial num.
    );

   
    experiment.setPointIterator  = (pRpi)->
    (
        if experiment.trials()=!=0 then 
            error ("cannot change point iterator - experiment was already run! You could call clear() and retry.");
        pointIterator = pRpi;
    );

    experiment.setPointGenerator  = (pGen) ->
    (
        wrip := createRandomPointIterator(pGen);
        experiment.setPointIterator(wrip);
    );

    experiment.trials = ()-> pointIterator.position();

    runExperiment := method();
    runExperiment(ExperimentData, Thing, ZZ) := Tally => (experimentData, pPointIterator, newTrials) -> 
    ( 
        for i in 1..newTrials do
        (
            assert( pPointIterator.next() );
            runExperimentOnce( experimentData, pPointIterator.getPoint(), pointsPerComponent );
            experimentData.trials =  experiment.trials();
        );
    );


    positionsOfProperties := (newWatchedList)->
    ( 
        return  apply( newWatchedList, 
                        targetProperty -> position( experiment.watchedProperties() , (prop)->prop==targetProperty)
            );
    );

    updateWatchedListIsProjection := (newWatchedList)->
    (
        propertyPositions :=  positionsOfProperties(newWatchedList);

        return( 0 == #(select(propertyPositions, (pos)->pos===null)) ); 
    );

    projectionUpdate :=  ( newWatchedList) -> 
    (     
        -- assume: new watched property list is projection of old one.

        -- 1. get the tuple of positions of new watchedProperties in propertyList.
        propertyPositions :=  positionsOfProperties(newWatchedList);


        assert( 0 == #(select(propertyPositions, (pos)->pos===null)) );        

        --    experimentData.propertyList 

        newCountData := new MutableHashTable;

        newKey := null;

        -- sum up counts
        for key in keys experimentData.countData do
        (
            newKey = apply( propertyPositions, pos-> key#pos);
            if (not newCountData#?newKey) then  (    newCountData#newKey =   experimentData.countData#key     )
                                            else (    newCountData#newKey =   newCountData#newKey + experimentData.countData#key     );

        );
        -- reclassify collected points

        newPoints := new MutableHashTable;

        for key in keys experimentData.pointData do
        (
            newKey = apply( propertyPositions, pos-> key#pos);
            if (not newPoints#?newKey)    then  (    newPoints#newKey =   experimentData.pointData#key     )
                                            else (    newPoints#newKey =   newCountData#newKey |  experimentData.pointData#key     );

        );

        experimentData.countData = new Counts from new Tally from newCountData;
        experimentData.pointData    = newPoints;
        
        --experimentData.propertyList = newWatchedList;
    );

    
    catchedPointProperty := (propertyName, point)->
    (
        try
        (
            resultOrException := catch (blackBoxIdeal.pointProperty(propertyName))(point);
            if isDerivedFrom(resultOrException, Exception) then
            (
                return class resultOrException;
            )
            else
            (
                return resultOrException;
            );
        )
        then
        (
        )
        else
        (
            return Error;
        );        
    );

    setWatchedPropertiesInternal := (propListToObserve)->
    ( 
        for propertyName in propListToObserve do
        (
            if not blackBoxIdeal.hasPointProperty(propertyName) then 
                error ("blackBoxIdeal seems not to have property" | propertyName );
        );
        experimentData.propertyList=propListToObserve;
        propertiesAt = (point)->
        ( 
            --apply( experimentData.propertyList, propertyName->( (blackBoxIdeal.pointProperty(propertyName))(point) ) )
            apply( experimentData.propertyList, propertyName->( catchedPointProperty(propertyName,point) ) )  
        );   
    );

    experiment.useRankJacobianAt = (rankJacobianAtName)->
    ( 
        if experiment.trials()=!=0 then 
            error ("cannot change rankJacobianAt  - experiment was already run! You could clear() the statistics and retry. ");

        if rankJacobianAtName===null then
        (
            rankJacobianAtKey = null;
            rankJacobianAt = null ;
            return;
        );

        if (not blackBoxIdeal.hasPointProperty(rankJacobianAtName)) then 
            error ("blackBoxIdeal seems not to have property " | rankJacobianAtName );
    
        if (rankJacobianAtKey=!=null) then
        (
            if (experiment.propertyIsWatched(rankJacobianAtKey)) then
            (
                experimentData.propertyList = delete(rankJacobianAtKey, experimentData.propertyList ) ;
                setWatchedPropertiesInternal( experimentData.propertyList );   
            );
        );

        rankJacobianAtKey = rankJacobianAtName;
        rankJacobianAt = blackBoxIdeal.pointProperty(rankJacobianAtName) ;
        experiment.watchProperty(rankJacobianAtKey);
    
    );



    experiment.clearWatchedProperties = (   )->
    (
        experiment.clear(); 
        setWatchedPropertiesInternal({});
    );

    experiment.reset = (   )->
    (
        --redundant: experiment.clear(); 
        experiment.clear(); 
        clearWatchedProperties();
    );

    --newPropertyListIsProjection = (newPL)->
    --(
    --     
    --);

    experiment.setWatchedProperties = ( propertyStringList )->
    (  

        if ( updateWatchedListIsProjection(propertyStringList) ) then 
        (
            projectionUpdate(propertyStringList);
            setWatchedPropertiesInternal(propertyStringList);
        )
        else
        (

            if experiment.trials()=!=0 then error ("cannot change watched properties - experiment was already run! Clear statistics and retry.");

            -- improvement: allow in case the new properties is a projection
            setWatchedPropertiesInternal(propertyStringList);
        );
    );

    UpdateWatchedPropertiesError := "cannot change watched properties - experiment was already run! You could clear() the statistics and retry.";
  

    experiment.propertyIsWatched = method();
    experiment.propertyIsWatched (String) := Boolean => (propertyName)->
    (
        if ( #(select( experiment.watchedProperties(), (prop)->propertyName==prop)) >0 ) then
        ( 
            return true;
        );    
        return false;
    );

    watchProperty := method();
    watchProperty ( String ) := List => (propertyName)->
    (
        if not blackBoxIdeal.hasPointProperty(propertyName) then 
            error ("blackBoxIdeal seems not to have property" | propertyName );

        if (not experiment.propertyIsWatched(propertyName)) then
        (
            if (experiment.trials()=!=0 ) then error (UpdateWatchedPropertiesError);
        );

        experimentData.propertyList = unique (experimentData.propertyList | { propertyName }) ;
        setWatchedPropertiesInternal( experimentData.propertyList );   
      
    );
    
    watchProperty ( Function ) := List => (propertyMethod)->
    (
         watchProperty(toString propertyMethod);
    );
    
    experiment.watchProperty = method();
    experiment.watchProperty ( String ) := List => (propertyName)->
    (
        watchProperty(propertyName);
        return experiment.watchedProperties();
    );
    
    experiment.watchProperty ( Function ) := List => (propertyFunction)->
    (
        return experiment.watchProperty(toString propertyFunction);
    );

    experiment.watchProperties = (propertyNameList)->
    (
        for propertyName in propertyNameList do
        (
            watchProperty(propertyName);
        );
        return experiment.watchedProperties();
    );

    

    assertPropertyIsWatched := (propertyName) ->
    (
        if (not experiment.propertyIsWatched(propertyName)) then 
            error ("given property '" |propertyName| "' is not watched !");
    );

    assertIgnorePropertyAllowed := (propertyName) ->
    (
        if (propertyName == "rankJacobianAt") then
        (
            if (class blackBoxIdeal===BlackBoxIdeal) then
            (
                error (" removing 'rankJacobianAt' from watched properties for a " | toString class blackBoxIdeal | " not allowed !")
            );
        );
    );


    experiment.ignoreProperty = (propertyName)->
    (
        
        assertPropertyIsWatched(propertyName);
        assertIgnorePropertyAllowed(propertyName);

    
        newPropertyList := experimentData.propertyList ;
    
        newPropertyList = delete(propertyName, newPropertyList ) ;
    
        projectionUpdate( newPropertyList );

        setWatchedPropertiesInternal( newPropertyList );   
    );


    experiment.ignoreProperties = (ignorePropertyStringList)->
    (
        for propertyName in ignorePropertyStringList do
        (
            assertPropertyIsWatched(propertyName);
            assertIgnorePropertyAllowed(propertyName);
        );

        newPropertyList := experimentData.propertyList ;
        apply( ignorePropertyStringList, propToIgnore-> ( newPropertyList= delete(propToIgnore, newPropertyList ); ));
        projectionUpdate( newPropertyList );
        setWatchedPropertiesInternal(newPropertyList);    
    );

 
    experiment.interpolateComponents = (maxDeg, onComponentPrecision)->
    (
            blackBoxIdeal.interpolateComponents( experiment.smoothPoints(10,10), maxDeg, onComponentPrecision);
    );


    experiment.watchedProperties =  ()->
    (
        return experimentData.propertyList;
    );

    
    experiment.setIsInteresting = (pIsInteresting)->
    (  
       if experiment.trials()=!=0 then 
            error ("cannot change isInteresting - experiment was already run! You could call clear() and retry.");
        if ( pIsInteresting=!=isInteresting ) then
        (
            isInteresting = pIsInteresting;
        );
    );

    ---- syntax for the moment too hard (method without parameters)
    --experiment.runExperimentOnce = method(Options => (options runExperimentOnce));
    -- experiment.runExperimentOnce() := Thing => opts->()->
    --(
    --   return runExperimentOnce(experimentData);
    --);  
  

    experiment.run = method();
    experiment.run(ZZ) := Thing => (newTrials)->
    (
        runExperiment( experimentData, pointIterator, newTrials );
        return sort experiment.counts();
    );
    
    --
    -- returns a hashtable with 
    -- 
    experiment.pointLists = ()->
    (
        return new HashTable from experimentData.pointData;
    );

    experiment.pointsByKey = (key)->
    (
        return  experiment.pointsByPropertyValues(key);
    );
    
    experiment.pointsByPropertyValues = (key)->
    (
        if not (experimentData.pointData)#?key then 
            error "invalid key";
        return  (experimentData.pointData)#key;
    );
    
      -- returns a HashTable with watched BlackBoxIdeal properties as keys and corresponding occured count as values.
    -- naming is unfortunate
    experiment.counts = ()->
    (
        return new Counts from experimentData.countData;
    );    
    
    -- count the number of collected points!
    experiment.collectedCount = ()->
    (
        return new Counts from apply( experiment.pointKeys(), key->( key=> #(experimentData.pointData)#key ) );
    );
    
    -- or observedPointPropertySetValues, observedPropertyTupleValues
    experiment.observedPropertyTuples = ()->
    (
        return keys experimentData.pointData;
    );


    -- deprecated
    experiment.pointKeys = ()->
    (
        print("--warning: .pointKeys() is deprecated. use .realizedProperties()");
        return experiment.observedPropertyTuples();
    );
    
    -- (JK) we do not need this one.
    experiment.observedPropertyTuple = method();
    experiment.observedPropertyTuple (ZZ) := Thing =>(index)->
    (
        propertyvalues := experiment.observedPropertyTuples();
        if ( (index<0) or (index>= #propertyvalues) ) then error "invalid point position ";
        return propertyvalues#index;
    );
    
    -- (JK) we do not need this one.
    experiment.pointKey = method();
    experiment.pointKey(ZZ) := Thing => (index)->
    (
       print("--warning: .pointKey() is deprecated. use .realizedProperty()");
       return experiment.observedPropertyTuple(index); 
    );
    
    

   --init part:

        -- todo: test if changing blackBoxIdeal.rankJacobianAt is transparent
        -- (means after blackBoxIdeal.updatePointProperty("rankJacobianAt") the new one is called)
 
        experiment.watchProperties( {} );

        if  blackBoxIdeal.hasPointProperty("isZeroAt") then
        (
            --print("isInteresting"| toString  (blackBoxIdeal.isZeroAt ));
            isInteresting = blackBoxIdeal.pointProperty("isZeroAt");

            --     experiment.watchProperties( {"isZeroAt"} );
        );

        if blackBoxIdeal.hasPointProperty("jacobianAt") then 
        (
            assert ( blackBoxIdeal.hasPointProperty("rankJacobianAt") );
        );

        if blackBoxIdeal.hasPointProperty("rankJacobianAt") then 
        (
            assert ( blackBoxIdeal.hasPointProperty("rankJacobianAt") );
            rankJacobianAtKey = "rankJacobianAt";
            rankJacobianAt =  blackBoxIdeal.pointProperty("rankJacobianAt");


            experiment.watchProperties( {"rankJacobianAt"} );

        );



    --end init:

    experiment.tryProperty = (tProp) -> 
    (
        ffelog.info("-- ( " | toString experiment.watchedProperties() |" | " | tProp | " ) => count " );
        pointListsCopy := experiment.pointLists();
        tally flatten apply(keys pointListsCopy, --keys are the value tuples of the watched properties,
                       valueTuple->apply(pointListsCopy#valueTuple, -- for a value tuple set we have a list of stored points.
                                     point->( valueTuple, (blackBoxIdeal.pointProperty(tProp))(point))
                                 ) 
                 )
    );
   
    experiment.saveData  = (filename) ->
    (
        f := openOut(filename);        
        --maybe not that smart...
        externalString := toExternalString experiment.experimentData(); 
        --also not that smart...
        f << "savedExperimentData412398472398473923847 = " << externalString;
        f << flush;
        f << close;
    );

    experiment.loadData = (filename) ->
    (
        load filename;
        experimentData = savedExperimentData412398472398473923847;
        apply( keys experimentData.pointData, key-> 
                                        ( (experimentData.pointData)#key = apply( (experimentData.pointData)#key , point->
                                                                                sub(point, experimentData.coefficientRing )
                                                                            )
                                        )
                );
    );
    
 
 

 
    ffelog.debug ("type of internal experiment variable is : " | toString class experiment );
    return experiment; 
    -- return new HashTable from experiment; 
);


