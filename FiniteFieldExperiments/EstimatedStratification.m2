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
