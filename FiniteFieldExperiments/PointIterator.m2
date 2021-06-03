 
PointIterator = new Type of HashTable;
new PointIterator from Thing := (E, thing) -> 
(
    error("not implemented");
);

RandomPointIterator = new Type of PointIterator;
new RandomPointIterator from Thing := (E, thing) -> 
(
    error("not implemented");
);

createRandomPointIterator = method();

createRandomPointIterator (Function) := RandomPointIterator => ( weakRandomPointGenerator )->
(
    -- todo improvement: use own seek and remember initial seek to get reproducible    
    rpi := new MutableHashTable;
    randomPoint := null;

    currTrial := 0;

    rpi.next=()->
    (
        while(true) do 
        (
                randomPoint = weakRandomPointGenerator();
                currTrial = currTrial + 1;
                if randomPoint =!= null then break;
        );
        
        return true;
    );
    rpi.position = ()->
    (
        return currTrial;
    );
        
    rpi.setPosition =(trials)->
    (
        currTrial= trials;
    );
    
    -- usually only for loading from file.    
    rpi.setPoint = (point)->
    (
        randomPoint = point;
    );
    
    rpi.reset = () ->
    (
            randomPoint = null;
            currTrial = 0;
    );

    rpi.begin = ()->
    (
        -- jk why do we this???
        ri := createRandomPointIterator(weakRandomPointGenerator);
        ri.next();
        return ri;
    );

    rpi.getPoint = ()-> randomPoint;
    rpi = new HashTable from rpi;
    rpi = newClass(RandomPointIterator,rpi);
    return rpi;
)

createRandomPointIterator (Ring,ZZ) := RandomPointIterator => ( coeffRing,numVariables )->
(
    rpi := new MutableHashTable;
    randomPoint := null;

    currTrial := 0;

    rpi.next=()->
    (
        randomPoint = random( coeffRing^1, coeffRing^numVariables );         
        currTrial = currTrial + 1;

        return true;
    );

    rpi.reset = () ->
    (
        randomPoint = null;
        currTrial = 0;
    );

    rpi.position = ()->
    (
        return currTrial;
    );

    rpi.begin=()->
    (
        ri := createRandomPointIterator(coeffRing, numVariables );
        ri.next();
        return ri;
    );

    rpi.getPoint=()-> randomPoint;
    
    rpi = new HashTable from rpi;
    rpi = newClass(RandomPointIterator,rpi);
    return rpi;
);

createIterator = method();
createIterator (List) := PointIterator =>( pPoints )->
(
    pIterator := new MutableHashTable; 

    points := pPoints  ;

    point := null;

    pointCount := #points;

    currPosition := 0;

    pIterator.position = ()->
    (
        return currPosition;
    );

    pIterator.reset = () ->
    (
            point = null;
            currPosition = 0;
    );

    pIterator.next = ()->
    (
        if (currPosition+1 >= pointCount) then return false;
        point = points#currPosition;
        currPosition = currPosition + 1;
        return (true) ;
    );

    pIterator.begin = ()->
    (
        localPointIterator := createIterator( pPoints);
        localPointIterator.next();
        return localPointIterator;
    );

    pIterator.getPoint = ()-> point;
    pIterator = new HashTable from pIterator;
    pIterator = newClass(PointIterator,pIterator);
    return pIterator;
);



