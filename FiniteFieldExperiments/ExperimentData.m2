 
new ExperimentData from Ring := (E, coeffRing) -> 
(
     ffelog.debug (toString E);
     e := new MutableHashTable;
     e.coefficientRing = coeffRing;
     e.pointData = new MutableHashTable; -- (JK) unfortunate naming
    -- format: key=>{ideal, maxDegree, name} --later: data type for interpolated ideal (RichInterpolatedIdeal?)
     e.countData = new Counts;
     e.trials = 0;
     e.propertyList = {};
     e.isRandom = null;
     return e;
);
 

createExperimentData = (coeffRing,pointData,countData, trials, propertyList,isRandom) -> (
    e := new ExperimentData;
    e.coefficientRing = coeffRing;
    e.pointData = new MutableHashTable from pointData;
    e.countData = countData;
    e.trials = trials;
    e.propertyList = propertyList;
    e.isRandom = isRandom;
    return e;
)

new ExperimentData from ExperimentData := (E,ed) -> 
(
    -- print (toString E);
    e := new MutableHashTable;
    e.coefficientRing = ed.coefficientRing;
    e.pointData = copy ed.pointData;
    e.countData = copy ed.countData;
    e.trials = ed.trials;
    e.propertyList = copy ed.propertyList;
    e.isRandom = ed.isRandom;
    return e;
);

ExperimentData == ExperimentData := Boolean=>(ed1,ed2)->
(

    if ( ed1.coefficientRing === ed2.coefficientRing and
            ed1.propertyList    == ed2.propertyList and
            keys ed1.pointData      == keys ed2.pointData and    
            keys ed1.countData      == keys ed2.countData and  
            ed1.isRandom        == ed2.isRandom  and
            ed1.trials        == ed2.trials  ) then 
    (
        for key in keys ed2.pointData do
        ( 
            if not ed1.pointData#key == ed2.pointData#key then 
            return false;
        );

        -- TODO HC+M: should the following be countData instead?
        for key in keys ed2.pointData do
        ( 
            if not ed1.countData#key == ed2.countData#key then 
            return false;
        );
        return true;
    )
    else return false; 
);

--coeffRing,pointData,countData,trials,propertyList,isRandom

toExternalString(ExperimentData) := String=> (ed)->
( 
   return "createExperimentData " | "(" 
   | toExternalString ed.coefficientRing | ", \n "
   | toExternalString (new HashTable from ed.pointData) | ", \n" -- HC:introduced External
   | toExternalString ed.countData | ",\n"
   | toExternalString ed.trials | ",\n"
   | toExternalString ed.propertyList | ",\n"
   | toExternalString ed.isRandom 
   | ")" ;
)

ExperimentData + ExperimentData := ExperimentData=>(ed1,ed2)->
(

    if ( ed1.coefficientRing === ed2.coefficientRing and
         ed1.propertyList    == ed2.propertyList and
         ed1.isRandom        == ed2.isRandom  ) then 
    (
        edNew := new ExperimentData from ed1;
        
        for key in keys ed2.pointData do
        ( 
        if not edNew.pointData#?key then 
            edNew.pointData#key = copy ed2.pointData#key
        else
            edNew.pointData#key = edNew.pointData#key | copy ed2.pointData#key;
        );
        edNew.countData = ed1.countData + ed2.countData;
        edNew.trials = ed1.trials + ed2.trials;
        return edNew;
    )
    else error ("+: experiment data not compatible");                   
);

