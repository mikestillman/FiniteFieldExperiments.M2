--purpose of new Type Counts: to install sort as a method for Counts, but not for all Tally types.
--
Counts = new Type of Tally; 


--net (Counts) := Net =>(cs)->
--(
--    strcountinfo := "-- count() structure: \n-- values of watched properties" => count ";   
--    sss = stac (strcountinfo, net cs);
--    return net sss;
--)


SortableCounts = new Type of List;

new SortableCounts from Counts := (E, thing) -> 
(
    sortableCounts:= apply (keys thing, key->(key,thing#key));
    return sortableCounts;
)


net (SortableCounts) := Net =>(cs)->
(
    strcountinfo := "-- count structure: (values of watched properties) =>  count ";
    L := apply(cs, entry->  stack ( horizontalJoin(net entry#0 ," => " , net entry#1), " "));    
    L2 := stack L;
    L3 := horizontalJoin(toString class cs,"{",L2, "}");
    L4 := stack (strcountinfo, L3);
    return net L4;
)

-- sortfkt is a either sort or rsort.
-- internal method
--
sortCounts := method();
sortCounts (SortableCounts, MethodFunctionWithOptions) := SortableCounts => (countDataList, sortfkt)->
(    
    counts := new Counts from countDataList;
    
    --print (toString counts);
    prerearrangedData := new MutableHashTable;
    
    -- 1. use the count number as key and the values of corresponding watched properties as values.
    for key in keys counts do
    (
        if not  prerearrangedData#?(counts#key) then 
            prerearrangedData#(counts#key)= { key }
        else
        (
            prerearrangedData#(counts#key)=  prerearrangedData#(counts#key) | { key };
        );
    );
    -- 2. now create a list of the point count and sort it
    ---- bug fixed (unique missing) todo: test for this bug!
    toSort := unique apply( keys counts, key -> (counts#key));
    sorted := sortfkt (toSort); 
    rearrangedData := {};

    -- 3.  asseble the result
    for count in sorted do
    (
        for entry in prerearrangedData#count do
        --rearrangedData = rearrangedData | {(count,entry)};
        rearrangedData = rearrangedData | {(entry,count)};
    );
    --print (toString rearrangedData);
    --return new List from rearrangedData;
    return new SortableCounts from rearrangedData;
);

sort (SortableCounts) := SortableCounts =>opts-> (countData)->
(
    return sortCounts(countData, sort);
);

rsort (SortableCounts) := SortableCounts =>opts-> (countData)->
(
    return sortCounts(countData, rsort);
);

-- sorts experiment statistics by count ascending
sort (Counts) := Counts =>opts-> (countData)->
(
    --  a HashTable is not sortable; => converting counts to a list of pairs (realizedpropertyValueTuple, count) 
    countDataList := new SortableCounts from countData;
    return sortCounts(countDataList, sort);
);

-- sorts experiment statistics by count descending

rsort (Counts) := Counts =>opts-> (countData)->
( 
    --  a HashTable is not sortable; => converting counts to a list of pairs (realizedpropertyValueTuple, count) 
    countDataList := new SortableCounts from countData;
    return sortCounts(countDataList, rsort);
);
