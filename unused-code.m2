-- Note: this code appeared after the "end--" in the main package file, and we are
-- not quite sure what it includes, so we keep it here for now (Jan 2021).

-- Todo: introduce force mode option for registerPointProperty? (overwriting registered property?)
-- todo: introduce for all known properties a precomposition with checkInputPoint

-- Estring = new Type of String ; the goal was to distinguies string keys 
-- from symbol keys without changing string visuaulization (net())
-- but that turned out to be impossible(or too hard), because e.g. the operator '#' cannot be overloaded.


--net (String) := Net =>(str)->(   strl := "\""| str| "\"" ;   return stack separate strl; );


--(JK) why did I need comparison between a string and a symbol ? (jk) - if I recall correctly, it was related for intervals...

String ? Symbol := (str,symb)->
(
    1 ? 2
);

Symbol ? String := (str,symb)->
(
    2 ? 1
);
-- Q: how to get the parameter list of a function or method?
--
-- answer:  not possible yet, but it is possible to get the number of parameters 
-- using 'disassemble' for functions (where the number of variables is fixed and not variable)
-- and for methods it forks as follows: consider a method foo with several functions installed.
-- then   apply(methods foo, m-> (m, disassemble lookup m) )
   
-- just after exporting:
-- why is/was setting 'jacobianAt' necessary??? (jk)

-- jacobianAt := global jacobianAt;
-- jacobianAt = global jacobianAt;
--
--

 

-- need test for randomIterator (for fixed error : to less trials  )
--@TO2{jetAt,"jet"}@ computations at a point independently of the ideal representation. \break \break 


--        (clearCoeffDenominators, Ideal )    


-- dient dazu, an einen Punkt die BlackBox mit anzuheften.
Point = new Type of HashTable;

pointObject = method();
pointObject (Thing, Matrix) := Point =>(parent, point)->
(
    resultPoint := new HashTable from {("parent", parent),
                                       ("point", point)};
    resultPoint 
)


-- parent is needed for coercion

Point := new Type of HashTable;


pointObject (Thing, Matrix) := (thing, point)->
(
    if not (thing.hasElement(point)) then 
        error(" point is not on parent ");
    pointObject = new HashTable from {("parent",thing),
                                      ("point",point)
                                     }
    return pointObject;
);


BlackBoxPoint := new Type of Point
{
    parent is of type BlackBoxParameterSpace or derived
    point is a Matrix?
}

blackBoxPointObject (BlackBoxParameterSpace, Matrix) := (blackBox, point)->
(
    if not (blackBox.isZeroAt(point)) then error(" point is not on BlackBox ");
    pointObject = new HashTable from {("parent",blackBox),
                                      ("point",point)
                                     }
    return pointObject;
);

-- single trial returns 

(jet, bestJet, failedJetLength)





JetInfoAt 
(
    bestJet
    worstJet
    Tally failedJetLength=> count
    targetLength
)

-- or even better, JetInfoAt contains pairs length -> list of jets with that length.

-- 
 
-- and now we can pass the black box and the point ! (we have free another two parameters)

-- we could do precision Jet and then check  first "jetLength", then 'precision ring jet'.


InterpolatedComponent: parent is JetSet

Jet: parent is Point ? (or black box and point)


maximalConditions = method();
maximalConditions( JetSet ) := ZZ => (jetSet)->
(
    return 1+sum apply(jetSet#"jets", jet->length jet);
)




-- jetAtOrNull ()
--
-- computes a jet with given jetLength once using jetAtWithInfo()
-- returns the computed jet if succeeded, otherwise returns null.
-- 

jetAtOrNull = method();
jetAtOrNull( BlackBoxParameterSpace, Matrix, ZZ) := Jet => ( blackBox,  point, jetLength )  ->
(
    
    jetResult  := jetAtWithInfo ( blackBox,  point, jetLength);      
    
    return jetResult#"jet";
);


--
-- Continues a given jet up to a requested jetLength (if possible) using continueJetWithInfo()
-- returns the computed jet if succeeded, otherwise null.
--
--
continueJetOrNull = method();
continueJetOrNull( Jet, ZZ) := Jet => (  jet, jetLength )  ->
(
    
    jetResult  := continueJetWithInfo (  jet, jetLength);      
    
    return jetResult#"jet";
);
