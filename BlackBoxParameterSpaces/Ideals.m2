-- disable BlackBoxIdeal construction using new without parameters ( bb = new BlackBoxIdeal ):
new BlackBoxIdeal   := (E) -> 
(
    error "creating empty blackbox not possible. You have at least to provide the number of variables and their ring";
);

-- disable BlackBoxIdeal construction using new for an arbitrary parameter. ( bb=new BlackBoxIdeal from ... ):
new BlackBoxIdeal from Thing := ( E, thing) -> 
(
    error "creating blackbox from  type " | toString E | " not implemented ";
);
 
 
blackBoxIdeal = method();

 

-- todo: how to check, if 'ring equationsIdeal' is not a quotient ring?

--
-- this function is final, that means nobody should use this method for creating a derived object
--
new BlackBoxIdeal from Ideal := (E, equationsIdeal)->
(
     blackBox :=  blackBoxParameterSpaceInternal( BlackBoxIdeal, ring equationsIdeal );
    
    
    -- maybe blackBox.addProperty( ideal, equationsIdeal)
    blackBox#"ideal" = equationsIdeal;     
    blackBox.ideal = equationsIdeal;      
    
   
    
    
    -- registering valuesAt generates 'isZeroAt' and 'jacobianAt', too !  
    blackBox.registerPointProperty("valuesAt",( bb, point)->  
        (
            result :=  gens sub( equationsIdeal , point);
            return  result;
        )
    );


    -- maybe blackBox.addProperty( jacobian, jacobian gens  equationsIdeal )
    blackBox.jacobian = jacobian gens  equationsIdeal;


    -- we have to call updatePointProperty for "jacobianAt", because "jacobianAt" is present. 

    blackBox.updatePointProperty( "jacobianAt",
        ( bb, point )->
            (  
                -- attention, duplicate code!!!
                if (blackBox.withChecks) then
                (
                    if (not ( blackBox.valuesAt( point )==0))  then  
                    (
                        --error("point does not belong to the ideal ! ");
                        throw new PointNotOnBlackBox from {"errorMessage" => " Point " |toString point|" not on Black box"};
                    );
                );
                -- attention, duplicate code!!!
                jacobianM2MatrixAt := sub( blackBox.jacobian , point);
                return jacobianM2MatrixAt;
            )
    );   
    
    return blackBox; 
)

--
-- this function is final, that means nobody should use this method for creating a derived object
--
blackBoxIdeal (Ideal) := BlackBoxIdeal =>(equationsIdeal)->
(
   return new BlackBoxIdeal from equationsIdeal;
)

blackBoxIdeal (Ideal, Boolean) := BlackBoxIdeal =>(equationsIdeal, withChecks)->
(
    bb := new BlackBoxIdeal from equationsIdeal;
    bb.disableChecks();
    return bb;
)


--
--  creates a BlackBoxIdeal from a given evaluation method ('valuesAt') which takes a point (a row matrix)
--
--  parameters: numVariables in the parameter space,
--              coefficientRing , the 
--              valuesAt: a method for evaluating the object at one parameter point.
--
--
-- this function is final, that means nobody should use this method for creating a derived object
--

blackBoxIdealFromEvaluation = method();


blackBoxIdealFromEvaluation( Ring, Function ) := HashTable => ( pRing, pValuesAt ) ->
(

   blackBox := blackBoxParameterSpaceInternal(BlackBoxIdeal, pRing );
   blackBox.registerPointProperty ("valuesAt", (bb,point)->pValuesAt(point) ); --sets isZeroAt, jacobianAt, rankJacobianAt and numGenerators

   check := ()->
   (
        numVariables :=  blackBox.numVariables;
        
        point := matrix { apply(numVariables, i-> 0_(blackBox.coefficientRing) ) };
        blackBox.valuesAt( point );
        blackBox.isZeroAt( point );
   );

   check(); 
   return blackBox;
)

ideal (BlackBoxIdeal) := Ideal =>(bbI)->
(
    if (bbI#?"ideal") then
        return bbI#"ideal";
    if (bbI#?"ring") then
    (
        -- optional todo : we could cache the ideal here, once it is computed,
        -- but then it gets complicated to assure consistency
        return  ideal bbI.valuesAt(gens ideal gens bbI#"ring");        
    );
    error ("internally no ring is stored");    
);

