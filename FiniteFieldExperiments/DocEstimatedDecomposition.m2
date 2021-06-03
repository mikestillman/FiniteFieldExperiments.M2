-- doc for estimated decompositions

doc ///
   Key
        "estimateDecomposition"
   Headline
        shows how often points with particular properties were found by an experiment
   Usage   
        e.estimateDecomposition()
   Inputs  
        e:Experiment 
            an Experiment
   Description
        Text
           Returns a HashTable with an estimate of the number of components 
           in each codimension for all combinations of properties found. 
 
           This is done by assuming that for each component of codimension 
           c the fraction of points found is the expected 1/p^c and 
           by further assuming that the components are smooth at most 
           of there rational points P. In this case
           c is equal to the rank of the jacobi matrix at this point.
                        
           Lets see how this works in an example.                
           First we create an ideal we want to analyse and put it into a blackbox:
        Example      
           K = ZZ/5;
           R = K[x,y,z];
           I = ideal (x*z,y*z);
           bb = blackBoxIdeal I;
        Text
           \break The ideal describes a line and a plane intersecting at the origin. \break     
           \break Now we create the experiment:
        Example
           e = new Experiment from bb;
           e.run(500) 
           e.estimateDecomposition()
        Text
           We see that up to 
           a margin of error this gives the correct answer of 1 component
           in codimension 1 and 2 each and no component of codimension 0.
           
           (the point where the line and the plane intersect has
           rankJacobiMatrix equal to 0. Fortunately there are not enough
           such points to fool the algorithm.)
           
           estimateDecomposition works only if the black box used in this experiment
           provides a property rankJacobianAt and this property is
           watched by the experiment. As explained above,
           this function uses the rank of 
           the jacobi matrix at each point as an estimate for the codimension
           of the component the point lies on. In effect assuming that
           the point lies in the smooth part of the component.      
///

