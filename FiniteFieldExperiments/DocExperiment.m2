doc ///
    Key
        FiniteFieldExperiments
    Headline
          heuristic decomposition of black box ideals
    Description
        Text
            This package allows one to study a parameter space
            together with its universal family, by sampling points
            over small finite fields, and using heuristics derived
            from the Weil conjectures to determine likely strata and
            components of such parameter spaces.
        Text
            This study comes in two flavours.
        Text
            @UL {
            {"In the first situation one is given a unirational parametrisation
            \\[
                \\phi \\colon {\\mathbb A}^n \\to X
            \\]
            of some parameter space.  One is often
            interested in the stratification of $X$ by some property
            of the parametrised objects. Our running example for this
            type of question is the stratification of the space of
            cubic surfaces in $\\PP^3$ by singularity type. Here $X$ is
            represented by a ", TO BlackBoxParameterSpace, "."
            },
            {"In the second situation $X$ is given as a subvariety of some larger unirational moduli space
            \\[
                X \\subset {\\mathbb A}^n.
           \\]
           One is often interested in the components of $X$ and their
           moduli interpretation. Our running example in this case is
           the variety of complexes. Here $X$ is represented by a ", TO BlackBoxIdeal, "."}
           }@
       Text
           In both cases the study of $X$ 
           is done over a finite field
           by looking at a large number of random points in
           ${\mathbb A}^n$. Since we are over a finite field, we have some 
           non-zero chance of finding points in interesting strata (in the
           first case) or on interesting components (in the second
           case). Moreover the statistics of such a {\bf finite field experiment}
           contain heuristic information about
           the stratification (in the first case) or the irreducible
           components of $X$ (in the second case).
           This type of experiment is organized by an onject of type 
           @TO Experiment@.
           
           The package {\tt FiniteFieldExperiments} contains the tools
           to run a finite field experiment, collect the results and
           interpret the statistical information gathered.

           See @TO "Experiment example"@ for a tutorial. (Remove this line).
    Caveat
         The package development is at alpha status and the package is not threadsafe. 
         The interpolation is not time-optimized.
         The documentation is not finished.
///

doc ///
   Key
        Experiment
   Headline
        an unified interface to an experiment
   Description
         Text
            With an @TO{Experiment}@ it is possible to check point properties of an @TO BlackBoxParameterSpace@ or @TO BlackBoxIdeal@ 
            at random points and collect user-defined statistics. \break
            If the black box from supports evaluation, then at each point the jacobian can be computed
            and jets at smooth ones. From the collected statistics a heuristic decomposition can be estimated and finally performed using interpolation methods, see @TO "Experiment example"@
            
            The {\tt  Experiment } objects implements the following interface    \break 

            construction :\break
            \,\, \bullet \, @TO "(NewFromMethod, Experiment, BlackBoxParameterSpace)" @ \break
            \break \break

            properties \break
            \,\, \bullet \,{\tt coefficientRing}:  . \break
            \,\, \bullet \,{\tt coefficientRingCardinality}:  . \break
            \,\, \bullet \,{\tt collectedCount}: . \break
            \,\, \bullet \,{\tt count}:  \break
            \,\, \bullet \,{\tt countsByCount}: \break
            \,\, \bullet \,{\tt membershipPrecision}:  \break
            \,\, \bullet \,{\tt pointsPerComponent}: \break
            \,\, \bullet \,{\tt points}:  \break
            \,\, \bullet \,{\tt pointKeys}: \break
            \,\, \bullet \,{\tt pointLists}:  \break
            \,\, \bullet \,{\tt pointsByKey}:  \break
            \,\, \bullet \,{\tt trials}:  \break
            \,\, \bullet \,{\tt watchedProperties}: \break
            \,\, \bullet \,{\tt stratificationIntervalView}:  \break
            \break \break

            methods: \break
            \,\, \bullet \,{\tt setIsInteresting}: set a filter for points to consider. \break
            \,\, \bullet \,{\tt setMembershipPrecision}:  \break
            \,\, \bullet \,{\tt setPointsPerComponent}:  \break
            \,\, \bullet \,{\tt setPointGenerator}: \break
            \,\, \bullet \,{\tt setPointIterator}:  \break
            \,\, \bullet \,{\tt useRankJacobianAt}:  \break
            \,\, \bullet \,{\tt watchProperties, watchProperty }:  \break
            \,\, \bullet \,{\tt ignoreProperties,  ignoreProperty }:  \break
            \,\, \bullet \,{\tt clearWatchedProperties}:  \break
            \,\, \bullet \,{\tt run}: find interesting points \break
            \,\, \bullet \,{\tt tryProperty}:  \break
            \,\, \bullet \,{\tt clear}: \break
           
        Text
            \break  For an example see @TO "Experiment example"@
///

doc ///
   Key
        "new Experiment"
        (NewFromMethod, Experiment, BlackBoxParameterSpace)
        (NewFromMethod, Experiment, Thing)
   Headline
        type to collect data from a finite field experiment
   Usage   
        e = new Experiment from bb
        e.run(trials)
        e.counts()
        e.pointLists()
        e.estimateStratification()
        e.estimateDecomposition()
   Inputs  
        bb:HashTable 
            a blackBoxIdeal
   Description
        Text
            Creates an @TO Experiment@ from a black box, see @TO BlackBoxParameterSpaces@.
                
            First we create an ideal we want to analyse and put it into a blackbox:
        Example      
           K = ZZ/5;
           R = K[x,y,z];
           I = ideal (x*z,y*z);
           bb = blackBoxIdeal I;       
        Text
           \break Now we are able to create the Experiment object
       
        Example
           e = new Experiment from bb;
   Caveat
        does not check if the ideal ring is a quotient ring (not supported)
   SeeAlso
        "Experiment example"
        "run Experiment"
        estimateDecomposition
                 
       
///


---    Outputs
---        :HashTable
---            observed point number for each value tuple of the watched properties

doc ///
   Key
        "run Experiment"
   Headline
        run an experiment
   Usage   
        e.run(trials)
   Inputs  
        e:Experiment 
            a finite field Experiment
        trials:ZZ
            number of evaluations at random points
   Description
        Text
           To run an experiment we first create an BlackBoxIdeal or a BlackBoxParameterSpage  we want to analyse
        Example      
           K = ZZ/5;
           R = K[x,y,z];
           I = ideal (x*z,y*z);
           bb = blackBoxIdeal I;       
        Text
           Now we create the experiment object
        Example
           e = new Experiment from bb;
        Text
           If a black box has a property "rankJacobianAt" it is
           automatically watched:
        Example
           e.watchedProperties() 
        Text
           Now we run the experiment by evaluating at 1250 random points 
        Example
           time e.run(1250)    
        Text
           As return value we get the collected point number grouped by the values of the watched properties.
           Later these statistics are acceccible via count() 
        Example
           sort e.counts()            
        Text
            dfdf
   SeeAlso
        watchProperty
        watchProperties
        watchedProperties


///

doc ///
   Key
        "Experiment example"
   Headline
        typical finite field experiment example
   Usage   
        e = new Experiment from bb
        e.run(trials)
        e.counts()
        e.pointLists()
        e.estimateStratification()
        e.estimateDecomposition()
   Inputs  
        bb:HashTable 
            a blackBoxIdeal
   Description
        Text
            With an {\tt  Experiment } it is possible to check point properties of an @TO BlackBoxParameterSpace@ or @TO BlackBoxIdeal@ 
            at random points and collect user-defined statistics.
            If the black box supports evaluation, then at each point the jacobian can be computed
            and jets at smooth ones. 
            From the collected statistics a heuristic decomposition can be estimated and finally performed using interpolation methods.

            Here is a typical extremely simple minded application:
                
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
        Text
           Our black box constructed several default point properties, including @TO isZeroAt@.
           The user may add new properties, see 
           running the experiment will trigger evaluation the ideal at random points.
        Example
           bb.hasPointProperty("isZeroAt")
           bb.isZeroAt(matrix{{0_K,0,1}})
        Text 
           If the point is in the vanishing set of the ideal (i.e. either on the point
           or on the line), the point will be considered as insteresting, see @TO setIsInteresting@.
           For all interesting points the experiment will compute the @TO watchedProperties@
           and keep that statistics.
        Text
           \break If a black box has a property "rankJacobianAt" it is
           automatically watched.
        Example
           e.watchedProperties() 
        Text 
           The experiment calculate the rank of the jacobi matrix at interesting points.
           (2 on the line, 1 on the plane, 0 in the origin). 
        Example
           bb.isZeroAt(matrix{{0_K,0,1}})
           bb.rankJacobianAt(matrix{{0_K,0,1}})
           bb.rankJacobianAt(matrix{{1_K,2,0}})
           bb.rankJacobianAt(matrix{{0_K,0,0}}) 
        Text
           \break Now we run the experiment by evaluating at 1250 random points:  
        Example
           time e.run(1250)      
        Text
           As return value we get the collected point number grouped by the values of the watched properties.
           Later these statistics are acceccible via count() :
        Example
           sort e.counts()                     
        Text
           There are 125 points in (F_5)^3 of which 25 are on the
           plane, 5 are on the line and 1 (the origin) is on the line and the plane.
           We therefore expect about 10 points with rankJacobian = 0, 240 with rankJacobian = 1
           and 40 with rankJacobian = 2.
           
           Often it is useful to have explicit points on all components of a variety. Therefore
           the experiment has stored some of the points:
        Example
           e.pointLists()
        Text
           Points with a particular set of properties can be selected
           like this:
        Example
           e.pointsByPropertyValues({2})
        Text
           Since one always finds many points found on components of low
           codimension it is not useful to remember all of them. The experiment
           remembers by default only about 10 points per component:
        Example
           e.pointsPerComponent()
           e.collectedCount() 
        Text
           Here we have not collected exactly 10 points per component
           since the experiment uses the upper end of the confidence
           interval for the number of components ( see @TO
           "estimateNumberOfComponents"@) as guide for the number of
           points to keep.  The amount of stored points can be
           adjusted:
        Example
           e.setPointsPerComponent(20)
           -- collect about 20 points per component now:
           time e.run(1250);
           e.collectedCount() 
        Text          
           Lets now estimate the number and codimension of reduced components
           of the vanishing set:
        Example
           e.estimateDecomposition()       
        Text
           Indeed we see that there is probably no component of codimension 0 and about
           1 component of codimension 1 and 2. The count for the component of 
           codimension 2 is slightly lower since we see only 4 of the 5 points
           on the line - one is the origin that has a tangent space smaller 
           codimension. So we expect the heuristic count to be 4/5 = 0.8. The
           same problem occurs for the plane but there it is not so relevant. We
           expect 24/25 = 0.96 in this case. For higher characteristics this
           systematic error gets smaller.
           
           If one finds the confidence intervals to large one can continue
           to run the experiment:
        Example
           time e.run(1250)
           e.estimateDecomposition()     
        Text
           A doubling of the number of experiments is expected to divide the
           width of a confidence interval by the square root of 2.         
   Caveat
        does not check if the ideal ring is a quotient ring (not supported)
       
///

doc ///
   Key
        "clear"
   Headline
        deletes data of an experiment to start from a clean slate
   Usage   
        e.clear()
   Inputs  
        e:Experiment 
            an Experiment
   Description
        Text
            Sometimes one wants to restart an experiment from a clean slate,
            for example if one has changed some properties or has implemented
            new properties one wants to watch. 
            
            This can be done with clear. 
            
            More precisely clear erases the collected points, the statistics.
            The number of trials is set to zero. If a iterator is used to 
            generate the points, this is also reset to the starting point.
            This is useful if the iterator is 
            used to enumerate a given set of non random points (e.g all points
            in IP^n). 
            
            The black box itself and the list of watched properties
            are not changed.
            
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
           e.run(100)          
           e.trials()
           e.counts()
           e.pointLists()
           e.watchedProperties()
        Text
           \break Now we clear the statistics and point lists:
        Example
           e.clear()
           e.trials()
           e.counts()
           e.pointLists()
           e.watchedProperties()
///


doc ///
   Key
        "clearWatchedProperties"
   Headline
        deletes the list of watched properties
   Usage   
        e.clearWatchedProperties()
   Inputs  
        e:Experiment 
            an Experiment
   Description
        Text
            This is the same as  @TO2{clear,"e.clear()"}@ with the additional effect that
            the watched property list is erased.
          
            The black box itself is not changed.
            
            It is not usesful to erase only the watched property list
            without setting the statistics to zero, since the statistics
            count the number of times a particular property has occured.
            
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
           e.run(100)          
           e.trials()
           e.counts()
           e.pointLists()
           e.watchedProperties()
        Text
           \break Now we clear the statistics, the point lists and the watched properties.
        Example
           e.clearWatchedProperties()
           e.trials()
           e.counts()
           e.pointLists()
           e.watchedProperties()
///

doc ///
   Key
        "collectedCount"
   Headline
        counts the number of collected points
   Usage   
        e.collectedCount()
   Inputs  
        e:Experiment 
            an Experiment
   Description
        Text
            An experiment collects a limited number of points
            for each combination of properies it encounters. 
            
            This is useful if one wants to inspect
            points with special properties in more detail. If the 
            points are on a moduli space one can create the
            corresponding object and study it in detail.
            
            On strata of low codimension many points are found. To
            avoid memory problems only a small number of points
            are collected (see setPointsPerComponent). Therefore
            the number of collected points is usually smaller than
            the number of found points. 
            
            collectedCount() returns an HashTable containing
            the number of collected points
            for each combination of properties.

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
        Text
           \break We set the initial random seed to produce deterministic results
           for the documentation:
        Example
           setRandomSeed(42); 
           e.run(100)          
           e.collectedCount()
           e.pointLists()
           e.pointsByPropertyValues({2})
           e.pointsPerComponent()
        Text
           \break Notice that the number of collected points can be larger than
           the number pointsPerComponent() since the experiment
           tries to estimate the number of components for each combination
           of properties. In the beginning where only a few points have
           been found the statistics might be so errorprone that some extra
           points are collected. 
   SeeAlso
          pointLists
          pointsByPropertyValues
          pointsPerComponent
          setPointsPerComponent             
///

doc ///
   Key
        "counts"
   Headline
        shows how often points with particular properties were found by an experiment
   Usage   
        e.counts()
   Inputs  
        e:Experiment 
            an Experiment
   Description
        Text
            Returns a Tally with the number of points that
            an experiment has found so far for each combination
            of wached properties.

            This gives a rough overview over which properties 
            occur in large/small strata. For an estimation of the
            codimension and number of components for each such stratum
            (see estimateDecomposition).
            
            e.counts() is called automatically when e.run is finished.
            
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
           e.run(125) 
           e.counts()
        Text
           \break There are 125 point over F_5 in A^3. Of these 25 lie on 
           the plane and 5 on the line. As one can see this is also about
           the number found by a random search.  
        Example
           e.estimateDecomposition()
        Text
           The expected fraction of points on a codim c component is 
           1/p^c. This is used by estimateDecomposition to guess the 
           number of components in each codimension. We see that up to 
           a margin of error this gives the correct answer of 1 component
           in codimension 1 and 2 each.       
///

doc ///
   Key
        "countsByCount"
   Headline
        sorts the statistics by number of points found
   Usage   
        e.countsByCount()
   Inputs  
        e:Experiment 
            an Experiment
   Description
        Text
            Returns a Tally with the number of points that
            an experiment has found so far for each combination
            of wached properties sorted by the number of points found.
            (not by alpha numeric sort of the properties)

            This gives a rough overview over which properties 
            occur in large/small strata. 
            
            This is particularily usefull when working with an
            black box parameter space that provides no propery
            rankJacobianAt and therefore no direct estimation of
            the codimension of each component.
                   
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
           e.countsByCount()
           sort e.counts()
///

doc ///
   Key
        "ignoreProperty"
   Headline
        deletes a property from the list of watched properties
   Usage   
        e.ignoreProperty(propertyName)
   Inputs  
        e:Experiment 
            an Experiment
        propertyName: String
            the name of a property
   Description
        Text
           This removes a property from the list of watched properties.
           This works only if the experiment has been reset with e.clear()
           since otherwise the statistics would be inconsistent.
                                 
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
           e.watchedProperties()
        Text
           \break Lets not only watch the codimension of the tangentspace
           at a random point, but also whether the point is probably smooth
        Example
           e.watchProperty("isProbablySmoothAt")
           e.watchedProperties()
           e.run(250)
        Text
           \break Lets assume that from the last experiment we conclude,
           that the smoothness of a point does not yield any interesting 
           information for us.
           In this case we would cease to watch this property in 
           follow up experiments. Before we can do that, we have to
           clear the statistics.
        Example
           e.clear()
           e.ignoreProperty("isProbablySmoothAt")
           e.watchedProperties()
           e.run(250)  
   SeeAlso
      ignoreProperties
      watchProperty
      watchProperties
      watchedProperties  
///

doc ///
   Key
        "ignoreProperties"
   Headline
        deletes a property from the list of watched properties
   Usage   
        e.ignoreProperties(L)
   Inputs  
        e:Experiment 
            an Experiment
        L:List
            a list of property names. 
   Description
        Text
           This removes several properties from the list of watched properties.
           This works only if the experiment has been reset with e.clear()
           since otherwise the statistics would be inconsistent.
                                 
           Lets see how this works in an artificial but instructive example.                
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
           e.watchedProperties()
        Text
           \break Lets not only watch the codimension of the tangentspace
           at a random point, but also whether the point is probably smooth
           and what the values at the random points are.
        Example
           e.watchProperty("isProbablySmoothAt")
           e.watchProperty("valuesAt")
           e.watchedProperties()
           e.run(250)
        Text
           \break Lets assume that from the last experiment we conclude,
           that the smoothness of a point and the value of
           the polynomial at the point does not yield any interesting 
           information for us.
           In this case we would cease to watch these properties in 
           follow up experiments. Before we can do that, we have to
           clear the statistics.
        Example
           e.clear()
           e.ignoreProperties({"isProbablySmoothAt","valuesAt"})
           e.watchedProperties()
           e.run(250)  
   SeeAlso
      ignoreProperty
      watchProperty
      watchProperties
      watchedProperties   
///

doc ///
   Key
        "pointsPerComponent"
   Headline
        the number of points an experiment tries to collect on each component.
   Usage   
        e.pointsPerComponent()
   Inputs  
        e:Experiment 
            an Experiment
   Description
        Text
            An experiment collects a limited number of points
            for each combination of properies it encounters. 
            
            This is useful if one wants to inspect
            points with special properties in more detail. If the 
            points are on a moduli space one can create the
            corresponding object and study it in detail.
            
            On strata of low codimension many points are found. To
            avoid memory problems only a small number of points
            are collected.
            
            This function returns the number of points that an
            experiment tries to collect on each component. Since
            the number of components is estimated heuristically, the 
            number of points collected is often different from the
            goal given by this function. 
 
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
           e.run(100)   
        Text
           \break The number of points the experiment tries to collect per component:
        Example
           e.pointsPerComponent()
        Text 
           \break The number of point the experiment has collected
        Example      
           e.collectedCount()
           e.run(100)
           e.collectedCount()
        Text
           \break Lets now increase the number of points we want to collect
        Example
           e.setPointsPerComponent(20)
           e.pointsPerComponent()    
           e.run(100)
           e.collectedCount()
           e.run(100)
           e.collectedCount()
   SeeAlso
        setPointsPerComponent
        collectedCount
        pointLists
        pointsByPropertyValues
///


doc ///
   Key
        "pointLists"
   Headline
        the points an experiment has collected for closer inspection
   Usage   
        e.pointLists()
   Inputs  
        e:Experiment 
            an Experiment
   Description
        Text
            An experiment collects a limited number of points
            for each combination of properies it encounters. 
            
            This is useful if one wants to inspect
            points with special properties in more detail. If the 
            points are on a moduli space one can create the
            corresponding object and study it in detail.
            
            On strata of low codimension many points are found. To
            avoid memory problems only a small number of points
            are collected.
            
            This function returns a hashTable containing a list
            of collected points for each combination of properties.
  
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
           e.run(100)   
        Text
            \break The number of points the experiment has collected:
        Example      
           e.collectedCount()
        Text
           \break The points themselves:
        Example
           e.pointLists()
        Text
           \break The points for a particular set of properites:
        Example   
           (e.pointLists())#{2}
        Text
           The brackets used in this example are NOT redundant. Better
           readable is the following syntax
        Example
           e.pointsByPropertyValues({2})
   SeeAlso
        pointsPerComponent
        setPointsPerComponent
        collectedCount        
        pointsByPropertyValues
///        

doc ///
   Key
        "pointsByPropertyValues"
   Headline
        the points an experiment has collected for a particular set of properties
   Usage   
        e.pointsByPropertyValues(key)
   Inputs  
        e:Experiment 
            an Experiment
        key: List
            of property values
   Description
        Text
            An experiment collects a limited number of points
            for each combination of properies it encounters. 
            
            This is useful if one wants to inspect
            points with special properties in more detail. If the 
            points are on a moduli space one can create the
            corresponding object and study it in detail.
            
            On strata of low codimension many points are found. To
            avoid memory problems only a small number of points
            are collected.
            
            This function returns a List 
            of collected points for a given combination of properties.
  
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
           e.run(100)   
        Text
            \break The number of points the experiment has collected:
        Example      
           e.collectedCount()
        Text
           \break The points themselves:
        Example
           e.pointLists()
        Text
           \break The points for a particular set of properites:
        Example
           e.pointsByPropertyValues({2})
   SeeAlso
        pointsPerComponent
        setPointsPerComponent
        collectedCount        
        pointLists
        pointKeys
///        

doc ///
   Key
        "setPointsPerComponent"
   Headline
        change the number of points an experiment tries to collect on each component.
   Usage   
        e.setPointsPerComponent(num)
   Inputs  
        e:Experiment 
            an Experiment
        num: ZZ
            the number of points an experiment should try to collect on
            each component.
   Description
        Text
            An experiment collects a limited number of points
            for each combination of properies it encounters. 
            
            This is useful if one wants to inspect
            points with special properties in more detail. If the 
            points are on a moduli space one can create the
            corresponding object and study it in detail.
            
            On strata of low codimension many points are found. To
            avoid memory problems only a small number of points
            are collected.
            
            This function changes the number of points that an
            experiment tries to collect on each component. Since
            the number of components is estimated heuristically, the 
            number of points collected is in the end often different from 
            (but close to) the
            goal given by this function. 
 
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
           e.run(100)   
        Text
           \break The number of points the experiment tries to collect per component:
        Example
           e.pointsPerComponent()
        Text 
           \break The number of point the experiment has collected
        Example      
           e.collectedCount()
           e.run(100)
           e.collectedCount()
        Text
           \break Lets now increase the number of points we want to collect
        Example
           e.setPointsPerComponent(20)
           e.pointsPerComponent()    
           e.run(100)
           e.collectedCount()
           e.run(100)
           e.collectedCount()
   SeeAlso
        pointsPerComponent
        collectedCount
        pointLists
        pointsByPropertyValues
///

doc ///
   Key
        "pointKeys"
   Headline
        a list of property combinations found by an experiment
   Usage   
        e.pointKeys()
   Inputs  
        e:Experiment 
            an Experiment
        num: ZZ
            the number of points an experiment should try to collect on
            each component.
   Description
        Text
            An experiment collects a limited number of points
            for each combination of properies it encounters. 
            
            This is useful if one wants to inspect
            points with special properties in more detail. If the 
            points are on a moduli space one can create the
            corresponding object and study it in detail.
            
            
            This function returns a list of those property combinations
            that it has encountered. This is sometimes useful if
            each property is very long or difficult to enter.
            
            Lets see how this works in an example. Here we look
            for matrices with special syzygies
                           
            First we create a black box that makes random matrices of quadrics.
        Example      
           K = ZZ/2;
           R = K[x,y,z,w];
        Text
           We want to study the parameter space of 2x3 matrices
           with quadratic entries. Such matrices are defined by
           60 coefficients.
        Example   
           bb = blackBoxParameterSpace(80,K);
        Text
           We start by making quadrics from 10 coefficiets
        Example
           mons2 = matrix entries transpose super basis(2,R)
           quadricAt = (point10) -> (point10*mons2)_0_0;
           quadricAt(matrix{{1,0,0,0,1,0,0,1,0,1}})
        Text
           Now we make a 2x3 matrix of quadrics from 80 coefficients
        Example
           matrixAt = (point80) -> matrix apply(2,i->
                apply(4,j->(
                          quadricAt(point80_{i*10+j..i*10+j+9})
                          )
                     )
                )          
           matrixAt(random(K^1,K^80))
        Text
           For later use we register this function in the 
           black box parameter space
        Example
           bb = bb.registerPointProperty("matrixAt",matrixAt);
        Text
           Now we look at the syzygies of such a matrix
        Example
           bettiAt = (point80) -> betti res coker matrixAt(point80)
           bettiAt(random(K^1,K^80))
           bb = bb.rpp("bettiAt",bettiAt);
        Text
           Now we 
           make an experiment to study this parameter space
        Example   
           e = new Experiment from bb;
        Text
           We are interested in the betti tableau ot the minimal free resolution.
        Example   
           e.watchProperty("bettiAt")
           e.run(100)
           sort e.counts()
        Text
           We now want to look at the collected point with special
           betti tableaus
        Example
           e.observedPropertyTuples()
           e.observedPropertyTuple(0)
           e.pointsByPropertyValues e.observedPropertyTuple(0)
   SeeAlso
        pointLists
        pointsByPropertyValues
        collectedCount
        pointsPerComponent
        setPointsPerComponent
///


doc ///
   Key
        "e.run"
   Headline
        runs an experiment for a given number of trials
   Usage   
        e.run(trials)
   Inputs  
        e:Experiment 
            an Experiment
        trials: ZZ
            the number of random point the experiment shall try out
   Description
        Text
           This is the central function of this package that does all 
           the work. It 
           starts an experiment to evaluate the watched properties
           at a given number of random points. The experiment then
           counts the number of times a particular property is encontered.
        
           If the experiment is made from a black box ideal, only points
           on which the generators of the ideal vanish are counted. If
           the experiment is made form a black box parameter space, all
           points are counted.
        
           Also the experiment stores some points for later inspection. 
           For documentationa about how this works see collectedCount
        
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
           e.watchedProperties()
           e.run(100)
        Text
           \break The experiment has evaluated the black box ideal at 
           100 random points and for each point where the ideal vanished it calculated
           the rank of the jacobi matrix at this point. Then the
           number of points for each rank are counted.
       
           If one wants better statistics one can run the experiment for more
           trials
        Example
           e.trials()   
           e.run(100)
           e.trials()
        Text
           The experiment has autmatically collected some points
           for each combination of properties
        Example
           e.pointLists()
           e.pointsByPropertyValues({2})
           e.collectedCount()
        Text
           Notice that the experiment has not collected all points it
           found. This is done to save memory space.
   SeeAlso
       trials
       watchedProperties
       pointLists
       pointsByPropertyValues
       collectedCount
///                 
         
doc ///
   Key
        "trials"
   Headline
        the number of trials an experiment has been run so far
   Usage   
        e.trials()
   Inputs  
        e:Experiment 
            an Experiment
   Description
        Text
           Returns the number of trials an experment has been run so far.
           This might be important to interpret the statistics of 
           the experiment. It is used for example in estimateStratification
           and estimateDecomposition.  
             
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
           e.watchedProperties()
           e.run(100)
           e.trials()   
           e.run(100)
           e.trials()
///                 
 
doc ///
   Key
        "tryProperty"
   Headline
        evaluate a new property on the collected points.
   Usage   
        e.tryProperty(name)
   Inputs  
        e:Experiment 
            an Experiment
        name:String
            the name of a property.
   Description
        Text
           Evaluates a given property on all collected points.
           
           This is useful if in the course of experimenting one thinks
           of a new property that might help in the analysis of 
           the problem at hand. Before running the complete experiment
           again an watching the new property it is much faster to
           begin by evaluating the new property on the collected points.
           Since the experiment collects points on all interesting strata
           this gives a good first overview of what the new property
           will do.
             
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
           e.watchedProperties()
           e.run(250)
           e.collectedCount()
        Text 
           \break Perhaps we are wondering is points with a certain
           rank of the Jacobi matrix are allways singular:
        Example
           e.tryProperty("isCertainlySingularAt")
        Text
           It seems that all points with rank 0 are singular. Indeed
           this is true in this example since rank 0 only occurs at
           the intersection point of the line and plane.
           
           For a more realistic example see @TO "Experiment for singularities of cubic surfaces" @
   SeeAlso
       watchedProperties
       watchProperty
       watchProperties
///                 

doc ///
   Key
        "usedRankJacobianAt"
   Headline
        the name of the black box property used to calculate the codimension of the tangent space at a point.
   Usage   
        e.usedRankJacobianAt()
   Inputs  
        e:Experiment 
            an Experiment
   Description
        Text
           Sometimes it is useful to implement a custom method for
           calculating the codimension of the tangent space at a point.

           This method must be first be registered as a property of the black box 
           used in the experiment. Then one tells the experiment to
           use this new property for caculating tangent spaces.
           
           This function documented here can then be used to see which property
           is currently used for calculating the codimension of a tangent
           space at a point.
            
           Lets see how this works in an example.                
        
           First we create an ideal we want to analyse and put it into a blackbox:
        Example      
           K = ZZ/5;
           R = K[x,y,z];
           I = ideal (x*z,y*z);
           bb = blackBoxIdeal I;
        Text
           \break We now create (in this case stupid) new property:
        Example
           rankAllways5At = (point) -> 5;
           bb = bb.rpp("rankAllways5At",rankAllways5At);
           bb.pointProperties()
        Text
           \break Now make an experiment from the blackbox:
        Example        
           e = new Experiment from bb;
           e.usedRankJacobianAt()
           e.run(100)
        Text 
           \break Now we change the method for calculating the
           rank of jacobi matrices. Before doing this we must
           clear the statistics.
        Example
           e.clear()
           e.useRankJacobianAt("rankAllways5At")
           e.usedRankJacobianAt()
           e.run(100)
        Text
           A more realistic application is for example the case
           where the equations of our ideal can be written 
           as A*B = 0 with A and B matrices with polynomial entries.
           We can then use the product rule 
           (A*B)' = A'*B + A*B' to differentiate. This is often faster.
           
           An other case where this might be used is when we have a morphism
           X -> Y and look at random points in X but are interested in the
           tangent space after projecting to Y.
   SeeAlso
      useRankJacobianAt
///                 

doc ///
   Key
        "useRankJacobianAt"
   Headline
       change the black box property used to calculate the codimension of the tangent space at a point.
   Usage   
        e.useRankJacobianAt(name)
   Inputs  
        e:Experiment 
            an Experiment
        name: String
            name of the new property to be used
   Description
        Text
           Sometimes it is useful to implement a custom method for
           calculating the codimension of the tangent space at a point.

           This method must be first be registered as a property of the black box 
           used in the experiment. Then one uses the function
           documented here to tell the experiment to
           use this new property for caculating tangent spaces.
           
           It is important to tell the experiment explicitly which property
           calculates the codimension of the tangenspace at a point since
           this is used in calculating estimates of decompositions. 
            
           Lets see how this works in an example.                
        
           First we create an ideal we want to analyse and put it into a blackbox:
        Example      
           K = ZZ/5;
           R = K[x,y,z];
           I = ideal (x*z,y*z);
           bb = blackBoxIdeal I;
        Text
           \break We now create (in this case stupid) new property:
        Example
           rankAllways5At = (point) -> 5;
           bb = bb.rpp("rankAllways5At",rankAllways5At);
           bb.pointProperties()
        Text
           \break Now make an experiment from the blackbox:
        Example        
           e = new Experiment from bb;
           e.usedRankJacobianAt()
           e.run(100)
           e.estimateDecomposition()
        Text 
           \break Now we change the method for calculating the
           rank of jacobi matrices. Before doing this we must
           clear the statistics.
        Example
           e.clear()
           e.useRankJacobianAt("rankAllways5At")
           e.watchProperty("rankAllways5At")
           e.usedRankJacobianAt()
           e.run(100)
           e.estimateDecomposition()
        Text
           A more realistic application is for example the case
           where the equations of our ideal can be written 
           as A*B = 0 with A and B matrices with polynomial entries.
           We can then use the product rule 
           (A*B)' = A'*B + A*B' to differentiate. This is often faster.
           
           An other case where this might be used is when we have a morphism
           X -> Y and look at random points in X but are interested in the
           tangent space after projecting to Y.
   SeeAlso
      usedRankJacobianAt
///                 
 

doc ///
   Key
        "watchProperty"
   Headline
        add a property the list of watched properties
   Usage   
        e.watchProperty(name)
   Inputs  
        e:Experiment 
            an Experiment
        name:String
            the name of the black box property to be added. 
   Description
        Text
           This add a property to the list of watched properties.
           This works only if the experiment has been reset with e.clear()
           since otherwise the statistics would be inconsistent.
                                 
           Lets see how this works in an artificial but instructive example.                
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
           e.watchedProperties()
        Text
           \break Lets not only watch the codimension of the tangentspace
           at a random point, but also whether the point is probably smooth.
        Example
           e.watchProperty "isProbablySmoothAt"
           e.watchedProperties()
           e.run(500)
   SeeAlso
      ignoreProperty
      ignoreProperties
      watchProperties
      watchedProperties   
///
 
doc ///
   Key
        "watchProperties"
   Headline
        add a list of properties the list of watched properties
   Usage   
        e.watchProperties(L)
   Inputs  
        e:Experiment 
            an Experiment
        L:String
            of name of property to be added. 
   Description
        Text
           This add several properties to the list of watched properties.
           This works only if the experiment has been reset with e.clear()
           since otherwise the statistics would be inconsistent.
                                 
           Lets see how this works in an artificial but instructive example.                
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
           e.watchedProperties()
        Text
           \break Lets not only watch the codimension of the tangentspace
           at a random point, but also whether the point is probably smooth.
        Example
           e.watchProperties({"isProbablySmoothAt","isCertainlySingularAt"})
           e.watchedProperties()
           e.run(300)
   SeeAlso
      ignoreProperty
      ignoreProperties
      watchProperty
      watchedProperties   
///
 
doc ///
   Key
        "watchedProperties"
   Headline
        returns the list of properties that are watched by an experiment
   Usage   
        e.watchedProperties()
   Inputs  
        e:Experiment 
            an Experiment
   Description
        Text
           During an experiment a black box is evaluated in random points.
           The experiment automatically looks at certain properties 
           associated to the point. This could be the codimension of 
           the tangent space at this point or wether the variety considered
           is singular or smooth.
           
           More interesting applications are possible if the variety
           considered is a parameter space of certain algebraic objects 
           (curves, matrices, etc). In this case the object parametrized
           by a point can have interestering properties wich we want to 
           study (eg. number of singularities in the case of curve or
           betti numbers of their kernels in the case of matrices).
      
           The experiment keeps automatically counts the number of
           times each combination of properties was encountered
           during the experiment so far. This gives some heuristic
           information about the size of the strata in which these occur.
           
           The function documented shows which properties are currently
           watched.
           
           Lets see how this works in an artificial but instructive example.                
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
           e.watchedProperties()
        Text
           \break rankJacobianAt is automatically watched since the rank of
           the Jacobi matrix is the codimension of the tangent space to
           the variety at this point. This gives an upper bound to the
           codimension of the component of the variety on which the 
           point lies. This is used in the heuristic estimates given
           by this package. 
        Example
           e.run(200)
           e.estimateDecomposition()
        Text   
           \break Lets now also watch whether the points considered
           are probably smooth. For this the statitics have to be cleared
           first:
        Example
           e.clear()
           e.watchProperty("isProbablySmoothAt")
           e.watchedProperties()
           e.run(300)
   SeeAlso
      ignoreProperty
      ignoreProperties
      watchProperty
      watchProperties
///

doc ///
    Key
        "Experiment for singularities of cubic surfaces"
    Headline
        use an Experiment to study the space of cubic surfaces
    Description
        Text
            A black box parameter space is used to implement parameter spaces
            with their universal families in a pointwise fashion.
            
            Let us build the parameter space of cubic surfaces with a view of 
            studying its stratification with respect to singularity type.
            
            We work in charateristic 7.            
        Example    
            K = ZZ/7
        Text
            The coordinate ring of IP^3
        Example
            R = K[x,y,z,w]
        Text  
            Make an empty blackbox which will later contain our 
            describtion of the parameter space of cubic surfaces.
            It will depend on 20 parameters, since acubic polynomial 
            in 4 variables has 20 coefficients.
        Example
            bbC = blackBoxParameterSpace(20,K);
            bbC.pointProperties()
        Text
            We now build the cubics from the coefficents, i.e. we
            construct the member of the universal familiy over 
            a given parameter point:
        Example
            mons3 = matrix entries transpose super basis(3,R)
            cubicAt = (point) -> matrix entries (point*mons3)
        Text 
            register this function in the black box 
        Example
            bbC = bbC.registerPointProperty("cubicAt",cubicAt);
            bbC.pointProperties()
        Text
            Lets test this functionality with some special cubics.
            The first example is the cubic cone. It is singular
            at (0:0:0:1):                   
        Example    
            cubicCone = matrix{{x^3+y^3+z^3}}
            coeffCubicCone = contract(transpose mons3,cubicCone)
            bbC.cubicAt(coeffCubicCone)
        Text
            The second example is the Fermat cubic. It is smooth everywhere    
        Example
            cubicFermat = matrix{{x^3+y^3+z^3+w^3}}
            coeffCubicFermat = contract(transpose mons3,cubicFermat)
            bbC.cubicAt(coeffCubicFermat)
        Text
            Now we want to implement the stratification by singularity type.
            For this we first determine the singular locus of a cubic surface:
        Example
            singularLocusAt = (point) -> ideal jacobian cubicAt(point)
            bbC = bbC.rpp("singularLocusAt",singularLocusAt);
            bbC.pointProperties()
            bbC.singularLocusAt(coeffCubicCone)   
            bbC.singularLocusAt(coeffCubicFermat)
        Text
            As a first approximation of the singularity type we use
            the number of points in the singular locus (counted with multiplicity)
        Example
            numPoints = (s) -> (
                 if dim s == 0 then return 0;                
                 if dim s == 1 then return degree s;                 
                 if dim s >= 2 then return infinity;
                 );
            degreeSingularLocusAt = (point) -> (
                 numPoints singularLocusAt(point)
              )
            bbC = bbC.rpp("degreeSingularLocusAt",degreeSingularLocusAt);
            bbC.pointProperties()
        Text
            Calculate the degree of the singular locus for our examples
        Example
            bbC.degreeSingularLocusAt(coeffCubicCone)
            bbC.degreeSingularLocusAt(coeffCubicFermat)
        Text
            Now the BlackBoxParameterspace hat a number of point properties
        Example
            bbC.pointProperties()
        Text
            These properties can now be used in a finite field experiment
            that studies the statification of our parameter space. 
        Example
            e = new Experiment from bbC;
        Text
            We are interested in the degree of the singular Locus of our
            cubics so we ask the experiment to watch this property
        Example
            e.watchProperty("degreeSingularLocusAt")
            e.run(200) 
        Text
            We see that there is an open stratum of smooth cubics. 
            Lets estimate the codimension of the other strata. This is
            done by assuming that the number of points in each stratum
            is irreducible an therefore the fraction of points on
            a codimension c stratum is approximately 1/p^c:
        Example
            e.estimateStratification()
        Text
            We see that the stratum of degree 1 singularities seems to
            be a divisor. The codimension of the strata are not yet estimated,
            since we have not found enough points on them to make the
            estimation precise enough.
            
            Lets find more points
        Example
            time e.run(700)
            e.estimateStratification()
        Text
            This is strange. The experiment feels that we have enough
            points to estimate the codimension, but it is neither 1 or 2.
            This can happen if our assumption that the strata are irreducible
            is not satisfied. Lets now assume more generally, that the
            statum of cubics with degree 2 singularities is irreducible with
            d components of codimension c. In this case the fraction of 
            points on this stratum would be about d/p^c. Taking -log_p
            of this (as the estimation procedure does) gives
            
            c - log_p(d)
            
            so the estimation is somewhat smaller than the true codimension.
            In our case p=7 and we have
            
            log_7(2) = 0.35
            log_7(3) = 0.56
            log_7(4) = 0.72
            
            And 2-log_7(2) = 1.65. So it seems as if we have 2 irreducible
            strata of degree 2 singularities.
            
            To figure out what these two strata might be we can look
            at some examples the experiment has collected for us:
        Example
            e.collectedCount()            
            e.pointsByPropertyValues({2})
        Text
            The experiment does not save all points it encounters, since
            this would quickly lead to memory problems. Rather it tries to
            collects 10 points per stratum:
        Example
            e.pointsPerComponent()
        Text
            Lets now look at the singularities of these examples in
            more detail. One idea might be to look at the support 
            of the singular locus:
        Example
            apply(e.pointsByPropertyValues({2}),i->radical (bbC#"singularLocusAt")(i))
        Text
            or maybe the degrees of the support:
        Example
            apply(e.pointsByPropertyValues({2}),i->degree radical (bbC#"singularLocusAt")(i))
        Text
            It seems that about half of the points have support in one point
            and the other half has support in 2 points. So half have 2 nodal
            singularities and halb have 1 cuspidal singularity. So maybe
            a more precise invariant to distunguish irreducible strata
            in our statification would be the degrees of the irreducible 
            components of our singularity. Lets make a property for this.
            
            This has to be a little more involved than expected since
            macaulay calculates the primary decomposition of an ideal
            over the finite field F_7 and not over its algebraic closure.
        Example
            partitionSingularLocusAt = (point) -> (
                 pI := primaryDecomposition(singularLocusAt(point));
                 sort flatten apply(pI,i->(   
                      d := numPoints i;
                      if d==0 then return {};
                      if d==infinity then return {infinity};
                      r := numPoints radical i;
                      -- the number of support point in this
                      -- component is r. Therefore 
                      -- the multiplicities of these points 
                      -- must be d/r.
                      return toList(r:(d/r))
                      ))
                 )            
            bbC = bbC.rpp("partitionSingularLocusAt",partitionSingularLocusAt);                     
            bbC.pointProperties()
        Text
            Lets try out what this property gives on the collected points:
        Example
            e.tryProperty("partitionSingularLocusAt")
        Text
            This looks interesting, so lets restart the experiment and
            watch for this property.
        Example
            e.clear()
            e.watchProperty("partitionSingularLocusAt")
            e.watchedProperties()
            time e.run(300)
            e.estimateStratification()
        Text
            This takes much longer, but still lets find more points
            to see what we find in the degree 2 strata
        Example
            time e.run(2000)
        Text
            It seems that indeed we have several strata of similar size
            for each degree. Lets estimate their codimensions
        Example
            e.estimateStratification()
        Text
            To get good estimates for the degree 3 partitions one
            would have to run through another 10.000 random points. 
            (do this on your machine. It takes a couple of minutes).
            
            For good estimates of degree 4 partitions about 50.000
            have to be evaluated (do this also...). In degree 4 the
            pattern breaks down and the partition {4} seem again 
            to have two components. Some further analysis might
            show that these components belong to A_4 and D_4 singularities
            respectively.
            
            Given enough time and energy one might in this way rediscover
            the beautiful classical theorem about singularities of cubic surfaces
            Namely that the irreducible strata of singular cubic
            surfaces are parametrized by sub-Dynkin-diagramms of the
            E_7 Dynkin diagramm. (see the Chapter on singularities of cubic surfaces in
            Dolgachev's Book "Topics in Classical Algebraic Geometry"). 
///
