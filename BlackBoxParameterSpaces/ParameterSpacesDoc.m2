 doc ///
    Key
        (blackBoxParameterSpace, ZZ, Ring)
        blackBoxParameterSpace
        BlackBoxParameterSpace
    Headline
        create a BlackBoxParameterSpace.
    Usage   
        blackBoxParameterSpace(d,K)
    Inputs  
        d: ZZ 
         the (affine) dimension of the parameter space
        K: Ring
             a field
    Outputs
        : BlackBoxParameterSpace
    Description
        Text
            A black box parameter space is used to implement parameter spaces
            with their universal families in a pointwise fashion.                      
    
            For a quick start see the @TO "Singularities of cubic surfaces" @-tutorial
            \break \break
            
            A more trivial example is the parameter space of 2x2 matrices
            with entries in a finite field. This is
            an affine 4 dimensional space:
            
        Example
            K = ZZ/11;
            bb = blackBoxParameterSpace(4,K);
        Text
            For each point in this parameter space we have a 2x2-matrix:
        Example
            matrixAt = (point) -> matrix{{point_0_0,point_1_0},{point_2_0,point_3_0}};
            bb = bb.rpp("matrixAt",matrixAt);
            bb.matrixAt(matrix{{1,2,3,4_K}})
        Text
            Properties of the parametrized objects can also be store in
            the Black box. Maybe one is especially interested in the rank
            filtration of the parameter space:
        Example
            rankAt = (point) -> rank matrixAt(point)
            bb = bb.rpp("rankAt",rankAt);
            rankAt(matrix{{1,1,1,1_K}})
            rankAt(matrix{{1,2,3,4_K}})
        Text
            The black box parameter space keeps track of all such properties
            defined:
        Example
            bb.pointProperties()
        Text
            How such properties stratify the parameter space can be systematically
            evaluated by using the package FiniteFieldExperiments.
    Caveat
            There are several special property names: 
            
            If  @TO valuesAt@ is registered or updated, this will
            will implicitly construct   
            @TO isZeroAt@, @TO numGenerators@,  @TO jacobianAt@,
             @TO rankJacobianAt@, @TO isCertainlySingularAt@ and @TO isProbablySmoothAt@.

            This happens automatically if the BlackBox is created using
            @TO blackBoxIdealFromEvaluation@.
            
            If  @TO jacobianAt@ is registered or updated, this will
            will implicitly construct   
             @TO rankJacobianAt@, @TO isCertainlySingularAt@ and @TO isProbablySmoothAt@.
///
            

doc ///
    Key
        "Singularities of cubic surfaces"
    Headline
        use a blackBoxParameterSpace to study the space of cubic surfaces
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
            singularLocusAt = (bb,point) -> ideal jacobian bb.cubicAt(point)
            bbC = bbC.rpp("singularLocusAt",singularLocusAt);
            bbC.pointProperties()
            bbC.singularLocusAt(coeffCubicCone)   
            bbC.singularLocusAt(coeffCubicFermat)
        Text
            As a first approximation of the singularity type we use
            the degree of the singular locus
        Example
            degreeSingularLocusAt = (bb,point) -> (
                 s := bb.singularLocusAt(point);
                 if dim s == 0 then return 0;                
                 if dim s == 1 then return degree s;                 
                 if dim s >= 2 then return infinity;
              )
            bbC = bbC.rpp("degreeSingularLocusAt",degreeSingularLocusAt);
            bbC.pointProperties()
        Text
            Calculate the degree of the singular locus for our examples
        Example
            bbC.degreeSingularLocusAt(coeffCubicCone)
            bbC.degreeSingularLocusAt(coeffCubicFermat)
        Text
            Now the BlackBoxParameterSpace hat a number of point properties
        Example
            bbC.pointProperties()
        Text
            These properties can now be used in a finite field experiment
            that studies the statification of our parameter space. Here is a
            simple minded version of such an experiment:
        Example
            tally apply(100,i->bbC.degreeSingularLocusAt(random(K^1,K^20))) 
        Text
            We see that there is an open stratum of smooth cubics. The
            largest closed stratum consists of those cubics with a A1 singularity.
            The package FiniteFieldExperiments helps to do the bookkeeping 
            for such experiments and also provides more detailed interpretation
            of the results.

///

doc ///
    Key
        "registerPointProperty"
        "rpp"
    Headline
        register a new point property in a black box.
    Usage   
        bbI = bbI.registerPointProperty(name,propertyAt)
        bbI = bbI.rpp(name,propertyAt)
    Inputs  
        bbI: BlackBoxParameterSpace
        name : String
          name of the new point property
        propertAt: Function 
          that takes coordinates of a point and returns anything.
    Outputs
        : BlackBoxParameterSpace
    Description
        Text
          rpp and 
          registerPointProperty are synonymous. rpp is provided
          to save typing time...
          
          This method is used to register new property in a
          blackBoxIdeal or a blackBoxParameterSpace. 
          
          Lets for example build a black box parameter space
          for cubic surfaces in IP^3 over the finite field with
          7 Elements.
        Example
          Fp = ZZ/7
          R = Fp[x,y,w,z]
        Text
          there are 20 monomials of degree 3 in 4 variables
        Example
          mons3 = basis(3,R)
        Text
          Therefore we need a 20 dimensional parameter space
        Example
          bbC = blackBoxParameterSpace(20,Fp);
        Text
          This has no known point properties 
        Example
           bbC.pointProperties()
        Text
          We now make a function that constructs a cubic
          polynomial from 20 parameters
        Example
          cubicAt = (point) -> point*transpose mons3
          cubicAt(matrix{{1_Fp,19:0}})
          fermatPoint = matrix {{1_Fp,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1}}
          cubicAt(fermatPoint)
        Text
          To have this available inside the blackbox we need to register
          it
        Example
          bbC = bbC.registerPointProperty("cubicAt",cubicAt);
          bbC.pointProperties()
        Text
          Now we can use the property from the black box
        Example
          bbC#"cubicAt"(fermatPoint)
          bbC.cubicAt(fermatPoint)
        Text
          Registering a point property is useful when the black box
          is used in a finite field experiment. Registered
          point properties are available to an experiment while it is
          running. Also it is useful to register for bookkeeping reasons,
          so that for example pointProperties will return
          the correct answer.
///

doc ///
    Key
        "updatePointProperty"
        "upp"
    Headline
        change a point property in a black box.
    Usage   
        bbI.updatePointProperty(name,propertyAt)
        bbI.upp(name,propertyAt)
    Inputs  
        bbI: BlackBoxParameterSpace
        name : String
          name of point property to be changed
        propertAt: Function 
          that takes coordinates of a point and returns anything.
    Outputs
        : BlackBoxParameterSpace
    Description
        Text
          upp and 
          updatePointProperty are synonymous.
          
          This method is used to change a property of 
          blackBoxIdeal or a blackBoxParameterSpace. The need
          for this arises usually while programming when one
          realizes that a programming mistake was made.  
          
          Lets look at a stupid, but illustrative example:       
        Example
          bbC = blackBoxParameterSpace(2,QQ);
          bbC = bbC.rpp("product",(point) -> sum flatten entries point);
          bbC.product(matrix{{5,6_QQ}})
          bbC.upp("product",(point) -> product flatten entries point);
          bbC.product(matrix{{5,6_QQ}})
        Text
          It is also possible to update pointProperties that are
          predefined by the package. Here
          valuesAt and jacobianAt are special pointProperties,
          in the sense that the package uses them to compute other
          pointProperties. Therefore
          updating valuesAt triggers updates for 
          isZeroAt, numGenerators, jacobianAt, rankJacobianAt.
          Similarily updating jacobianAt triggers an update 
          for 'rankJacobianAt'.  
    SeeAlso
         registerPointProperty
         rpp
         valuesAt
         jacobianAt
///

doc ///
    Key
        "hasPointProperty"
    Headline
        check whether a point property of a black box is defined
    Usage   
        bbI.hasPointProperty(name)
    Inputs  
        bbI: BlackBoxParameterSpace
        name : String
          name of point property to be checked
    Outputs
        : Boolean
    Description
        Text
          check whether a point property is defined.
          
          Every BlackBoxIdeal has the property "jacobianAt" since
          it has (at least implicitly) access to a representation of 
          the generators of the ideal:
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal (x^2-y^3);
          bbI.hasPointProperty("jacobianAt")
          bbI.pointProperties()
        Text
          A blackBoxParameterSpace usually does not have 
          the property "jacobianAt" since such a parameter space
          does not even have an implicit representation of 
          the equations.
        Example
          bbParam = blackBoxParameterSpace(2,QQ);     
          bbParam.hasPointProperty("jacobianAt") 
          bbParam.pointProperties()
        Text
          To illustrate why this can happen think for example of 
          the space of
          singular cubics. In a BlackBoxParameterSpace one
          would simply test the smoothness of a give cubic
          via Groebner basis calculation. This does not automatically
          give rise to a representation of the corresponding
          Diskriminant. 
    SeeAlso
         pointProperties
         registerPointProperty
///

doc ///
    Key
        "memberMethods"
        (memberMethods, BlackBoxParameterSpace)
    Headline
        list the methods of a black box 
    Usage   
        bbI.memberMethods()
    Inputs  
        bbI: BlackBoxParameterSpace
    Outputs
        : List       
    Description
        Text
          Every BlackBoxIdeal has some methods
          provided by the package 
          (no new attributes can be defined by the user).
          The difference between attributes and methods is,
          that an attribute is a constant while a property is 
          a function.
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal (x^2-y^3);
          bbI.memberMethods()
        Text
          Each of these methods has their own documentation node.
    SeeAlso
         hasPointProperty
         attributes
         pointProperties         
         registerPointProperty
         rpp
         setSingularityTestOptions
         updatePointProperty
         upp
///

doc ///
    Key
        "pointProperties"   
        (pointProperties, BlackBoxParameterSpace)
    Headline
        list the pointProperties of a black box 
    Usage   
        bbI.pointProperties()
        pointProperties bbI
    Inputs  
        bbI: BlackBoxParameterSpace
    Outputs
        : List       
    Description
        Text
          A pointProperty of a black box is a function that depends
          only on the coordinates of a point in the parameter space. 
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal (x^2-y^3);
          bbI.pointProperties()
          bbI.isZeroAt(matrix{{0,0_QQ}})
          bbI.jacobianAt(matrix{{1,1_QQ}})
          bbI.isProbablySmoothAt(matrix{{1,1_QQ}})
          bbI.isCertainlySingularAt(matrix{{0,0_QQ}})
        Text
          Each of these pointProperties has their own documentation node.
          New pointProperties can be defined by user using registerPointProperty.
          Exisiting pointProperties can be changed by using 
          updatePointProperty. 
          
          valuesAt and jacobianAt are special pointProperties,
          in the sense that the package uses them to compute other
          point properties. Therefore
          updating valuesAt triggers updates for 
          isZeroAt, numGenerators, jacobianAt, rankJacobianAt.
          Similarily updating jacobianAt triggers an update 
          for 'rankJacobianAt'.
    SeeAlso
      isCertainlySingularAt
      isProbablySmoothAt
      isZeroAt
      jacobianAt
      rankJacobianAt
      valuesAt
      registerPointProperty
      updatePointProperty
///

