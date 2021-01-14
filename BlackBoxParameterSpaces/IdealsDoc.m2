doc ///
   Key
        (ideal, BlackBoxIdeal)
   Headline
        extracts the ideal from a BlackBoxIdeal object
   Usage   
        ideal bbI
   Inputs  
        bbI:BlackBoxIdeal
             a BlackBoxIdeal
   Outputs
        : Ideal
            the corresponding ideal of the black box
   Description   
        Text    
           Extracts an ideal for a black box, if the equations were given explicitly
           or reconstructs the ideal from an evaluation. 
        Text
           Lets start with the simpler example where the ideal equations were given explicitly to the black box:
        Example
           K = ZZ/11;
           R  = K[x,y];
           I = ideal (x^2-y^2);
           bbI = new BlackBoxIdeal from I;
           ideal bbI
        Text 
           Now we give an example where the ideal is reconstructed from an evaluation.
        Text
           As an example we use the computation of a determinant (see @TO "blackBoxIdealFromEvaluation" @)
        Text
           We construct the black box:
        Example            
           K = ZZ/11;
           n = 2;
           matrixAt = point -> matrix apply(n,i->apply(n,j->point_(i*n+j)_0)) ;             
           detAt = point -> matrix{{det matrixAt(point)}};
           R = K[a_(0,0)..a_(n-1,n-1)];
           bbDet = blackBoxIdealFromEvaluation(R,detAt);
        Text
           Now it is possible to extract the explicit equations from the black box (though it is usually not recomended)    
        Example
           ideal bbDet
///

doc ///
   Key
        BlackBoxIdeal
        (NewFromMethod, BlackBoxIdeal, Ideal)
   Headline
        a type
   Description
         Text
            A @TO BlackBoxIdeal @ is a special @TO BlackBoxParameterSpace @.
            It is used when the equations of a stratum of
            a parameter space are known at least implicitly.
            
            \,\, \bullet \, If the equations are known explicitly use 
            @TO blackBoxIdeal @ to create a @TO BlackBoxIdeal @.
            
            \,\, \bullet \, If the equations are known implicitly, i.e.
            an algoithm to evaluate the equations is known,
            then use @TO blackBoxIdealFromEvaluation@
            to create a @TO BlackBoxIdeal @.
            
            In both cases the BlackBoxIdeal has the following
            point properties predefined:

            \,\, \bullet \, @TO "isZeroAt" @ \break
            \,\, \bullet \, @TO "valuesAt" @ \break
            \,\, \bullet \, @TO "jacobianAt" @ \break
            \,\, \bullet \, @TO "rankJacobianAt" @ \break
                        
///

doc ///
   Key
        blackBoxIdealFromEvaluation        
   Headline
        create a  BlackBoxIdeal  from an evaluation method
   Usage   
        blackBoxIdealFromEvaluation( R, evaluationMethod )
   Inputs  
        R:Ring
             that contains the implicitly given Ideal
        evaluationMethod:Function
             accepting a point given as a row matrix 
             (with the same number of entries as the number of
             variables of R)
             and returns 
             the evaluation of the generators of the ideal 
             at this point as a row matrix.
   Outputs
        : BlackBoxIdeal
   Description   
        Text    
           Creates a blackbox describing an implicitly given ideal from an evaluation method \break
           A BlackBoxIdeal is a special @TO BlackBoxParameterSpace @.
           
           This is useful when the generators of an ideal are
           to big to write down, but an algorithm evaluating the generators
           of at a point is known.
           
           A trivial example is the determinant of a n x n matrix. 
           The explicit polynomial has n! terms. For big enough n
           it is impossible to even store the n! terms of it. 
           But for a given point in K^{n*n}, the
           matrix has entries in a field and the Gauss-Algorithm can 
           be used to evaluate the determinant. This takes only O(n^3) steps:
           
           Make an n x n matrix from a vector of length n^2:
        Example
           K = ZZ/11
           n = 10;
           matrixAt = point -> matrix apply(n,i->apply(n,j->point_(i*n+j)_0))              
           testPoint = random(K^1,K^(n*n));          
           matrixAt(testPoint)
        Text
           Now take the determinant. To be able to take this
           as an evaluation method of a BlackBoxIdeal it has to
           take a 1 x n^2 matrix and has to return a 1 x m matrix
           (in this case m=1).
        Example
           detAt = point -> matrix{{det matrixAt(point)}};
           detAt(testPoint)
        Text
           Now make a BlackBox from this evaluation method:
        Example
           R = K[a_(0,0)..a_(n-1,n-1)];
           bbDet = blackBoxIdealFromEvaluation(R,detAt);
        Text
           Evaluate the BlackBox at the test point:
        Example
           bbDet.valuesAt(testPoint) == det(matrixAt(testPoint))
        Text
           Using the Package FiniteFieldExperiments
           heuristic information about the irreducible components
           of a variety defined by an black box
           can be obtained.
   Caveat
        may have problems if the coefficient ring is something obscure? Rationals and integers as coefficient ring should be ok.
///

--        new BlackBoxIdeal from Ideal
doc ///
   Key
        blackBoxIdeal
   Headline
        create a {\tt BlackBoxIdeal } 
   Usage   
        blackBoxIdeal(I)
   Inputs  
        I:Ideal
   Outputs
        : BlackBoxIdeal
   Description      
        Text

          Sometimes one wants to use the algorithms implemented
          for black box ideals even though explicit generators
          of the ideal are known. In this case one can create
          a BlackBoxIdeal from an ordinary ideal:
        Example
          K = ZZ/11;
          R = K[x,y,z]
          bb = blackBoxIdeal(ideal(x*y,x*z));
          -- this defines a plane x=0 and a line y=z=0
          bb.isZeroAt(matrix{{0,1,1_K}})          
          bb.isProbablySmoothAt(matrix{{0,1,1_K}})
          bb.isCertainlySingularAt(matrix{{0,0,0_K}})
   Caveat
        does not check if the ideal ring is a quotient ring (not supported?)
///


doc ///
    Key
        "Variety of Complexes"
    Headline
        use a blackBoxIdeal to study a space of complexes
    Description
        Text
            A black box parameter space is used to implement a parameter spaces
            with its universal families in a pointwise fashion.
            A black box ideal is used if in addition the equations
            of some interesting subvariety of such an parameter space
            are known.
            
            Let us build the parameter space of complexes of the
            form 
            
            $K^1 \to K^2 \to K^2$

            We work in charateristic 5.            
        Example    
            K = ZZ/5
        Text
            Our complexes will be represented as points
            in K^6, the first 2 coordinates are the
            entries of the 1 x 2 matrix (A) and the last
            4 coordinates are the entries of the 2 x 2 matrix
            (B).
        Example
            testPoint = matrix{{1,2,3,4,5,6_K}};
            matrixAat = (point) -> matrix{{point_0_0,point_1_0}};
            matrixAat(testPoint)            
            matrixBat = (point) -> matrix{{point_2_0,point_3_0},{point_4_0,point_5_0}}
            matrixBat(testPoint)
        Text
            A and B define a complex if AB=0.
        Example
            ABat = (point) -> matrixAat(point)*matrixBat(point);
            ABat(testPoint)
        Text
            Notice that the test point does not correspond
            to a complex. Notice also that ABat defines
            the equations for the variety of complexes
            implicitly - i.e. we can evaluate the
            equations at every point even though no
            polynomials are given (this is a trivial example.
            explicit equation could be given easily. But in
            more involved circumstances an implicit description
            might be more accessible than a direct one.)
        
            Nevertheless, lets now make a BlackBoxIdeal from this 
            evaluation algorithm. 
        Example
            R = K[a_0,a_1,b_0,b_1,b_2,b_3] 
            bb = blackBoxIdealFromEvaluation(R,ABat);
        Text
            The coordinate ring of the ambient space 
            is given, such that the package nows which
            variables to use later for interpolation.
            
            Lets find some points on the variety of complexes
            (usually this is done by a FiniteFieldExperiment)
        Example
            randomPoints = apply(100,i->random(K^1,K^6));
            interestingPoints = select(randomPoints,point->0==ABat(point)); 
            #interestingPoints
        Text
            We can now try to find components of the 
            variety of complexes by interpolation
            at the found points.
        Example
            bb.interpolateComponentsAt(interestingPoints,2)
        Text
            Indeed we find two components. One where A=0 and
            the other one where det B=0 and A is in the kernel
            of B.
            
            From this calculation we do not know wether
            we found all components. Much more (albeit heuristic)
            information can be obtained with a FiniteFieldExperiment.
            See the documentation there for an extented version 
            of this example.
            
            As a check we can 
            look at the explicit ideal of the variety of
            complexes and do an irreducible decompositon
            of this ideal with the usual algorithms:
        Example
            I = ideal bb
            decompose I
        Text
            We see, that we had indeed found all components. 
            
            The interpolation algorithm excells if one has
            a quick evaluation procedure for the
            equations of an ideal, while the explicit
            equations are long and complicated. 
            
            In the case where explicit equations are 
            used for the interpolation, the interpolation
            is usually slower that the
            use of "decompose". 
///

doc ///
    Key
        "isZeroAt"
    Headline
         Check if a given point lies on the vanishing set defined by a black box ideal.
    Usage   
         bbI.isZeroAt(point)
    Inputs  
        bbI: BlackBoxIdeal
        point: Matrix
             coordinates of a point
    Outputs
        : Boolean
    Description
        Text
          This is a point property.
          Checks if the the point lies on the vanishing set defined by the
          black box ideal. 
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal(x^2-y^3);
          bbI.isZeroAt(matrix{{0,0_QQ}})
          bbI.isZeroAt(matrix{{8,4_QQ}})            
          bbI.isZeroAt(matrix{{8,5_QQ}})
///

doc ///
    Key
        "valuesAt"
    Headline
        evaluate the generators of black box ideal at a given point 
    Usage   
        bbI.valuesAt(point)
    Inputs  
        bbI: BlackBoxIdeal
        point: Matrix
             coordinates of a point
    Outputs
        : Boolean
    Description
        Text
          This is a point property. It evaluates the generators of the
          black box ideal at the point
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal(x^2,y^3);
          bbI.valuesAt(matrix{{2,3_QQ}})
        Text
          valuesAt is a special pointProperty
          in the sense that the package uses them to compute other
          point properties. Therefore
          updating valuesAt triggers updates for 
          isZeroAt, numGenerators, jacobianAt, rankJacobianAt. 
    Caveat
          This works only with black box ideals, since they contain an algorithm
          that can evaluate the generators of the black box ideal. A black box parameter spaces
          might contain only an algorithm that checks whether all generators vanish. 
          This happens for example if one considers the moduli space of singular cubics. 
          One can check whether a
          given cubic is singular without calculating the value of the 
          corresponding discriminant. 
///

doc ///
    Key
        "jacobianAt"
    Headline
        evaluate the jacobian matrix of a black box ideal at a given point 
    Usage   
        bbI.jacobianAt(point)
    Inputs  
        bbI: BlackBoxIdeal
        point: Matrix
             coordinates of a point
    Outputs
        : Boolean
    Description
        Text
          This is a point property. It evaluates the jacobian matrix of the
          black box ideal at the point
        Example
          R = QQ[x,y]
          I = ideal(x^2-y^3);
          bbI = blackBoxIdeal I;
          point = matrix{{8,4_QQ}}
          bbI.isZeroAt(point)
          bbI.jacobianAt(point)
          sub(jacobian I,point)
          bbI.isProbablySmoothAt(point)
        Text
          The cuspidal cubic considered above is singular at the
          origin. Therefore the jacobian matrix vanishes there:
        Example
          origin = matrix{{0,0_QQ}}
          bbI.isZeroAt(origin)
          bbI.jacobianAt(origin)
          bbI.isCertainlySingularAt(origin)
        Text
          jacobianAt is a special pointProperty
          in the sense that the package uses it to compute other
          pointProperties. Therefore an update of 
          jacobianAt triggers an update 
          for 'rankJacobianAt'.  
    Caveat
          This works only with black box ideals, since they contain an algorithm
          that can evaluate the generators of the black box ideal. A black box parameter space
          might contain only an algorithm that checks whether all generators vanish. 
          This happens for example if one considers the moduli space of singular cubics. 
          One can check whether a
          given cubic is singular without calculating the value of the 
          corresponding discriminant. 
///

doc ///
    Key
        "attributes"
        "numVariables"
        "numGenerators"
    Headline
        list the attributes of a black box 
    Usage   
        bbI.attributes()
    Inputs  
        bbI: BlackBoxParameterSpace
    Outputs
        : List       
    Description
        Text
          Every BlackBoxIdeal has some attributes
          provided by the package 
          (no new attributes can be defined by the user).
          The difference between attributes and methods is,
          that an attribute is a constant while a property is 
          a function.
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal (x^2-y^3);
          bbI.attributes()
        Text
          Lets look at these attributes in turn
        Example
          bbI.coefficientRing        
          bbI.ideal
          bbI.jacobian
          bbI.numVariables
          bbI.ring
          class bbI
        Text
          The type can also be "BlackBoxParameterSpace"  
        Text
          Lets now look at a blackBoxIdeal defined by an
          evaluation. The standart example is the determinant
          of a matrix:
        Example
          M = (point) -> (point_{0,1,2}||point_{3,4,5}||point_{6,7,8})
          phonePoint = matrix{{1,2,3,4,5,6,7,8,9_QQ}}
          M(phonePoint)
          detM = (point) -> matrix{{det(M(point))}}
          detM(phonePoint)   
          S = QQ[m_1..m_9]  
          bbE = blackBoxIdealFromEvaluation( S, detM );
          bbE.valuesAt(phonePoint)
          bbE.attributes()
        Text
          Notice that  "jacobian" is missing, since
          no explicit equations of the blackBoxIdeal are provided.
        Example
          bbP = blackBoxParameterSpace(2,QQ);
          bbP.attributes()
        Text
          For a blackPointParameterSpace "ring" is 
          missing since there are no equations (not even implicit ones)  
    SeeAlso
         pointProperties
         memberMethods
///


doc ///
    Key
        "rankJacobianAt"
    Headline
        determine the rank of the jacobian matrix of a black box ideal at a given point 
    Usage   
        bbI.rankJacobianAt(point)
    Inputs  
        bbI: BlackBoxIdeal
        point: Matrix
             coordinates of a point
    Outputs
        : Boolean
    Description
        Text
          This is a point property. It evaluates the jacobian matrix of the
          black box ideal at the point and determines its rank.
        Example
          R = QQ[x,y]
          I = ideal(x^2-y^3);
          bbI = blackBoxIdeal I;
          point = matrix{{8,4_QQ}}
          bbI.isZeroAt(point)
          bbI.jacobianAt(point)
          bbI.rankJacobianAt(point)
          bbI.isProbablySmoothAt(point)
        Text
          The cuspidal cubic considered above is singular at the
          origin. Therefore the jacobian matrix hat rank 0 there:
        Example
          origin = matrix{{0,0_QQ}}
          bbI.isZeroAt(origin)
          bbI.jacobianAt(origin)
          bbI.rankJacobianAt(origin)
          bbI.isCertainlySingularAt(origin)
        Text
          This point property is usefull when running experiments on
          black boxes that have serveral components of unknown 
          codimension. The rank of the jacobian matrix gives a upper bound on 
          the codimension of the components containing the point (with
          equality if the vanishing set is smooth at the point). Sorting 
          the number of points found in an experiment by the rank of their
          jacobian matrices
          helps to estimate the number of components of the vanishing set
          of the black box in each codimension.   
    Caveat
          This works only with black box ideals, since they contain an algorithm
          that can evaluate the generators of the black box ideal. A black box parameter spaces
          might contain only an algorithm that checks whether all generators vanish. 
          This happens for example if one considers the moduli space of singular cubics. 
          One can check whether a
          given cubic is singular without calculating the value of the 
          corresponding discriminant. 
///
