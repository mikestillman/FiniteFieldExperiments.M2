doc ///
    Key
        getEpsRing
        (getEpsRing, Ring, ZZ)
        eps
    Headline
        get a ring for jet calculations
    Usage   
        getEpsRing(R,d)
    Inputs  
        R: Ring
             the coefficient Ring
        d: ZZ 
         the precision of the Ring for jet calculations
    Outputs
        : Ring
             R[eps]/eps^{d+1}
    Description
        Text
           The advantage of using this function is that for each
           precision and coefficient ring R[eps]/eps^{d+1} is
           created only once.
        Example          
          E2 = getEpsRing(QQ,2)
          eps^2+eps^3
          E3 = getEpsRing(QQ,3)
          eps^2+eps^3
        Text
          Be aware that there are now several eps-es. This can
          lead to unexpected behavior:
        Example
          use E2; eps
        Text  
          Notice that this still gives the eps in E3.
          The underscore notation also does not help in this situation:
        Example
          try eps_E2 then "works" else "error"
        Text      
          The following works, but is not recomended since the 
          code does not check whether eps is a variable in a ring:
        Example
          sub(eps,E2)
          eps = 1
          sub(eps,E2)
        Text  
          We recommend the following:
        Example
          E2.eps
          E3.eps
          E2.eps^3
        Text
          This was implemented by hand for these rings. It does not work
          in general. 
///

doc ///
    Key
        singularityTestOptions
    Headline
        show current parameters for singularity test
    Usage   
        bb.singularityTestOptions()
    Inputs  
        bb: BlackBoxIdeal
             an black box ideal
    Outputs
        : HashTable
            with  keys { \tt precision } (the required length of the computed jets)
            and { \tt numTrials } ( the number of required jet computations )
    Description
        Text
            Show currently used parameters for @TO2{isCertainlySingularAt,"point singularity test"}@
            There are two parameters, the required length of the computed jets ( { \tt precision } )
            and  the number of required jet computations ({ \tt numTrials }). If one of the jet computations fails,
            the point is certainly not smooth.

            Let us construct a black box
        Example
            R = QQ[x,y]
            bbI = blackBoxIdeal ideal(x^2-y^3);
        Text
            Now we may look at the default singularity test parameters:
        Example
            bbI.singularityTestOptions()
        Text
            The test parameters may be modified by the user with @TO{setSingularityTestOptions}@
        Example
            jetLength = 3;
            numTrials = 5;
            bbI.setSingularityTestOptions(jetLength, numTrials);
            bbI.singularityTestOptions()
    SeeAlso
        setSingularityTestOptions
        isProbablySmoothAt
        isCertainlySingularAt
///

doc ///
    Key
        isProbablySmoothAt
        isCertainlySingularAt
    Headline
        heuristic test of smoothness
    Usage   
        bb.isProbablySmoothAt(point)
        bb.isCertainlySingularAt(point)
    Inputs  
        bb: BlackBoxIdeal
             an black box ideal
        point: Matrix 
         the coordinates of a point
    Outputs
        : Boolean
    Description
        Text
          Checks for smoothness of a point on the
          vanishing set of the black box ideal, by
          trying to find a @TO2{jetAt,"jet"}@ starting at the point.
          
          If the point is smooth on the vanishing
          set of the black box ideal, arbitray jets
          can always be found. If one or more jets are found
          the point is probably smooth. If the search
          for a jet failes only once, the vanishing
          set is certainly singular at this point.
          
          Consinder for example the cuspidal cubic:
        Example
          R = QQ[x,y]
          bbI = blackBoxIdeal ideal(x^2-y^3);
        Text
          The cuspidal cubic is singular at the origin:
        Example    
          origin = matrix{{0,0_QQ}}
          bbI.isCertainlySingularAt(origin)
        Text
          For the tests the length is taken by the precision of 
          @TO{singularityTestOptions}@ and the required number of successful trials  
          is given by 'numTrials'
        Example    
          bbI.singularityTestOptions()
        Text
          The default singularityTestOptions can be changed with
          @TO{setSingularityTestOptions}@
          Consider a point on the cuspidal cubic different from the origin:
        Example
          otherPoint = matrix{{8,4_QQ}}
        Text
          Check whether the other point lies on the cuspidal cubic:
        Example  
          bbI.isZeroAt(otherPoint)
        Text
          We now check for smoothness:
        Example
          bbI.isCertainlySingularAt(otherPoint)
          bbI.isProbablySmoothAt(otherPoint)
        Text
          If the point is not on the vanishing set defined by
          the black box ideal, an exception PointNotOnBlackBox is thrown
        Example
          pointNotOnCurve = matrix{{4,5_QQ}}
          bbI.isZeroAt(pointNotOnCurve)
          catch bbI.isCertainlySingularAt(pointNotOnCurve)
          wantedJetLength = 1
          catch bbI.jetAt(pointNotOnCurve,wantedJetLength)
    SeeAlso
       setSingularityTestOptions
       jetAt
      
///

-- (jetAt, BlackBoxParameterSpace, Matrix, ZZ)

doc ///
    Key       
        "jetAt"
    Headline
        finds a jet on a variety defined by a black box
    Usage   
        bb.jetAt(point,length)
    Inputs  
        bb: BlackBoxIdeal
             an black box ideal
        point: Matrix 
             the coordinates of a point
        length: ZZ
             the length of the desired jet
    Outputs
        : Jet
    Description
        Text 
          It tries to find a jet starting at a given point on 
          a variety given by a black box.
          
          If the variety defined by the black box is smooth at
          the given point, arbitray jets can be found by the
          implemented algorithm. If the point is singular, the
          algorithm fails after a finite number of steps. If 
          this happens an exception is raised. This is implemented
          using the throw/catch mechanism.
          
          Consinder for example a cuspidal cubic:
        Example
          Fp = ZZ/101
          R = Fp[x,y]
          I = ideal(x^2-y^3)
          bbI = blackBoxIdeal I;
        Text
          Consider a point on the cuspidal cubic different from the origin:
        Example
          point = matrix{{8,4_Fp}}
        Text
          Check whether the point lies on the cuspidal cubic:
        Example  
          bbI.isZeroAt(point)
        Text
          We now look for a jet:
        Example
          j = bbI.jetAt(point,3)
        Text
          The defining equations of the ideal indeed vanish on the jet:
        Example
          sub(I,j)
        Text
          At the origin the cuspidal cubic is singular. Short jets can be found,
          but not long ones.
        Example
          origin = matrix{{0,0_Fp}}
          catch bbI.jetAt(origin,1)  
          catch bbI.jetAt(origin,2)  
          catch bbI.jetAt(origin,3)  
        Text
          Notice that one has to use the catch/throw mechanism
          to obtain readable error messages. 
    SeeAlso       
        isProbablySmoothAt
        isCertainlySingularAt
///
 
--(continueJet,Jet,ZZ)

doc ///
    Key
        continueJet 
      
    Headline
        increases the length of a given jet on a variety defined by a black box
    Usage   
        bb.continueJet(jet,length)
    Inputs       
        bb: BlackBoxIdeal
             an black box ideal
        jet: Jet 
        length: ZZ
             the length of the desired jet
    Outputs
        : Jet
    Description
        Text 
          This takes a jet of a given length and tries
          to continues the algorithm of @TO jetAt@ until
          the jet has the desired length.
          
          If the variety defined by the black box is smooth at
          the given point, arbitray jets can be found by the
          implemented algorithm. If the point is singular, the
          algorithm fails after a finite number of steps. If 
          this happens an exception is raised. This is implemented
          using the throw/catch mechanism.
          
          Consinder for example a cuspidal cubic:
        Example
          Fp = ZZ/101
          R = Fp[x,y]
          I = ideal(x^2-y^3)
          bbI = blackBoxIdeal I;
        Text
          Consider a point on the cuspidal cubic different from the origin:
        Example
          point = matrix{{8,4_Fp}}
        Text
          Check whether the point lies on the cuspidal cubic:
        Example  
          bbI.isZeroAt(point)
        Text
          We now look for a jet:
        Example
          j = bbI.jetAt(point,3)
        Text
          Now we increase the length of the jet
        Example
          bbI.continueJet(j,4)
        Text
          At the origin the cuspidal cubic is singular. Short jets can be found,
          but not long ones.
        Example
          origin = matrix{{0,0_Fp}}
          j = bbI.jetAt(origin,1)  
          catch bbI.continueJet(j,3)
        Text
          Notice that one has to use the catch/throw mechanism
          to obtain readable error messages. 
    SeeAlso
        jetAt
///


doc ///
    Key
        jetStatsAt
    Headline
        counts possible jet lengths at a singular point
    Usage   
        bb.jetStatsAt(point,trials,length)
    Inputs  
        bb: BlackBoxIdeal
             a black box ideal
        point: Matrix 
             the coordinates of a point
        trials: ZZ
             the number of times the jet-finding procedure
             is started
        length: ZZ
             the maximum length used in the search
    Outputs
        : Jet
    Description
        Text 
          "jetAtOrException" can be abbreviated as "jetAt".
          It tries to find a jet starting at a given point on 
          a variety given by a black box.
          
          If the variety defined by the black box is smooth at
          the given point, arbitray jets can be found by the
          implemented algorithm. If the point is singular, the
          algorithm fails after a finite number of steps.
          
          Consinder for example a cuspidal cubic:
        Example
          Fp = ZZ/5
          R = Fp[x,y]
          bbCusp = blackBoxIdeal ideal(x^2-y^3);
        Text
          At the origin the cuspidal cubic is singular. Short jets can be found,
          but not long ones.
        Example
          origin = matrix{{0,0_Fp}}
          catch bbCusp.jetAt(origin,3)  
        Text
          Lets check how often this happens:
        Example
          bbCusp.jetStatsAt(origin,10,5^3)
        Text
          Now lets do the same for a node:
        Example
          bbNode = blackBoxIdeal ideal(x*y);
          bbNode.jetStatsAt(origin,10,5^3)
        Text
          Notice that the statistics is significantly different
          form the cusp example. Possibly some help in 
          classifying implicitly given singularities can be
          obtained from this.  
    SeeAlso
        jetAt
        isProbablySmoothAt
        isCertainlySingularAt
///
 
doc ///
    Key
        (length, Jet)
    Headline
        length of a Jet
    Usage   
        length(jet)
    Inputs  
        jet: Jet
    Outputs
        : ZZ
          the length of the input jet.
    Description
        Text 
          Algebraically a jet of length d is a map 
          
             j : R \to K[e]/e^{d+1} 
          
          where R is a polynomial ring in n variables.
          Geometrically it is a truncated curve germ.
          In particular a jet of length zero is a point
          and a jet of length one is a tangent vector
          to a point.
          
          Consinder for example a point on the cuspidal cubic:
        Example
          Fp = ZZ/5
          R = Fp[x,y]
          bbCusp = blackBoxIdeal ideal(x^2-y^3);
          pointOnCusp = matrix{{1,1_Fp}}
          bbCusp.isZeroAt(pointOnCusp)
        Text
          Here are some jets of different length at this point:
        Example
          j0 = bbCusp.jetAt(pointOnCusp,0)
          j1 = bbCusp.continueJet(j0,1)      
          j2 = bbCusp.continueJet(j1,2)
        Text
          indeed:
        Example
          length j0
          length j1
          length j2
///
 
doc ///
    Key
        "setSingularityTestOptions"
    Headline
        change how singularities are detected
    Usage   
        bbI.setSingularityTestOptions(precision,numTrials)
    Inputs  
        bbI: BlackBoxIdeal
        precision: ZZ
             length of jets used
        numTrials: ZZ
             number of such jets required     
    Outputs
        : Boolean
    Description
        Text
          To test wether a given point P on a variety given by
          a black box ideal is smooth, the package tries to construct
          several jets of certain length. The number of required trials and jetlengths
          is given by @TO{singularityTestOptions}@ and the default is to compute
          2 jets of length 10 starting at P. If the
          variety is smooth at P such jets always exists. If the variety is
          singular at P a generic jet can not be extended to arbitrary length.
          If the required number of jets can not be found the point property 
          isCertainlySingular has the value true. If the required number 
          of jets can be found the point property @TO{isProbablySmoothAt}@ has the
          value true.
        Example
          K = QQ
          R = QQ[x,y,z]      
          I = ideal (x*z,y*z)
          bbI = blackBoxIdeal I;
          pointOnLine = matrix{{0,1,0_K}}
          bbI.isProbablySmoothAt(pointOnLine)
          origin = matrix{{0,0, 0_K}}
          bbI.isProbablySmoothAt(origin)
          bbI.isCertainlySingularAt(origin)
        Text
          Since the singularity of the above curve
          at the origin is of small degree
          it is detected by looking at jets of length 10.
          If we change the test options to look only at jets of length 4,
          the singularity can not be detected.
        Example
          bbI.setSingularityTestOptions(4,1)
          bbI.singularityTestOptions()
          bbI.isCertainlySingularAt(origin)
        Text
          The construction of jets is time intensive. For many applications
          precision=2 and numTrials=1 is sufficient, even if this only detects
          the most simple singularities.  
    Caveat
          This works only with black box ideals, since they contain an algorithm
          that can calculate the derivative of the equations at a given point
          (jacobianAt). This is needed to construct jets iteratively.
    SeeAlso
          singularityTestOptions
          isCertainlySingularAt
          isProbablySmoothAt
          jetAt
///

doc ///
    Key
        "Jet"
    Headline
        a type for handling jets
    Description
       Text
          Algebraically a jet is a map 
          
             j : R \to K[e]/e^{d+1} 
          
          where R is a polynomial ring in n variables.
          Geometrically it is a truncated curve germ.
          
          Here a jet is modeled as a row matrix with n
          entries in an eps-Ring of precision d (see @TO getEpsRing @).
          
          As an example consider the cuspidal cubic curve
          in the plane:                    
       Example
          K = ZZ/7
          R = K[x,y]      
          I = ideal (x^2-y^3)
          bbI = blackBoxIdeal I;
       Text
          the following point lies on the cuspidal cubic
       Example
          smoothPoint = matrix{{1,1_K}}
          bbI.isZeroAt(smoothPoint)
       Text
          Lets now make a jet of lenght 2 lying on cuspidal
          cubic and starting in the above point.
       Example   
          j = bbI.jetAt(smoothPoint,2)
       Text
          The jet does indeed lie on the cuspidal cubic
       Example
          sub(I,j)
       Text
          A jet remembers from which point it started
          from:
       Example
          j#"point"    
       Text
          Also it knows its length:
       Example
          j#"jetLength"
          length j
       Text
          Finally it remembers from which BlackBoxIdeal it
          was created
       Example
          class j#"parent"
       Text
          The length of a jet can be increased:
       Example
          j3 = bbI.continueJet(j,3)    
       Text
          Jets can only be reliably created in smooth
          points of a variety (by a variant of Newtons method).
          If the starting point of the jet is not smooth
          on the variety defined by the BlackBoxIdeal, then
          after a finite number of steps the algorithm can 
          not continue. If this happens
          an exception is raised. 
          
          For readable error messages
          and more precise information one has to use 
          the throw/catch mechanism:
       Example
          singularPoint = matrix{{0,0_K}};
          catch bbI.jetAt(singularPoint,1)          
          catch bbI.jetAt(singularPoint,2)
       Text                 
          Notice that the search for fails at length 2 most of the time,
          since the singularity has multiplicity 2. If one tries
          long enough, a longer jet can be found (lying on one
          of the branches at the origin):
       Example        
          bbI.jetStatsAt(singularPoint,3,200) 
    Caveat
    SeeAlso
///

doc ///
    Key
        "JetSet"
    Headline
        a type for handling a set of jets starting at the same point
    Description
       Text
          A JetSet is a set of jets that start at the same point.
          This is important since jets starting at the
          same (smooth) point of a variety X must 
          lie on the same component of X.
       
          For example consider the cuspidal cubic:
       Example
          K = ZZ/101
          R = K[x,y]
          bbCusp = blackBoxIdeal ideal(x^2-y^3);
       Text
          Lets make a jet at a smooth point:
       Example
          smoothPoint = matrix{{1,1_K}}
          j = bbCusp.jetAt(smoothPoint,3)
       Text
          We now make a JetSet containing one Element:
       Example
          js = new JetSet from j
       Text
          We add another jet to this collection:
       Example
          size js
          addElement(js,bbCusp.jetAt(smoothPoint,3)) 
          size js
       Text
          Lets now consider a differnent point on 
          the cuspidal cubic:
       Example
          otherSmoothPoint = matrix{{8,4_K}}
          otherJet = bbCusp.jetAt(otherSmoothPoint,3)
       Text
          This jet can not be added to our JetSet, because
          the jet starts at a different point.
       Example
          1; --addElement(js,otherJet)
    Caveat
    SeeAlso
///

doc ///
    Key
        (size, JetSet)
    Headline
        the number of Jets in a JetSet
    Usage   
        size(jetSet)
    Inputs  
        jetSet: JetSet
    Outputs
        : ZZ
          the number of jets in the jetSet
    Description
        Text 
          A JetSet is a set of jets starting at the same point.
          size returns the number of jets in such a JetSet
                  
          Consinder for example a smooth point on the cuspidal cubic:
        Example
          Fp = ZZ/5
          R = Fp[x,y]
          bbCusp = blackBoxIdeal ideal(x^2-y^3);
          smoothPoint = matrix{{1,1_Fp}}
          bbCusp.isZeroAt(smoothPoint)
        Text
          We collect some jets starting at this point
          in a JetSet:
        Example
          js = new JetSet from bbCusp.jetAt(smoothPoint,2)
          size js
          addElement(js, bbCusp.jetAt(smoothPoint,2))
          size js
 ///
 


doc ///
    Key
        addElement
        (addElement, JetSet, Jet)        
    Headline
        adds a Jet to a JetSet
    Usage   
        addElement(jetSet,jet)
    Inputs  
        jetSet: JetSet
        jet: Jet 
    Outputs
        : JetSet
    Description
       Text
          Adds a jet to a JetSet if both lie on 
          the variety defined by the same BlackBoxIdeal 
          and both start at the same point.
          
          For example consider the cuspidal cubic:
       Example
          K = ZZ/101
          R = K[x,y]
          bbCusp = blackBoxIdeal ideal(x^2-y^3);
       Text
          Lets make a jet at a smooth point:
       Example
          smoothPoint = matrix{{1,1_K}}
          j = bbCusp.jetAt(smoothPoint,3)
       Text
          We now make a JetSet containing one Element:
       Example
          js = new JetSet from j
       Text
          We add another jet to this collection:
       Example
          size js
          addElement(js,bbCusp.jetAt(smoothPoint,3)) 
          size js
    Caveat
    SeeAlso
///

