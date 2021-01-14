doc ///
    Key
        "InterpolatedComponent"
    Headline
        a type for handling partial information about irreducible components
    Description
       Text
          Let $X = X_1 \cup \dots \cup X_k$ be the
          decomposition of a variety in its irreducible
          components.
                    
          An @TO InterpolatedComponent @ is a type for
          collecting partial information about 
          pairs $(X_i,P)$ where $X_i$ is
          an irreducible component of $X$ and $P$ is a smooth
          point on $X_i$ as well as on $X$.
          
          This partial information is usually obtained
          by interpolating the equations of $X_i$ up to 
          a certain maximal degree.
          
          As an example consider the union of a line and
          a plane conic in IP^3:      
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y);
          conic = ideal (w,x^2+y^2-z^2);
          bbI = blackBoxIdeal intersect(line,conic);
       Text 
          To get reproducible results we set the random seed:
       Example
          setRandomSeed(42); 
       Text
          Consider the following two points:
       Example
          pointOnLine = matrix{{0,0,1,2_K}}
          pointOnConic = matrix{{3,4,5,0_K}}
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(pointOnConic)
       Text
          Lets now recover the linear equations of
          the first component via interpolation:
       Example   
          bbI.interpolateComponentAt(pointOnLine,1)
       Text
          We see, that the InterpolatedComponent contains
          a name ("c1"), the equations up to a maximal degree,
          the point from which the interpolation started,
          an a set of jets starting at that point. (The interpolation
          algorithm works as follows: find a long enough jet
          starting at the given point and then find all polynomials
          of degree at most maxDegree vanishing on this jet.)
      
          Also the @TO InterpolatedComponent @ knows which 
          @TO BlackBoxIdeal @
          it belongs to (this information is not printed).
 
          The @TO InterpolatedComponent @ is also stored inside the
          corresponding @TO BlackBoxIdeal @:
       Example
          bbI.interpolatedComponents()
       Text   
          The @TO InterpolatedComponent @ can be recovered 
          from its name:
       Example
          bbI.interpolatedComponentByName("c1")   
       Text                    
          The name of an @TO InterpolatedComponent @ can be
          changed:
       Example
          bbI.renameInterpolatedComponent("c1","line")
       Text
          Sometimes one wants to extract only the interpolated 
          equations of the interpolated ideal:
       Example
          ideal bbI.interpolatedComponentByName("line")
       Text
          Lets now recover the linear equations of the
          second component:
       Example
          bbI.interpolateComponentAt(pointOnConic,1)
          bbI.interpolatedComponents()
          bbI.renameInterpolatedComponent("c2","conic")
          bbI.interpolatedComponents()
       Text
          Notice that the information about the conic component
          does not jet seem to be complete, since the quadratic
          equation has not been explicitly computed. Nevertheless
          this partial information is enough to decide
          for a given point on which component it lies.
       Example
          bbI.interpolatedComponentNamesAt(pointOnLine)
          bbI.interpolatedComponentNamesAt(pointOnConic)   
       Text
          This even works the point that lies on the
          intersection of the line with the plane spanned
          by the plane conic:
       Example
          pointOnLineAndPlane = matrix{{0,0,1,0_K}}
          bbI.isZeroAt(pointOnLineAndPlane)
          bbI.interpolatedComponentNamesAt(pointOnLineAndPlane)          
       Text
          This is done as follows: a short jet on the 
          variety defined by the BlackBoxIdeal is calculated
          at the given point. Then is is checked wether the
          interpolated equations vanish on this jet. 
          
          In the
          above example the short jet lies on the line, but
          leaves the plane spanned by the conic:
       Example
          j := bbI.jetAt(pointOnLineAndPlane,2)
          0==sub(ideal w,j)
       Text
          This is a infinitesimal version of the 
          "whitness point"-concept in numerical algebraic geometry.
          
          The length of the jet used in detecting on which
          component a point lies can be changed:
       Example
          bbI.onComponentPrecision()
          bbI.setOnComponentPrecision(0)
          bbI.onComponentPrecision()
       Text
          Notice that with precision zero no jet is calculated,
          and only the point itself is used.
          In this case the algorithm can not jet classify
          the point on the line and plane correctly:
       Example
          bbI.interpolatedComponentNamesAt(pointOnLineAndPlane)
       Text
          Even though we did not need the conic equation
          for the classification of points, lets still
          recover it for the sake of completeness
       Example
          bbI.interpolateComponentAt(pointOnConic,2)  
       Text
          Now also the precision zero method can distinguish
          the components:
       Example
          bbI.onComponentPrecision()   
          bbI.interpolatedComponentNamesAt(pointOnLineAndPlane) 
       Text
          One can also interpolate all components for a list of
          points:
       Example
          bbI.resetInterpolation()           
          bbI.interpolatedComponents()
          bbI.interpolateComponentsAt({pointOnLine,pointOnConic,pointOnLineAndPlane},1)
       Text
          Here the interpolation was done only for the first 2
          points, because the third one was already on one of
          the previously calculated components. 
          
          If one is only interested in the names of the components
          one can use:
       Example
          bbI.interpolatedComponentNames()
    Caveat
    SeeAlso
///

doc ///
    Key
        interpolateComponentAt
    Headline
        interpolate equations for a component of a variety defined by a BlackBoxIdeal
    Usage   
        bb.interpolateComponentAt(P,d)
    Inputs  
        bb: BlackBoxIdeal 
        P: Matrix
           the coordinates of a point in the parameter space.        
        d: ZZ
           the maximal degree for the interpolation 
    Outputs
        : InterpolatedComponent         
    Description
       Text
          Let $X$ be the variety defined by a BlackBoxIdeal,
          and let $X' \subset X$ be the component of $X$
          on which a given point $P$ lies. This function
          tries to find low degree equations of $X'$. This
          is done by finding a long enough jet starting at $P$
          and calculating all polynomials of degree at most
          $d$ vanishing on this jet. 
          
          This algorithm works only if $X$ and $X'$ are smooth
          in $P$. If this is not the case an Exception is raised.
          
          As an example consider the union of a line and
          a plane conic in IP^3:      
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y);
          conic = ideal (w,x^2+y^2-x*z);
          bbI = blackBoxIdeal intersect(line,conic);
       Text
          Consider the following two points:
       Example
          pointOnLine = matrix{{0,0,1,2_K}}
          pointOnConic = matrix{{1,1,2,0_K}}
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(pointOnConic)
       Text
          Lets now recover the linear equations of
          the first component via interpolation:
       Example   
          bbI.interpolateComponentAt(pointOnLine,1)
       Text
          This component is stored in the BlackBoxIdeal.
       Example
          bbI.interpolatedComponents()
       Text
          Lets also recover the other component
       Example
          bbI.interpolateComponentAt(pointOnConic,1)
       Text
          We see that not all equations of the second
          component were found (because of the degree bound).
          Sometimes this partial information is already
          useful, see @TO InterpolatedComponent @.
          
          If we interpolate again at the same point, 
          the old InterpolatedComponent is replaced by
          a new, more precise one (i.e. the same name is used):
       Example
          bbI.interpolateComponentAt(pointOnConic,2)
       Text
          If we start with the point on the intersection
          of line and conic, the algorithm can not work:
       Example
          intersectionPoint = matrix{{0,0,1,0_K}}
          bbI.isZeroAt(intersectionPoint)
          bbI.isCertainlySingularAt(intersectionPoint)
          catch bbI.interpolateComponentAt(intersectionPoint,1)
       Text
          Notice that we have to use "catch" to obtain 
          readable error messages.
    Caveat
    SeeAlso
///

doc ///
    Key
        interpolateComponentsAt
    Headline
        interpolate equations for several components of a variety defined by a BlackBoxIdeal
    Usage   
        bb.interpolateComponentsAt(L,d)
    Inputs 
        bb: BlackBoxIdeal 
        L: List
           of points in the parameter space.        
        d: ZZ
           the maximal degree for the interpolation 
    Outputs
        : List         
    Description
       Text
          This does the same as @TO interpolateComponentAt@
          the only difference is that a list of points
          is given (instead of just one point).
          
          For each of the point it is checked wether it
          lies on a known component. If not a new interpolation
          is started at the point. 
          
          If the interpolation fails because the variety
          defined by the BlackBoxIdeal is not smooth there,
          the point is discarded (no Exception is raised).
 
          Consider for example the union of a plane conic
          an a line in IP^3:
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y);
          conic = ideal (w,x^2+y^2-x*z);
          bbI = blackBoxIdeal intersect(line,conic);
       Text
          We pick some points:
       Example
          pointOnLine = matrix{{0,0,1,2_K}}
          anotherPointOnLine = matrix{{0,0,2,3_K}}
          pointOnConic = matrix{{1,1,2,0_K}}
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(anotherPointOnLine)
          bbI.isZeroAt(pointOnConic)
       Text
          Lets now recover the linear equations of the components
          via interpolation:
       Example   
          bbI.interpolateComponentsAt({pointOnLine,anotherPointOnLine,pointOnConic},1)
       Text
          Notice that no interpolation was done for the second
          point on the line. Lets now look the intersection point of
          conic and line:
       Example
          intersectionPoint = matrix{{0,0,1,0_K}}
          bbI.isZeroAt(intersectionPoint)
          bbI.isCertainlySingularAt(intersectionPoint)
       Text
          We reset the interpolation and add the
          intersection point to our list of points:
       Example
          bbI.resetInterpolation()
          bbI.interpolatedComponents()
          catch bbI.interpolateComponentsAt({intersectionPoint,pointOnLine,pointOnConic},1)
       Text
          Notice that no exception was raised.
    Caveat
    SeeAlso
///

doc ///
    Key
        interpolatedComponentByName
    Headline
        select a component of a variety defined by a BlackBoxIdeal
    Usage   
        bb.interpolatedComponentByName(name)
    Inputs 
        bb: BlackBoxIdeal 
        name: String
           the name of an interpolated component
    Outputs
        : InterpolatedComponent         
    Description
       Text
          Every interpolated component of a variety
          defined by a BlackBoxIdeal is automatically
          given a name "cxxx" with "xxx" a number.
          
          This name can be used to retrieve the 
          interpolated component from the BlackBoxIdeal.
          
          Consider for example the union of a plane conic
          an a line in IP^3:
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y);
          conic = ideal (w,x^2+y^2-x*z);
          bbI = blackBoxIdeal intersect(line,conic);
       Text
          We pick some points:
       Example
          pointOnLine = matrix{{0,0,1,2_K}}
          pointOnConic = matrix{{1,1,2,0_K}}
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(pointOnConic)
       Text
          Lets now recover the linear equations of the components
          via interpolation:
       Example   
          bbI.interpolateComponentsAt({pointOnLine,pointOnConic},1)
       Text
          We can not select the components using their name:
       Example
          bbI.interpolatedComponentByName("c1")
       Text
          The generic names can be changed to more 
          meaningful ones:
       Example
          bbI.renameInterpolatedComponent("c1","line")
          bbI.interpolatedComponentByName("line")
    Caveat
    SeeAlso
///

doc ///
    Key
        interpolatedComponentNames
    Headline
        lists the names of known components
    Usage   
        bb.interpolatedComponentNames()
    Inputs 
        bb: BlackBoxIdeal 
    Outputs
        : List
           of component names.      
    Description
       Text
          Every interpolated component of a variety
          defined by a BlackBoxIdeal is automatically
          given a name "cxxx" with "xxx" a number.
          
          A list of all current names can be obtained
          with the above function.         
          
          Consider for example the union of a plane conic
          an a line in IP^3:
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y);
          conic = ideal (w,x^2+y^2-x*z);
          bbI = blackBoxIdeal intersect(line,conic);
       Text
          We pick some points:
       Example
          pointOnLine = matrix{{0,0,1,2_K}}
          pointOnConic = matrix{{1,1,2,0_K}}
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(pointOnConic)
       Text
          Lets now recover the linear equations of the components
          via interpolation:
       Example   
          bbI.interpolateComponentsAt({pointOnLine,pointOnConic},1)
       Text
          We can now look at the list of names:
       Example
          bbI.interpolatedComponentNames()
       Text
          The generic names can be changed to more 
          meaningful ones:
       Example
          bbI.renameInterpolatedComponent("c2","conic")
          bbI.interpolatedComponentNames()
    Caveat
    SeeAlso
///

doc ///
    Key
        interpolatedComponents
    Headline
        lists the known components of a variety defined by a BlackBoxIdeal
    Usage   
        bb.interpolatedComponents()
    Inputs 
        bb: BlackBoxIdeal 
    Outputs
        : List
           InterpolatedComponents.      
    Description
       Text
          This returns a list of all InterpolatedComponents
          of a BlackBoxIdeal that have so far been
          calculated.
          
          If this is too much information try @TO interpolatedComponentNames @.
          
          Consider for example the union of a plane conic
          an a line in IP^3:
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y);
          conic = ideal (w,x^2+y^2-x*z);
          bbI = blackBoxIdeal intersect(line,conic);
       Text
          We pick some points:
       Example
          pointOnLine = matrix{{0,0,1,2_K}}
          pointOnConic = matrix{{1,1,2,0_K}}
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(pointOnConic)
       Text
          Lets now recover the linear equations of the components
          via interpolation:
       Example   
          bbI.resetInterpolation()
          bbI.interpolateComponentAt(pointOnLine,1)
          bbI.interpolateComponentAt(pointOnConic,1)
          bbI.interpolatedComponents()
          bbI.interpolatedComponentNames()
    Caveat
    SeeAlso
///

doc ///
    Key
        interpolatedComponentNamesAt
    Headline
        lists the names of possible components containing a given point
    Usage   
        bb.interpolatedComponentNamesAt(point)
    Inputs 
        bb: BlackBoxIdeal 
        point: Matrix
           coordinates of a point in the parameter space of the BlackBoxIdeal
    Outputs
        : List
           of interpolated component names
    Description
       Text
          This is a point property, i.e it can be used
          in an FiniteFieldExperiment.
          
          This point property returns a list of the names 
          of all InterpolatedComponents
          of a BlackBoxIdeal that that seem to contain the
          given point.
          
          If an interpolated component appears on this list it is probable,
          but not certain that it contains the point. 
                    
          Interpolated components that do not appear in this
          list are garanteed not to contain the point. 
          
          Notice that the test wether some point is on 
          a component uses not only the interpolated equations,
          but also the associated BlackBoxIdeal. It is done
          by first finding a short jet starting at the 
          point using the BlackBoxIdeal and then checking 
          wether the interpolated
          equations vanish on the jet.
          
          This means that it is usuall enough to find ONE equation via interpolation that
          contains only the given component but no others. 
          (The hypersurface defined by this equation will
          intersect other components, but by looking at 
          jets this does not matter)
      
          See below for some examples illustrating these
          points.
          
          Since jets are used in this algorithm, it is
          garanteed to work only if the given point is
          smooth on the variety defined by the BlackBoxIdeal.
          
          As an example consider three curves in IP^3:
          a conic and a plane cubic in the same plane and
          a line intersecting the plane outside of
          the two other curves:
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y);
          conic = ideal (w,x^2+y^2-z^2);
          cubic = ideal (w,x^3+y^3+z^3);
          bbI = blackBoxIdeal intersect(line,conic,cubic);
       Text
          We pick a point on each component and
          also the intersection point of the line
          and the plane spanned by the conic and the cubic. 
          (Usually such
          points are found using a FiniteFieldExperiment):
       Example
          pointOnLine = matrix{{0,0,1,2_K}}
          pointOnConic = matrix{{3,4,5,0_K}}
          pointOnCubic = matrix{{1,-1,0,0_K}}
          pointOnLineAndPlane = matrix{{0,0,1,0_K}}
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(pointOnConic)
          bbI.isZeroAt(pointOnCubic)
          bbI.isZeroAt(pointOnLineAndPlane)
          points = {pointOnLine,pointOnConic,pointOnCubic,pointOnLineAndPlane};
       Text
          Lets now recover the linear equations of the components
          via interpolation:
       Example   
          bbI.interpolateComponentsAt(points,1)
       Text
          Notice that we have only two interpolated components,
          since with linear equations the two plane curves
          can not be distinguished.
          
          Lets see how our points are attributed to the
          different components:
       Example
          apply(points,bbI.interpolatedComponentNamesAt)
       Text
          As expected the first point lies on the first
          component and the next two on the second one.
          
          Notice that this type of problem can be
          detected heuristically using an FiniteFieldExperiment.
          Such an experiment would find some number of 
          points on "c1" and twice as many point on "c2".
          This would automatically by interpreted as 
          indication that "c2" is probably comprised of two
          components, while "c1" is probably irreducible.
          
          Interestingly the point on the intersection
          between line and plane is classified correctly
          as lying on the line, even though the
          interpolated equations of both components
          vanish there:
       Example
          Ic1 = ideal bbI.interpolatedComponentByName("c1")
          Ic2 = ideal bbI.interpolatedComponentByName("c2")
          sub(Ic1,pointOnLineAndPlane)
          sub(Ic2,pointOnLineAndPlane)
       Text
          The classification is done internally by looking
          at jets. For illustration of the idea we do this
          explicitly once:
       Example
          j = bbI.jetAt(pointOnLineAndPlane,2)
          sub(Ic1,j)
          sub(Ic2,j)
       Text
          We see that the interpolated equation of the
          second component does NOT vanish on the jet. 
          Geometrically this is plausible: 
          
          The jet 
          will lie on the variety defined by the BlackBoxIdeal.
          Since the starting point is smooth and lies
          on the line, the jet will also lie on the line.
          One can visualise this as a small curve segment
          starting at the point and lying on the line. 
          The plane will intersect the line in the 
          point, but the jet will stick out of the plane.
          Therefore the equation of the plane will not
          vanish on the jet.
          
          With this trick one can correctly classify points
          with high probability as soon as one has ONE equation
          for each component that contains no other components
          (but may intersect other components). This
          can save a lot of time.
          
          Lets now interpolate the degree 2 equations
          at each point:
       Example
          bbI.interpolateComponentsAt(points,2)
       Text
          We see that for the point on the cubic
          a new component was created. This component
          still has only a linear interpolated equation,
          while our conic component now has recovered
          all its generators.
          
          Lets see how the points are now classified:
       Example
          apply(points,bbI.interpolatedComponentNamesAt)
       Text
          We see that the points on the line and the
          cubic are correctly labeled, while the
          algorithm can not decide wether the
          second point lies on the conic or the cubic.
          
          This becomes clear when one thinks of the
          algorithm: We choose a jet at the second point. This
          jet will lie on the plane conic. Since the
          plane spanned by the cubic is the same as the plane
          spanned by the conic, this jet will not stick
          out of this plane. So from the information available
          this jet could also lie on the component "c3". 
        
          If we now interpolate the degree 3 equations, we get
          a complete unambiguous classification:
       Example
          bbI.refineInterpolation()
          apply(points,bbI.interpolatedComponentNamesAt)
    Caveat
    SeeAlso
       interpolatedComponentsAt
///


doc ///
    Key
        interpolatedComponentsAt
    Headline
        lists possible components containing a given point
    Usage   
        bb.interpolatedComponentsAt(point)
    Inputs 
        bb: BlackBoxIdeal 
        point: Matrix
           coordinates of a point in the parameter space of the BlackBoxIdeal
    Outputs
        : List
           of InterpolatedComponents
    Description
       Text
          This is a point property, i.e it can be used
          in an FiniteFieldExperiment. Usually it
          provides to much information for such an
          experiment, and @TO interpolatedComponentNamesAt @
          is used instead.
           
          This point property returns a list of 
          all InterpolatedComponents
          of a BlackBoxIdeal that that seem to contain the
          given point.
          
          If an interpolated component appears on this list it is probable,
          but not certain that it contains the point. 
                    
          Interpolated components that do not appear in this
          list are garanteed not to contain the point. 
          
          For an explanation and examples of 
          the subtleties of this
          classication process see 
          @TO interpolatedComponentNamesAt @.
                 
          As a trivial example consider the union
          of a line and a plane conic in IP^3:
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y);
          conic = ideal (w,x^2+y^2-z^2);
          bbI = blackBoxIdeal intersect(line,conic);
       Text
          We pick a point on each component (usually such
          points are found using a FiniteFieldExperiment):
       Example
          pointOnLine = matrix{{0,0,1,2_K}}
          pointOnConic = matrix{{3,4,5,0_K}}
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(pointOnConic)
       Text
          Lets now recover the linear equations of the components
          via interpolation:
       Example   
          bbI.interpolateComponentsAt({pointOnLine,pointOnConic},1)
       Text
          Now look at some other points on the variety
          defined by the BlackBoxIdeal
       Example
          anotherPoint = matrix{{0,0,3,4_K}}
          jetAnotherPoint = matrix{{5,12,13,0_K}}
       Text
          Lets check on which component these points lie:
       Example
          bbI.interpolatedComponentsAt(anotherPoint)
          bbI.interpolatedComponentsAt(jetAnotherPoint)       
    Caveat
    SeeAlso
       interpolatedComponentNamesAt
///

doc ///
    Key
        isOnInterpolatedComponent
    Headline
        checks if a point is probably on a given component
    Usage   
        bb.isOnInterplatedComponent(name,point)        
        bb.isOnInterplatedComponent(interpolatedComponent,point)        
    Inputs 
        bb: BlackBoxIdeal 
        name: String
           of an interpolated component of bb
        interpolatedComponent: InterpolatedComponent
        point: Matrix
           coordinates of a point in the parameter space of the BlackBoxIdeal
    Outputs
        : Boolean
    Description
       Text
          If this function returns "true" it is probable,
          but not certain that it contains the point. If it
          returns "false" it is certain that the point is
          either not on the component or that the point is singular
          on the variety defined by the BlackBoxIdeal. The second
          case can also occur if not Exception is raised. 
          
          Notice that the test wether some point is on 
          a component uses not only the interpolated equations,
          but also the associated BlackBoxIdeal. It is done
          by first finding a short jet starting at the 
          point using the BlackBoxIdeal and then checking 
          wether the interpolated
          equations vanish on the jet (the length of the jet
          used is determined by @TO onComponentPrecision @).
          
          This means that it is usuall enough to find ONE equation via interpolation that
          contains only the given component but no others. 
          (The hypersurface defined by this equation will
          intersect other components, but by looking at 
          jets this does not matter)
      
          See @TO interpolatedComponentNamesAt @ 
          for some examples illustrating these
          points.
          
          Since jets are used in this algorithm, it is
          guaranteed to work only if the given point is
          smooth on the variety defined by the BlackBoxIdeal.
          If the algorithm can not finish, an exception
          is raised.
          
          As a trivial example consider a line and
          a plane cuspidal cubic in IP^3 that do not intersect
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y-z);
          cubic = ideal (w,x^3-z*y^2);
          bbI = blackBoxIdeal intersect(line,cubic);
       Text
          We pick a point on each component and
          also the intersection point of the line
          and the plane spanned by the cubic. 
          (Usually such
          points are found using a FiniteFieldExperiment):
       Example
          pointOnLine = matrix{{0,1,1,2_K}};
          pointOnCubic = matrix{{1,1,1,0_K}};
          pointOnLineAndPlane = matrix{{0,1,1,0_K}};
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(pointOnCubic)
          bbI.isZeroAt(pointOnLineAndPlane)
       Text
          Lets now recover the linear equation of cuspidal cubic
          via interpolation:
       Example
          bbI.interpolateComponentAt(pointOnCubic,1)
       Text
          We can now ask, which of our points lie on
          this component:
       Example
          bbI.isOnInterpolatedComponent("c1",pointOnCubic)
          bbI.isOnInterpolatedComponent("c1",pointOnLine)
          bbI.isOnInterpolatedComponent("c1",pointOnLineAndPlane)
       Text
          The last result is somewhat surprising, since all
          interpolated equations of "c1" do vanish on
          the point:
       Example
          Ic1 = ideal bbI.interpolatedComponentByName("c1")
          sub(Ic1,pointOnLineAndPlane)
       Text
          The reason for this unexpected (but correct) answer
          is, that the test is done with jets rather than
          points.
          For illustration of the idea we do this
          explicitly once:
       Example
          j = bbI.jetAt(pointOnLineAndPlane,2)
          sub(Ic1,j)
       Text
          We see that the interpolated equation of the
          second component does NOT vanish on the jet. 
          Geometrically this is plausible: 
          
          The jet 
          will lie on the variety defined by the BlackBoxIdeal.
          Since the starting point is smooth and lies
          on the line, the jet will also lie on the line.
          One can visualise this as a small curve segment
          starting at the point and lying on the line. 
          The plane will intersect the line in the 
          point, but the jet will stick out of the plane.
          Therefore the equation of the plane will not
          vanish on the jet.
          
          With this trick one can correctly classify points
          with high probability as soon as one has ONE equation
          for each component that contains no other components
          (but may intersect other components). This
          can save a lot of time.

          The length of the jet used in this algorithm
          can be changed:
       Example
          bbI.onComponentPrecision()
          bbI.setOnComponentPrecision(0)
          bbI.onComponentPrecision()
       Text
          Here precision 0 means that we only use points,
          and precision 1 means that we look also at
          tangent vectors. 
          
          With precision 0 the above trick does not work,
          and we get a different answer for our point 
          on the intersection of line and plane
       Example
          bbI.isOnInterpolatedComponent("c1",pointOnLineAndPlane)
       Text
          Remember that "true" only means "probably on the component"
          while "false" means "not on the component or not smooth".
          
          If the point we are interested in is not smooth,
          the algorithm is not guaranteed to work.
       Example
          singularPoint = matrix{{0,0,1,0_K}};   
          bbI.isCertainlySingularAt(singularPoint)
          bbI.setOnComponentPrecision(2)
          catch bbI.isOnInterpolatedComponent("c1",singularPoint)
       Text
          !!! At the moment of this writing no Exception was raised !!!
 
          If we return to a smaller precision the algorithm 
          works again, because even in singular points very small
          jets can be found:
       Example
          bbI.setOnComponentPrecision(1)
          bbI.isOnInterpolatedComponent("c1",singularPoint)
       Text
          This should not be seen as feature, but as a
          weakness of the algorithm. If for example a point
          is on the intersection of 2 components a
          length 1 jet can always be found, but in mayority 
          of cases the algorithm will say that the point
          is on neither of the two components. Geometrically
          this makes sense: 
          
          Let for example P be a point on the 
          transversal intersection of two smooth curves. Then
          the tangent space of the union of the two cures 
          is the span of the two tangent spaces of each
          of the individual curves. The algorithm will
          pic a random tangent vector inside this span,
          but this will usually not be tangent to 
          either of the individual curves.
    Caveat
    SeeAlso
       interpolatedComponentsAt
       interpolatedComponentNamesAt
       onComponentPrecision
       setOnComponentPrecision
///


doc ///
    Key
        onComponentPrecision
    Headline
        the length of the jets used for determining component membership
    Usage   
        bb.onComponentPrecision()        
    Inputs 
        bb: BlackBoxIdeal 
    Outputs
        : ZZ
           the length of jets used in @TO isOnInterpolatedComponent @
    Description
       Text
          Displays the length of jets currently used
          in @TO isOnInterpolatedComponent @.
          
          For a discussion of how this number influcences
          the behavior of @TO isOnInterpolatedComponent @,
          see the end of the documentation of  
          @TO isOnInterpolatedComponent @.
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          bbI = blackBoxIdeal ideal(x,y);
          bbI.onComponentPrecision()
          bbI.setOnComponentPrecision(3)
          bbI.onComponentPrecision()
    Caveat
    SeeAlso
       interpolatedComponentsAt
       interpolatedComponentNamesAt
       isOnInterpolatedComponent
       setOnComponentPrecision
///

doc ///
    Key
        setOnComponentPrecision
    Headline
        changes the length of the jets used for determining component membership
    Usage   
        bb.setOnComponentPrecision()        
    Inputs 
        bb: BlackBoxIdeal 
    Description
       Text
          Changes the length of jets used
          in @TO isOnInterpolatedComponent @.
          
          For a discussion of how this number influcences
          the behavior of @TO isOnInterpolatedComponent @,
          see the end of the documentation of  
          @TO isOnInterpolatedComponent @.
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          bbI = blackBoxIdeal ideal(x,y);
          bbI.onComponentPrecision()
          bbI.setOnComponentPrecision(3)
          bbI.onComponentPrecision()
    Caveat
    SeeAlso
       interpolatedComponentsAt
       interpolatedComponentNamesAt
       isOnInterpolatedComponent
       onComponentPrecision
///

doc ///
    Key
        renameInterpolatedComponent
    Headline
        changes the name of an interpolated component
    Usage   
        bb.renameInterpolatedComponent(oldName,newName)        
    Inputs 
        bb: BlackBoxIdeal 
        oldName: String
           the current name of an interpolated component
        newName: String
           the new name of this interpolated component
    Description
       Text
          Every interpolated component of a variety
          defined by a BlackBoxIdeal is automatically
          given a name "cxxx" with "xxx" a number.
          
          Sometimes it is useful to change these
          generic names to something more readable         
          
          Consider for example the union of a plane conic
          an a line in IP^3:
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y);
          conic = ideal (w,x^2+y^2-x*z);
          bbI = blackBoxIdeal intersect(line,conic);
       Text
          We pick some points:
       Example
          pointOnLine = matrix{{0,0,1,2_K}}
          pointOnConic = matrix{{1,1,2,0_K}}
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(pointOnConic)
       Text
          Lets now recover the linear equations of the components
          via interpolation:
       Example   
          bbI.interpolateComponentsAt({pointOnLine,pointOnConic},2)
       Text
          We can now look at the list of names and change one of them
       Example
          bbI.interpolatedComponentNames()
          bbI.renameInterpolatedComponent("c2","conic")
          bbI.interpolatedComponentNames()
          bbI.interpolatedComponentByName("conic")
    SeeAlso
      interpolatedComponentNames
///

doc ///
    Key
        resetInterpolation
    Headline
        erases all interpolated components
    Usage   
        bb.resetInterpolation()      
    Inputs 
        bb: BlackBoxIdeal 
    Description
       Text
          Sometimes one wants to restart the interpolation
          process from scratch (for example when testing
          code)
      
          Consider for example the union of a plane conic
          an a line in IP^3:
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y);
          conic = ideal (w,x^2+y^2-x*z);
          bbI = blackBoxIdeal intersect(line,conic);
       Text
          We pick some points:
       Example
          pointOnLine = matrix{{0,0,1,2_K}}
          pointOnConic = matrix{{1,1,2,0_K}}
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(pointOnConic)
       Text
          Lets now start the interpolation:
       Example   
          bbI.interpolatedComponentNames()
          bbI.interpolateComponentsAt({pointOnLine,pointOnConic},1)
          bbI.interpolatedComponentNames()
       Text
          Now erase the results:
       Example
          bbI.interpolatedComponentNames()
          bbI.resetInterpolation()
          bbI.interpolatedComponentNames()
          bbI.interpolatedComponents()
///

doc ///
    Key
        refineInterpolation
    Headline
        increases the maximal interpolation degree for all interpolated components
    Usage   
        bb.refineInterpolation()      
    Inputs 
        bb: BlackBoxIdeal 
    Description
       Text
          Often it is useful to do the interpolation
          degree by degree. (for example until a classification of
          points into irreducible components becomes
          possible.). 
      
          Consider for example the union of a plane conic
          an a line in IP^3:
       Example
          K = ZZ/101
          R = K[x,y,z,w]      
          line = ideal (x,y);
          conic = ideal (w,x^2+y^2-x*z);
          bbI = blackBoxIdeal intersect(line,conic);
       Text
          We pick some points:
       Example
          pointOnLine = matrix{{0,0,1,2_K}};
          pointOnConic = matrix{{1,1,2,0_K}};
          bbI.isZeroAt(pointOnLine)
          bbI.isZeroAt(pointOnConic)
       Text
          Lets now start the interpolation:
       Example   
          bbI.interpolateComponentsAt({pointOnLine,pointOnConic},1)
       Text
          Notice that "maxdegree" is 1 for both interpolated compoenents.
          
          Now increase the degree of interpolated equations:
       Example
          bbI.refineInterpolation()
          bbI.resetInterpolation()
       Text
          Notice that "maxdegree" is now 2 for both interpolated components.
///

