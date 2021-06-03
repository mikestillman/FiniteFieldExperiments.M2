doc ///
   Key
        (createRandomPointIterator, Ring, ZZ)
   Headline
        create  iterator over random points given by ground ring and number of coordinates.
   Usage   
        pointIterator = createRandomPointIterator(R, dim)
   Inputs  
         rng: Ring
            the coefficient ring 
         n:ZZ
            dimension of the vector space over rng 
   Description
        Text
           Create an iterator over random points \in R^{n}, given as matrices \break
           See also PointIterator.
        Example
           rng = QQ
           numCoordinates := 4_ZZ
           pointIterator = createRandomPointIterator(rng, numCoordinates);
        Text
           now we are able to generate random points by calling next():
        Example
           pointIterator.next()
           pointIterator.getPoint()
           pointIterator.position()
           pointIterator.next()
           pointIterator.getPoint()
           pointIterator.position()       
///

doc ///
   Key
        (createRandomPointIterator, Function)
   Headline
        create iterator over points given by a point generator.
   Usage   
        pointIterator = createRandomPointIterator(weakPointGenerator)
   Inputs  
        weakPointGenerator: Function
            a function which generates a random point of type Matrix or returns null
   Description
        Text
           Create an iterator over random points using a given random point generator \break
           See also PointIterator.
        Example
           weakPointGenerator := ()-> (if odd random(ZZ) then random(QQ^1,QQ^3) );
           pointIterator = createRandomPointIterator(weakPointGenerator);
        Text
           now we are able to generate random points by calling next():
        Example
           pointIterator.next()
           pointIterator.getPoint()
           pointIterator.position()
           pointIterator.next()
           pointIterator.getPoint()
           pointIterator.position()        
///

doc ///
   Key
        (createIterator)
   Headline
        create iterator over elements given by a list
   Usage   
        pointIterator = createIterator(list)
   Inputs  
        list: List
            a list of ite
   Description
        Text
           Create an iterator over random points using a given random point generator \break
           See also PointIterator.
        Example
           pointList = apply (4, i -> random( ZZ^1, ZZ^3 ) )
           pointIterator = createIterator(pointList);
        Text
           now we are able to iterate over all points:
        Example
           while  pointIterator.next() do pointIterator.getPoint()
           pointIterator.next()
///

doc ///
   Key
        "setPointIterator"
   Headline
        changes the way random points are chosen
   Usage   
        e.setPointIterator(iterator)
   Inputs  
        e:Experiment 
            an Experiment
        itearator: HashTable
            defining a point iterator            
   Description
        Text
           Sometimes it is usefull to change the way random points of an 
           experiment are chosen. 
           
           Lets look at a typical but somewhat artificial example:           
        Example
           K = ZZ/11;
           R = K[a,b,c,x,y,z];
           I = ideal (a*x^2+b*y^2+c*z^2,a*x+b*y+c*z);
           bb = blackBoxIdeal I;
        Text     
           Notice that the equations of the ideal are linear in a,b,c.
           Therefore to find a point in the vanishing set, 
           we can choose x,y,z randomly first and solve the resulting
           linear equations for a,b,c later:
        Example
           pointX = random(K^1,K^3)
        Text    
           We substitute these values into the equations of the
           ideal to get linear equations for a,b,c:
        Example
           gensA = matrix{{a,b,c}}
           IA = sub(I,gensA|pointX)
        Text
           We now extract the coefficients of these linear equations
        Example
           coefficientsLinearEquation = transpose sub(diff(transpose gensA,gens IA),K)
        Text
           and look at a basis of the linear subspace defined by these equations
        Example
           basisOfLinearSpace = syz coefficientsLinearEquation
        Text
           Notice that this space can sometimes be more than 1 dimensional
           if the coefficient vectors are linearly dependent.
           
           Now we choose a random point inside the linear space by
           taking a random linear combination of the basis vectors
        Example
           pointA = transpose (basisOfLinearSpace * random(source basisOfLinearSpace,K^1))
        Text
           The values for a,b,c and for x,y,z give our random point
        Example
           point = pointA|pointX
        Text
           indeed it lies in the vanishing set of our ideal:
        Example
           sub(I,point)
        Text
           Lets put this into a function:
        Example
           smartRandomPoint = () -> (
                -- choose x,y,z randomly
                pointX = random(K^1,K^3);
                -- substituting these values we get linear equations for a,b,c
                IA = sub(I,gensA|pointX);
                -- we now extract the coefficients of these linear equations
                coefficientsLinearEquation = transpose sub(diff(transpose gensA,gens IA),K);
                -- and look at a basis of the linear subspace defined by these equations
                basisOfLinearSpace = syz coefficientsLinearEquation;
                -- now choose a random point inside this linear space
                pointA = transpose (basisOfLinearSpace * random(source basisOfLinearSpace,K^1));
                point = pointA|pointX
                );
        Text
           Indeed this gives points in the vanishing set of I:
        Example
           sub(I,smartRandomPoint())
        Text
           We now want to use this function to choose random points
           in an experiment. For this we frist have to create an 
           iterator from it:
        Example
           smartIterator = createRandomPointIterator(smartRandomPoint);
        Text
           Now we create the experiment:
        Example
           e = new Experiment from bb;
           e.run(200)
        Text
           running the experiment with the usual random generator
           finds only a few points on the variety. (about 200/11^2 = 8 since
           two equations have to vanish).
      
           We now change the random generator to the function above.
           Before doing this we must clear the statistics since they
           would be useless if points found by different methods are mixed
        Example
           e.clear()
           e.setPointIterator(smartIterator)
           e.run(100)
        Text
           We see that many more points are found in the same time. 
           This reduces the time needed to find interesting points
           in high codimension.          
///

