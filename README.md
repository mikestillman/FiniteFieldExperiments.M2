### FiniteFieldExperiments.M2

A Macaulay2-Framework for finite field experiments for explicit and implicitly given ideals and parameter spaces
( FiniteFieldExperiments.m2, based on top of BlackBoxIdeals), in preliminary beta version.


The package provides also lifting of isolated (polynomial) system solutions over a prime field to an extension field of rationals
( padicLift.m2 )


To use the packages,
an installed M2 (available at http://www.math.uiuc.edu/Macaulay2/Downloads/), >=v1.8 ) and a git client installation is mandatory.

The next step is to checkout the package files via
`git clone https://github.com/jakobkroeker/FiniteFieldExperiments.M2.git`,
start Macaulay2,
add the package location to the M2 path varible
`path = append(path,"$PutLocalCloneLocationHere")`
and install the packages:

installPackage("M2Logging",UserMode =>true)
installPackage("IntervalPkg",UserMode =>true)
installPackage("BlackBoxIdeals",UserMode =>true)
installPackage("FiniteFieldExperiments",UserMode =>true)


Then look at the examples in the *experiments*-folder
or read the online help with 
`viewHelp BlackBoxIdeals`
`viewHelp FiniteFieldExperiments`


Todo:

- BlackBoxIdeals: rename to BlackBoxParameterSpaces
- Mike puts cubic example (mega) into tex format
    i.e. in Documentation tex dir: add M2-code stuff to be able to run examples.
- Chris: shorten the tex file to only the parameter space version.
- Mike: finish tex file
- after we are happy with it: we put it back into the package documentation
-  (Mike: check on katex use)
-  some itemize lists, and links might be to be handled by hand.
- these are the packages to get ready for distribution with M2
   BlackBoxParameterSpaces, FiniteFieldExperiments, M2Logging, IntervalPkg.
-- fix bugs and interface for BlackBoxIdeals


