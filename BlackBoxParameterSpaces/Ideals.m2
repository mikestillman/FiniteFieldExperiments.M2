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
 
 
