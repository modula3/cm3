PROCEDURE %name(self: T;
 VAR result: %return%uparams) = BEGIN
 IF result=NIL THEN
   result:=NewPT(self.allocate_%return,TYPECODE(%return));
 END;END %name;

