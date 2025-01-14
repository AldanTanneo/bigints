procedure Tests.Sqrt is
   use U256s;

   X, S : U256;
begin
   for I in 1 .. 100000 loop
      X := Random_U256;
      S := Sqrt (X);

      Assert (Square (S) <= X);
      Assert (Square (S + ONE) > X);
      Assert_Eq (Sqrt (Square (S)), S);
   end loop;
end Tests.Sqrt;