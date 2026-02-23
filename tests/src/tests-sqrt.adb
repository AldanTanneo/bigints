procedure Tests.Sqrt is
   use U256s;

   X, S : U256;

   N : constant Natural := (if Test_Is_Github_CI then 10 ** 6 else 10 ** 5);
begin
   for I in 1 .. N loop
      X := Random_U256;
      S := Sqrt (X);

      Assert (Square (S) <= X);
      Assert (Square (S + ONE) > X);
      Assert_Eq (Sqrt (Square (S)), S);
   end loop;
end Tests.Sqrt;
