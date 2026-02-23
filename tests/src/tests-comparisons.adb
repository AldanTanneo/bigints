with Ada.Text_IO;          use Ada.Text_IO;
with Bigints.Const_Choice; use Bigints.Const_Choice;

procedure Tests.Comparisons is
   use U256s;

   X, Y   : U64;
   A, B   : U256;
   Ab, Bb : Big_Natural;

   N : constant Natural := (if Test_Is_Github_CI then 10 ** 6 else 10 ** 5);
begin
   for I in 1 .. N loop
      X := Random_U64;
      Y := Random_U64;

      Assert_Ct (Ct_Eq (X, X), "invalid primitive eq");
      if X /= Y then
         Assert_Ct (not (Ct_Eq (X, Y)), "invalid primitive eq");
      end if;

      Assert_Ct (not Ct_Gt (X, X), "invalid primitive gt");
      if X > Y then
         Assert_Ct (Ct_Gt (X, Y), "invalid primitive gt");
      elsif X = Y then
         Assert_Ct (Ct_Eq (X, Y), "invalid primitive eq");
      else
         Assert_Ct (Ct_Gt (Y, X), "invalid primitive gt");
      end if;
   end loop;

   for I in 1 .. N loop
      A := Random_U256;
      B := Random_U256;
      Ab := To_Big_Number (A);
      Bb := To_Big_Number (B);

      Assert (A = A, "invalid eq");
      Assert ((A = B) = (Ab = Bb), "invalid eq");

      Assert (not (A /= A), "invalid ne");
      Assert ((A /= B) = (Ab /= Bb), "invalid ne");

      Assert (not (A < A), "invalid lt");
      Assert ((A < B) = (Ab < Bb), "invalid lt");

      Assert (A <= A, "invalid le");
      Assert ((A <= B) = (Ab <= Bb), "invalid le");

      Assert (not (A > A), "invalid gt");
      Assert ((A > B) = (Ab > Bb), "invalid gt");

      Assert (A >= A, "invalid ge");
      Assert ((A >= B) = (Ab >= Bb), "invalid ge");

      Assert_Ct (A = A, "invalid eq");

      if Ab /= Bb then
         Assert_Ct (not (A = B), "invalid eq");
      end if;

      Assert_Ct (not (A > A), "invalid gt");
      if Ab > Bb then
         Assert_Ct (A > B, "invalid gt");
      elsif Ab = Bb then
         Assert_Ct (B > A, "invalid gt");
      end if;
   end loop;
end Tests.Comparisons;
