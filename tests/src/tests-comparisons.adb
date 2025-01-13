with Ada.Text_IO; use Ada.Text_IO;

procedure Tests.Comparisons is
   use U256s;

   A, B   : U256;
   Ab, Bb : Big_Natural;
begin
   for I in 1 .. 100000 loop
      A  := Random_U256;
      B  := Random_U256;
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
   end loop;
end Tests.Comparisons;
