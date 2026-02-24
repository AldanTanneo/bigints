with Bigints.Machine_Ints;

package Bigints.Primitives
  with SPARK_Mode => On, Pure
is
   use Bigints.Machine_Ints;

   type Recip is private;

   function Create_Recip (Divisor : U64) return Recip
   with
     Inline,
     Pre  => Divisor > 0,
     Post => Get_Shift (Create_Recip'Result) = Leading_Zeros (Divisor);

   function Get_Shift (R : Recip) return Natural
   with Inline_Always, Post => Get_Shift'Result < 64;
   --  Get the shift value

   function Get_Divisor (R : Recip) return U64
   with Inline_Always;
   --  Get the original divisor

   function Get_Divisor_Normalized (R : Recip) return U64
   with Ghost;
   --  Get the shifted divisor

   function Div2By1 (U1, U0 : U64; Re : Recip) return Tuple
   with Inline;
   --  Calculate the quotient and the remainder of the division of a wide word
   --  (supplied as high and low words U1 and U0) by `D`, with a precalculated
   --  reciprocal `Re`.

   function Div3By2 (U2, U1, U0 : U64; V1_Recip : Recip; V0 : U64) return U64
   with Inline, Pre => Get_Shift (V1_Recip) = 0;
   --  Given two long integers `U = (..., U0, U1, U2)` and `V = (..., V0, V1)`
   --  (where `U2` and `V1` are the most significant limbs), where
   --  `floor(U / V) <= U64'Last`, calculates `Q` such that
   --  `Q - 1 <= floor(U / V) <= Q`.
   --  In place of `V1` takes its reciprocal, and assumes that `V` was already
   --  pre-shifted so that V1 has its most significant bit set (that is, the
   --  reciprocal's `Shift` is 0).

   function Reciprocal (D : U64) return U64
   with Inline, Pre => D >= 2 ** 63;
   --  Calculates the reciprocal of the given 64-bit divisor with the
   --  highmost bit set.

private
   type Recip is record
      Div_Normalized : U64 := U64'Last;
      Shift          : Natural := 0;
      Reciprocal     : U64 := 1;
   end record
   with Type_Invariant => Recip.Div_Normalized >= 2 ** 63 and then Recip.Shift < 64;

   function Get_Shift (R : Recip) return Natural
   is (R.Shift);

   function Get_Divisor (R : Recip) return U64
   is (Shift_Right (R.Div_Normalized, R.Shift));

   function Get_Divisor_Normalized (R : Recip) return U64
   is (R.Div_Normalized);

end Bigints.Primitives;
