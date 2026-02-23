pragma Warning_As_Error ("intrinsic binding type mismatch");
pragma Warning_As_Error ("profile of * doesn't match the builtin it binds");

package Bigints.Machine_Ints
  with SPARK_Mode => On, Pure
is
   pragma Warnings (GNATProve, Off, "pragma * ignored (not yet supported)");
   pragma Extensions_Allowed (On);

   package Builtins
     with SPARK_Mode => On
   is
      function Leading_Zeros_32 (Value : U32) return Natural
      with
        Pre    => Value > 0, --  __builtin_clz is undefined for 0
        Post   =>
          Leading_Zeros_32'Result < 32
          and then
            (Value * 2 ** Leading_Zeros_32'Result)
            / 2 ** Leading_Zeros_32'Result
            = Value
          and then Value * 2 ** (Leading_Zeros_32'Result) >= 2 ** 31,
        Global => null,
        Inline;
      pragma Import (Intrinsic, Leading_Zeros_32, "__builtin_clz");

      function Leading_Zeros_64 (Value : U64) return Natural
      with
        Pre    => Value > 0, --  __builtin_clzll is undefined for 0
        Post   =>
          Leading_Zeros_64'Result < 64
          and then
            (Value * 2 ** Leading_Zeros_64'Result)
            / 2 ** Leading_Zeros_64'Result
            = Value
          and then Value * 2 ** (Leading_Zeros_64'Result) >= 2 ** 63,
        Global => null,
        Inline;
      pragma Import (Intrinsic, Leading_Zeros_64, "__builtin_clzll");

      function Trailing_Zeros_32 (Value : U32) return Natural
      with
        Pre    => Value > 0, --  __builtin_ctz is undefined for 0
        Post   =>
          Trailing_Zeros_32'Result < 32
          and then
            (Value / 2 ** Trailing_Zeros_32'Result)
            * 2 ** Trailing_Zeros_32'Result
            = Value
          and then Value / 2 ** (Trailing_Zeros_32'Result) mod 2 = 1,
        Global => null,
        Inline;
      pragma Import (Intrinsic, Trailing_Zeros_32, "__builtin_ctz");

      function Trailing_Zeros_64 (Value : U64) return Natural
      with
        Pre    => Value > 0, --  __builtin_ctzll is undefined for 0
        Post   =>
          Trailing_Zeros_64'Result < 64
          and then
            (Value / 2 ** Trailing_Zeros_64'Result)
            * 2 ** Trailing_Zeros_64'Result
            = Value
          and then Value / 2 ** (Trailing_Zeros_64'Result) mod 2 = 1,
        Global => null,
        Inline;
      pragma Import (Intrinsic, Trailing_Zeros_64, "__builtin_ctzll");
   end Builtins;

   function Leading_Zeros (Value : U32) return Natural
   is (Builtins.Leading_Zeros_32 (Value))
   with
     Pre  => Value > 0,
     Post =>
       Leading_Zeros'Result < 32
       and then
         (Value * 2 ** Leading_Zeros'Result) / 2 ** Leading_Zeros'Result
         = Value
       and then Value * 2 ** (Leading_Zeros'Result) >= 2 ** 31,
     Inline_Always;

   function Trailing_Zeros (Value : U32) return Natural
   is (Builtins.Trailing_Zeros_32 (Value))
   with
     Pre  => Value > 0,
     Post =>
       Trailing_Zeros'Result < 32
       and then
         (Value / 2 ** Trailing_Zeros'Result) * 2 ** Trailing_Zeros'Result
         = Value
       and then Value / 2 ** (Trailing_Zeros'Result) mod 2 = 1,
     Inline_Always;

   function Shift_Left (Value : U32; Amount : Natural) return U32
   with
     Pre        => Amount < 32,
     Post       => Shift_Left'Result = Value * 2 ** Amount,
     Global     => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

   function Shift_Right (Value : U32; Amount : Natural) return U32
   with
     Pre        => Amount < 32,
     Post       => Shift_Right'Result = Value / 2 ** Amount,
     Global     => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

   function Leading_Zeros (Value : U64) return Natural
   is (Builtins.Leading_Zeros_64 (Value))
   with
     Pre  => Value > 0,
     Post =>
       Leading_Zeros'Result < 64
       and then
         (Value * 2 ** Leading_Zeros'Result) / 2 ** Leading_Zeros'Result
         = Value
       and then Value * 2 ** (Leading_Zeros'Result) >= 2 ** 63,
     Inline_Always;

   function Trailing_Zeros (Value : U64) return Natural
   is (Builtins.Trailing_Zeros_64 (Value))
   with
     Pre  => Value > 0,
     Post =>
       Trailing_Zeros'Result < 64
       and then
         (Value / 2 ** Trailing_Zeros'Result) * 2 ** Trailing_Zeros'Result
         = Value
       and then Value / 2 ** (Trailing_Zeros'Result) mod 2 = 1,
     Inline_Always;

   function Shift_Left (Value : U64; Amount : Natural) return U64
   with
     Pre        => Amount < 64,
     Post       => Shift_Left'Result = Value * 2 ** Amount,
     Global     => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

   function Shift_Right (Value : U64; Amount : Natural) return U64
   with
     Pre        => Amount < 64,
     Post       => Shift_Right'Result = Value / 2 ** Amount,
     Global     => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

   function Shift_Left (Value : U128; Amount : Natural) return U128
   with
     Pre        => Amount < 128,
     Post       => Shift_Left'Result = Value * 2 ** Amount,
     Global     => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

   function Shift_Right (Value : U128; Amount : Natural) return U128
   with
     Pre        => Amount < 128,
     Post       => Shift_Right'Result = Value / 2 ** Amount,
     Global     => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

   type Tuple is record
      Fst, Snd : U64;
   end record;

   function Low (X : U128) return U64
   with Inline, Post => U128 (Low'Result) = X mod (2 ** 64);

   function High (X : U128) return U64
   with Inline, Post => U128 (High'Result) = X / (2 ** 64);

   function Mul_Wide (X, Y : U64) return Tuple
   with Inline_Always, Post => Mul_Wide'Result.Fst in 0 .. 2 ** 64 - 2;
   --  Multiplies `X` and `Y`, returning the most significant
   --  and the least significant words as `(hi, lo)`.

   function Add_Wide (X_Hi, X_Lo, Y_Hi, Y_Lo : U64) return Tuple
   with Inline_Always;
   --  Adds wide numbers represented by pairs of (most significant word, least
   --  significant word) and returns the result in the same format `(hi, lo)`.

   function Add_Carry (X, Y, Carry : U64) return Tuple
   with
     Inline_Always,
     Post =>
       Add_Carry'Result.Snd in 0 .. 2
       and then
         X + Y + Carry = Add_Carry'Result.Fst + 2 ** 64 * Add_Carry'Result.Snd;
   --  Computes `X + Y + Carry`, returning the result along with the new carry
   --  (0, 1, or 2).

   function Overflowing_Add (X, Y : U64) return Tuple
   with Inline_Always, Post => Overflowing_Add'Result.Snd in 0 .. 1;
   --  Computes `X + Y`, returning the result along with the carry (0 or 1).

   function Sub_Borrow (X, Y, Borrow : U64) return Tuple
   with
     Inline_Always,
     Pre  => Borrow = 0 or else Borrow = U64'Last,
     Post =>
       (Sub_Borrow'Result.Fst = X - (Y + Borrow / 2 ** 63)
        and then
          (Sub_Borrow'Result.Snd = U64'Last)
          = (U128 (X) < U128 (Y) + U128 (Borrow) / 2 ** 63)
        and then
          (Sub_Borrow'Result.Snd = 0
           or else Sub_Borrow'Result.Snd = U64'Last));
   --  Computes `X - (Y + Borrow)`, returning the result along with the new
   --  borrow.

   function Mac (A, B, C, Carry : U64) return Tuple
   with Inline_Always;
   --  Computes `a + (b * c) + carry`, returning the result along with the new
   --  carry.

   function Saturating_Sub (A, B : U64) return U64
   with
     Inline_Always,
     Post =>
       (if A >= B
        then Saturating_Sub'Result = A - B
        else Saturating_Sub'Result = 0);

end Bigints.Machine_Ints;
