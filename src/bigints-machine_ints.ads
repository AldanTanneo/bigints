pragma Warning_As_Error ("intrinsic binding type mismatch");
pragma Warning_As_Error ("profile of * doesn't match the builtin it binds");

package Bigints.Machine_Ints
  with SPARK_Mode => On
is
   pragma Warnings (GNATProve, Off, "pragma * ignored (not yet supported)");
   pragma Extensions_Allowed (On);

   type U32 is mod 2 ** 32;
   for U32'Size use 32;

   type U64 is mod 2 ** 64;
   for U64'Size use 64;

   type U128 is mod 2 ** 128;
   for U128'Size use 128;

   package Builtins
     with SPARK_Mode => On
   is
      function Leading_Zeros_32 (Value : U32) return Natural
      with
        Pre => Value > 0, --  __builtin_clz is undefined for 0
        Post =>
          Leading_Zeros_32'Result < 32
          and then (Value * 2 ** Leading_Zeros_32'Result)
                   / 2 ** Leading_Zeros_32'Result
                   = Value
          and then Value * 2 ** (Leading_Zeros_32'Result) >= 2 ** 31,
        Global => null,
        Inline;
      pragma Import (Intrinsic, Leading_Zeros_32, "__builtin_clz");

      function Leading_Zeros_64 (Value : U64) return Natural
      with
        Pre => Value > 0, --  __builtin_clzll is undefined for 0
        Post =>
          Leading_Zeros_64'Result < 64
          and then (Value * 2 ** Leading_Zeros_64'Result)
                   / 2 ** Leading_Zeros_64'Result
                   = Value
          and then Value * 2 ** (Leading_Zeros_64'Result) >= 2 ** 63,
        Global => null,
        Inline;
      pragma Import (Intrinsic, Leading_Zeros_64, "__builtin_clzll");

      function Trailing_Zeros_32 (Value : U32) return Natural
      with
        Pre => Value > 0, --  __builtin_ctz is undefined for 0
        Post =>
          Trailing_Zeros_32'Result < 32
          and then (Value / 2 ** Trailing_Zeros_32'Result)
                   * 2 ** Trailing_Zeros_32'Result
                   = Value
          and then Value / 2 ** (Trailing_Zeros_32'Result) mod 2 = 1,
        Global => null,
        Inline;
      pragma Import (Intrinsic, Trailing_Zeros_32, "__builtin_ctz");

      function Trailing_Zeros_64 (Value : U64) return Natural
      with
        Pre => Value > 0, --  __builtin_ctzll is undefined for 0
        Post =>
          Trailing_Zeros_64'Result < 64
          and then (Value / 2 ** Trailing_Zeros_64'Result)
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
     Pre => Value > 0,
     Post =>
       Leading_Zeros'Result < 32
       and then (Value * 2 ** Leading_Zeros'Result) / 2 ** Leading_Zeros'Result
                = Value
       and then Value * 2 ** (Leading_Zeros'Result) >= 2 ** 31,
     Inline_Always;

   function Trailing_Zeros (Value : U32) return Natural
   is (Builtins.Trailing_Zeros_32 (Value))
   with
     Pre => Value > 0,
     Post =>
       Trailing_Zeros'Result < 32
       and then (Value / 2 ** Trailing_Zeros'Result)
                * 2 ** Trailing_Zeros'Result
                = Value
       and then Value / 2 ** (Trailing_Zeros'Result) mod 2 = 1,
     Inline_Always;

   function Shift_Left (Value : U32; Amount : Natural) return U32
   with
     Pre => Amount < 32,
     Post => Shift_Left'Result = Value * 2 ** Amount,
     Global => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

   function Shift_Right (Value : U32; Amount : Natural) return U32
   with
     Pre => Amount < 32,
     Post => Shift_Right'Result = Value / 2 ** Amount,
     Global => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

   function Leading_Zeros (Value : U64) return Natural
   is (Builtins.Leading_Zeros_64 (Value))
   with
     Pre => Value > 0,
     Post =>
       Leading_Zeros'Result < 64
       and then (Value * 2 ** Leading_Zeros'Result) / 2 ** Leading_Zeros'Result
                = Value
       and then Value * 2 ** (Leading_Zeros'Result) >= 2 ** 63,
     Inline_Always;

   function Trailing_Zeros (Value : U64) return Natural
   is (Builtins.Trailing_Zeros_64 (Value))
   with
     Pre => Value > 0,
     Post =>
       Trailing_Zeros'Result < 64
       and then (Value / 2 ** Trailing_Zeros'Result)
                * 2 ** Trailing_Zeros'Result
                = Value
       and then Value / 2 ** (Trailing_Zeros'Result) mod 2 = 1,
     Inline_Always;

   function Shift_Left (Value : U64; Amount : Natural) return U64
   with
     Pre => Amount < 64,
     Post => Shift_Left'Result = Value * 2 ** Amount,
     Global => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

   function Shift_Right (Value : U64; Amount : Natural) return U64
   with
     Pre => Amount < 64,
     Post => Shift_Right'Result = Value / 2 ** Amount,
     Global => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

   function Shift_Left (Value : U128; Amount : Natural) return U128
   with
     Pre => Amount < 128,
     Post => Shift_Left'Result = Value * 2 ** Amount,
     Global => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

   function Shift_Right (Value : U128; Amount : Natural) return U128
   with
     Pre => Amount < 128,
     Post => Shift_Right'Result = Value / 2 ** Amount,
     Global => null,
     Import,
     Convention => Intrinsic,
     Static,
     Inline_Always;

end Bigints.Machine_Ints;
