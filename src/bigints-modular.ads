with Bigints.Uints;
with Bigints.Uints.Modulo_Ops;
with Bigints.Machine_Ints; use Bigints.Machine_Ints;

generic
   with package Uints is new Bigints.Uints (<>);
   P : Uints.Uint;
package Bigints.Modular with
  SPARK_Mode        => On,
  Initial_Condition => (for some I in 1 .. Uints.N => P (I) /= 0)
is
   MODULUS : constant Uints.Uint := P;
   N       : constant Positive   := Uints.N;

   type Fp is private;

   ONE  : constant Fp;
   ZERO : constant Fp;

   function Create (U : Uints.Uint) return Fp;
   function Create (U : U64) return Fp;
   function Retrieve (F : Fp) return Uints.Uint;

   function "+" (A, B : Fp) return Fp;
   function "-" (A : Fp) return Fp;
   function "-" (A, B : Fp) return Fp;
   function "*" (A, B : Fp) return Fp;
   function "**" (A : Fp; N : Uints.Uint) return Fp;

   overriding function "=" (A, B : Fp) return Boolean with
     Inline;

   function Pow (A : Fp; N : Uints.Uint) return Fp;
   function Pow_Vartime (A : Fp; N : Uints.Uint) return Fp;
   function Div_By_2 (A : Fp) return Fp;
   function Inv (A : Fp) return Fp with
     Pre => A /= ZERO;
   function Inv_Vartime (A : Fp) return Fp with
     Pre => A /= ZERO;

private

   use Uints;
   type Fp is new Uint;

   function Sub_Mod_With_Carry
     (Lhs : Uint; Carry : U64; Rhs, P : Uint) return Uint;

   function Montgomery_Reduction
     (Value : Wide_Uint; Modulus : Uint; Mod_Neg_Inv : U64) return Uint;

   overriding function "=" (A, B : Fp) return Boolean is (Uint (A) = Uint (B));

   package Uints_Modulo is new Uints.Modulo_Ops;
   package Uints_Wide is new Bigints.Uints (2 * N);

   P_MINUS_TWO       : constant Uint    := P - Uints.From_U64 (2);
   ZERO              : constant Fp      := Fp (Uints.ZERO);
   ONE               : constant Fp      := Fp ((MAX mod P) + Uints.ONE);
   R2                : constant Uint    :=
     Truncate
       (Wide_Uint
          (Uints_Wide."mod"
             (Uints_Wide.Uint (Mul_Wide (ONE, ONE)),
              Uints_Wide.Uint (Concat (P, Uints.ZERO)))));
   INV_MOD           : constant Fp      := Fp (Inv_Mod2k_Vartime (P, BITS));
   MOD_NEG_INV       : constant U64     := -INV_MOD (1);
   MOD_LEADING_ZEROS : constant Natural :=
     Natural'Min (Leading_Zeros (P), BITS - 1);

end Bigints.Modular;
