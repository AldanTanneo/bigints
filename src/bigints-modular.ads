with Bigints.Const_Choice;
with Bigints.Uints;
with Bigints.Uints.Modulo_Ops;
with Bigints.Machine_Ints; use Bigints.Machine_Ints;

generic
   with package Uints is new Bigints.Uints (<>);
   P : Uints.Uint;
package Bigints.Modular with
  SPARK_Mode => On, Initial_Condition => (P (1) mod 2 /= 0)
is
   MODULUS : constant Uints.Uint := P;

   type Fp is private;
   --  Element of GF(P)

   ONE  : constant Fp;
   ZERO : constant Fp;

   function Create (U : Uints.Uint) return Fp;
   --  Create a field element from a Uint

   function Create (U : U64) return Fp;
   --  Create a field element from a U64

   function Retrieve (F : Fp) return Uints.Uint;
   --  Retrieve an integer representing the field element

   function "+" (A, B : Fp) return Fp;
   --  Addition in GF(P)

   function "-" (A : Fp) return Fp;
   --  Negation in GF(P)

   function "-" (A, B : Fp) return Fp;
   --  Substraction in GF(P)

   function "*" (A, B : Fp) return Fp;
   --  Multiplication in GF(P);

   function "**" (A : Fp; N : Uints.Uint) return Fp;
   --  Exponentiation in GF(P)

   overriding function "=" (A, B : Fp) return Boolean with
     Inline;
   --  Constant time equality check

   function Pow (A : Fp; N : Uints.Uint) return Fp;
   --  Constant time exponentiation

   function Pow_Vartime (A : Fp; N : Uints.Uint) return Fp;
   --  Variable time (wrt Modulus) exponentiation

   function Div_By_2 (A : Fp) return Fp;
   --  Division by 2 in GF(P)

   function Inv (A : Fp) return Fp with
     Pre => A /= ZERO;
   --  Constant time inversion in GF(P) (fermat method)

   function Inv_Vartime (A : Fp) return Fp with
     Pre => A /= ZERO;
   --  Variable time (wrt Modulus) inversion in GF(P)

   function Cond_Select (A, B : Fp; C : Const_Choice.Choice) return Fp with
     Post =>
      (Cond_Select'Result = (if Const_Choice.To_Bool (C) then B else A));
   --  Constant time select

   procedure CSwap (A, B : in out Fp; C : Const_Choice.Choice) with
     Post =>
      (if Const_Choice.To_Bool (C) then (A = B'Old and then B = A'Old)
       else (A = A'Old and then B = B'Old));
   --  Constant time swap

private
   N : constant Positive := Uints.N;

   use Uints;
   type Fp is new Uints.Uint;

   function Sub_Mod_With_Carry
     (Lhs : Uint; Carry : U64; Rhs, P : Uint) return Uint;

   function Montgomery_Reduction
     (Value : Wide_Uint; Modulus : Uint; Mod_Neg_Inv : U64) return Uint;

   overriding function "=" (A, B : Fp) return Boolean is (Uint (A) = Uint (B));

   package Uints_Modulo is new Uints.Modulo_Ops;
   package Uints_Wide is new Bigints.Uints (2 * N);

   P_MINUS_TWO : constant Uint := P - Uints.From_U64 (2);
   ZERO        : constant Fp   := Fp (Uints.ZERO);
   ONE         : constant Fp   := Fp ((Uints.MAX mod P) + Uints.ONE);
   R2          : constant Uint :=
     Truncate
       (Wide_Uint
          (Uints_Wide."mod"
             (Uints_Wide.Uint (Mul_Wide (ONE, ONE)),
              Uints_Wide.Uint (Concat (P, Uints.ZERO)))));
   INV_MOD     : constant Uint := Inv_Mod2k_Vartime (P, BITS);
   MOD_NEG_INV : constant U64  := -INV_MOD (1);

end Bigints.Modular;

pragma Static_Elaboration_Desired (Bigints.Modular);
