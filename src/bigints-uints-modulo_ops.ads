generic
package Bigints.Uints.Modulo_Ops with SPARK_Mode => On, Pure is
   function Add_Mod (A, B, P : Uint) return Uint
   with Pre => (for some I in 1 .. N => P (I) /= 0);

   function Double_Mod (A, P : Uint) return Uint
   with Pre => (for some I in 1 .. N => P (I) /= 0);

   function Sub_Mod (A, B, P : Uint) return Uint
   with Pre => (for some I in 1 .. N => P (I) /= 0);

   function Neg_Mod (A, P : Uint) return Uint;
end Bigints.Uints.Modulo_Ops;
