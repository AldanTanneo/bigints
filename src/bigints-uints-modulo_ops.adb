with Bigints.Machine_Ints; use Bigints.Machine_Ints;

package body Bigints.Uints.Modulo_Ops
  with SPARK_Mode => On
is

   function Add_Mod (A, B, P : Uint) return Uint is
      WCarry  : constant Uint_Carry := Add_Carry (A, B, 0);
      WBorrow : constant Uint_Carry := Sub_Borrow (WCarry.Res, P, 0);
      Mask    : constant Tuple := Sub_Borrow (WCarry.Carry, 0, WBorrow.Carry);
   begin
      return WBorrow.Res + (P and Mask.Snd);
   end Add_Mod;

   function Double_Mod (A, P : Uint) return Uint is
      WCarry  : constant Uint_Carry := Shl1 (A);
      WBorrow : constant Uint_Carry := Sub_Borrow (WCarry.Res, P, 0);
      Mask    : constant Tuple := Sub_Borrow (WCarry.Carry, 0, WBorrow.Carry);
   begin
      return WBorrow.Res + (P and Mask.Snd);
   end Double_Mod;

   function Sub_Mod (A, B, P : Uint) return Uint is
      OutMask : constant Uint_Carry := Sub_Borrow (A, B, 0);
   begin
      return OutMask.Res + (P and OutMask.Carry);
   end Sub_Mod;

   function Neg_Mod (A, P : Uint) return Uint
   is (Cond_Select (P - A, ZERO, A = ZERO));

end Bigints.Uints.Modulo_Ops;
