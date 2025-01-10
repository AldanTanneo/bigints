pragma Warnings (GNAT, Off, "no entities of * are referenced");
pragma Warnings (GNAT, Off, "unit * is not referenced");
pragma Warnings (GNAT, Off, "use clause for package * has no effect");
pragma Warnings (GNAT, Off, "*useless assignment to *, value *");

with GNAT.Source_Info;
with Bigints.Primitives;   use Bigints.Primitives;
with Bigints.Machine_Ints; use Bigints.Machine_Ints;
with Bigints.U256s;
with Bigints.U256_Modulo;
with Bigints.F25519;       use Bigints;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

package Tests is
   subtype U256 is U256s.Uint;
   subtype Fp is F25519.Fp;

   function Random_U256 return U256;

   function To_Big_Number (A : U256) return Big_Natural;
   function To_Big_Number (A : U256s.Wide_Uint) return Big_Natural;
   function To_Big_Number (A : U64) return Big_Natural;
   function To_Uint (A : Big_Natural) return U256;
   function To_Wide_Uint (A : Big_Natural) return U256s.Wide_Uint;
   function To_U64 (A : Big_Natural) return U64;

   procedure Put_U64 (A : U64);
   procedure Put_U256 (A : U256);
   procedure Put_Fp (A : Fp);

   procedure Assert
     (Condition : Boolean; Msg : String := "";
      Test_Name : String  := GNAT.Source_Info.Enclosing_Entity;
      Line      : Natural := GNAT.Source_Info.Line);

   procedure Assert_Eq
     (Left, Right : U64; Msg : String := "";
      Test_Name   : String  := GNAT.Source_Info.Enclosing_Entity;
      Line        : Natural := GNAT.Source_Info.Line);

   procedure Assert_Eq
     (Left, Right : U256; Msg : String := "";
      Test_Name   : String  := GNAT.Source_Info.Enclosing_Entity;
      Line        : Natural := GNAT.Source_Info.Line);

   procedure Assert_Eq
     (Left, Right : U256s.Wide_Uint; Msg : String := "";
      Test_Name   : String  := GNAT.Source_Info.Enclosing_Entity;
      Line        : Natural := GNAT.Source_Info.Line);
end Tests;
