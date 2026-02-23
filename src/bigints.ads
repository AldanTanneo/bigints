with Interfaces;

package Bigints
  with SPARK_Mode => On, Pure
is
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   subtype U128 is Interfaces.Unsigned_128;

   pragma Warnings (Off, "use clause for type * has no effect");
   use type U32;
   use type U64;
   use type U128;
   pragma Warnings (On, "use clause for type * has no effect");
end Bigints;
