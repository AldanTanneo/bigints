with Bigints.Uints;
with Rand_Core.Generators;

generic
   with package Uints is new Bigints.Uints (<>);
package Rand_Bigints is


   generic
      type Rng (<>) is limited new Rand_Core.Generators.Rng with private;
   function Generic_Gen (R : in out Rng) return Uints.Uint
   with Inline;

   function Gen (R : in out Rand_Core.Generators.Rng'Class) return Uints.Uint
   with Inline;

end Rand_Bigints;
