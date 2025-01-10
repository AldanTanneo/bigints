pragma SPARK_Mode (On);

with Bigints.Modular;
with Bigints.U256s;

package Bigints.F25519 is new Bigints.Modular
  (U256s,
   [16#FFFF_FFFF_FFFF_FFED#, 16#FFFF_FFFF_FFFF_FFFF#, 16#FFFF_FFFF_FFFF_FFFF#,
   16#7FFF_FFFF_FFFF_FFFF#]);
