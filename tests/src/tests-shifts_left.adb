procedure Tests.Shifts_Left is
   use U256s;

   N            : constant U256 :=
     From_Hex
       ("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141");
   TWO_N        : constant U256 :=
     From_Hex
       ("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD755DB9CD5E9140777FA4BD19A06C8282");
   FOUR_N       : constant U256 :=
     From_Hex
       ("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAEABB739ABD2280EEFF497A3340D90504");
   SIXTY_FIVE   : constant U256 :=
     From_Hex
       ("FFFFFFFFFFFFFFFD755DB9CD5E9140777FA4BD19A06C82820000000000000000");
   EIGHTY_EIGHT : constant U256 :=
     From_Hex
       ("FFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD03641410000000000000000000000");
   SIXTY_FOUR   : constant U256 :=
     From_Hex
       ("FFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD03641410000000000000000");

   T : U256 := From_Hex ("1");
begin
   Assert_Eq (Shl (T, 1), From_Hex ("2"), "T << 1 != 2");
   T := From_Hex ("3");
   Assert_Eq (Shl (T, 8), From_Hex ("300"), "T << 8 != 0x300");

   Assert_Eq (Shl1 (N), TWO_N, "N << 1 != 2N");
   Assert_Eq (Shl (N, 1), TWO_N, "N << 1 != 2N");

   Assert_Eq (Shl (N, 2), FOUR_N, "N << 2 != 4N");
   Assert_Eq (Shl_Limb (N, 2).Res, FOUR_N, "N << 2 != 4N");

   Assert_Eq (Shl (N, 65), SIXTY_FIVE, "invalid N << 65");

   Assert_Eq (Shl (N, 88), EIGHTY_EIGHT, "invalid N << 88");

   Assert_Eq (Shl (N, 64), SIXTY_FOUR, "invalid N << 64");
end Tests.Shifts_Left;
