with AUnit.Test_Fixtures;

package Protocol_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Encode_Basic (T : in out Test);
   procedure Test_Encode_With_Zeros (T : in out Test);
   procedure Test_Decode_Basic (T : in out Test);
   procedure Test_Decode_With_Zeros (T : in out Test);
   procedure Test_Round_Trip (T : in out Test);
   procedure Test_Decode_Error_Zero_Byte (T : in out Test);
   procedure Test_Decode_Error_Length (T : in out Test);

end Protocol_Tests;
