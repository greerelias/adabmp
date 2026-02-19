with AUnit.Test_Fixtures;

package Board_Info_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Board_Info_Success (T : in out Test);
   procedure Test_Board_Info_Not_Found (T : in out Test);
   procedure Test_Board_Info_Format ( T : in out Test);
   procedure Test_Board_Info_Comm_Error (T : in out Test);

end Board_Info_Tests;
