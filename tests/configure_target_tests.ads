with AUnit.Test_Fixtures;

package Configure_Target_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Configure_Target_Success (T : in out Test);
   procedure Test_Configure_Target_No_Response (T : in out Test);
   procedure Test_Configure_Target_Bad_File (T : in out Test);
   procedure Test_Configure_Target_No_File (T : in out Test);
   procedure Test_Configure_Target_Failure (T : in out Test);

end Configure_Target_Tests;
