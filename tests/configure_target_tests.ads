with AUnit.Test_Fixtures;

package Configure_Target_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Configure_Target_Success (T : in out Test);
   procedure Test_Configure_Target_No_Reponse (T : in out Test);
   procedure Test_Configure_Target_Bad_File (T : in out Test);
   procedure Test_Configure_Target_No_File (T : in out Test);
   procedure Test_Configure_Target_Write_Timeout (T : in out Test);
end Connection_Tester_Tests;
