with AUnit.Test_Fixtures;

package Integration_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Find_Device_Integration (T : in out Test);
   procedure Test_Get_Programmer_Info_Integration (T : in out Test);
   procedure Test_Run_Test_Integration (T : in out Test);

end Integration_Tests;
