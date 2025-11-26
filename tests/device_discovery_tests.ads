with AUnit.Test_Fixtures;

package Device_Discovery_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Find_Device_Success (T : in out Test);
   procedure Test_Find_Device_Not_Found (T : in out Test);
   procedure Test_Find_Device_USB_Structure (T : in out Test);

end Device_Discovery_Tests;
