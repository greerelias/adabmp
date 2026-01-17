with AUnit.Test_Fixtures;

package Board_Info_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Board_Info_Success (T : in out Test);
   procedure Test_Board_Info_Not_Found (T : in out Test);
   procedure Test_Board_Info_Format ( T : in out Test);

   -- TODO: Come up with test prototypes
   --  procedure Test_Find_Device_Success (T : in out Test);
   --  procedure Test_Find_Device_Not_Found (T : in out Test);
   --  procedure Test_Find_Device_USB_Structure (T : in out Test);

end Board_Info_Tests;
