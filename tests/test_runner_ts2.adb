with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with AUnit.Test_Caller;

with Integration_Tests;

procedure Test_Runner_TS2 is
   use AUnit.Test_Suites;

   package Integration_Caller is new
     AUnit.Test_Caller (Integration_Tests.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Integration_Caller.Create
           ("Test Find Device (Integration)",
            Integration_Tests.Test_Find_Device_Integration'Access));
      Ret.Add_Test
        (Integration_Caller.Create
           ("Test Get Programmer Info (Integration)",
            Integration_Tests.Test_Get_Programmer_Info_Integration'Access));
      Ret.Add_Test
        (Integration_Caller.Create
           ("Test Test Connection (Integration)",
            Integration_Tests.Test_Run_Test_Integration'Access));
      return Ret;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner_TS2;
