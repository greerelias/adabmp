with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with Device_Tester_Tests;
with AUnit.Test_Caller;

procedure Test_Runner is
   use AUnit.Test_Suites;

   package Caller is new AUnit.Test_Caller (Device_Tester_Tests.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create
           ("Test Successful Communication",
            Device_Tester_Tests.Test_Successful_Communication'Access));
      Ret.Add_Test
        (Caller.Create
           ("Test Failure Response",
            Device_Tester_Tests.Test_Failure_Response'Access));
      Ret.Add_Test
        (Caller.Create
           ("Test No Response", Device_Tester_Tests.Test_No_Response'Access));
      return Ret;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner;
