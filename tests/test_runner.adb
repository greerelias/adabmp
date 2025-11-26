with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with Connection_Tester_Tests;
with Device_Discovery_Tests;
with AUnit.Test_Caller;

procedure Test_Runner is
   use AUnit.Test_Suites;

   package Connection_Caller is new
     AUnit.Test_Caller (Connection_Tester_Tests.Test);
   package Discovery_Caller is new
     AUnit.Test_Caller (Device_Discovery_Tests.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      -- Connection Tester Tests
      Ret.Add_Test
        (Connection_Caller.Create
           ("Test Successful Communication",
            Connection_Tester_Tests.Test_Successful_Communication'Access));
      Ret.Add_Test
        (Connection_Caller.Create
           ("Test Failure Response",
            Connection_Tester_Tests.Test_Failure_Response'Access));
      Ret.Add_Test
        (Connection_Caller.Create
           ("Test No Response",
            Connection_Tester_Tests.Test_No_Response'Access));

      -- Device Discovery Tests
      Ret.Add_Test
        (Discovery_Caller.Create
           ("Test Find Device Success",
            Device_Discovery_Tests.Test_Find_Device_Success'Access));
      Ret.Add_Test
        (Discovery_Caller.Create
           ("Test Find Device Not Found",
            Device_Discovery_Tests.Test_Find_Device_Not_Found'Access));
      Ret.Add_Test
        (Discovery_Caller.Create
           ("Test Find Device USB Structure",
            Device_Discovery_Tests.Test_Find_Device_USB_Structure'Access));

      return Ret;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner;
