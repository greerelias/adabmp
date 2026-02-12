with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with Configure_Target_Tests;
with AUnit.Test_Caller;

procedure Test_Runner_TS4 is
   use AUnit.Test_Suites;

   package Configure_Target_Caller is new
     AUnit.Test_Caller (Configure_Target_Tests.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      -- Configure Target Tests
      Ret.Add_Test
        (Configure_Target_Caller.Create
           ("Test Configure Target Success",
            Configure_Target_Tests.Test_Configure_Target_Success'Access));
      Ret.Add_Test
        (Configure_Target_Caller.Create
           ("Test Configure Target No Response",
            Configure_Target_Tests.Test_Configure_Target_No_Response'Access));
      Ret.Add_Test
        (Configure_Target_Caller.Create
           ("Test Configure Target Bad File",
            Configure_Target_Tests.Test_Configure_Target_Bad_File'Access));
      Ret.Add_Test
        (Configure_Target_Caller.Create
           ("Test Configure Target No File Error",
            Configure_Target_Tests.Test_Configure_Target_No_File'Access));
      Ret.Add_Test
        (Configure_Target_Caller.Create
           ("Test Configure Target Failure",
            Configure_Target_Tests.Test_Configure_Target_Failure'Access));

      return Ret;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner_TS4;
