with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with Board_Info_Tests;
with AUnit.Test_Caller;

procedure Test_Runner_TS3 is
   use AUnit.Test_Suites;

   package Board_Info_Caller is new
     AUnit.Test_Caller (Board_Info_Tests.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      -- Get Board Info Tests
      Ret.Add_Test
         (Board_Info_Caller.Create
            ("Test Get Board Info Success",
            Board_Info_Tests.Test_Board_Info_Success'Access));
      Ret.Add_Test
         (Board_Info_Caller.Create
            ("Test Get Board Info Not Found",
            Board_Info_Tests.Test_Board_Info_Not_Found'Access));
      Ret.Add_Test
         (Board_Info_Caller.Create
            ("Test Get Board Info Format",
            Board_Info_Tests.Test_Board_Info_Format'Access));
      Ret.Add_Test
         (Board_Info_Caller.Create
            ("Test Get Board Info Communication Error",
               Board_Info_Tests.Test_Board_Info_Comm_Error'Access));

      return Ret;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner_TS3;