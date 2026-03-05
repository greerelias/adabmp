with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with UART_Tests;
with AUnit.Test_Caller;

procedure Test_Runner_TS5 is
   use AUnit.Test_Suites;

   package UART_Tests_Caller is new AUnit.Test_Caller (UART_Tests.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      -- Start UART Tests
      Ret.Add_Test
        (UART_Tests_Caller.Create
           ("Start UART Success", UART_Tests.Test_UART_Success'Access));
      Ret.Add_Test
        (UART_Tests_Caller.Create
           ("Start UART No Response",
            UART_Tests.Test_UART_No_Response'Access));
      Ret.Add_Test
        (UART_Tests_Caller.Create
           ("Start UART Bad Response",
            UART_Tests.Test_UART_Bad_Response'Access));
      return Ret;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner_TS5;
