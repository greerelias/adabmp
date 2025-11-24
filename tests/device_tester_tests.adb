with AUnit.Assertions;      use AUnit.Assertions;
with Serial_Interface.Stub;
with Connection_Tester;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Device_Tester_Tests is

   procedure Test_Successful_Communication (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean;
      Msg     : Unbounded_String;
   begin
      Port.Set_Input ("PONG");
      Connection_Tester.Run_Test (Port, "PING", "PONG", Success, Msg);

      Assert (Success, "Test should succeed");
      Assert
        (Port.Get_Output = "PING" & ASCII.CR & ASCII.LF,
         "Should send command with CRLF");
   end Test_Successful_Communication;

   procedure Test_Failure_Response (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean;
      Msg     : Unbounded_String;
   begin
      Port.Set_Input ("ERROR");
      Connection_Tester.Run_Test (Port, "PING", "PONG", Success, Msg);

      Assert (not Success, "Test should fail");
   end Test_Failure_Response;

   procedure Test_No_Response (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean;
      Msg     : Unbounded_String;
   begin
      --  Empty input
      Connection_Tester.Run_Test (Port, "PING", "PONG", Success, Msg);

      Assert (not Success, "Test should fail on timeout");
   end Test_No_Response;

end Device_Tester_Tests;
