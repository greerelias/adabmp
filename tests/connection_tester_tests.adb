with AUnit.Assertions;      use AUnit.Assertions;
with Serial_Interface.Stub;
with Connection_Tester;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;

package body Connection_Tester_Tests is

   procedure Test_Successful_Communication (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean;
      Msg     : Unbounded_String;
   begin
      Connection_Tester.Run_Test (Port, Success, Msg);

      Assert (Success, "Test should succeed");
   end Test_Successful_Communication;

   procedure Test_Failure_Response (T : in out Test) is
      Port     : Serial_Interface.Stub.Mock_Port;
      Success  : Boolean;
      Msg      : Unbounded_String;
      Bad_Data : Stream_Element_Array := (2, 1, 0); -- Decodes to (1)
   begin
      Port.Loopback_Enabled := False;
      Port.Set_Input (Bad_Data);

      Connection_Tester.Run_Test (Port, Success, Msg);
      Assert (not Success, "Test should fail");
   end Test_Failure_Response;

   procedure Test_No_Response (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean;
      Msg     : Unbounded_String;
   begin
      Port.Loopback_Enabled := False;
      --  Empty input
      Connection_Tester.Run_Test (Port, Success, Msg);

      Assert (not Success, "Test should fail on timeout");
   end Test_No_Response;

end Connection_Tester_Tests;
