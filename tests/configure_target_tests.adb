with Ada.Streams;      use Ada.Streams;
with AUnit.Assertions; use AUnit.Assertions;
with Configure_Target;
with Serial_Interface.Stub;
with Packet_Formatter; use Packet_Formatter;
with Protocol;         use Protocol;

package body Configure_Target_Tests is


   procedure Test_Configure_Target_Success (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := False;
   begin
      Configure_Target.Load_Bitstream (Port, "tester.bit", Success);
      Assert (Success, "Test should succed.");
   end Test_Configure_Target_Success;

   procedure Test_Configure_Target_No_Response (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Port.Enabled := False;
      Configure_Target.Load_Bitstream (Port, "tester.bit", Success);
      Assert (not Success, "Test should fail.");
   end Test_Configure_Target_No_Response;

   procedure Test_Configure_Target_Bad_File (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Configure_Target.Load_Bitstream (Port, "bad_tester.bit", Success);
      Assert (not Success, "Test should fail.");
   end Test_Configure_Target_Bad_File;

   procedure Test_Configure_Target_No_File (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Configure_Target.Load_Bitstream (Port, "none.bit", Success);
      Assert (not Success, "Test should fail.");
   end Test_Configure_Target_No_File;

   procedure Test_Configure_Target_Failure (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Configure_Target.Load_Bitstream (Port, "tester.bit", Success);
      Assert (not Success, "Test should fail.");
   end Test_Configure_Target_Failure;
end Configure_Target_Tests;
