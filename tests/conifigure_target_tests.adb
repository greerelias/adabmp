with Ada.Streams;      use Ada.Streams;
with Configure_Target;
with Serial_Interface.Stub;
with Packet_Formatter; use Packet_Formatter;
with Protocol;         use Protocol;

package Configure_Target_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Configure_Target_Success (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean;
      Msg     : Unbounded_String;
   begin
      Configure_Target.Load_Bitstream (Port, "tester.bit", Success);
      Assert (Success, "Test should succed.");
   end Test_Configure_Target_Success;
   procedure Test_Configure_Target_No_Response (T : in out Test);
   procedure Test_Configure_Target_Bad_File (T : in out Test);
   procedure Test_Configure_Target_No_File (T : in out Test);
   procedure Test_Configure_Target_Write_Timeout (T : in out Test);
end Connection_Tester_Tests;
