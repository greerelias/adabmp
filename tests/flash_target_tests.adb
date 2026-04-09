with Ada.Characters.Latin_1;
with Ada.Streams;      use Ada.Streams;
with AUnit.Assertions; use AUnit.Assertions;
with Flash_Target;
with Serial_Interface.Stub;
with Packet_Formatter; use Packet_Formatter;
with Protocol;         use Protocol;

package body Flash_Target_Tests is

   -- Flash Bitstream Tests
   procedure Test_Flash_Bitstream_Success (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := False;
   begin
      Flash_Target.Flash_Bitstream (Port, "tester.bit", Success);
      Assert (Success, "Test should succeed.");
   end Test_Flash_Bitstream_Success;

   procedure Test_Flash_Bitstream_No_Response (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Port.Enabled := False;
      Flash_Target.Flash_Bitstream (Port, "tester.bit", Success);
      Assert (not Success, "Test should fail.");
   end Test_Flash_Bitstream_No_Response;

   procedure Test_Flash_Bitstream_Bad_File (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Flash_Target.Flash_Bitstream (Port, "bad_tester.bit", Success);
      Assert (not Success, "Test should fail.");
   end Test_Flash_Bitstream_Bad_File;

   procedure Test_Flash_Bitstream_No_File (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Flash_Target.Flash_Bitstream (Port, "none.bit", Success);
      Assert (not Success, "Test should fail.");
   end Test_Flash_Bitstream_No_File;

   procedure Test_Flash_Bitstream_Failure (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Port.Fail_During_Write := True;
      Flash_Target.Flash_Bitstream (Port, "tester.bit", Success);
      Assert (not Success, "Test should fail.");
   end Test_Flash_Bitstream_Failure;

   -- Flash Firmware Tests
   procedure Test_Flash_Firmware_Success (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := False;
   begin
      Flash_Target.Flash_Firmware (Port, "tester.bit", Success);
      Assert (Success, "Test should succed.");
   end Test_Flash_Firmware_Success;

   procedure Test_Flash_Firmware_No_Response (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Port.Enabled := False;
      Flash_Target.Flash_Firmware (Port, "tester.bit", Success);
      Assert (not Success, "Test should fail.");
   end Test_Flash_Firmware_No_Response;

   procedure Test_Flash_Firmware_Invalid_Filename (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Flash_Target.Flash_Firmware
        (Port, "invalid" & Ada.Characters.Latin_1.NUL, Success);
      Assert (not Success, "Test should fail.");
   end Test_Flash_Firmware_Invalid_Filename;

   procedure Test_Flash_Firmware_No_File (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Flash_Target.Flash_Firmware (Port, "none.bit", Success);
      Assert (not Success, "Test should fail.");
   end Test_Flash_Firmware_No_File;

   procedure Test_Flash_Firmware_Failure (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Port.Fail_During_Write := True;
      Flash_Target.Flash_Firmware (Port, "tester.bit", Success);
      Assert (not Success, "Test should fail.");
   end Test_Flash_Firmware_Failure;
end Flash_Target_Tests;
