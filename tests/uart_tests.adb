with Ada.Streams;      use Ada.Streams;
with AUnit.Assertions; use AUnit.Assertions;
with Commands;
with Serial_Interface.Stub;
with Packet_Formatter; use Packet_Formatter;
with Protocol;
with UART;

package body UART_Tests is

   procedure Test_UART_Success (T : in out Test) is
      Port       : Serial_Interface.Stub.Mock_Port;
      Success    : Boolean := False;
      Raw_Packet : constant Stream_Element_Array :=
        Make_Packet (Commands.Ready, (1 .. 0 => 0));
      Encoded    : constant Stream_Element_Array :=
        Protocol.Encode (Raw_Packet);
      Input      : Stream_Element_Array (1 .. Encoded'Length + 1);
   begin
      Input (1 .. Encoded'Length) := Encoded;
      Input (Input'Last) := 0;
      Port.Set_Input (Input);
      UART.Start_UART (Port, Success);
      Assert (Success, "Test should succed.");
   end Test_UART_Success;

   procedure Test_UART_Bad_Response (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;

      --  Invalid SOF (0xBB instead of 0xAA)
      Raw_Packet : constant Stream_Element_Array := (16#BB#, 0, 0);
      Encoded    : constant Stream_Element_Array :=
        Protocol.Encode (Raw_Packet);
      Input      : Stream_Element_Array (1 .. Encoded'Length + 1);
   begin
      Input (1 .. Encoded'Length) := Encoded;
      Input (Input'Last) := 0;

      Port.Set_Input (Input);
      UART.Start_UART (Port, Success);
      Assert (not Success, "Test should fail.");
   end Test_UART_Bad_Response;

   procedure Test_UART_No_Response (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean := True;
   begin
      Port.Enabled := False;
      UART.Start_UART (Port, Success);
      Assert (not Success, "Test should fail.");
   end Test_UART_No_Response;

end UART_Tests;
