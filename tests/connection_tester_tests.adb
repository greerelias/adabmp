with AUnit.Assertions;      use AUnit.Assertions;
with Serial_Interface.Stub;
with Connection_Tester;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;
with Ada.Text_IO;           use Ada.Text_IO;
with Protocol;
with Packet_Formatter;
with Commands;

package body Connection_Tester_Tests is

   procedure Test_Successful_Communication (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean;
      Msg     : Unbounded_String;
   begin
      Connection_Tester.Run_Test (Port, Success, Msg);
      if not Success then
         Put_Line ("Test Failed with Message: " & To_String (Msg));
      end if;
      Assert (Success, "Test should succeed");
   end Test_Successful_Communication;

   procedure Test_Bad_Response (T : in out Test) is
      Port     : Serial_Interface.Stub.Mock_Port;
      Success  : Boolean;
      Msg      : Unbounded_String;
      Bad_Data : Stream_Element_Array := (2, 1, 0); -- Decodes to (1)
   begin
      Port.Loopback_Enabled := False;
      Port.Set_Input (Bad_Data);

      Connection_Tester.Run_Test (Port, Success, Msg);
      Put_Line (To_String (Msg));
      Assert (not Success, "Test should fail");
   end Test_Bad_Response;

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

   procedure Test_Get_Programmer_Info (T : in out Test) is
      use Packet_Formatter;
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean;
      Info    : Unbounded_String;

      Expected_Info : constant String := "AdaBMP v. 0.1.0";
      Payload       :
        Stream_Element_Array
          (1 .. Stream_Element_Offset (Expected_Info'Length));

      Cmd : constant Command_Id := Commands.Get_Programmer_Info;
   begin
      Port.Loopback_Enabled := False;

      for I in Expected_Info'Range loop
         Payload (Stream_Element_Offset (I - Expected_Info'First + 1)) :=
           Character'Pos (Expected_Info (I));
      end loop;

      declare
         Raw_Packet : constant Stream_Element_Array :=
           Make_Packet (Cmd, Payload);
         Encoded    : constant Stream_Element_Array :=
           Protocol.Encode (Raw_Packet);
         Input      : Stream_Element_Array (1 .. Encoded'Length + 1);
      begin
         Input (1 .. Encoded'Length) := Encoded;
         Input (Input'Last) := 0;

         Port.Set_Input (Input);
      end;

      Connection_Tester.Get_Programmer_Info (Port, Success, Info);

      Assert (Success, "Get_Programmer_Info should succeed");
      Assert
        (To_String (Info) = Expected_Info,
         "Expected '" & Expected_Info & "', got '" & To_String (Info) & "'");
   end Test_Get_Programmer_Info;

   procedure Test_Get_Programmer_Info_Failure (T : in out Test) is
      use Packet_Formatter;
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean;
      Info    : Unbounded_String;

      --  Invalid SOF (0xBB instead of 0xAA)
      Raw_Packet : constant Stream_Element_Array := (16#BB#, 0, 0);
      Encoded    : constant Stream_Element_Array :=
        Protocol.Encode (Raw_Packet);
      Input      : Stream_Element_Array (1 .. Encoded'Length + 1);
   begin
      Port.Loopback_Enabled := False;

      Input (1 .. Encoded'Length) := Encoded;
      Input (Input'Last) := 0;

      Port.Set_Input (Input);

      Connection_Tester.Get_Programmer_Info (Port, Success, Info);

      Assert
        (not Success, "Get_Programmer_Info should fail with invalid packet");
      Assert
        (To_String (Info) = "Invalid packet received",
         "Expected 'Invalid packet received', got '" & To_String (Info) & "'");
   end Test_Get_Programmer_Info_Failure;

   procedure Test_Get_Programmer_Info_No_Response (T : in out Test) is
      Port    : Serial_Interface.Stub.Mock_Port;
      Success : Boolean;
      Msg     : Unbounded_String;
   begin
      Port.Loopback_Enabled := False;
      --  Empty input
      Connection_Tester.Get_Programmer_Info (Port, Success, Msg);

      Assert (not Success, "Test should fail on timeout");
   end Test_Get_Programmer_Info_No_Response;
end Connection_Tester_Tests;
