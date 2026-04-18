with Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded;
with Board_Info;
with Board_Info_Printer;
with Configure_Target;
with Interfaces;
with Serial_Interface.Impl;
with Device_Discovery;
with Connection_Tester;
with Flash_Target;
with UART;
with Ada.Exceptions;   use Ada.Exceptions;

procedure Adabmp is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   Target_VID : constant String := "6666";
   Target_PID : constant String := "4242";

   procedure Open_Device_Port
     (Port    : in out Serial_Interface.Impl.Com_Port;
      Success : out Boolean;
      Verbose : in Boolean := False) is
   begin
      Success := False;

      if Verbose then
         Put_Line
           ("Searching for device (VID="
            & Target_VID
            & ", PID="
            & Target_PID
            & ")...");
      end if;

      declare
         Found : constant String :=
           Device_Discovery.Find_Device (Target_VID, Target_PID);
      begin
         if Verbose then
            Put_Line ("Device found at " & Found);
            Put_Line ("Opening port...");
         end if;
         Port.Open (Found);
         Success := True;
      end;
   exception
      when Device_Discovery.Device_Not_Found =>
         Put_Line ("Error: Device not found.");
         return;
      when others =>
         Put_Line ("Error: Failed to open serial port.");
         return;
   end Open_Device_Port;

   procedure Run_Connection_Test is
      Port    : Serial_Interface.Impl.Com_Port;
      Success : Boolean;
      Msg     : Unbounded_String;
   begin

      Open_Device_Port (Port, Success, True);
      if not Success then
         return;
      end if;
      Put_Line ("Getting Programmer Info...");

      Connection_Tester.Get_Programmer_Info (Port, Success, Msg);

      if Success then
         Put_Line ("Found programmer firmware: " & To_String (Msg));
         Connection_Tester.Run_Test (Port, Success, Msg);
         if Success then
            Put_Line ("Result: " & To_String (Msg));
         else
            Put_Line ("Result: " & To_String (Msg));
         end if;
      else
         Put_Line ("Error: Programmer Info Not Received.");
      end if;

      Port.Close;
   exception
      when others =>
         Put_Line ("An unexpected error occurred.");
         begin
            Port.Close;
         exception
            when others =>
               null;
         end;
   end Run_Connection_Test;

   procedure Run_Get_Board_Info is
      Port    : Serial_Interface.Impl.Com_Port;
      Info    : Board_Info.Board_Info_Record_Access;
      Success : Boolean;
   begin
      Open_Device_Port (Port, Success);
      if not Success then
         return;
      end if;

      Board_Info.Get_Board_Info (Port, Info, Success);
      if Success then
         Board_Info_Printer.Print_Board_Info (Info);
      end if;

      Port.Close;
   exception
      when E : others =>
         Put_Line ("Exception: " & Exception_Name (E));
         Put_Line ("Message:   " & Exception_Message (E));
         Put_Line ("Info:      " & Exception_Information (E));
         begin
            Port.Close;
         exception
            when others =>
               null;
         end;
   end Run_Get_Board_Info;

   procedure Run_Configure_Target (Path : String) is
      Port    : Serial_Interface.Impl.Com_Port;
      Success : Boolean;
   begin
      Open_Device_Port (Port, Success);
      if not Success then
         return;
      end if;

      Configure_Target.Load_Bitstream (Port, Path, Success, True);
      Port.Close;
   exception
      when E : others =>
         Put_Line ("Exception: " & Exception_Name (E));
         Put_Line ("Message:   " & Exception_Message (E));
         Put_Line ("Info:      " & Exception_Information (E));
         begin
            Port.Close;
         exception
            when others =>
               null;
         end;
   end Run_Configure_Target;

   procedure Run_UART is
      Port    : Serial_Interface.Impl.Com_Port;
      Success : Boolean := False;
   begin
      -- Have to call Find_Device here because we want the port name
      declare
         Found : constant String :=
           Device_Discovery.Find_Device (Target_VID, Target_PID);
      begin
         Port.Open (Found);
         UART.Start_UART (Port, Success);
         if Success then
            Put_Line ("UART Ready on: " & Found);
         else
            Put_Line ("Error: Failed start UART");
         end if;
         Port.Close;
      end;
   exception
      when Device_Discovery.Device_Not_Found =>
         Put_Line ("Error: Device not found.");

      when others =>
         Put_Line ("Error: Failed to open serial port.");
   end Run_UART;

   procedure Run_Flash_Bitstream (Path : String) is
      Port    : Serial_Interface.Impl.Com_Port;
      Success : Boolean := False;
   begin
      Open_Device_Port (Port, Success);
      if not Success then
         return;
      end if;
      Flash_Target.Flash_Bitstream (Port, Path, Success, True);
      Port.Close;
   exception
      when E : others =>
         Put_Line ("Exception: " & Exception_Name (E));
         Put_Line ("Message:   " & Exception_Message (E));
         Put_Line ("Info:      " & Exception_Information (E));
         begin
            Port.Close;
         exception
            when others =>
               null;
         end;
   end Run_Flash_Bitstream;

   procedure Run_Flash_Firmware (Path : String; Address_Str : String := "") is
      use type Interfaces.Unsigned_32;
      Port    : Serial_Interface.Impl.Com_Port;
      Success : Boolean := False;
      Address : Interfaces.Unsigned_32;
   begin
      if Address_Str = "" then
         Address := 16#300000#;
      else
         begin
            if Address_Str'Length > 2
              and then
                (Address_Str (Address_Str'First .. Address_Str'First + 1)
                 = "0x"
                 or else
                   Address_Str (Address_Str'First .. Address_Str'First + 1)
                   = "0X")
            then
               Address :=
                 Interfaces.Unsigned_32'Value
                   ("16#"
                    & Address_Str (Address_Str'First + 2 .. Address_Str'Last)
                    & "#");
            else
               Address := Interfaces.Unsigned_32'Value (Address_Str);
            end if;
         exception
            when Constraint_Error =>
               Put_Line
                 ("Error: '"
                  & Address_Str
                  & "' is not a valid decimal or hex number.");
               return;
         end;
      end if;

      if Address mod Interfaces.Unsigned_32'(16#10000#) /= 0 then
         Put_Line ("Error: Address must be 64KB aligned.");
         return;
      end if;

      Open_Device_Port (Port, Success);
      if not Success then
         return;
      end if;
      Flash_Target.Flash_Firmware (Port, Path, Success, Address, True);
      Port.Close;
   exception
      when E : others =>
         Put_Line ("Exception: " & Exception_Name (E));
         Put_Line ("Message:   " & Exception_Message (E));
         Put_Line ("Info:      " & Exception_Information (E));
         begin
            Port.Close;
         exception
            when others =>
               null;
         end;
   end Run_Flash_Firmware;

begin
   if Argument_Count = 0 then
      Put_Line ("--- Ada Baremetal Programmer Commands ---");
      Put_Line
        ("  -t, --test-connection                  : Tests connection to programmer");
      Put_Line
        ("  -i, --board-info                       : Retrieves information about target board");
      Put_Line
        ("  -c, --configure-target <path>          : Configure target with bitstream");
      Put_Line
        ("  -u, -uart                              : Start UART interface");
      Put_Line
        ("  -b, --flash-bitstream <path>           : Flash bitstream to target");
      Put_Line
        ("  -f, --flash-firmware <path> [address]  : Flash firmware to target (address defaults to 0x300000)");
      return;
   end if;

   if Argument (1) = "-t" or Argument (1) = "--test-connection" then
      Run_Connection_Test;
   elsif Argument (1) = "-i" or Argument (1) = "--board-info" then
      Run_Get_Board_Info;
   elsif Argument (1) = "-c" or Argument (1) = "--configure-target" then
      if Argument_Count > 1 then
         Run_Configure_Target (Argument (2));
      else
         Put_Line ("Error: Missing bitstream path.");
         return;
      end if;
   elsif Argument (1) = "-u" or Argument (1) = "-uart" then
      Run_UART;
   elsif Argument (1) = "-b" or Argument (1) = "--flash-bitstream" then
      if Argument_Count > 1 then
         Run_Flash_Bitstream (Argument (2));
      else
         Put_Line ("Error: Missing bitstream file path.");
         return;
      end if;
   elsif Argument (1) = "-f" or Argument (1) = "--flash-firmware" then
      if Argument_Count > 1 then
         if Argument_Count > 2 then
            Run_Flash_Firmware (Argument (2), Argument (3));
         else
            Put_Line
              ("No base address provided, defaulting to 0x300000." & ASCII.LF);
            Run_Flash_Firmware (Argument (2));
         end if;
      else
         Put_Line ("Error: Missing firmware file path.");
         return;
      end if;
   else
      Put_Line ("Unknown argument: " & Argument (1));
   end if;
end Adabmp;
