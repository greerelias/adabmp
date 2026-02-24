with Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded;
with Board_Info;
with Commands;
with Configure_Target;
with Protocol;
with Serial_Interface.Impl;
with Device_Discovery;
with Connection_Tester;

with Ada.Exceptions; use Ada.Exceptions;

procedure Adabmp is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   Target_VID : constant String := "6666";
   Target_PID : constant String := "4242";

   procedure Run_Connection_Test is
      Port_Name : String (1 .. 100);
      Port_Len  : Natural := 0;
      Port      : Serial_Interface.Impl.Com_Port;
      Success   : Boolean;
      Msg       : Unbounded_String;
   begin
      Put_Line
        ("Searching for device (VID="
         & Target_VID
         & ", PID="
         & Target_PID
         & ")...");

      begin
         declare
            Found : constant String :=
              Device_Discovery.Find_Device (Target_VID, Target_PID);
         begin
            if Found'Length > Port_Name'Length then
               Put_Line ("Error: Port name too long.");
               return;
            end if;
            Port_Name (1 .. Found'Length) := Found;
            Port_Len := Found'Length;
         end;
      exception
         when Device_Discovery.Device_Not_Found =>
            Put_Line ("Error: Device not found.");
            return;
      end;

      Put_Line ("Device found at " & Port_Name (1 .. Port_Len));
      Put_Line ("Opening port...");

      begin
         Port.Open (Port_Name (1 .. Port_Len));
      exception
         when others =>
            Put_Line ("Error: Failed to open serial port.");
            return;
      end;

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
      end if;

      Port.Close;
   exception
      when others =>
         Put_Line ("An unexpected error occurred.");
         if Port_Len > 0 then
            begin
               Port.Close;
            exception
               when others =>
                  null;
            end;
         end if;
   end Run_Connection_Test;

   procedure Run_Get_Board_Info is
      Port_Name : String (1 .. 100);
      Port_Len  : Natural := 0;
      Port      : Serial_Interface.Impl.Com_Port;
      Info      : Board_Info.Board_Info_Record_Access;
      Success   : Boolean;
   begin
      begin
         declare
            Found : constant String :=
              Device_Discovery.Find_Device (Target_VID, Target_PID);
         begin
            if Found'Length > Port_Name'Length then
               Put_Line ("Error: Port name too long.");
               return;
            end if;
            Port_Name (1 .. Found'Length) := Found;
            Port_Len := Found'Length;
         end;
      exception
         when Device_Discovery.Device_Not_Found =>
            Put_Line ("Error: Device not found.");
            return;
      end;

      begin
         Port.Open (Port_Name (1 .. Port_Len));
      exception
         when others =>
            Put_Line ("Error: Failed to open serial port.");
            return;
      end;

      Board_Info.Get_Board_Info (Port, Info, Success);

      Port.Close;
   exception
      when E : others =>
         Put_Line ("Exception: " & Exception_Name (E));
         Put_Line ("Message:   " & Exception_Message (E));
         Put_Line ("Info:      " & Exception_Information (E));
         if Port_Len > 0 then
            begin
               Port.Close;
            exception
               when others =>
                  null;
            end;
         end if;
   end Run_Get_Board_Info;

   procedure Run_Configure_Target (Path : String) is
      Port_Name : String (1 .. 100);
      Port_Len  : Natural := 0;
      Port      : Serial_Interface.Impl.Com_Port;
      Success   : Boolean;
      Msg       : Unbounded_String;
   begin
      begin
         declare
            Found : constant String :=
              Device_Discovery.Find_Device (Target_VID, Target_PID);
         begin
            if Found'Length > Port_Name'Length then
               Put_Line ("Error: Port name too long.");
               return;
            end if;
            Port_Name (1 .. Found'Length) := Found;
            Port_Len := Found'Length;
         end;
      exception
         when Device_Discovery.Device_Not_Found =>
            Put_Line ("Error: Device not found.");
            return;
      end;

      begin
         Port.Open (Port_Name (1 .. Port_Len));
      exception
         when others =>
            Put_Line ("Error: Failed to open serial port.");
            return;
      end;

      Configure_Target.Load_Bitstream (Port, Path, Success, True);
      Port.Close;
   exception
      when E : others =>
         Put_Line ("Exception: " & Exception_Name (E));
         Put_Line ("Message:   " & Exception_Message (E));
         Put_Line ("Info:      " & Exception_Information (E));
         if Port_Len > 0 then
            begin
               Port.Close;
            exception
               when others =>
                  null;
            end;
         end if;
   end Run_Configure_Target;

   procedure Run_UART is
      Port_Name : String (1 .. 100);
      Port_Len  : Natural := 0;
      Port      : Serial_Interface.Impl.Com_Port;
   begin
      -- TODO: move getting port name/opening port into their own functions
      -- this code is repeated multiple times
      ---------------------------------------------------------------------------------
      begin
         declare
            Found : constant String :=
              Device_Discovery.Find_Device (Target_VID, Target_PID);
         begin
            if Found'Length > Port_Name'Length then
               Put_Line ("Error: Port name too long.");
               return;
            end if;
            Port_Name (1 .. Found'Length) := Found;
            Port_Len := Found'Length;
         end;
      exception
         when Device_Discovery.Device_Not_Found =>
            Put_Line ("Error: Device not found.");
            return;
      end;

      begin
         Port.Open (Port_Name (1 .. Port_Len));
      exception
         when others =>
            Put_Line ("Error: Failed to open serial port.");
            return;
      end;
      Protocol.Send_Command_Packet (Port, Commands.Start_UART);
      delay (0.002);
      if Protocol.Receive_Ready_Packet (Port) then
         Put_Line ("UART Ready on: " & Port_Name (1 .. Port_Len));
      else
         Put_Line ("Error: Failed start UART");
      end if;
      Port.Close;
   end Run_UART;

begin
   if Argument_Count = 0 then
      Put_Line ("--- Commands ---");
      Put_Line
        ("'adabmp --test-connection'  : Tests connection to programmer");
      Put_Line
        ("'adabmp --board-info'       : Retrieves information about target board");
      return;
   end if;

   if Argument (1) = "--test-connection" then
      Run_Connection_Test;
   elsif Argument (1) = "--board-info" then
      Run_Get_Board_Info;
   elsif Argument (1) = "-ct" and then Argument_Count > 1 then
      Run_Configure_Target (Argument (2));
   elsif Argument (1) = "--uart" or Argument (1) = "-u" then
      Run_UART;
   else
      Put_Line ("Unknown argument: " & Argument (1));
   end if;
end Adabmp;
