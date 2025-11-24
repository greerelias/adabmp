with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Serial_Interface.Impl;
with Device_Discovery;
with Connection_Tester;

procedure Adabmp is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   Target_VID : constant String := "6666";
   Target_PID : constant String := "4242";
   Test_Cmd   : constant String := "PING";
   Test_Resp  : constant String := "PONG";

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

      Put_Line ("Sending command: " & Test_Cmd);

      Connection_Tester.Run_Test (Port, Test_Cmd, Test_Resp, Success, Msg);

      Put_Line (To_String (Msg));

      if Success then
         Put_Line ("Test PASSED.");
      else
         Put_Line ("Test FAILED.");
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

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Put_Line ("Usage: adabmp --test-connection");
      return;
   end if;

   if Ada.Command_Line.Argument (1) = "--test-connection" then
      Run_Connection_Test;
   else
      Put_Line ("Unknown argument: " & Ada.Command_Line.Argument (1));
   end if;
end Adabmp;
