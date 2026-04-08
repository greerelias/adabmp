with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

package body Progress_Bar is
   package TIO renames Ada.Text_IO;

   task body Bar_Task is
      Running       : Boolean := True;
      Msg_Start     : String (1 .. 128);
      Msg_Start_Len : Natural := 0;
      Msg_Stop      : String (1 .. 128);
      Msg_Stop_Len  : Natural := 0;
      P_Total       : Volatile_Integer_Access;
      P_Sent        : Volatile_Integer_Access;
      P_Show_Bytes  : Boolean := False;
      Bar_Width     : constant Integer := 40;

      procedure Update is
         Percent      : Float;
         Filled_Count : Integer;
         Empty_Count  : Integer;
         Bar          : String (1 .. Bar_Width);
         Total        : Volatile_Integer;
         Sent         : Volatile_Integer;
      begin
         if P_Total /= null and P_Sent /= null then
            Total := P_Total.all;
            Sent := P_Sent.all;
         else
            return;
         end if;

         if Total = 0 then
            Percent := 100.0;
         else
            Percent := (Float (Sent) / Float (Total)) * 100.0;
         end if;

         Filled_Count := Integer (Float (Bar_Width) * (Percent / 100.0));
         if Filled_Count > Bar_Width then
            Filled_Count := Bar_Width;
         end if;
         Empty_Count := Bar_Width - Filled_Count;

         if Filled_Count > 0 then
            Move
              (Source => (1 .. Filled_Count => '='),
               Target => Bar (1 .. Filled_Count));
            if Filled_Count < Bar_Width then
               Bar (Filled_Count) := '>';
            end if;
         end if;

         if Empty_Count > 0 then
            Bar (Filled_Count + 1 .. Bar_Width) := (others => ' ');
         end if;

         TIO.Put
           (Ada.Characters.Latin_1.CR
            & "["
            & Bar
            & "] "
            & Integer (Percent)'Image
            & "% ");

         if P_Show_Bytes then
            TIO.Put ("(" & Sent'Image & " /" & Total'Image & " bytes)");
         end if;
         TIO.Flush;
      end Update;
   begin
      select
         accept Start
           (Start_Message : String;
            Stop_Message  : String;
            Total_Bytes   : Volatile_Integer_Access;
            Bytes_Sent    : Volatile_Integer_Access;
            Show_Bytes    : Boolean := False)
         do
            Msg_Start_Len := Start_Message'Length;
            if Msg_Start_Len > 128 then
               Msg_Start_Len := 128;
            end if;
            Msg_Start (1 .. Msg_Start_Len) :=
              Start_Message
                (Start_Message'First
                 .. Start_Message'First + Msg_Start_Len - 1);

            Msg_Stop_Len := Stop_Message'Length;
            if Msg_Stop_Len > 128 then
               Msg_Stop_Len := 128;
            end if;
            Msg_Stop (1 .. Msg_Stop_Len) :=
              Stop_Message
                (Stop_Message'First .. Stop_Message'First + Msg_Stop_Len - 1);

            P_Total := Total_Bytes;
            P_Sent := Bytes_Sent;
            P_Show_Bytes := Show_Bytes;

            TIO.Put_Line (Msg_Start (1 .. Msg_Start_Len));
         end Start;
      or
         accept Stop (Success : Boolean := True) do
            pragma Unreferenced (Success);
            Running := False;
         end Stop;
      or
         terminate;
      end select;

      while Running
        and then
          (P_Total /= null
           and then P_Sent /= null
           and then P_Sent.all <= P_Total.all)
      loop
         select
            accept Stop (Success : Boolean := True) do
               Update; -- Ensure final status is printed
               Running := False;
               if Success then
                  TIO.Put_Line
                    (Ada.Characters.Latin_1.LF & Msg_Stop (1 .. Msg_Stop_Len));
               end if;
            end Stop;
         or
            delay 0.050;
            Update;
         end select;
      end loop;

      -- Consume any final Stop signal if we exited due to completion
      if Running then
         select
            accept Stop (Success : Boolean := True) do
               Update; -- Ensure final status is printed
               if Success then
                  TIO.Put_Line
                    (Ada.Characters.Latin_1.LF & Msg_Stop (1 .. Msg_Stop_Len));
               end if;
            end Stop;
         or
            terminate;
         end select;
      end if;
   end Bar_Task;

end Progress_Bar;
