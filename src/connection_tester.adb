with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Connection_Tester is

    procedure Run_Test
      (Port    : in out Serial_Interface.Serial_Port'Class;
      Command : String;
      Expect  : String;
      Success : out Boolean;
      Message : out Unbounded_String)
   is
      Buffer : String (1 .. 1024);
      Last   : Natural := 0;
      Total  : Natural := 0;
   begin
      Success := False;

      --  Send Command
      begin
         Port.Write (Command & ASCII.CR & ASCII.LF);
      exception
         when others =>
            Message := To_Unbounded_String ("Failed to write to port");
            return;
      end;

      --  Read Response
      --  We try to read a few times to get the full response
      for I in 1 .. 5 loop
         declare
            Chunk_Last : Natural;
            Chunk      : String (1 .. 256);
         begin
            Port.Read (Chunk, Chunk_Last);
            if Chunk_Last > 0 then
               if Total + Chunk_Last <= Buffer'Last then
                  Buffer (Total + 1 .. Total + Chunk_Last) :=
                    Chunk (1 .. Chunk_Last);
                  Total := Total + Chunk_Last;
               end if;
            end if;

            --  Check if we have the expected response
            if Ada.Strings.Fixed.Index
                 (Buffer (1 .. Total), Expect & ASCII.CR & ASCII.LF)
              > 0
            then
               Success := True;
               Message :=
                 To_Unbounded_String
                   ("Device responded correctly: " & Buffer (1 .. Total));
               return;
            end if;

            delay 0.1; --  Wait a bit for more data
         end;
      end loop;

      Message :=
        To_Unbounded_String
          ("Device did not respond with expected '"
           & Expect
           & "'. Received: "
           & Buffer (1 .. Total));
      Success := False;

   exception
      when others =>
         Message := To_Unbounded_String ("Exception during test execution");
         Success := False;
   end Run_Test;

end Connection_Tester;
