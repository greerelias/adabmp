with Board_Info;
with Serial_Interface;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;          use Ada.Streams;
with AUnit.Assertions;     use AUnit.Assertions;

package body Board_Info_Tests is

   ---------------------------------------------------------------------------
   -- Mock serial port
   ---------------------------------------------------------------------------
   type Mock_Serial_Port is
     new Serial_Interface.Serial_Port with record
        Response      : Stream_Element_Array (1 .. 256);
        Response_Last : Stream_Element_Offset := 0;
        Raise_On_Read : Boolean := False;
        No_Device     : Boolean := False;
     end record;

   --  ALL overrides must appear immediately after the type
   overriding
   procedure Open (Port : in out Mock_Serial_Port; Name : String);

   overriding
   procedure Close (Port : in out Mock_Serial_Port);

   overriding
   procedure Write
     (Port : in out Mock_Serial_Port;
      Data : Stream_Element_Array);

   overriding
   procedure Read
     (Port   : in out Mock_Serial_Port;
      Buffer : in out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   ---------------------------------------------------------------------------
   -- Override implementations
   ---------------------------------------------------------------------------
   procedure Open (Port : in out Mock_Serial_Port; Name : String) is
   begin
      null;
   end Open;

   procedure Close (Port : in out Mock_Serial_Port) is
   begin
      null;
   end Close;

   procedure Write
     (Port : in out Mock_Serial_Port;
      Data : Stream_Element_Array) is
   begin
      null;
   end Write;

   procedure Read
     (Port   : in out Mock_Serial_Port;
      Buffer : in out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      if Port.Raise_On_Read then
         raise Constraint_Error;
      elsif Port.No_Device then
         Last := 0;
      else
         Buffer (Buffer'First ..
                 Buffer'First + Port.Response_Last - 1) :=
           Port.Response (1 .. Port.Response_Last);

         Last := Buffer'First + Port.Response_Last - 1;
      end if;
   end Read;

   ---------------------------------------------------------------------------
   -- Helper to load mock response
   ---------------------------------------------------------------------------
   procedure Set_Response
     (Port : in out Mock_Serial_Port;
      Text : String) is
   begin
      Port.Response_Last := Text'Length;

      for I in Text'Range loop
         Port.Response (Stream_Element_Offset (I)) :=
           Stream_Element (Character'Pos (Text (I)));
      end loop;
   end Set_Response;

   ---------------------------------------------------------------------------
   -- Tests
   ---------------------------------------------------------------------------
   procedure Test_Board_Info_Success (T : in out Test) is
      Port : aliased Mock_Serial_Port;
      Info : Unbounded_String;
   begin
      Set_Response (Port, "TYPE=DEVBOARD;REV=A1");

      Board_Info.Get_Board_Info (Port, Info);

      Assert
      (To_String (Info) = "TYPE=DEVBOARD;REV=A1",
         "Get_Board_Info should return the FPGA board info string");
   end Test_Board_Info_Success;

   procedure Test_Board_Info_Not_Found (T : in out Test) is
      Port : aliased Mock_Serial_Port;
      Info : Unbounded_String;
   begin
      Port.No_Device := True;

      Board_Info.Get_Board_Info (Port, Info);

      Assert (False, "Get_Board_Info should raise Board_Not_Found when no device responds");
   exception
      when Board_Info.Board_Not_Found =>
         null;
   end Test_Board_Info_Not_Found;


   procedure Test_Board_Info_Format (T : in out Test) is
      Port : aliased Mock_Serial_Port;
      Info : Unbounded_String;
   begin
      Set_Response (Port, "GARBAGE DATA");

      Board_Info.Get_Board_Info (Port, Info);

      Assert (False, "Get_Board_Info should raise Board_Bad_Format for malformed responses");
   exception
      when Board_Info.Board_Bad_Format =>
         null;
   end Test_Board_Info_Format;


   procedure Test_Board_Info_Comm_Error (T : in out Test) is
      Port : aliased Mock_Serial_Port;
      Info : Unbounded_String;
   begin
      Port.Raise_On_Read := True;

      Board_Info.Get_Board_Info (Port, Info);

      Assert (False, "Get_Board_Info should raise Communication_Error on serial failure");
   exception
      when Board_Info.Communication_Error =>
         null;
   end Test_Board_Info_Comm_Error;


end Board_Info_Tests;
