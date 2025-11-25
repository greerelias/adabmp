with Ada.Streams; use Ada.Streams;
with GNAT.Serial_Communications;

package Serial_Interface.Impl is

   type Com_Port is limited new Serial_Interface.Serial_Port with private;

   overriding
   procedure Open (Port : in out Com_Port; Name : String);

   overriding
   procedure Close (Port : in out Com_Port);

   overriding
   procedure Write (Port : in out Com_Port; Data : String);

   overriding
   procedure Write (Port : in out Com_Port; Data : Stream_Element_Array);

   overriding
   procedure Read
     (Port : in out Com_Port; Buffer : out String; Last : out Natural);

   procedure Read
     (Port   : in out Com_Port;
      Buffer : in out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

private
   type Com_Port is limited new Serial_Interface.Serial_Port with record
      Port : GNAT.Serial_Communications.Serial_Port;
   end record;

end Serial_Interface.Impl;
