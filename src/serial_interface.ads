with Ada.Streams; use Ada.Streams;

package Serial_Interface is

   type Serial_Port is limited interface;

   procedure Open (Port : in out Serial_Port; Name : String) is abstract;

   procedure Close (Port : in out Serial_Port) is abstract;

   procedure Write (Port : in out Serial_Port; Data : Stream_Element_Array)
   is abstract;

   procedure Read
     (Port   : in out Serial_Port;
      Buffer : in out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is abstract;

end Serial_Interface;
