with Ada.Streams;
with Serial_Interface;
with Ada.Strings.Unbounded;
with Interfaces;       use Interfaces;
with Packet_Formatter; use Packet_Formatter;
with Ada.Streams;      use Ada.Streams;
with Commands;

package Board_Info is


   -- TODO: Ask Olivier about needing to use these custom bit types?
   type Unsigned_11 is mod 2 ** 11;
   type Unsigned_4 is mod 2 ** 4;

   type Board_Info_Record is record
      Mandatory_Bit : Boolean;
      Manufacturer  : Unsigned_11;
      Part_Number   : Unsigned_16;
      Version       : Unsigned_4;
   end record
   with Size => 32;

   for Board_Info_Record use
     record
       Mandatory_Bit at 0 range 0 .. 0;
       Manufacturer  at 0 range 1 .. 11;
       Part_Number   at 0 range 12 .. 27;
       Version       at 0 range 28 .. 31;
     end record;


   type Board_Info_Record_Access (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            Values : Board_Info_Record;

         when True =>
            Bytes : Ada.Streams.Stream_Element_Array (1 .. 4);
      end case;
   end record
   with Unchecked_Union;

   Board_Not_Found     : exception;
   Board_Bad_Format    : exception;
   Communication_Error : exception;

   procedure Get_Board_Info
     (Port : in out Serial_Interface.Serial_Port'Class; Info: out Board_Info_Record_Access; Success : out Boolean);

   
   procedure Print_Board_Info
      (Info : in Board_Info_Record_Access);


end Board_Info;
