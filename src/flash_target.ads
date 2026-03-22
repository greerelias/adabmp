with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Serial_Interface;
with Interfaces;            use Interfaces;

package Flash_Target is
   SPI_JTAG_BS_Path : constant String := "spiOverJtag_xc7a35tcpg236.bit";
   procedure Load_SPI_Over_Jtag
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Success : in out Boolean;
      Path    : in String := SPI_JTAG_BS_Path;
      Verbose : Boolean := False);
   procedure Flash_Bitstream
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Path    : in String;
      Success : in out Boolean;
      Verbose : Boolean := False);
   procedure Flash_Firmware
     (Port         : in out Serial_Interface.Serial_Port'Class;
      Path         : in String;
      Success      : in out Boolean;
      Base_Address : Unsigned_32 := 0;
      Verbose      : Boolean := False);
private
   procedure Flash
     (Port         : in out Serial_Interface.Serial_Port'Class;
      Path         : in String;
      Data_Size    : in Unsigned_32;
      Success      : in out Boolean;
      Data_Offset  : in Ada.Streams.Stream_IO.Count := 1;
      Base_Address : Unsigned_32 := 0;
      Verbose      : Boolean := False);

   function Get_Erase_Delay (Size : Unsigned_32) return Duration;
end Flash_Target;
