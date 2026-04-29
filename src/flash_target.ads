with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Serial_Interface;
with Interfaces;            use Interfaces;

package Flash_Target is
   -- Path to SPI over JTAG bitstream for Basys 3 board
   SPI_JTAG_BS_Path : constant String :=
     "./spi_jtag/spiOverJtag_xc7a35tcpg236.bit";
   Block_Size_64    : constant Unsigned_32 := 2 ** 16;
   Block_Size_32    : constant Unsigned_32 := 2 ** 15;
   Sector_Size      : constant Unsigned_32 := 2 ** 12;
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
   procedure Erase_Flash
     (Port         : in out Serial_Interface.Serial_Port'Class;
      Data_Size    : in Unsigned_32;
      Success      : in out Boolean;
      Base_Address : Unsigned_32 := 0;
      Verbose      : Boolean := False);
   procedure Flash
     (Port         : in out Serial_Interface.Serial_Port'Class;
      Path         : in String;
      Data_Size    : in Unsigned_32;
      Success      : in out Boolean;
      Data_Offset  : in Ada.Streams.Stream_IO.Count := 1;
      Base_Address : Unsigned_32 := 0;
      Verbose      : Boolean := False);

end Flash_Target;
