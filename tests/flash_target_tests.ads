with AUnit.Test_Fixtures;

package Flash_Target_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Load_SPI_JTAG_Success (T : in out Test);
   procedure Test_Load_SPI_JTAG_No_Response (T : in out Test);
   procedure Test_Load_SPI_JTAG_Bad_File (T : in out Test);
   procedure Test_Load_SPI_JTAG_No_File (T : in out Test);
   procedure Test_Load_SPI_JTAG_Failure (T : in out Test);
   procedure Test_Flash_Bitstream_Success (T : in out Test);
   procedure Test_Flash_Bitstream_No_Response (T : in out Test);
   procedure Test_Flash_Bitstream_Bad_File (T : in out Test);
   procedure Test_Flash_Bitstream_No_File (T : in out Test);
   procedure Test_Flash_Bitstream_Failure (T : in out Test);
   procedure Test_Flash_Firmware_Success (T : in out Test);
   procedure Test_Flash_Firmware_No_Response (T : in out Test);
   procedure Test_Flash_Firmware_Invalid_Filename (T : in out Test);
   procedure Test_Flash_Firmware_No_File (T : in out Test);
   procedure Test_Flash_Firmware_Failure (T : in out Test);

end Flash_Target_Tests;
