with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with Flash_Target_Tests;
with AUnit.Test_Caller;

procedure Test_Runner_TS6 is
   use AUnit.Test_Suites;

   package Flash_Target_Tests_Caller is new
     AUnit.Test_Caller (Flash_Target_Tests.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      -- Flash Target Tests
      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Load SPI over JTAG Success",
            Flash_Target_Tests.Test_Load_SPI_JTAG_Success'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Load SPI over JTAG No Response",
            Flash_Target_Tests.Test_Load_SPI_JTAG_No_Response'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Load SPI over JTAG Bad File",
            Flash_Target_Tests.Test_Load_SPI_JTAG_Bad_File'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Load SPI over JTAG No File",
            Flash_Target_Tests.Test_Load_SPI_JTAG_No_File'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Load SPI over JTAG Failure",
            Flash_Target_Tests.Test_Load_SPI_JTAG_Failure'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Flash Bitstream Success",
            Flash_Target_Tests.Test_Flash_Bitstream_Success'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Flash Bitstream No Response",
            Flash_Target_Tests.Test_Flash_Bitstream_No_Response'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Flash Bitstream Bad File",
            Flash_Target_Tests.Test_Flash_Bitstream_Bad_File'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Flash Bitstream No File",
            Flash_Target_Tests.Test_Flash_Bitstream_No_File'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Flash Bitstream Failure",
            Flash_Target_Tests.Test_Flash_Bitstream_Failure'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Flash Firmware Success",
            Flash_Target_Tests.Test_Flash_Firmware_Success'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Flash Firmware No Response",
            Flash_Target_Tests.Test_Flash_Firmware_No_Response'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Flash Firmware Invalid Filename",
            Flash_Target_Tests.Test_Flash_Firmware_Invalid_Filename'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Flash Firmware No File",
            Flash_Target_Tests.Test_Flash_Firmware_No_File'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Start Flash Firmware Failure",
            Flash_Target_Tests.Test_Flash_Firmware_Failure'Access));

      return Ret;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner_TS6;
