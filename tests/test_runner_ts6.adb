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

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Test Flash Bitstream Success",
            Flash_Target_Tests.Test_Flash_Bitstream_Success'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Test Flash Bitstream No Response",
            Flash_Target_Tests.Test_Flash_Bitstream_No_Response'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Test Flash Bitstream Bad File",
            Flash_Target_Tests.Test_Flash_Bitstream_Bad_File'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Test Flash Bitstream No File",
            Flash_Target_Tests.Test_Flash_Bitstream_No_File'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Test Flash Bitstream Failure",
            Flash_Target_Tests.Test_Flash_Bitstream_Failure'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Test Flash Firmware Success",
            Flash_Target_Tests.Test_Flash_Firmware_Success'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Test Flash Firmware No Response",
            Flash_Target_Tests.Test_Flash_Firmware_No_Response'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Test Flash Firmware Invalid Filename",
            Flash_Target_Tests.Test_Flash_Firmware_Invalid_Filename'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Test Flash Firmware No File",
            Flash_Target_Tests.Test_Flash_Firmware_No_File'Access));

      Ret.Add_Test
        (Flash_Target_Tests_Caller.Create
           ("Test Flash Firmware Failure",
            Flash_Target_Tests.Test_Flash_Firmware_Failure'Access));

      return Ret;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner_TS6;
