with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with AUnit.Test_Caller;

with Connection_Tester_Tests;
with Device_Discovery_Tests;
with Board_Info_Tests;
with Configure_Target_Tests;
with UART_Tests;
with Flash_Target_Tests;

procedure Test_Runner_All_Unit is
   use AUnit.Test_Suites;

   package Connection_Caller is new
     AUnit.Test_Caller (Connection_Tester_Tests.Test);
   package Discovery_Caller is new
     AUnit.Test_Caller (Device_Discovery_Tests.Test);
   package Board_Info_Caller is new AUnit.Test_Caller (Board_Info_Tests.Test);
   package Configure_Target_Caller is new
     AUnit.Test_Caller (Configure_Target_Tests.Test);
   package UART_Tests_Caller is new AUnit.Test_Caller (UART_Tests.Test);

   package Flash_Target_Tests_Caller is new
     AUnit.Test_Caller (Flash_Target_Tests.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin

      -- Connection Tester Tests
      Ret.Add_Test
        (Connection_Caller.Create
           ("Test Successful Communication",
            Connection_Tester_Tests.Test_Successful_Communication'Access));
      Ret.Add_Test
        (Connection_Caller.Create
           ("Test Bad Response",
            Connection_Tester_Tests.Test_Bad_Response'Access));
      Ret.Add_Test
        (Connection_Caller.Create
           ("Test No Response",
            Connection_Tester_Tests.Test_No_Response'Access));
      Ret.Add_Test
        (Connection_Caller.Create
           ("Test Get Programmer Info",
            Connection_Tester_Tests.Test_Get_Programmer_Info'Access));
      Ret.Add_Test
        (Connection_Caller.Create
           ("Test Get Programmer Info Bad Response",
            Connection_Tester_Tests.Test_Get_Programmer_Info_Failure'Access));
      Ret.Add_Test
        (Connection_Caller.Create
           ("Test Get Programmer Info No Response",
            Connection_Tester_Tests
              .Test_Get_Programmer_Info_No_Response'Access));

      -- Device Discovery Tests
      Ret.Add_Test
        (Discovery_Caller.Create
           ("Test Find Device Success",
            Device_Discovery_Tests.Test_Find_Device_Success'Access));
      Ret.Add_Test
        (Discovery_Caller.Create
           ("Test Find Device Not Found",
            Device_Discovery_Tests.Test_Find_Device_Not_Found'Access));
      Ret.Add_Test
        (Discovery_Caller.Create
           ("Test Find Device USB Structure",
            Device_Discovery_Tests.Test_Find_Device_USB_Structure'Access));

      -- Board Info Tests
      Ret.Add_Test
        (Board_Info_Caller.Create
           ("Test Get Board Info Success",
            Board_Info_Tests.Test_Board_Info_Success'Access));
      Ret.Add_Test
        (Board_Info_Caller.Create
           ("Test Get Board Info Not Found",
            Board_Info_Tests.Test_Board_Info_Not_Found'Access));
      Ret.Add_Test
        (Board_Info_Caller.Create
           ("Test Get Board Info Format",
            Board_Info_Tests.Test_Board_Info_Format'Access));
      Ret.Add_Test
        (Board_Info_Caller.Create
           ("Test Get Board Info Communication Error",
            Board_Info_Tests.Test_Board_Info_Comm_Error'Access));

      -- Configure Target Tests
      Ret.Add_Test
        (Configure_Target_Caller.Create
           ("Test Configure Target Success",
            Configure_Target_Tests.Test_Configure_Target_Success'Access));
      Ret.Add_Test
        (Configure_Target_Caller.Create
           ("Test Configure Target No Response",
            Configure_Target_Tests.Test_Configure_Target_No_Response'Access));
      Ret.Add_Test
        (Configure_Target_Caller.Create
           ("Test Configure Target Bad File",
            Configure_Target_Tests.Test_Configure_Target_Bad_File'Access));
      Ret.Add_Test
        (Configure_Target_Caller.Create
           ("Test Configure Target No File Error",
            Configure_Target_Tests.Test_Configure_Target_No_File'Access));
      Ret.Add_Test
        (Configure_Target_Caller.Create
           ("Test Configure Target Failure",
            Configure_Target_Tests.Test_Configure_Target_Failure'Access));

      -- UART Tests
      Ret.Add_Test
        (UART_Tests_Caller.Create
           ("Start UART Success", UART_Tests.Test_UART_Success'Access));
      Ret.Add_Test
        (UART_Tests_Caller.Create
           ("Start UART No Response",
            UART_Tests.Test_UART_No_Response'Access));
      Ret.Add_Test
        (UART_Tests_Caller.Create
           ("Start UART Bad Response",
            UART_Tests.Test_UART_Bad_Response'Access));

      -- Flash Tests
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
end Test_Runner_All_Unit;
