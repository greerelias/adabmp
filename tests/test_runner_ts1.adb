with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;
with Connection_Tester_Tests;
with Device_Discovery_Tests;
with Board_Info_Tests;
with Protocol_Tests;
with AUnit.Test_Caller;

procedure Test_Runner_TS1 is
   use AUnit.Test_Suites;

   package Connection_Caller is new
     AUnit.Test_Caller (Connection_Tester_Tests.Test);
   package Discovery_Caller is new
     AUnit.Test_Caller (Device_Discovery_Tests.Test);
   package Protocol_Caller is new 
     AUnit.Test_Caller (Protocol_Tests.Test);
   package Board_Info_Caller is new
     AUnit.Test_Caller (Board_Info_Tests.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      -- Protocol Tests
      Ret.Add_Test
        (Protocol_Caller.Create
           ("Test Encode Basic", Protocol_Tests.Test_Encode_Basic'Access));
      Ret.Add_Test
        (Protocol_Caller.Create
           ("Test Encode With Zeros",
            Protocol_Tests.Test_Encode_With_Zeros'Access));
      Ret.Add_Test
        (Protocol_Caller.Create
           ("Test Decode Basic", Protocol_Tests.Test_Decode_Basic'Access));
      Ret.Add_Test
        (Protocol_Caller.Create
           ("Test Decode With Zeros",
            Protocol_Tests.Test_Decode_With_Zeros'Access));
      Ret.Add_Test
        (Protocol_Caller.Create
           ("Test Round Trip", Protocol_Tests.Test_Round_Trip'Access));
      Ret.Add_Test
        (Protocol_Caller.Create
           ("Test Decode Error Zero Byte",
            Protocol_Tests.Test_Decode_Error_Zero_Byte'Access));
      Ret.Add_Test
        (Protocol_Caller.Create
           ("Test Decode Error Length",
            Protocol_Tests.Test_Decode_Error_Length'Access));

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

      -- Get Board Info Tests
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

      return Ret;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner_TS1;
