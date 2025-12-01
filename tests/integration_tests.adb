with AUnit.Assertions;      use AUnit.Assertions;
with Serial_Interface.Impl;
with Connection_Tester;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Device_Discovery;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

package body Integration_Tests is

   procedure Test_Find_Device_Integration (T : in out Test) is
      Prefix : constant String := "/dev/ttyACM";
   begin
      declare
         Result : constant String :=
           Device_Discovery.Find_Device ("6666", "4242");
      begin
         -- Check that it starts with "/dev/ttyACM"
         Assert
           (Ada.Strings.Fixed.Index (Result, Prefix) = Result'First,
            "Result should start with " & Prefix & ", got: " & Result);

         -- Check that the rest is a number
         declare
            Suffix : constant String :=
              Result (Result'First + Prefix'Length .. Result'Last);
         begin
            Assert (Suffix'Length > 0, "Result should have a numeric suffix");
            for C of Suffix loop
               Assert
                 (Ada.Characters.Handling.Is_Digit (C),
                  "Suffix should be numeric, got: " & Suffix);
            end loop;
         end;
         Put_Line ("Integration Test Passed: " & Result);
      end;
   end Test_Find_Device_Integration;

   procedure Test_Get_Programmer_Info_Integration (T : in out Test) is
      Port    : Serial_Interface.Impl.Com_Port;
      Success : Boolean;
      Info    : Unbounded_String;

      Target_VID : constant String := "6666";
      Target_PID : constant String := "4242";
      Expected   : constant String := "AdaBMP v0.1.0";
      Port_Name  : Unbounded_String;
   begin
      -- 1. Find Device
      begin
         Port_Name :=
           To_Unbounded_String
             (Device_Discovery.Find_Device (Target_VID, Target_PID));
      exception
         when Device_Discovery.Device_Not_Found =>
            Assert
              (False,
               "Integration Test Failed: Device not found (VID="
               & Target_VID
               & ", PID="
               & Target_PID
               & ")");
            return;
      end;

      -- 2. Open Port
      begin
         Port.Open (To_String (Port_Name));
      exception
         when others =>
            Assert
              (False,
               "Integration Test Failed: Could not open port "
               & To_String (Port_Name));
            return;
      end;

      -- 3. Run Test
      Connection_Tester.Get_Programmer_Info (Port, Success, Info);

      -- 4. Cleanup
      Port.Close;

      -- 5. Assertions
      Assert
        (Success,
         "Get_Programmer_Info failed (Success=False). Info: "
         & To_String (Info));
      Assert
        (To_String (Info) = Expected,
         "Expected '" & Expected & "', got '" & To_String (Info) & "'");

      Put_Line ("Integration Test Passed: " & To_String (Info));
   end Test_Get_Programmer_Info_Integration;

   procedure Test_Run_Test_Integration (T : in out Test) is
      Port    : Serial_Interface.Impl.Com_Port;
      Success : Boolean;
      Msg     : Unbounded_String;

      Target_VID : constant String := "6666";
      Target_PID : constant String := "4242";
      Port_Name  : Unbounded_String;
   begin
      -- 1. Find Device
      begin
         Port_Name :=
           To_Unbounded_String
             (Device_Discovery.Find_Device (Target_VID, Target_PID));
      exception
         when Device_Discovery.Device_Not_Found =>
            Assert
              (False,
               "Integration Test Failed: Device not found (VID="
               & Target_VID
               & ", PID="
               & Target_PID
               & ")");
            return;
      end;

      -- 2. Open Port
      begin
         Port.Open (To_String (Port_Name));
      exception
         when others =>
            Assert
              (False,
               "Integration Test Failed: Could not open port "
               & To_String (Port_Name));
            return;
      end;

      -- 3. Run Test
      -- Note: Run_Test now expects a handshake. The real device might not be programmed to respond to Test_Connection yet.
      -- If the integration test fails here, it means the device firmware needs to be updated to support the handshake.
      Connection_Tester.Run_Test (Port, Success, Msg);

      -- 4. Cleanup
      Port.Close;

      -- 5. Assertions
      Assert
        (Success,
         "Run_Test failed (Success=False). Message: " & To_String (Msg));
      Assert
        (To_String (Msg) = "SUCCESS: Data integrity verified.",
         "Expected 'SUCCESS: Data integrity verified.', got '"
         & To_String (Msg)
         & "'");

      Put_Line ("Integration Test Passed: " & To_String (Msg));
   end Test_Run_Test_Integration;

end Integration_Tests;
