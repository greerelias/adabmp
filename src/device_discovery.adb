with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Device_Discovery is

   function Read_File_Content (Path : String) return String is
      File : Ada.Text_IO.File_Type;
      Line : String (1 .. 100);
      Last : Natural;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Path);
      Ada.Text_IO.Get_Line (File, Line, Last);
      Ada.Text_IO.Close (File);
      return Line (1 .. Last);
   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
         return "";
   end Read_File_Content;

   function Find_Device (Vendor_ID, Product_ID : String) return String is
      use Ada.Directories;
      Search   : Search_Type;
      Dir_Ent  : Directory_Entry_Type;
      Sys_Path : constant String := "/sys/class/tty";

      function Check_Device (Name : String) return Boolean is
         Dev_Path : constant String := Sys_Path & "/" & Name & "/device";
         Vid_Path : constant String := Dev_Path & "/../idVendor";
         Pid_Path : constant String := Dev_Path & "/../idProduct";

         Vid : String (1 .. 10);
         Pid : String (1 .. 10);
      begin
         --  Check if device link exists
         if not Exists (Dev_Path) then
            return False;
         end if;

         --  Read VID/PID
         --  Note: The path might vary slightly depending on kernel/driver,
         --  but usually it's in the parent USB device.
         --  Sometimes it is directly in device/idVendor if it's a USB device.
         --  Let's try device/idVendor first, then device/../idVendor.

         declare
            V : constant String := Read_File_Content (Dev_Path & "/idVendor");
            P : constant String := Read_File_Content (Dev_Path & "/idProduct");
         begin
            if V = Vendor_ID and then P = Product_ID then
               return True;
            end if;
         end;

         declare
            V : constant String := Read_File_Content (Vid_Path);
            P : constant String := Read_File_Content (Pid_Path);
         begin
            if V = Vendor_ID and then P = Product_ID then
               return True;
            end if;
         end;

         return False;
      end Check_Device;

   begin
      Start_Search (Search, Sys_Path, "");

      while More_Entries (Search) loop
         Get_Next_Entry (Search, Dir_Ent);
         declare
            Name : constant String := Simple_Name (Dir_Ent);
         begin
            if (Name'Length >= 6 and then Name (1 .. 6) = "ttyACM")
              or else (Name'Length >= 6 and then Name (1 .. 6) = "ttyUSB")
            then
               if Check_Device (Name) then
                  return "/dev/" & Name;
               end if;
            end if;
         end;
      end loop;

      raise Device_Not_Found;
   end Find_Device;

end Device_Discovery;
