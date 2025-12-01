with Filesystem.Host;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Device_Discovery is

   function Find_Device
     (Vendor_ID, Product_ID : String;
      FS                    : access Filesystem.Instance'Class := null)
      return String
   is
      Default_FS : aliased Filesystem.Host.Host_Filesystem;
      Actual_FS  : access Filesystem.Instance'Class := FS;

      Sys_Path : constant String := "/sys/class/tty";

      function Check_Device (Name : String) return Boolean is
         Dev_Path : constant String := Sys_Path & "/" & Name & "/device";
         Vid_Path : constant String := Dev_Path & "/../idVendor";
         Pid_Path : constant String := Dev_Path & "/../idProduct";
      begin
         if not Actual_FS.Exists (Dev_Path) then
            return False;
         end if;

         declare
            V : constant String :=
              Actual_FS.Read_USB_File (Dev_Path & "/idVendor");
            P : constant String :=
              Actual_FS.Read_USB_File (Dev_Path & "/idProduct");
         begin
            if V = Vendor_ID and then P = Product_ID then
               return True;
            end if;
         end;

         declare
            V : constant String := Actual_FS.Read_USB_File (Vid_Path);
            P : constant String := Actual_FS.Read_USB_File (Pid_Path);
         begin
            if V = Vendor_ID and then P = Product_ID then
               return True;
            end if;
         end;

         return False;
      end Check_Device;

   begin
      if Actual_FS = null then
         Actual_FS := Default_FS'Access;
      end if;

      declare
         List : constant Filesystem.String_Array :=
           Actual_FS.List_Directory (Sys_Path);
      begin
         for I in List'Range loop
            declare
               Name : constant String := To_String (List (I));
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
      end;

      raise Device_Not_Found;
   end Find_Device;

end Device_Discovery;
