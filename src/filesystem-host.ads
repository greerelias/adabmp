package Filesystem.Host is

   type Host_Filesystem is new Filesystem.Instance with null record;

   overriding
   function Read_USB_File
     (Self : Host_Filesystem; Path : String) return String;

   overriding
   function Exists (Self : Host_Filesystem; Path : String) return Boolean;
   overriding
   function List_Directory
     (Self : Host_Filesystem; Path : String) return Filesystem.String_Array;

end Filesystem.Host;
