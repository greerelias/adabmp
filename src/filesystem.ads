with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Filesystem is

   type String_Array is array (Positive range <>) of Unbounded_String;

   type Instance is interface;

   function Read_File (Self : Instance; Path : String) return String
   is abstract;

   function Exists (Self : Instance; Path : String) return Boolean is abstract;

   function List_Directory (Self : Instance; Path : String) return String_Array
   is abstract;

end Filesystem;
