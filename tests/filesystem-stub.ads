with Filesystem;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Hash;

package Filesystem.Stub is

   type Mock_Filesystem is new Filesystem.Instance with private;

   overriding
   function Read_USB_File
     (Self : Mock_Filesystem; Path : String) return String;

   overriding
   function Exists (Self : Mock_Filesystem; Path : String) return Boolean;
   overriding
   function List_Directory
     (Self : Mock_Filesystem; Path : String) return Filesystem.String_Array;

   -- Helper to setup the mock
   procedure Add_File
     (Self : in out Mock_Filesystem; Path : String; Content : String);
   procedure Add_Directory (Self : in out Mock_Filesystem; Path : String);
   procedure Add_Directory_Entry
     (Self : in out Mock_Filesystem; Dir : String; Name : String);

private

   function Hash (Key : Unbounded_String) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash (To_String (Key)));

   package File_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Unbounded_String,
        Hash            => Hash,
        Equivalent_Keys => "=");

   package Dir_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Filesystem.String_Array,
        Hash            => Hash,
        Equivalent_Keys => "=");

   type Mock_Filesystem is new Filesystem.Instance with record
      Files : File_Maps.Map;
      Dirs  : Dir_Maps.Map;
   end record;

end Filesystem.Stub;
