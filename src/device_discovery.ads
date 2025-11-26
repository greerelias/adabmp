with Filesystem;

package Device_Discovery is

   Device_Not_Found : exception;

   --  Returns the path to the serial device (e.g., "/dev/ttyACM0")
   --  Raises Device_Not_Found if no matching device is found.
   --  If FS is null, uses the host filesystem.
   function Find_Device
     (Vendor_ID, Product_ID : String;
      FS                    : access Filesystem.Instance'Class := null)
      return String;

end Device_Discovery;
