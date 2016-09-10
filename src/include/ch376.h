

#define CH376_DATA $340
#define CH376_COMMAND $341
#define CH376_DETECTED $AA


#define CH376_GET_IC_VER 1
#define CH376_SET_BAUDRATE 2
#define CH376_GET_ENTER_SLEEP 3

#define CH376_RESET_ALL 5
#define CH376_CHECK_EXIST 6

#define CH376_GET_FILE_SIZE $0C ; Get the current file length

#define CH376_SET_USB_MODE $15
#define CH376_GET_STATUS $22

#define CH376_SET_FILE_NAME $2f

#define CH376_RD_USB_DATA0 $27

#define CH376_DISK_CONNECT $30 ; check the disk connection status
#define CH376_DISK_MOUNT $31
#define CH376_FILE_OPEN $32
#define CH376_FILE_ENUM_GO $33
#define CH376_FILE_CLOSE $36

#define CH376_BYTE_READ $3a

#define CH376_DISK_CAPACITY $3E

#define CH376_SET_FILE_NAME $2F


#define CH376_DISK_RD_GO $55

/*CODE FOR CH376_SET_USB_MODE **************************************************/

// The code of 06H means switch to valid USB-HOST, produce SOF package automatically. 
#define CH376_SET_USB_MODE_CODE_USB_HOST_SOF_PACKAGE_AUTOMATICALLY $06




#define CH376_USB_INT_SUCCESS $14
