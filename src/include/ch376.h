#define CH376_DATA            $340
#define CH376_COMMAND         $341
#define CH376_DETECTED        $AA

#define CH376_CMD_NONE   		  $00
#define CH376_GET_IC_VER 		  $01
#define CH376_SET_BAUDRATE 		$02
#define CH376_GET_ENTER_SLEEP	$03
#define CH376_RESET_ALL 		  $05
#define CH376_CHECK_EXIST 		$06
#define CH376_GET_FILE_SIZE 	$0C ; Get the current file length
#define CH376_SET_USB_MODE 		$15
#define CH376_GET_STATUS 		  $22
#define CH376_RD_USB_DATA0 		$27
#define CH376_WR_USB_DATA 		$2C
#define CH376_CMD_WR_REQ_DATA $2D
#define CH376_SET_FILE_NAME 	$2F
#define CH376_DISK_CONNECT 		$30 ; check the disk connection status
#define CH376_DISK_MOUNT 		  $31
#define CH376_FILE_OPEN 		  $32
#define CH376_FILE_ENUM_GO 		$33
#define CH376_CMD_FILE_CREATE $34
#define CH376_FILE_ERASE		  $35
#define CH376_FILE_CLOSE 		  $36
#define CH376_BYTE_LOCATE 		$39
#define CH376_BYTE_READ 		  $3A
#define CH376_BYTE_RD_GO 		  $3B
#define CH376_BYTE_WRITE 		  $3C
#define CH376_BYTE_WR_GO		  $3D
#define CH376_DISK_CAPACITY 	$3E
#define CH376_DISK_QUERY   		$3F
#define CH376_DIR_CREATE      $40

#define CH376_DISK_RD_GO 		  $55

/*CODE FOR CH376_SET_USB_MODE **************************************************/

#define CH376_SET_USB_MODE_CODE_SDCARD                              $03
// The code of 06H means switch to valid USB-HOST, produce SOF package automatically. 
#define CH376_SET_USB_MODE_CODE_USB_HOST_SOF_PACKAGE_AUTOMATICALLY  $06

#define CH376_USB_INT_SUCCESS 		$14
#define CH376_USB_INT_CONNECT 		$15
#define CH376_USB_INT_DISCONNECT	$16
#define CH376_USB_INT_BUF_OVER 		$17
#define CH376_USB_INT_USB_READY 	$18
#define CH376_USB_INT_DISK_READ 	$1d
#define CH376_USB_INT_DISK_WRITE 	$1e
#define CH376_USB_INT_DISK_ERR 		$1f

#define CH376_ERR_OPEN_DIR 			$41
#define CH376_ERR_MISS_FILE 		$42
#define CH376_ERR_FOUND_NAME 		$43
#define CH376_ERR_DISK_DISCON 	$82
#define CH376_ERR_LARGE_SECTOR 	$84
#define CH376_ERR_TYPE_ERROR 		$92
#define CH376_ERR_BPB_ERROR 		$A1
#define CH376_ERR_DISK_FULL 		$B1
#define CH376_ERR_FDT_OVER 			$B2
#define CH376_ERR_FILE_CLOSE 		$B4
