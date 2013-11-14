// client
package main

import (
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"strconv"
)

var (
	ip   = flag.String("ip", "localhost", "Server Ip Address")
	port = flag.Int("port", 1337, "Server port")
	msg  = flag.String("msg", "", "Message to send. if empty, input stream is being send")
)

func main() {
	flag.Parse()

	c, err := net.Dial("tcp", *ip+":"+strconv.Itoa(*port))
	if err != nil {
		fmt.Errorf("Dialing Error: %s\n", err)
		return
	}
	defer c.Close()

	if *msg != "" {
		n, err := c.Write([]byte(*msg))
		if err != nil {
			fmt.Errorf("Writing Error: %s\n", err)
			return
		}
		if n != len(*msg) {
			fmt.Errorf("Writing size Mismatch: %d != %d \n", n, len(*msg))
			return
		}
	} else {
		_, err = io.Copy(c, os.Stdin)
		if err != nil {
			fmt.Errorf("Copy Error: %s\n", err)
			return
		}
	}
}
