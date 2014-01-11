package main

import (
	"fmt"
	"os"
	"strconv"
)

type Address struct {
	ip   string
	port int
}

func (a *Address) IP() string {
	return a.ip
}

func (a *Address) Port() int {
	return a.port
}

func NewAddress(ip string, port int) *Address {
	if ip == "localhost" || ip == "" {
		ip = "127.0.0.1"
	}
	return &Address{ip, port}
}

func ParseAddress(fields []string) *Address {
	if len(fields) != 2 {
		fmt.Fprintln(os.Stderr, "Error parsing Address: need form '<ip> <port>'")
		return nil
	}
	port, err := strconv.Atoi(fields[1])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error parsing Address: port (%s) is not a number\n", fields[1])
		return nil
	}

	return NewAddress(fields[0], port)
}

func (a *Address) String() string {
	return a.ip + ":" + strconv.Itoa(a.port)
}
