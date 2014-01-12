package main

import "fmt"

type Address struct {
	IP   string
	Port int
}

func NewAddress(ip string, port int) *Address {
	if ip == "localhost" || ip == "" {
		ip = "127.0.0.1"
	}
	return &Address{ip, port}
}

func (a *Address) String() string {
	return fmt.Sprintf("%s:%d", a.IP, a.Port)
}
