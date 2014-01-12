package main

import "fmt"

var LocalIP = [4]byte{127, 0, 0, 1}

type Address struct {
	IP   [4]byte
	Port int
}

func NewAddress(ip [4]byte, port int) *Address {
	return &Address{ip, port}
}

func (a *Address) String() string {
	if a.IP == LocalIP {
		return fmt.Sprintf(":%d", a.Port)
	}
	return fmt.Sprintf("%d.%d.%d.%d:%d", a.IP[0], a.IP[1], a.IP[2], a.IP[3], a.Port)
}

func ParseIP(ipstring string) ([4]byte, error) {
	if ipstring == "localhost" || ipstring == "" {
		return LocalIP, nil
	}
	var ip [4]byte
	_, err := fmt.Sscan(ipstring, "%d.%d.%d.%d", &ip[0], &ip[1], &ip[2], &ip[3])
	return ip, err
}
