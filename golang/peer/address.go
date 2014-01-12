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
	//if a.IP == LocalIP {
	//	return fmt.Sprintf(":%d", a.Port)
	//}
	return fmt.Sprintf("%d.%d.%d.%d:%d", a.IP[0], a.IP[1], a.IP[2], a.IP[3], a.Port)
}

func ParseAddress(text string) (*Address, error) {
	var port int
	if len(text) > 1 && text[:1] == ":" {
		_, err := fmt.Sscanf(text[1:], "%d", &port)
		return NewAddress(LocalIP, port), err
	} else if len(text) > 10 && text[:10] == "localhost:" {
		_, err := fmt.Sscanf(text[10:], "%d", &port)
		return NewAddress(LocalIP, port), err
	}
	var ip [4]byte
	_, err := fmt.Sscanf(text, "%d.%d.%d.%d:%d", &ip[0], &ip[1], &ip[2], &ip[3], &port)
	return NewAddress(ip, port), err
}
