// client
package main

import (
	"bufio"
	"flag"
	"fmt"
	"net"
	"os"
	"strconv"
	"strings"

	"../client/protocol"
)

var (
	ip   = flag.String("ip", "localhost", "Server Ip Address")
	port = flag.Int("port", 1337, "Server port")
	//msg  = flag.String("msg", "", "Message to send. if empty, input stream is being send")
)

func parseStdIO(pc *protocol.Connection) {
	scanner := bufio.NewScanner(os.Stdin)

	for scanner.Scan() {
		text := scanner.Text()
		if idx := strings.Index(text, " "); idx != -1 {
			command := text[:idx]
			text = text[idx+1:]
			switch command {
			case "j":
				pc.WriteJoinTopics(strings.Split(text, ","))
			case "l":
				pc.WritePartTopics(strings.Split(text, ","))
			case "b":
				pc.WriteBroadCast(text)
			default:
				topics := strings.Split(command, ",")
				pc.WriteMessage(topics, text)
			}
		} else {
			switch text {
			case "t":
				pc.WriteAskTopics()
			case "q":
				pc.Close()
			}
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "Error reading stdio:", err)
	}
}

func main() {
	flag.Parse()

	c, err := net.Dial("tcp", *ip+":"+strconv.Itoa(*port))
	if err != nil {
		fmt.Fprintln(os.Stderr, "Dialing Error: ", err)
		return
	}
	defer c.Close()

	fmt.Println("COMMANDS")
	fmt.Println(" - q (QUIT)")
	fmt.Println(" - t (ask for topics list)")
	fmt.Println(" - j <channel> (join <channel>)")
	fmt.Println(" - l <channel> (leave <channel>)")
	fmt.Println(" - b <message> (broadcast <message>)")
	fmt.Println(" - <channel> <message> (send <message> to <channel>)")

	pc := protocol.NewConnection(c)

	go parseStdIO(pc)

	for {
		p, err := pc.ReadPackage()
		if err != nil {
			fmt.Fprintln(os.Stderr, "Error reading package: ", err)
			return
		}
		switch p.Flags {
		//case protocol.FlagJoin:
		//case protocol.FlagPart:
		//case protocol.FlagTopicAsk:
		case protocol.FlagTopicReceive:
			fmt.Printf("[TOPICS] %s\n", p.Topics)
		case protocol.FlagMessage:
			fmt.Printf("%v %s\n", p.Topics, p.Message)
		case protocol.FlagBinary:
			fmt.Printf("(b)%v %s\n", p.Topics, p.Message)
		case protocol.FlagBroadCast:
			fmt.Printf("[BROADCAST] %s\n", p.Message)
		}
	}
}
