package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

var port = flag.Int("p", 1337, "Port at which to listen")

func main() {
	flag.Parse()

	p := NewPeer(*port, graphCallback)

	consoleLoop(p)

	p.Shutdown()
}

func consoleLoop(p *Peer) {
	scanner := bufio.NewScanner(os.Stdin)

	for scanner.Scan() {
		text := scanner.Text()
		command := text
		args := ""
		if idx := strings.Index(text, " "); idx != -1 {
			command = text[:idx]
			args = text[idx+1:]
		}
		switch command {
		case "h":
			printHelp()
		case "q":
			return
		case "l":
			p.ListNodes()
		case "cycle":
			p.AddNode(nil)
		case "c":
			connectNewNode(p, args)
		case "cm":
			connectMany(p, args)
		case "d":
			disconnectNode(p, args)
		case "da":
			p.DisconnectAllNodes()
		case "b":
			p.Broadcast(args)
		case "g":
			p.GenerateGraph()
		case "gf":
			togglePeriodicGraphFile(p, args)
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "Error reading stdio:", err)
	}
}

func printHelp() {
	fmt.Println("HELP")
	fmt.Println("- h (print this help)")
	fmt.Println("- q (QUIT)")
	fmt.Println("- l (list all nodes)")
	fmt.Println("- cycle (create a node that points to itself)")
	fmt.Println("- c <ip> <port> (connect new node to <ip> <port>)")
	fmt.Println("- cm <num> <ip> <port> (connect many (num))")
	fmt.Println("- d [<#location>] (disconnect node on <#location>)")
	fmt.Println("- da (disconnect all)")
	fmt.Println("- b <text> (broadcast <text>)")
	fmt.Println("- g (generate graph)")
	fmt.Println("- gf [<intervall>] (periodically generate graph to file (in ms))")
}

func connectNewNode(p *Peer, text string) {
	addr := ParseAddress(strings.Split(text, " "))
	if addr == nil {
		return
	}
	p.AddNode(addr)
}

func connectMany(p *Peer, text string) {
	parts := strings.Split(text, " ")
	if len(parts) != 3 {
		fmt.Fprintln(os.Stderr, "Error: parameter needs to have form '<num> <ip> <port>'")
		return
	}

	num, err := strconv.Atoi(parts[0])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error parsing: <num> (%s) is not a number\n", parts[0])
		return
	}

	addr := ParseAddress(parts[1:])
	if addr == nil {
		return
	}

	for i := 0; i < num; i++ {
		p.AddNode(addr)
	}
}

func disconnectNode(p *Peer, text string) {
	loc := Location(255)
	if text != "" {
		idx, err := strconv.Atoi(text)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error parsing: <#location> (%s) is not a number!\n", err)
			return
		}
		if idx < 0 || idx >= 254 {
			fmt.Fprintf(os.Stderr, "Invalid <#location>!\n")
			return
		}
		loc = Location(idx)
	}

	if !p.DisconnectNode(loc) {
		fmt.Fprintf(os.Stderr, "Invalid <#location>!\n")
		return
	}
}

// graph ticker
var ticker *time.Ticker

func togglePeriodicGraphFile(p *Peer, sInterval string) {
	// Stop ticker, if its already running
	if ticker != nil {
		ticker.Stop()
		ticker = nil
		fmt.Println("Deactivated periodic graph query")
		return
	}

	// find out interval
	interval := 1000
	if sInterval != "" {
		var err error
		interval, err = strconv.Atoi(sInterval)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Parse Error: %s\n", err)
			return
		}
	}

	// periodically generate graph
	fmt.Printf("Activating periodic graph query (interval: %d ms)", interval)
	ticker = time.NewTicker(time.Duration(interval) * time.Millisecond)
	go func(t *time.Ticker) {
		for _ = range t.C {
			p.GenerateGraph()
		}
	}(ticker)
}

type jsonEntry struct {
	Id string
}

func writeJSONGraph(g []*NodeAttr) {
	var jG []jsonEntry
	for _, n := range g {
		jG = append(jG, jsonEntry{strconv.Itoa(n.Addr.Port()) + ":" + strconv.Itoa(int(n.Loc))})
	}

	data, err := json.Marshal(&jG)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error marshalling JSON: %s\n", err)
		return
	}

	f, err := os.Create("graph.json")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating file: %s\n", err)
		return
	}
	defer f.Close()

	_, err = f.Write(data)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error writing file: %s\n", err)
		return
	}
}

func graphCallback(g []*NodeAttr) {
	if ticker != nil {
		writeJSONGraph(g)
		return
	}
	fmt.Println("Graph:")
	for _, n := range g {
		fmt.Print(" -> (", n.Addr.Port(), ":", n.Loc, ")")
	}
	fmt.Print("\n")
}
